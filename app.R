# app.R

library(shiny)
library(nnet)

# Load model and scaling parameters
model <- readRDS("model.rds")
scale_params <- readRDS("scale_params.rds")

ui <- pageWithSidebar(
  headerPanel('Diabetes Class Predictor'),
  
  sidebarPanel(
    tags$label(h3('Input Parameters')),
    numericInput("BMI", "BMI:", value = 25),
    numericInput("AGE", "Age:", value = 40),
    numericInput("HbA1c", "HbA1c:", value = 6.5),
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')),
    verbatimTextOutput('status'),
    tableOutput('result')
  )
)

server <- function(input, output, session) {
  datasetInput <- reactive({
    req(input$submitbutton > 0)
    
    # Scale inputs using saved params
    bmi_scaled   <- (input$BMI   - scale_params$BMI$center) / scale_params$BMI$scale
    age_scaled   <- (input$AGE   - scale_params$AGE$center) / scale_params$AGE$scale
    hba1c_scaled <- (input$HbA1c - scale_params$HbA1c$center) / scale_params$HbA1c$scale
    
    new_data <- data.frame(
      BMI = as.numeric(bmi_scaled),
      AGE = as.numeric(age_scaled),
      HbA1c = as.numeric(hba1c_scaled)
    )
    
    # Predict
    prediction <- predict(model, newdata = new_data)
    probability <- predict(model, newdata = new_data, type = "probs")
    
    # Ensure it's a matrix even if single row
    if (is.vector(probability)) {
      probability <- t(as.matrix(probability))
      colnames(probability) <- levels(model$fitted.values)
    }
    
    # Build result table
    result <- data.frame(
      Prediction = dplyr::case_when(
        prediction == 0 ~ "No Diabetes (Class 0)",
        prediction == 1 ~ "Diabetes (Class 1)",
        prediction == 2 ~ "Pre-diabetic (Class 2)",
        TRUE ~ as.character(prediction)
      )
      
    )
    
    # Add probability columns dynamically (safe for 1, 2, or 3 class models)
    for (cls in colnames(probability)) {
      result[[paste0("Probability_", cls)]] <- probability[, cls]
    }
    
    return(result)
    
  })
  
  output$status <- renderPrint({
    if (input$submitbutton > 0) {
      isolate("Prediction complete.")
    } else {
      "Server ready for prediction."
    }
  })
  
  output$result <- renderTable({
    if (input$submitbutton > 0) {
      isolate(datasetInput())
    }
  })
}

shinyApp(ui = ui, server = server)
