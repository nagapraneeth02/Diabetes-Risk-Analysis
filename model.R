# model.R

library(caret)
library(nnet)

# Load and clean dataset
data <- read.csv("Dataset of Diabetes .csv")

data$CLASS <- trimws(data$CLASS)
data$CLASS <- dplyr::case_when(
  data$CLASS == 'Y' ~ 1,
  data$CLASS == 'N' ~ 0,
  data$CLASS == 'P' ~ 2
)
data$Gender <- dplyr::case_when(
  data$Gender %in% c('F', 'f') ~ 1,
  data$Gender == 'M' ~ 0
)
data$CLASS <- as.factor(data$CLASS)

# Scale selected columns and retain scaling attributes
columns_to_scale <- c("BMI", "AGE", "HbA1c")
scale_params <- lapply(columns_to_scale, function(col) {
  scaled <- scale(data[[col]])
  data[[col]] <<- as.numeric(scaled)
  list(center = attr(scaled, "scaled:center"), scale = attr(scaled, "scaled:scale"))
})
names(scale_params) <- columns_to_scale

# Save scaling parameters
saveRDS(scale_params, "scale_params.rds")

# Train multinomial logistic regression model
set.seed(42)
train_index <- createDataPartition(data$CLASS, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
model <- multinom(CLASS ~ BMI + AGE + HbA1c, data = train_data)

# Save model
saveRDS(model, "model.rds")
