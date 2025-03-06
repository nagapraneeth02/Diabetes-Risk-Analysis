install.packages("nnet")
install.packages("caret")
install.packages("dplyr")
install.packages("MASS")
install.packages("gridExtra")

# Load the dplyr package
library(dplyr)
library(nnet)
library(caret)
library(ggplot2)
library(MASS)
library(randomForest)
warnings()

data <- read.csv("C:/Users/prane/Downloads/Dataset of Diabetes .csv")
data
head(data)
# check for null values
sum(is.na(data))  

#check for duplicate rows
duplicate_rows <- duplicated(data)
num_duplicate_rows <- sum(duplicate_rows)
print(num_duplicate_rows)

print(colnames(data))

# List of columns to scale
columns_to_scale <- c('AGE', 'Urea', 'Cr', 'HbA1c', 'Chol', 'TG', 'HDL', 'LDL', 'VLDL', 'BMI')

# Applying the scale function to only the columns specified in columns_to_scale
data[, columns_to_scale] <- scale(data[, columns_to_scale])

#Q-Q plots
columns <- c("ID", "No_Pation", "Gender", "AGE", "Urea", "Cr", "HbA1c", "Chol", "TG", "HDL", "LDL", "VLDL", "BMI", "CLASS")

# Create Q-Q plots
par(mfrow = c(4, 4)) 

for (column in columns) {
  if(is.numeric(data[[column]])) {
    # Create a Q-Q plot for each column
    qqnorm(data[[column]], main = paste("Q-Q Plot of", column))
    qqline(data[[column]], col = "red")
  }
}

# Categorical to numerical values
data$CLASS <- trimws(data$CLASS)
data$CLASS <- case_when(
  data$CLASS == 'Y' ~ 1,
  data$CLASS == 'N' ~ 0,
  data$CLASS == 'P' ~ 2)
data$Gender <- case_when(
  data$Gender == 'F'~ 1,
  data$Gender == 'f'~ 1,
  data$Gender == 'M' ~ 0)
data
dim(data)
head(data)
tail(data,40)
data$HbA1c

# T-Test
class_column <- data$CLASS

# Function to perform t-test for a given column
perform_t_test <- function(column) {
  t_test_result <- t.test(column, class_column)
  return(t_test_result$p.value)
}

# Get numeric column names (excluding 'CLASS')
numeric_columns <- names(data)[sapply(data, is.numeric) & names(data) != "CLASS"]

# Loop through each numeric column and print the p-value
for (col in numeric_columns) {
  p_value <- perform_t_test(data[[col]])
  cat("Column:", col, "  p-value:", p_value, "\n")
}


#descriptive statistics
summary(data)

#Histogram distribution
par(mfrow=c(1, 3))

# Histogram for AGE
hist(data$AGE, main="Distribution of AGE", xlab="AGE", col="skyblue", border="black")

# Histogram for BMI
hist(data$BMI, main="Distribution of BMI", xlab="BMI", col="lightgreen", border="black")

# Histogram for HbA1c
hist(data$HbA1c, main="Distribution of HbA1c", xlab="HbA1c", col="lightcoral", border="black")

# Reset the layout
par(mfrow=c(1, 1))


par(mfrow=c(1, 3))
# Histogram for UREA
hist(data$Urea, main="Distribution of Urea", xlab="Urea", col="skyblue", border="black")

# Histogram for Cr
hist(data$Cr, main="Distribution of Cr", xlab="Cr", col="lightgreen", border="black")

# Histogram for Chol
hist(data$Chol, main="Distribution of Chol", xlab="Chol", col="lightcoral", border="black")

par(mfrow=c(1, 1))

par(mfrow=c(1, 3))
# Histogram for TG
hist(data$TG, main="Distribution of Trigleceraldehydes", xlab="TG", col="skyblue", border="black")

# Histogram for HDL
hist(data$HDL, main="Distribution of HDL", xlab="HDL", col="lightgreen", border="black")

# Histogram for LDL
hist(data$LDL, main="Distribution of LDL", xlab="LDL", col="lightcoral", border="black")

par(mfrow=c(1, 1))

# Histogram for VLDL
hist(data$VLDL, main="Distribution of VLDL", xlab="VLDL", col="lightcoral", border="black")

# Number of numeric columns (excluding CLASS)
num_cols = sum(sapply(data, is.numeric)) - 1

# Set the plot area to display multiple plots
par(mfrow=c(num_cols, 1))
for(column in colnames(data)) {
  if(column != "CLASS" && is.numeric(data[[column]])) {
    p <- ggplot(data, aes_string(x="CLASS", y=column)) + geom_boxplot()
    ggsave(paste("plot_", column, ".png", sep=""), plot=p)
  }
}

for(column in colnames(data)) {
  if(column != "CLASS" && is.numeric(data[[column]])) {
    p <- ggplot(data, aes_string(x="CLASS", y=column)) + geom_boxplot()
    print(p)
  }
}

summary(data$AGE)
# replace outliers with IQR range
# Function to replace outliers with IQR bounds
replace_outliers_with_iqr <- function(column) {
  Q1 <- quantile(data[[column]], 0.25)
  Q3 <- quantile(data[[column]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 #- 1.5 * IQR
  upper_bound <- Q3 #+ 1.5 * IQR
  
  # Replace values below the lower bound with the lower bound
  data[[column]][data[[column]] < lower_bound] <- lower_bound
  
  # Replace values above the upper bound with the upper bound
  data[[column]][data[[column]] > upper_bound] <- upper_bound
}

# Apply the function to BMI, AGE, and HbA1c columns
replace_outliers_with_iqr("BMI")
replace_outliers_with_iqr("AGE")
replace_outliers_with_iqr("HbA1c")

# View the modified data
head(data)
# Box plot for BMI
ggplot(data, aes(y = BMI)) +
  geom_boxplot() +
  labs(title = "Box Plot of BMI", y = "BMI")

# Box plot for AGE
ggplot(data, aes(y = AGE)) +
  geom_boxplot() +
  labs(title = "Box Plot of AGE", y = "AGE")

# Box plot for HbA1c
ggplot(data, aes(y = HbA1c)) +
  geom_boxplot() +
  labs(title = "Box Plot of HbA1c", y = "HbA1c")

# Loop through all columns and perform the Shapiro-Wilk test
results <- lapply(data[c("AGE", "Urea", "Cr", "HbA1c", "Chol", "TG", "HDL", "LDL", "VLDL", "BMI")], function(column) {
  # Check if the sample size is appropriate for Shapiro-Wilk test
  if (length(column) > 5000 || length(column) < 3) {
    return(c(Statistic = NA, p_value = NA))
  } else {
    test_result <- shapiro.test(column)
    return(c(Statistic = test_result$statistic, p_value = test_result$p.value))
  }
})

# Convert the list to a Dataframe for easier reading
normality_test_results <- do.call(rbind, results)
rownames(normality_test_results) <- c("AGE", "Urea", "Cr", "HbA1c", "Chol", "TG", "HDL", "LDL", "VLDL", "BMI")

# Print the results
print(normality_test_results)

data$Gender

#Chi-sq Test 
# Ensure that the 'Gender' and 'CLASS' columns are factors
data$Gender <- factor(data$Gender)
data$CLASS <- factor(data$CLASS)

# Create the contingency table
gender_table <- table(data$Gender, data$CLASS)

# Perform the chi-square test
chisq_result <- chisq.test(gender_table)

# Print the result
print(chisq_result)

# Create a bar plot for the count of each category in 'CLASS'
ggplot(data, aes(x = CLASS)) +
  geom_bar() +
  labs(title = "Count of Each Category in CLASS", x = "CLASS", y = "Count") +
  theme_minimal()

#FEATURE IMPORTANCE
data$CLASS <- as.factor(data$CLASS)

# Split the data into features and target variable
features <- data[, -which(names(data) == "CLASS")]
target <- data$CLASS

# Train a Random Forest model
rf_model <- randomForest(features, target, importance = TRUE, ntree = 500)

# View feature importance
importance(rf_model)

# You can also create a plot for feature importance
varImpPlot(rf_model)

#split data into feature and target variable
features <- c("BMI", "AGE", "HbA1c")
X <- data[features]

y <- data$CLASS

# Split the data into training and test sets
set.seed(42) 
trainIndex <- createDataPartition(y, p = .8, list = FALSE, times = 1)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Fit the multinomial logistic regression model
set.seed(42) 
multinom_model <- multinom(CLASS ~ BMI + AGE + HbA1c, data = data.frame(X_train, CLASS = as.factor(y_train)))

# Summary of the model
summary_model <- summary(multinom_model)
print(summary_model$r.squared)
# Make predictions on the test set
y_pred <- predict(multinom_model, newdata = X_test)

# Create a confusion matrix
conf_matrix <- confusionMatrix(as.factor(y_pred), as.factor(y_test))

# Print the confusion matrix
print(conf_matrix)


# Ensure you have the caret package loaded
library(caret)

# Assuming you have already created the conf_matrix object using the confusionMatrix function
# Extract the accuracy, lower and upper bounds of the confidence interval
accuracy <- conf_matrix$overall['Accuracy']
lower_ci <- conf_matrix$overall['AccuracyLower']
upper_ci <- conf_matrix$overall['AccuracyUpper']

# Print them out to check if they are finite numbers
print(accuracy)
print(lower_ci)
print(upper_ci)

# Assuming they are correct, we plot them
# Plot the CI for accuracy if they are not NA or infinite
if(all(is.finite(c(lower_ci, upper_ci)))) {
  plot(c(1, 1), c(lower_ci, upper_ci), xlim=c(0.5, 1.5), type='l', xaxt='n', ylab='Accuracy', xlab='', 
       main='95% CI for Accuracy')
  points(1, accuracy, col='blue', pch=19)
  arrows(1, lower_ci, 1, upper_ci, code=3, angle=90, length=0.05)
  
  # Add a text label for accuracy
  text(1, accuracy, labels = paste("Accuracy:", round(accuracy, 4)), pos=3)
} else {
  cat("The lower and upper CI values are not finite.")
}

# Load the necessary library
library(ggplot2)

#scatter plot
data$CLASS <- as.factor(data$CLASS)  # Make sure CLASS is a factor

# BMI vs. CLASS
p1 <- ggplot(data, aes(x=CLASS, y=BMI, color=CLASS)) +
  geom_jitter(width=0.2, height=0) +
  labs(title="BMI vs CLASS")

# AGE vs. CLASS
p2 <- ggplot(data, aes(x=CLASS, y=AGE, color=CLASS)) +
  geom_jitter(width=0.2, height=0) +
  labs(title="AGE vs CLASS")

# HbA1c vs. CLASS
p3 <- ggplot(data, aes(x=CLASS, y=HbA1c, color=CLASS)) +
  geom_jitter(width=0.2, height=0) +
  labs(title="HbA1c vs CLASS")
print(p3)

# Use the 'gridExtra' package to arrange the plots into a grid
library(gridExtra)
grid.arrange(p1, p2, p3, ncol=3)



