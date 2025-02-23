

setwd("/Users/charliebroveleit/Downloads/Data Mining")

install.packages("caTools")
library(caTools)
library(ggplot2)
library(caret)

df <- read.csv("admit.csv")

# Check structure and summary
str(df)
summary(df)

# Check class balance
table(df$admit) / nrow(df)

# Plot GRE score distribution
ggplot(df, aes(x = gre)) + 
  geom_histogram(binwidth = 20, fill = "blue", alpha = 0.7, color = "black") +
  theme_minimal() +
  ggtitle("Distribution of GRE Scores")

# Split data into training (80%) and testing (20%) sets
set.seed(42)
split <- sample.split(df$admit, SplitRatio = 0.8)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

# Fit logistic regression model
model <- glm(admit ~ gre + gpa + rank, data = train, family = binomial)

# Model summary
summary(model)

# Make predictions on test set
probabilities <- predict(model, test, type = "response")
predictions <- ifelse(probabilities > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = test$admit)
print(conf_matrix)

# Calculate performance metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])  # TP / (TP + FP)
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])  # TP / (TP + FN)

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")

# Variable importance based on coefficients
exp(coef(model))  # Odds ratio interpretation
------------------------
  #Confirmed imbalance (~68% not admitted)
  #The distribution of GRE scores appears to be approximately normal with a mean of 587.7 and a standard deviation of 115.52. The Distribution is pretty normal, with little to no skew.
  #GPA is the most influential variable because the higher GPA holds the most weight into getting someone accepted. Rank would also be one of the more influential variables because attending a high ranked school increases your odds of getting admitted.
-------------------------