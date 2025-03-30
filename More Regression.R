#1

library(ggplot2)
library(readxl)

data <- read_excel("/Users/charliebroveleit/Downloads/Data Mining/wages.xlsx")

str(data)
head(data)

ggplot(data, aes(x = Age, y = Wage)) +
  geom_point(color = "blue") +
  labs(title = "Wage vs Age", x = "Age", y = "Wage") +
  theme_minimal()

linear_model <- lm(Wage ~ Age, data = data)
summary(linear_model)

quadratic_model <- lm(Wage ~ poly(Age, 2), data = data)
summary(quadratic_model)

anova(linear_model, quadratic_model)

ggplot(data, aes(x = Age, y = Wage)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +  # Linear fit
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "green", se = FALSE) +  # Quadratic fit
  labs(title = "Linear vs Quadratic Fit", x = "Age", y = "Wage") +
  theme_minimal()


#2
multi_model <- lm(Wage ~ Age + Educ, data = data)

summary(multi_model)

interaction_model <- lm(Wage ~ Age * Educ, data = data)
summary(interaction_model)

anova(multi_model, interaction_model)

ggplot(data, aes(x = Age, y = Wage, color = factor(Educ))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Wage vs Age by Education Level", x = "Age", y = "Wage", color = "Education Level") +
  theme_minimal()

#3
quad_model <- lm(Wage ~ Age + I(Age^2) + Educ, data = data)

summary(quad_model)

linear_model <- lm(Wage ~ Age + Educ, data = data)  # Linear model for comparison
anova(linear_model, quad_model)  # Model comparison

ggplot(data, aes(x = Age, y = Wage)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Quadratic fit
  labs(title = "Quadratic Wage vs Age Relationship", x = "Age", y = "Wage") +
  theme_minimal()

#4
quad_model <- lm(Wage ~ Age + I(Age^2) + Educ, data = data)
new_data <- data.frame(Age = c(30, 50, 70), 
                       Educ = c(16, 16, 16))
predicted_wages <- predict(quad_model, new_data)
print(predicted_wages)
results <- data.frame(Age = new_data$Age, 
                      Educ = new_data$Educ, 
                      Predicted_Wage = predicted_wages)
print(results)

#5
quad_model <- lm(Wage ~ Age + I(Age^2) + Educ, data = data)

b1 <- coef(quad_model)["Age"]
b2 <- coef(quad_model)["I(Age^2)"]

optimal_age <- -b1 / (2 * b2)
print(optimal_age)

