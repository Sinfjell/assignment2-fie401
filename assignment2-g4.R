# Load necessary libraries
require(DescTools)
require(plm)
require(dplyr)
require(lmtest)
library(sandwich)
require(stargazer)
require(tidyverse)

# Load the data
data <- read.csv("SVI.csv")

# Remove missing observations and format date
data <- na.omit(data)
data$date <- as.Date(data$date, format="%d%b%Y")

# Declare the dataset to be a panel data
pdata <- pdata.frame(data, index = c("ticker","date"))

head(data)
summary(data)


# Task 1 ------------------------------------------------------------------
# Prepare your data
data$Abs_RET <- abs(data$RET)  # Compute absolute value of returns
data$ln_SVI <- log(1 + data$SVI)  # Compute ln(1 + SVI)

# Model with homoscedastic standard errors
homoscedastic_model <- lm(ln_SVI ~ Abs_RET, data = data)

# Model with heteroscedasticity-corrected standard errors
heteroscedasticity_corrected_errors <- coeftest(homoscedastic_model, vcov = vcovHC(homoscedastic_model, type = "HC3"))

# Model with stock-clustered standard errors
# Assuming 'stock' is the variable identifying different stocks
stock_clustered_errors <- coeftest(homoscedastic_model, vcov = vcovCL(homoscedastic_model, cluster = ~ stock, data = data))

# Output results in a summary table
stargazer(homoscedastic_model, heteroscedasticity_corrected_errors, stock_clustered_errors, type="text", se=list(NULL, vcovHC(homoscedastic_model, type = "HC3"), vcovCL(homoscedastic_model, cluster = ~ stock, data = data)))

# Plot
# Preparing predictions from the model
data$predicted <- predict(homoscedastic_model, data)

# Plotting the graph
plot(data$RET, data$ln_SVI,
     xlab = "RET",
     ylab = "ln(1 + SVI)",
     main = "Plot with Predictions from the Model",
     pch = 20)
abline(homoscedastic_model, col = "red")  # Adding regression line
points(data$RET, data$predicted, col = "blue", pch = 20)  # Adding predicted points