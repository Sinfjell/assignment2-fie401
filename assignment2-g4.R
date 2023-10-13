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

# Obtain heteroskedasticity-robust standard errors
robust_se <- coeftest(homoscedastic_model, vcov = vcovHC(homoscedastic_model, type = "HC3"))[, 2]

# Obtain stock-clustered standard errors
clustered_se <- coeftest(homoscedastic_model, vcov = vcovCL(homoscedastic_model, cluster = ~ ticker, data = data))[, 2]

# Generate regression table
stargazer(homoscedastic_model, homoscedastic_model, homoscedastic_model,
          se = list(NULL, robust_se, clustered_se),
          title = "Regression Table",
          header = FALSE,
          model.names = FALSE,
          omit.stat = "all",
          column.labels = c("Homoskedastic", "Heteroskedasticity-Corrected", "Stock-Clustered"),
          type = "text")

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