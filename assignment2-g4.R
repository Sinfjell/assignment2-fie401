# Set WD
setwd("/Users/sinfjell/Library/CloudStorage/OneDrive-NorgesHandelshøyskole/FIE401/Assignment-2/github")

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
data_original <- read.csv("SVI.csv")

# Remove missing observations and format date
data <- na.omit(data)
data$date <- as.Date(data$date, format="%d%b%Y")
# Declare the dataset to be a panel data
data <- pdata.frame(data, index = c("ticker","date"))

# How many rows were removed? 
original_rows <- nrow(data_original)
cleaned_rows <- nrow(data)
rows_removed <- original_rows - cleaned_rows
print(paste("Original dataset is", original_rows, "rows long, while this dataset is",
            cleaned_rows, "rows long. Meaning", rows_removed, "rows were removed."))


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
          type = "text", report = "vc*t", 
          covariate.labels = c("Stock returns (ABS)"), 
          dep.var.labels = "Log of Google Searchs")

# Plot
# Preparing predictions from the model
# Convert panel data variables to standard vector format for plotting
RET_vector <- as.vector(data$RET)
ln_SVI_vector <- as.vector(data$ln_SVI)

# Preparing predictions from the model
predicted_vector <- as.vector(predict(homoscedastic_model, data))

# Plotting the graph
plot(RET_vector, ln_SVI_vector,
     xlab = "Stock returns",
     ylab = "Log-adjusted Google Searches",
     main = "Google Searches based on the ticker returns",
     pch = 20)
points(RET_vector, predicted_vector, col = "blue", pch = 20)  # Adding predicted points 

# Task 2 ------------------------------------------------------------------
# Prepare data for regression analysis
manipulated_data <- data %>%
  group_by(ticker) %>%
  arrange(date) %>%
  mutate(ln_SVI_lag = lag(log(1 + SVI), order_by = date))  # Compute ln(1 + SVI)_lag correctly using order_by

# Convert the result back to a panel data object
data <- pdata.frame(manipulated_data, index = c("ticker", "date"))

# Handle missing values in ln_SVI_lag
data <- data %>% filter(!is.na(ln_SVI_lag))

data$Abs_vwretd <- abs(data$vwretd)  # Compute absolute value of market return

# Estimate Model 1
model_1 <- lm(ln_SVI ~ Abs_RET + Abs_vwretd, data = data)

# Cluster standard errors at stock level for Model 1
clustered_se_model_1 <- coeftest(model_1, vcov = vcovCL(model_1, cluster = ~ ticker, data = data))[, 2]

# Estimate Model 2
model_2 <- lm(ln_SVI ~ Abs_RET + ln_SVI_lag, data = data)

# Cluster standard errors at stock level for Model 2
clustered_se_model_2 <- coeftest(model_2, vcov = vcovCL(model_2, cluster = ~ ticker, data = data))[, 2]

# Estimate Model 3
model_3 <- lm(ln_SVI ~ Abs_RET + Abs_vwretd + ln_SVI_lag, data = data)

# Cluster standard errors at stock level for Model 3
clustered_se_model_3 <- coeftest(model_3, vcov = vcovCL(model_3, cluster = ~ ticker, data = data))[, 2]

# Create a regression table
stargazer(model_1, model_2, model_3,
          se = list(clustered_se_model_1, clustered_se_model_2, clustered_se_model_3),
          title = "Regression Table with Controls",
          header = FALSE,
          model.names = FALSE,
          omit.stat = "all",
          column.labels = c("Model 1", "Model 2", "Model 3"),
          type = "text", report = "vc*t", 
          covariate.labels = c("Stock returns (ABS)", "Index return (ABS)",
          "Log of Google Searches yesterday"), 
          dep.var.labels = "Log of Google Searches")

# Visualization for Model 2

# Step 1: No changes needed as ln_SVI_lag is now correctly computed
fit_1 <- lm(ln_SVI ~ ln_SVI_lag, data = data)
data$y_res <- residuals(fit_1)

# Step 2: Regress |RET| on ln(1 + SVI)_lag, save residuals
fit_2 <- lm(Abs_RET ~ ln_SVI_lag, data = data)
data$x_res <- residuals(fit_2)

# Step 3: Regress y_res on x_res, save predicted values
fit_3 <- lm(y_res ~ x_res, data = data)
data$y_res_hat <- predict(fit_3, data)
summary(fit_3)


# Step 4: Make a scatterplot
x_res_vector <- as.vector(data$x_res)
y_res_vector <- as.vector(data$y_res)

plot(x_res_vector, y_res_vector,
     xlab = "Rediuals of Stock Return",
     ylab = "Residuals of Google Search",
     main = "Scatterplot with Predicted Values",
     pch = 20)

abline(fit_3, col = "red")  # Adding regression line
points(data$x_res, data$y_res_hat, col = "blue", pch = 20)  # Adding predicted points

# Task 3 ------------------------------------------------------------------
# Model 1: Stock Fixed Effects
model_1_fe <- plm(ln_SVI ~ Abs_RET + Abs_vwretd + ln_SVI_lag, data = data, model = "within", effect = "individual")

# Cluster standard errors at stock level for Model 1
clustered_se_model_1 <- coeftest(model_1_fe, vcov = vcovHC(model_1_fe, cluster = "group", type = "sss"))[, 2]

# Model 2: Day Fixed Effects
model_2_fe <- plm(ln_SVI ~ Abs_RET + Abs_vwretd + ln_SVI_lag, data = data, model = "within", effect = "time")

# Cluster standard errors at stock level for Model 2
clustered_se_model_2 <- coeftest(model_2_fe, vcov = vcovHC(model_2_fe, cluster = "group", type = "sss"))[, 2]

# Model 3: Both Stock and Day Fixed Effects
# Example using lm() for two-way fixed effects
#model_3_fe <- plm(ln_SVI ~ Abs_RET + Abs_vwretd + ln_SVI_lag, data = data, model = "within", effect = "twoway")

# We are utilizing the `felm` function from the `lfe` package for Model 3 instead of the `plm` function from the `plm` package.
# This is due to the encountered issue with `plm` when specifying `effect = "twoway"` for both stock and day fixed effects,
# which led to a loading issue. The `felm` function facilitates the inclusion of two-way fixed effects seamlessly and also 
# allows for the computation of heteroskedasticity-robust standard errors, making it a suitable alternative for this analysis.

# Load the lfe package
library(lfe)

# Estimate your model using felm
model_3_fe <- felm(ln_SVI ~ Abs_RET + Abs_vwretd + ln_SVI_lag | factor(ticker) + factor(date), data = data)

# Obtain clustered standard errors
clustered_se_model_3 <- summary(model_3_fe, robust = TRUE)$coefficients[,2]

# Create a regression table
stargazer(model_1_fe, model_2_fe, model_3_fe,
          se = list(clustered_se_model_1, clustered_se_model_2, clustered_se_model_3),
          title = "Regression Table with Controls and Fixed Effects",
          header = TRUE,
          model.names = FALSE,
          omit.stat = "all",
          type = "text", report = "vc*t", 
          covariate.labels = c("Stock returns (ABS)", "Index return (ABS)", 
                               "Lagged Log of Google Searches"), 
          dep.var.labels = "Log of Google Searches")

