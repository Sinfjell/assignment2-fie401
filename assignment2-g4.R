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
date_original <- read.csv("SVI.csv")

# Remove missing observations and format date
data <- na.omit(data)
data$date <- as.Date(data$date, format="%d%b%Y")
# Declare the dataset to be a panel data
data <- pdata.frame(data, index = c("ticker","date"))



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
     xlab = "RET",
     ylab = "ln(1 + SVI)",
     main = "Plot with Predictions from the Model",
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


# Step 4: Make a scatterplot
x_res_vector <- as.vector(data$x_res)
y_res_vector <- as.vector(data$y_res)

plot(x_res_vector, y_res_vector,
     xlab = "x_res",
     ylab = "y_res",
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
model_3_fe <- plm(ln_SVI ~ Abs_RET + Abs_vwretd + ln_SVI_lag, data = data, model = "within", effect = "twoway")

# Cluster standard errors at stock level for Model 3
clustered_se_model_3 <- coeftest(model_3_fe, vcov = vcovHC(model_3_fe, cluster = "group", type = "sss"))[, 2]

# Create a regression table
stargazer(model_1_fe, model_2_fe, model_3_fe,
          se = list(clustered_se_model_1, clustered_se_model_2, clustered_se_model_3),
          title = "Regression Table with Controls and Fixed Effects",
          header = FALSE,
          model.names = FALSE,
          omit.stat = "all",
          add.lines = list(c('Stock FE', 'Yes', 'No', 'Yes'),
                           c('Day FE', 'No', 'Yes', 'Yes')),
          column.labels = c("Model 1 (Stock FE)", "Model 2 (Day FE)", "Model 3 (Stock & Day FE)"),
          type = "text", report = "vc*t")

