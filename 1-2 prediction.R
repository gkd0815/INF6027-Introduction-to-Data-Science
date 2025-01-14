##########################11111111111111111

# Employment and Unemployment Rate Plot
# Load data from CSV file
data <- read.csv("/Users/gkd0815/Documents/Data Visualization/R语言/data set/employment and unemployment rate.csv")

# Print column names to verify if correct
print(colnames(data))

# Extract year from the data, ignoring quarter information
data$Years <- as.numeric(sub(" Q[1-4]", "", data$Years))

# Column names for employment and unemployment rates
employment_col <- "Employment.rate"
unemployment_col <- "Unemployment.rate"

# Scale down employment and unemployment rates to millions
data$Employment.rate <- data$Employment.rate / 1e6
data$Unemployment.rate <- data$Unemployment.rate / 1e6

# Remove rows with missing values
data <- na.omit(data)

# Sort data by year for consistency
data <- data[order(data$Years), ]

# Split data: 80% for training, 20% for testing
train_size <- floor(0.8 * nrow(data))
train_data <- data[1:train_size, ]
test_data <- data[(train_size + 1):nrow(data), ]

# Fit polynomial regression models (degree 2)
employment_model <- lm(Employment.rate ~ poly(Years, 2), data = train_data)
unemployment_model <- lm(Unemployment.rate ~ poly(Years, 2), data = train_data)

# Predict on test data
test_data$Predicted_Employment <- predict(employment_model, newdata = test_data)
test_data$Predicted_Unemployment <- predict(unemployment_model, newdata = test_data)

# Predict future data from the last year to 2050
future_years <- data.frame(Years = seq(max(data$Years) + 1, 2050))
future_employment <- predict(employment_model, newdata = future_years)
future_unemployment <- predict(unemployment_model, newdata = future_years)

# Combine historical, test, and future data for plotting
all_years <- c(data$Years, test_data$Years, future_years$Years)
all_employment <- c(data$Employment.rate, test_data$Predicted_Employment, future_employment)
all_unemployment <- c(data$Unemployment.rate, test_data$Predicted_Unemployment, future_unemployment)

# Plot the data
plot(all_years, all_employment, type = "n", 
     ylab = "Rate (millions)", xlab = "Year", 
     main = "Employment and Unemployment Rates", 
     ylim = range(all_employment, all_unemployment), xlim = range(all_years))
lines(data$Years, data$Employment.rate, col = "blue", lwd = 2)
lines(data$Years, data$Unemployment.rate, col = "red", lwd = 2)
lines(test_data$Years, test_data$Predicted_Employment, col = "darkblue", lty = 2, lwd = 3)
lines(test_data$Years, test_data$Predicted_Unemployment, col = "darkred", lty = 2, lwd = 3)
lines(future_years$Years, future_employment, col = "blue", lty = 3, lwd = 3)
lines(future_years$Years, future_unemployment, col = "red", lty = 3, lwd = 3)

# Place the legend below the plot, vertically aligned, without a box, and with reduced text size
par(xpd = TRUE)
legend("bottom", inset = -0.25, legend = c("Employment Rate (Historical)", 
                                           "Unemployment Rate (Historical)", 
                                           "Employment Rate (Predicted)", 
                                           "Unemployment Rate (Predicted)", 
                                           "Employment Rate (Future Prediction)", 
                                           "Unemployment Rate (Future Prediction)"),
       col = c("blue", "red", "darkblue", "darkred", "blue", "red"), 
       lty = c(1, 1, 2, 2, 3, 3), 
       lwd = c(2, 2, 3, 3, 3, 3), 
       cex = 0.4, bty = "n", ncol = 1)

#########################22222222222222222222222


# Employment Rate by Gender Prediction Plot
# Load data from CSV file
data <- read.csv("/Users/gkd0815/Documents/Data Visualization/R语言/data set/Sex and population and years.csv")

# Print column names to verify if correct
print(colnames(data))

# Extract year from the data, ignoring quarter information
data$Years <- as.numeric(sub(" Q[1-4]", "", data$Years))

# Define columns for male and female employment rates
male_col <- "Male"
female_col <- "Female"
population_col <- "Population"

# Scale down population to millions
data$Scaled_Population <- data$Population / 1e6

# Remove rows with missing values
data <- na.omit(data)

# Sort data by year for consistency
data <- data[order(data$Years), ]

# Split data: 80% for training, 20% for testing
train_size <- floor(0.8 * nrow(data))
train_data <- data[1:train_size, ]
test_data <- data[(train_size + 1):nrow(data), ]

# Fit polynomial regression models (degree 2) for male and female employment rates
male_model <- lm(Male ~ poly(Years, 2), data = train_data)
female_model <- lm(Female ~ poly(Years, 2), data = train_data)

# Predict on test data
test_data$Predicted_Male <- predict(male_model, newdata = test_data)
test_data$Predicted_Female <- predict(female_model, newdata = test_data)

# Predict future data from the last year to 2050
future_years <- data.frame(Years = seq(max(data$Years) + 1, 2050))
future_male <- predict(male_model, newdata = future_years)
future_female <- predict(female_model, newdata = future_years)

# Combine historical, test, and future data for plotting
all_years <- c(data$Years, test_data$Years, future_years$Years)
all_male <- c(data$Male, test_data$Predicted_Male, future_male)
all_female <- c(data$Female, test_data$Predicted_Female, future_female)
all_population <- c(data$Scaled_Population, rep(NA, length(test_data$Years) + length(future_years$Years)))

# Plot the data
plot(all_years, all_male, type = "n", ylab = "Rate (millions)", xlab = "Year", main = "Employment Rates by Gender", ylim = range(all_male, all_female, na.rm = TRUE), xlim = range(all_years))
lines(data$Years, data$Male, col = "blue", lwd = 2)
lines(data$Years, data$Female, col = "red", lwd = 2)
lines(test_data$Years, test_data$Predicted_Male, col = "darkblue", lty = 2, lwd = 3)
lines(test_data$Years, test_data$Predicted_Female, col = "darkred", lty = 2, lwd = 3)
lines(future_years$Years, future_male, col = "blue", lty = 3, lwd = 3)
lines(future_years$Years, future_female, col = "red", lty = 3, lwd = 3)

# Place the legend below the plot, vertically aligned, without a box, and with reduced text size
par(xpd = TRUE)
legend("bottom", inset = -0.25, legend = c("Male Employment Rate (Historical)", "Female Employment Rate (Historical)", "Male Employment Rate (Predicted)", "Female Employment Rate (Predicted)", "Male Employment Rate (Future Prediction)", "Female Employment Rate (Future Prediction)"), col = c("blue", "red", "darkblue", "darkred", "blue", "red"), lty = c(1, 1, 2, 2, 3, 3), lwd = c(2, 2, 3, 3, 3, 3), cex = 0.4, bty = "n", ncol = 1)


