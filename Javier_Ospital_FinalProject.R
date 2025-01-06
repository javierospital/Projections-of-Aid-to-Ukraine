rm(list = ls())
set.seed(543210)
Sys.setlocale("LC_ALL", "C")
### Install necessary libraries -------------------------------------------
packages <- c("purrr", "dplyr", "FNN", "tidyr", "glmnet", "randomForest", "rpart", "gbm", "plotly", "readxl", "reshape2")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
}
# Load the libraries
library(purrr)
library(dplyr)
library(FNN)
library(tidyr)
library(glmnet)
library(randomForest)
library(rpart)
library(gbm)
library(plotly)
library(readxl)
library(reshape2)
### 

### Load the data -----------------------------------------------------------
UST_data <- read_excel("~/Mac/Desktop/Data Science/UST_data.xlsx")


# (1) Data Cleaning -----------------------------------------------------
#   Selecting columns-------------------------------------------------------------------------
# Drop some variables, keep only a few core ones 
colnames(UST_data)
keep <- c( "non EU NATO member", "EU", "ID", "EU_geo", "Type of donation", "Type of Aid General", "Announcement Date", "Countries", "Allocated Total in EUR (redistributed)", "Converted Value in EUR" ) #Alt minus gives you the arrow <-
data <- UST_data[, (colnames(UST_data) %in% keep)] # %in% checks if element on the left is an element of the set, ! negates what comes afterwards
colnames(data) #shows you name of the columns
head(data,10)
dim(data)

#   Create Month.Year column ------------------------------------------------

# Re-create Month.Year by handling numeric Excel serial dates and converting them to Year-Month format
data$Month.Year <- sapply(data$`Announcement Date`, function(x) {
  # Handle numeric Excel serial dates
  if (is.numeric(as.numeric(x))) {
    date <- as.Date("1899-12-30") + as.numeric(x)
    return(format(date, "%Y-%m"))  # Extract Year-Month
  }
  
  # For character entries, assume dd/mm/yyyy and use regex to extract month and year
  else if (is.character(x)) {
    x <- gsub("until ", "", x, ignore.case = TRUE)  # Remove "until"
    
    # Extract day, month, and year using regex and format as YYYY-MM
    day <- str_extract(x, "^\\d{1,2}")  # Extract day at start
    month <- str_extract(x, "(?<=/)(\\d{1,2})(?=/)")  # Extract month
    year <- str_extract(x, "\\d{4}$")  # Extract year at end
    
    # Combine year and month if both are found
    if (!is.na(month) & !is.na(year)) {
      return(paste(year, month, sep = "-"))
    }
  }
  
  return(NA)  # Return NA if format is unrecognized
})


#   Dealing with NAs in Month.Year ------------------------------------------

# Check the percentage of NA 
mean(is.na(data$Month.Year)) * 100

# View entries in Announcement Date that resulted in NA in Month.Year
na_entries <- data$`Announcement Date`[is.na(data$Month.Year)]
unique(na_entries)

# Perform a backward fill on the Month.Year column within each Countries group
data <- data %>%
  arrange(Countries, ID) %>%  # Arrange data by country and ID to ensure order
  group_by(Countries) %>%
  mutate(Month.Year = ifelse(is.na(Month.Year), lead(Month.Year), Month.Year)) %>%
  ungroup()

# Check the percentage of NA values
mean(is.na(data$Month.Year)) * 100

# Repeat the backward fill step to ensure all consecutive NAs are filled
data <- data %>%
  group_by(Countries) %>%
  mutate(Month.Year = ifelse(is.na(Month.Year), lead(Month.Year), Month.Year)) %>%
  ungroup()


# Check the percentage of NA values
mean(is.na(data$Month.Year)) * 100

# Perform a forward fill on the Month.Year column within each Countries group
data <- data %>%
  arrange(Countries, ID) %>%  # Arrange data by country and ID to ensure correct order
  group_by(Countries) %>%
  mutate(Month.Year = ifelse(is.na(Month.Year), lag(Month.Year), Month.Year)) %>%
  ungroup()


# Check the percentage of NA values
mean(is.na(data$Month.Year)) * 100

# Repeat the forward fill step to ensure all consecutive NAs are filled
data <- data %>%
  group_by(Countries) %>%
  mutate(Month.Year = ifelse(is.na(Month.Year), lag(Month.Year), Month.Year)) %>%
  ungroup()

# Check the percentage of NA values again to verify
mean(is.na(data$Month.Year)) * 100

na_entries <- data$`Announcement Date`[is.na(data$Month.Year)]
unique(na_entries)

# Replace remaining NA values in Month.Year with a default placeholder, e.g., "Unknown"
data$Month.Year[is.na(data$Month.Year)] <- "Unknown"

# Verify there are no NA values left
mean(is.na(data$Month.Year)) * 100


#   Dealing with NAs in Allocations and Commitments -----------------------------------------------------

colnames(data)
data <- data[, !colnames(data) %in% c("Announcement Date", "ID")]
colnames(data)
unique(data$Countries)

# Convert columns to numeric and fill NA values with 0
data$`Converted Value in EUR` <- as.numeric(gsub("[^0-9.-]", "", data$`Converted Value in EUR`))
data$`Converted Value in EUR`[is.na(data$`Converted Value in EUR`)] <- 0

data$`Allocated Total in EUR (redistributed)` <- as.numeric(gsub("[^0-9.-]", "", data$`Allocated Total in EUR (redistributed)`))
data$`Allocated Total in EUR (redistributed)`[is.na(data$`Allocated Total in EUR (redistributed)`)] <- 0

# Ensure binary columns are numeric
data$`non EU NATO member` <- as.numeric(data$`non EU NATO member`)
data$EU <- as.numeric(data$EU)
data$EU_geo <- as.numeric(data$EU_geo)

# Define the grouping columns
grouping_columns <- c("Month.Year", "Countries", "Type of Aid General", "Type of donation")

# Perform the aggregation
data_summary <- aggregate(
  data[, c("Converted Value in EUR", "Allocated Total in EUR (redistributed)")],
  by = data[grouping_columns],
  FUN = sum
)

# Aggregate the binary indicators to take value 1 if any entry in the group is 1, otherwise 0
data_summary$`non EU NATO member` <- aggregate(data$`non EU NATO member`, by = data[grouping_columns], FUN = max)$x
data_summary$EU <- aggregate(data$EU, by = data[grouping_columns], FUN = max)$x
data_summary$EU_geo <- aggregate(data$EU_geo, by = data[grouping_columns], FUN = max)$x

# View the result
head(data_summary)

# Move Month.Year to the first position
data_summary <- data_summary[, c("Month.Year", setdiff(names(data), "Month.Year"))]
colnames(data_summary)


#   Normalize allocations and commitments by GDP --------------------------------------------

head(data_summary)
nrow(data_summary)

data_summary$'Allocated Total in EUR (redistributed)'

# Create a data frame with 2023 GDP values in EUR
gdp_data <- data.frame(
  Countries = c("Australia", "Austria", "Belgium", "Bulgaria", "Canada", "China", "Croatia", "Cyprus", "Czech Republic", 
                "Denmark", "EU (Commission and Council)", "Estonia", "European Investment Bank", "European Peace Facility",
                "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "India", "Ireland", "Italy", "Japan",
                "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal",
                "Romania", "Slovakia", "Slovenia", "South Korea", "Spain", "Sweden", "Switzerland", "Taiwan", "Turkey",
                "United Kingdom", "United States"),
  GDP_2023_EUR = c(1400000000000, 480000000000, 500000000000, 81000000000, 1800000000000, 17734000000000, 65000000000, 
                   24000000000, 280000000000, 370000000000, 16000000000000, 36000000000, 16000000000000, 16000000000000,
                   270000000000, 2800000000000, 4200000000000, 220000000000, 190000000000, 27000000000, 3100000000000, 
                   500000000000, 2000000000000, 4900000000000, 30000000000, 67000000000, 84000000000, 17000000000,
                   830000000000, 250000000000, 440000000000, 690000000000, 260000000000, 300000000000, 130000000000,
                   60000000000, 1800000000000, 1400000000000, 600000000000, 820000000000, 660000000000, 880000000000,
                   3000000000000, 23000000000000)  
)

# Merge GDP data with data_summary
data_summary <- merge(data_summary, gdp_data, by = "Countries", all.x = TRUE)

# Normalize financial figures by GDP
data_summary$`Converted Value in EUR (% of GDP)` <- (data_summary$`Converted Value in EUR` / data_summary$GDP_2023_EUR) * 100
data_summary$`Allocated Total in EUR (redistributed as % of GDP)` <- (data_summary$`Allocated Total in EUR (redistributed)` / data_summary$GDP_2023_EUR) * 100

# View the result to verify
head(data_summary)

#   Further Data Cleaning -----------------------------------------------------------

unique(data_summary$Countries)
# Remove EU institutions to prevent double counting; their contributions have already been redistributed to member states according to capital share
exclude_institutions <- c("EU (Commission and Council)", "European Investment Bank", "European Peace Facility")

# Remove rows with specified countries in the 'Countries' column
data_summary <- data_summary[!data_summary$Countries %in% exclude_institutions, ]
unique(data_summary$Countries)











#########################################################################
# (2) Making plots --------------------------------------------------------
#   Fig. 1) Plotting Allocations ----------------------------------------------------
# Filter out rows with "Commitment" in the "Type of donation" column
filtered_data <- data_summary %>%
  filter(`Type of donation` != "Commitment")

# Prepare the data by creating groups based on "United States", "Europe" (EU_geo = 1), and "Other Countries"
plot_data <- filtered_data %>%
  mutate(Country_Group = case_when(
    Countries == "United States" ~ "United States",
    EU_geo == 1 ~ "Europe",
    TRUE ~ "Other Countries"
  )) %>%
  group_by(Month.Year, Country_Group) %>%
  summarize(Total_EUR_Allocated = sum(`Allocated Total in EUR (redistributed)`, na.rm = TRUE)) %>%
  ungroup()

# Convert Month.Year to a date format for plotting
plot_data$Month.Year <- as.Date(paste0(plot_data$Month.Year, "-01"))

# Create the plotly line plot
plot <- plot_ly(data = plot_data, x = ~Month.Year, y = ~Total_EUR_Allocated, color = ~Country_Group, type = 'scatter', mode = 'lines') %>%
  layout(
    title = "",
    xaxis = list(title = ""),
    yaxis = list(title = "Allocated Total in EUR"),
    legend = list(title = list(text = "Country Group"))
  )

plot


#   Fig. 2) Plotting Cumulative allocations ----------------------------------------


# Filter out rows with "Commitment" in the "Type of donation" column
filtered_data <- data_summary %>%
  filter(`Type of donation` != "Commitment")

# Prepare the data by creating groups based on "United States", "Europe" (EU_geo = 1), and "Other Countries"
plot_data <- filtered_data %>%
  mutate(Country_Group = case_when(
    Countries == "United States" ~ "United States",
    EU_geo == 1 ~ "Europe",
    TRUE ~ "Other Countries"
  )) %>%
  arrange(Month.Year) %>%  # Ensure data is ordered by Month.Year for correct cumulative sum
  group_by(Country_Group, Month.Year) %>%
  summarize(Monthly_Allocated = sum(`Allocated Total in EUR (redistributed)`, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Country_Group) %>%
  mutate(Total_Contribution = cumsum(Monthly_Allocated)) %>%
  ungroup()

# Convert Month.Year to a date format for plotting
plot_data$Month.Year <- as.Date(paste0(plot_data$Month.Year, "-01"))

# Create the plotly line plot
plot <- plot_ly(data = plot_data, x = ~Month.Year, y = ~Total_Contribution, color = ~Country_Group, type = 'scatter', mode = 'lines') %>%
  layout(
    title = "",
    xaxis = list(title = ""),
    yaxis = list(title = "Cumulative Allocations in EUR"),
    legend = list(title = list(text = ""))
  )

plot



#   Fig. 3) Plotting Cumulative Allocations (%GDP)  ----------------------------------------------------------

# Define Baltic Sea States
baltic_sea_states <- c("Denmark", "Finland", "Iceland", "Norway", "Sweden", "Poland", "Latvia", "Lithuania", "Estonia")

# Filter out rows with "Commitment" in the "Type of donation" column
filtered_data <- data_summary %>%
  filter(`Type of donation` != "Commitment")

# Prepare the data by creating groups based on specified countries
plot_data <- filtered_data %>%
  mutate(Country_Group = case_when(
    Countries == "United States" ~ "United States",
    Countries %in% baltic_sea_states ~ "Baltic Sea States",
    EU_geo == 1 ~ "Rest of Europe",  # Remaining European countries
    TRUE ~ "Other Countries"
  )) %>%
  arrange(Month.Year) %>%  # Ensure data is ordered by Month.Year for correct cumulative sum
  group_by(Country_Group, Month.Year) %>%
  summarize(Monthly_Allocated_Percent = sum(`Allocated Total in EUR (redistributed as % of GDP)`, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Country_Group) %>%
  mutate(Cumulative_Contribution_Percent = cumsum(Monthly_Allocated_Percent)) %>%
  ungroup()

# Convert Month.Year to a date format for plotting
plot_data$Month.Year <- as.Date(paste0(plot_data$Month.Year, "-01"))

# Create the plotly line plot
plot <- plot_ly(data = plot_data, x = ~Month.Year, y = ~Cumulative_Contribution_Percent, color = ~Country_Group, type = 'scatter', mode = 'lines') %>%
  layout(
    title = "",
    xaxis = list(title = ""),
    yaxis = list(title = "Cumulative Allocations (% of GDP)"),
    legend = list(title = list(text = ""))
  )

plot






#########################################################################
# (3) Preparing data for projection -------------------------------------
#   Prepare data for projection ---------------------------------------------

# Filter data_summary to include only rows with Type of donation = "Allocation"
data_allocations <- data_summary %>%
  filter(`Type of donation` == "Allocation")

# Display the first few rows to confirm
head(data_allocations)


#   Which country_type columns contribute the most to total aid? -----------

# Sum 'Allocated Total in EUR (redistributed)' by 'Countries' and 'Type of Aid General'
summary_total_allocations <- data_summary %>%
  group_by(Countries, `Type of Aid General`) %>%  # Group by country and aid type
  summarise(Sum_Allocated_EUR = sum(`Allocated Total in EUR (redistributed)`))  # Sum the 'Allocated Total'

# View the summarized data
print(summary_total_allocations)

# To view the output in a more readable format, you can sort it
sorted_summary <- summary_total_allocations %>%
  arrange(desc(Sum_Allocated_EUR))  # Sort by the sum in descending order

# View the sorted data
print(sorted_summary)

# Calculate the total sum of 'Sum_Allocated_EUR'
total_sum <- sum(sorted_summary$Sum_Allocated_EUR)

# Create a new column with the normalized value (each value divided by the total sum)
sorted_summary <- sorted_summary %>%
  mutate(Normalized_Sum = Sum_Allocated_EUR / total_sum)

# View the updated sorted_summary with the normalized values
print(sorted_summary)

# Exclude rows where 'Countries' is 'United States'
nonUS_sorted_summary <- sorted_summary[sorted_summary$Countries != "United States", ]

# View the resulting data
print(nonUS_sorted_summary)


# Assuming nonUS_sorted_summary is already available
sum_normalized_sums_first_20 <- sum(nonUS_sorted_summary$Normalized_Sum[1:20])

# Print the sum
print(sum_normalized_sums_first_20)

# Assuming 'nonUS_sorted_summary' is the dataframe you're working with
first_20_rows <- nonUS_sorted_summary[1:20, c("Countries", "Type of Aid General")]

# View the first 20 rows of 'Countries' and 'Type of Aid General'
print(first_20_rows)
#   Create the predictor matrix ---------------------------------------------

# Function to process data for a given country
process_country_aid <- function(country_name) {
  # Military data
  military <- data_summary %>%
    filter(`Type of Aid General` == "Military", Countries == country_name) %>%
    select(`Month.Year`, Allocated = `Allocated Total in EUR (redistributed as % of GDP)`) %>%
    group_by(`Month.Year`) %>%
    summarize(!!paste0(country_name, "_military") := sum(Allocated, na.rm = TRUE))
  
  # Humanitarian data
  humanitarian <- data_summary %>%
    filter(`Type of Aid General` == "Humanitarian", Countries == country_name) %>%
    select(`Month.Year`, Allocated = `Allocated Total in EUR (redistributed as % of GDP)`) %>%
    group_by(`Month.Year`) %>%
    summarize(!!paste0(country_name, "_humanitarian") := sum(Allocated, na.rm = TRUE))
  
  # Financial data
  financial <- data_summary %>%
    filter(`Type of Aid General` == "Financial", Countries == country_name) %>%
    select(`Month.Year`, Allocated = `Allocated Total in EUR (redistributed as % of GDP)`) %>%
    group_by(`Month.Year`) %>%
    summarize(!!paste0(country_name, "_financial") := sum(Allocated, na.rm = TRUE))
  
  # Merge the datasets
  country_aid <- military %>%
    full_join(humanitarian, by = "Month.Year") %>%
    full_join(financial, by = "Month.Year") %>%
    mutate(across(starts_with(country_name), ~replace_na(., 0))) %>%
    arrange(`Month.Year`)
  
  return(country_aid)
}

# Get the list of unique countries
countries <- unique(data_summary$Countries)

# Initialize an empty list to store country data
all_countries_aid <- list()

# Loop through each country (including the United States) and process the data
for (country in countries) {
  country_data <- process_country_aid(country)
  all_countries_aid[[country]] <- country_data
}

# Combine all countries' data into a single dataframe by merging on Month.Year
final_aid_data <- reduce(all_countries_aid, full_join, by = "Month.Year") %>%
  arrange(`Month.Year`)

# Replace NAs with 0 for the final dataset
final_aid_data <- final_aid_data %>%
  mutate(across(-`Month.Year`, ~replace_na(., 0)))

# Remove the row with 'Unknown' Month.Year 
final_aid_data <- final_aid_data %>%
  filter(Month.Year != "Unknown")


unique(final_aid_data$Month.Year) #last row is 2024-06
nrow(final_aid_data)
ncol(final_aid_data)

unique(final_aid_data$Month.Year)
nrow(final_aid_data)
colnames(final_aid_data)
unique(data_summary$Countries)


#   Create y (predicted) ------------------------------------------------------

# Sum the 'Allocated Total in EUR (redistributed)' for each 'Month.Year'
total_allocations <- aggregate(`Allocated Total in EUR (redistributed)` ~ Month.Year, 
                               data = data_allocations, 
                               sum)

# Step 3: Rename the resulting column to 'Total Allocations'
colnames(total_allocations) <- c("Month.Year", "Total Allocations")

# Step 4: View the resulting dataframe
total_allocations

# Remove the row with 'Unknown' Month.Year 
total_allocations <- total_allocations %>%
  filter(Month.Year != "Unknown")

# Calculate total GDP for 2023
group_GDP_2023 <- sum(gdp_data$GDP_2023_EUR)

# Add the new column 'Total Allocations (% GDP)'
total_allocations$`Total Allocations (% GDP)` <- total_allocations$`Total Allocations` / group_GDP_2023

y <- total_allocations$`Total Allocations (% GDP)`

#   Create X (predictor) ----------------------------------------------------

# Define the predictor variables X
X <- final_aid_data[, !(colnames(final_aid_data) %in% "Month.Year")]

# Inspect the resulting dataframe
head(X)
dim(X)
colnames(X)


#   Align y and X, separate data for training and testing  -----------------------------------------------------------

# Redefine y so that it aligns with X rows from the previous Month.Year
y <- as.vector(y[-1])  # Remove the first element of y (future y)
X <- X[-nrow(X), ]  # Remove the last row of X to match dimensions

# Verify alignment
length(y) == nrow(X)  # Should return TRUE
tail(y)
tail(X)

# Ensure y is a column vector
y <- as.data.frame(y)
colnames(y) <- "y"  # Rename the column to "y" 

# Concatenate y and X by columns
pred.df <- cbind(y, X)

# Define the cutoff point for training and testing
cutoff <- round(0.8 * nrow(pred.df))  # Use 80% of the data for training

# Split the data into training and testing sets
train.df <- pred.df[1:cutoff, ]  # Rows 1 to cutoff for training
test.df <- pred.df[(cutoff + 1):nrow(pred.df), ]  # Remaining rows for testing

# Replace spaces with underscores in so that all data frames match
colnames(pred.df) <- gsub(" ", "_", colnames(pred.df))
colnames(train.df) <- gsub(" ", "_", colnames(train.df))
colnames(test.df) <- gsub(" ", "_", colnames(test.df))

# Also, replace periods with underscores so that all data frames match
colnames(pred.df) <- gsub("\\.", "_", colnames(pred.df))
colnames(train.df) <- gsub("\\.", "_", colnames(train.df))
colnames(test.df) <- gsub("\\.", "_", colnames(test.df))

# Check if column names are now consistent
all(colnames(train.df) == colnames(pred.df))  # Should return TRUE
all(colnames(test.df) == colnames(pred.df))   # Should return TRUE


#########################################################################
# (4) Fitting models ----------------------------------------------------
#   1) Linear regression model -------------------------------------------

# Fit the full regression model
fullMdl <- lm(y ~ ., data = train.df)
summary(fullMdl)

# Generate predictions for the test data
fmPred <- predict(fullMdl, newdata = test.df)

# Preview the predictions
head(fmPred)

#   2) Lasso model ------------------------------------------------------------------

# Convert predictors to matrix form for glmnet
X_train_matrix <- as.matrix(train.df[ , -1])  # Exclude the response column
y_train_vector <- train.df$y

# 2.2.1) Fit LASSO with Cross-Validation
lasso_model <- cv.glmnet(X_train_matrix, y_train_vector, alpha = 1)

# Identify optimal lambda (lambda.min)
optimal_lambda <- lasso_model$lambda.min

# Refit LASSO model using optimal lambda
lasso_final_model <- glmnet(X_train_matrix, y_train_vector, alpha = 1, lambda = optimal_lambda)

# Generate predictions for the test data
X_test_matrix <- as.matrix(test.df[ , -1])  # Exclude the response column
lasso_pred <- predict(lasso_final_model, newx = X_test_matrix)

# Extract non-zero coefficients from the LASSO model
lasso_coefficients <- as.matrix(coef(lasso_final_model))

# Filter for non-zero coefficients
non_zero_coefficients <- lasso_coefficients[lasso_coefficients != 0, ]
print("Non-zero coefficients for lambda.min:")
print(non_zero_coefficients)

# Print the optimal lambda
cat("Optimal lambda (lambda.min):", optimal_lambda, "\n")

# Plot the cross-validation curve for LASSO
plot(lasso_model)

# 2.2.2) Perform 20-fold cross-validation LASSO
set.seed(123)  # For reproducibility
lasso_cv_20 <- cv.glmnet(
  X_train_matrix, 
  y_train_vector, 
  alpha = 1,  # LASSO penalty
  nfolds = 20,  # 20-fold cross-validation
  standardize = TRUE  # Standardize predictors
)

# Plot the cross-validation curve
plot(lasso_cv_20)

# Display the optimal lambda values
lambda_min <- lasso_cv_20$lambda.min  # Lambda that minimizes MSE
lambda_1se <- lasso_cv_20$lambda.1se  # Lambda within 1 standard error
cat("Optimal lambda (min) from 20-fold CV:", lambda_min, "\n")
cat("Optimal lambda (1se) from 20-fold CV:", lambda_1se, "\n")

# 2.2.3) Refit LASSO with optimal lambda (lambda.min)
lasso_final_model_min <- glmnet(X_train_matrix, y_train_vector, alpha = 1, lambda = lambda_min)

# Generate predictions for test data using lambda.min
lasso_pred_min <- predict(lasso_final_model_min, newx = X_test_matrix)

# Calculate RMSE for lambda.min model predictions
lasso_mse_min <- mean((test.df$y - lasso_pred_min)^2)
lasso_rmse_min <- sqrt(lasso_mse_min)
cat("LASSO Model RMSE with lambda.min:", lasso_rmse_min, "\n")

# Extract non-zero coefficients for lambda.min
lasso_coefficients_min <- coef(lasso_final_model_min)
non_zero_min <- lasso_coefficients_min[lasso_coefficients_min != 0]
cat("Non-zero coefficients for lambda.min:")
print(non_zero_min)

# Count non-zero coefficients for lambda.min
non_zero_min_count <- sum(lasso_coefficients_min != 0) - 1  # Exclude intercept
cat("Number of predictors with lambda.min:", non_zero_min_count, "\n")

# 2.2.4) Extract coefficients and calculate for lambda.1se

# Refit the LASSO model using lambda.1se
lasso_1se_model <- glmnet(X_train_matrix, y_train_vector, alpha = 1, lambda = lambda_1se)

# Generate predictions for the test data using lambda.1se
lasso_pred_1se <- predict(lasso_1se_model, newx = X_test_matrix)

# Calculate RMSE for lambda.1se model predictions
lasso_mse_1se <- mean((test.df$y - lasso_pred_1se)^2)
lasso_rmse_1se <- sqrt(lasso_mse_1se)
cat("LASSO Model RMSE with lambda.1se:", lasso_rmse_1se, "\n")

# Extract and display non-zero coefficients for lambda.1se
lasso_coefficients_1se <- coef(lasso_1se_model)
non_zero_1se_coefficients <- lasso_coefficients_1se[lasso_coefficients_1se != 0]
cat("Non-zero coefficients for lambda.1se:")
print(non_zero_1se_coefficients)

# Count non-zero coefficients for lambda.1se
non_zero_1se_count <- sum(lasso_coefficients_1se != 0) - 1  # Exclude intercept
cat("Number of predictors with lambda.1se:", non_zero_1se_count, "\n")


#   3) Ridge model -------------------------------------------------------------------

# Perform 20-fold cross-validated Ridge regression
set.seed(123)  # For reproducibility
ridge <- cv.glmnet(
  X_train_matrix, 
  y_train_vector, 
  alpha = 0,  # Ridge penalty
  nfolds = 20,  # 20-fold cross-validation
  standardize = TRUE  # Standardize predictors
)

# Plot the cross-validation curve for Ridge
plot(ridge)

# Extract optimal lambda values
ridge_lambda_min <- ridge$lambda.min  # Lambda that minimizes MSE
cat("Optimal lambda (ridge, min):", ridge_lambda_min, "\n")

# Refit the Ridge model using the optimal lambda (lambda.min)
ridge_final_model <- glmnet(X_train_matrix, y_train_vector, alpha = 0, lambda = ridge_lambda_min)

# Generate predictions for the test data
ridgePred <- predict(ridge_final_model, newx = X_test_matrix)


#   4) Elastic Net --------------------------------------------------------------

# Perform 20-fold Cross-Validated Elastic Net
set.seed(123)  # For reproducibility
elnet <- cv.glmnet(
  X_train_matrix,
  y_train_vector,
  alpha = 0.5,  # Elastic Net mixing parameter
  nfolds = 20,  # 20-fold cross-validation
  standardize = TRUE  # Standardize predictors
)

# Plot the cross-validation curve
plot(elnet)

# Extract optimal lambda values
elnet_lambda_min <- elnet$lambda.min
cat("Optimal lambda (Elastic Net, min):", elnet_lambda_min, "\n")

# Refit the Elastic Net model using the optimal lambda
elnet_final_model <- glmnet(X_train_matrix, y_train_vector, alpha = 0.5, lambda = elnet_lambda_min)

# Generate predictions for the test data
elnetPred <- predict(elnet_final_model, newx = X_test_matrix)

# Calculate RMSE for Elastic Net predictions
elnet_mse <- mean((test.df$y - elnetPred)^2)
elnet_rmse <- sqrt(elnet_mse)
cat("Elastic Net Model RMSE with lambda.min:", elnet_rmse, "\n")


#   5) KNN ---------------------------------------------------------------------

# Extract predictors and response variables for training and testing
X_train <- as.matrix(train.df[, -1])  # Exclude the response column
y_train <- train.df$y  # Response variable for training
X_test <- as.matrix(test.df[, -1])  # Exclude the response column
y_test <- test.df$y  # Response variable for testing

# Determine the maximum feasible k
max_k <- nrow(X_train)  # Maximum k is the number of training samples

# Define the range of k values to test
k_values <- 1:max_k

# Initialize variables to store results
mse_values <- numeric(length(k_values))  # To store MSE for each k

# Loop over k values and calculate MSE
for (k in k_values) {
  knn <- knn.reg(train = X_train, y = y_train, test = X_test, k = k)
  knnPred <- knn$pred  # Predictions
  mse_values[k] <- mean((y_test - knnPred)^2)  # Calculate MSE
}

# Find the optimal k that minimizes MSE
optimal_k <- which.min(mse_values)

# Display the optimal k and its corresponding MSE
cat("Optimal k:", optimal_k, "\n")
cat("Minimum MSE:", mse_values[optimal_k], "\n")

# Refit the model with the optimal k
knn <- knn.reg(train = X_train, y = y_train, test = X_test, k = optimal_k)
knnPred <- knn$pred  # Predictions with optimal k

#   6) dTree -------------------------------------------------------------------

# Fit the regression tree model
dTree <- rpart(y ~ ., data = train.df, method = "anova")  # "anova" for regression

# Display the tree structure
print(dTree)

# Generate predictions on the testing data
dTreePred <- predict(dTree, newdata = test.df)


#   7) Random Forest ---------------------------------------------
# Set the seed for reproducibility
set.seed(4621)
# Clean column names to remove spaces and special characters in column names for train and test data
colnames(train.df) <- make.names(colnames(train.df))  # Clean column names in training data
colnames(test.df) <- make.names(colnames(test.df))  # Clean column names in testing data

# Set up the grid of hyperparameters for the number of trees (ntree) and variables per split (mtry)
ntree_values <- seq(100, 500, by = 50)  # Define a sequence for ntree values (number of trees) from 100 to 500 with a step of 50
mtry_values <- floor(seq(1, ncol(X_train), length.out = 5))  # Define mtry values (number of variables to consider at each split), divided evenly over the number of features

# Initialize an empty matrix to store RMSE results for different ntree and mtry combinations
rf_rmse_results <- matrix(NA, nrow = length(ntree_values), ncol = length(mtry_values), 
                          dimnames = list(paste0("ntree=", ntree_values),  # Row names based on ntree values
                                          paste0("mtry=", mtry_values)))  # Column names based on mtry values

# Perform a grid search to evaluate performance with different combinations of ntree and mtry
for (ntree in ntree_values) {  # Loop over all values for ntree
  for (mtry in mtry_values) {  # Loop over all values for mtry
    # Fit Random Forest model with current ntree and mtry values
    rf_model <- randomForest(
      y ~ .,  # Predict y using all other variables
      data = train.df,  # Use training data
      ntree = ntree,  # Number of trees
      mtry = mtry,  # Number of variables to consider at each split
      importance = TRUE  # Track feature importance for each variable
    )
    
    # Generate predictions on the training data to evaluate model performance
    rf_pred_train <- predict(rf_model, newdata = train.df)
    
    # Calculate RMSE (Root Mean Squared Error) and store it in the results matrix
    rf_rmse_results[paste0("ntree=", ntree), paste0("mtry=", mtry)] <- 
      sqrt(mean((train.df$y - rf_pred_train)^2))  # RMSE for the training predictions
  }
}

# Identify the combination of ntree and mtry that gives the best (lowest) RMSE
optimal_params <- which(rf_rmse_results == min(rf_rmse_results), arr.ind = TRUE)
optimal_ntree <- ntree_values[optimal_params[1]]  # Optimal number of trees
optimal_mtry <- mtry_values[optimal_params[2]]  # Optimal number of variables per split

# Output the optimal parameters for ntree and mtry
cat("Optimal ntree:", optimal_ntree, "\n")  # Display optimal ntree
cat("Optimal mtry:", optimal_mtry, "\n")  # Display optimal mtry

# Fit the final Random Forest model using the optimal hyperparameters found
rf <- randomForest(
  y ~ .,  # Predict y using all other variables
  data = train.df,  # Training data
  ntree = optimal_ntree,  # Number of trees
  mtry = optimal_mtry,  # Number of variables to consider at each split
  importance = TRUE  # Track feature importance
)

# Generate predictions for the test data using the final Random Forest model
rfPred <- predict(rf, newdata = test.df)

#   8) Random Forest with feature selection ---------------------
# Extract variable importance from the trained Random Forest model
var_importance <- importance(rf_model)

# Identify variables that have negative %IncMSE (Mean Decrease in Mean Squared Error)
variables_to_drop <- rownames(var_importance[var_importance[, "%IncMSE"] < 0, ])  # Variables with negative impact
cat("Dropping variables:", variables_to_drop, "\n")

# Drop the identified variables from both the training and testing datasets
train.df_reduced <- train.df[, !(colnames(train.df) %in% variables_to_drop)]  # Reduced training data
test.df_reduced <- test.df[, !(colnames(test.df) %in% variables_to_drop)]  # Reduced testing data

# Define new search grid for the Random Forest model with reduced feature set
ntree_values <- seq(100, 500, by = 50)  # Number of trees to test
mtry_values <- floor(seq(1, ncol(train.df_reduced) - 1, length.out = 5))  # Variables per split, based on reduced dataset

# Reinitialize the results matrix for the grid search with the reduced dataset
rf_rmse_results <- matrix(
  NA, 
  nrow = length(ntree_values), 
  ncol = length(mtry_values),
  dimnames = list(paste0("ntree=", ntree_values), paste0("mtry=", mtry_values))  # Dim names for results
)

# Perform a grid search for optimal hyperparameters using the reduced dataset
for (ntree in ntree_values) {  # Loop over all ntree values
  for (mtry in mtry_values) {  # Loop over all mtry values
    # Fit Random Forest model using the reduced training dataset
    rf_model <- randomForest(
      y ~ .,  # Predict y using all other variables
      data = train.df_reduced,  # Training data (reduced)
      ntree = ntree,  # Number of trees
      mtry = mtry,  # Number of variables to consider at each split
      importance = TRUE  # Track feature importance
    )
    
    # Generate predictions on the training data to evaluate performance
    rf_pred_train <- predict(rf_model, newdata = train.df_reduced)
    rf_rmse_results[paste0("ntree=", ntree), paste0("mtry=", mtry)] <- 
      sqrt(mean((train.df_reduced$y - rf_pred_train)^2))  # RMSE for the reduced dataset
  }
}

# Find the best combination of ntree and mtry for the reduced dataset
optimal_params <- which(rf_rmse_results == min(rf_rmse_results, na.rm = TRUE), arr.ind = TRUE)
optimal_ntree <- ntree_values[optimal_params[1]]  # Optimal ntree for reduced dataset
optimal_mtry <- mtry_values[optimal_params[2]]  # Optimal mtry for reduced dataset

# Output the optimal parameters for ntree and mtry after reducing the dataset
cat("Optimal ntree:", optimal_ntree, "\n")
cat("Optimal mtry:", optimal_mtry, "\n")

# Fit the final Random Forest model with optimal parameters on the reduced dataset
rf_final <- randomForest(
  y ~ .,  # Predict y using all other variables
  data = train.df_reduced,  # Reduced training data
  ntree = optimal_ntree,  # Number of trees
  mtry = optimal_mtry,  # Number of variables to consider at each split
  importance = TRUE  # Track feature importance
)

# Generate predictions from the final Random Forest model using the reduced dataset
rfPred1 <- predict(rf_final, newdata = test.df_reduced)

#   9) Boosting (recommended: run this individually) ----------------------------------------------------------------
set.seed(543210)
# Ensure column names are clean
colnames(train.df) <- make.names(colnames(train.df))
colnames(test.df) <- make.names(colnames(test.df))

# Fit a Boosting model with adjusted parameters
boosting_model <- gbm(
  formula = y ~ ., 
  data = train.df, 
  distribution = "gaussian",  # For regression
  n.trees = 100,             # Number of boosting iterations
  interaction.depth = 3,     # Depth of each tree
  shrinkage = 0.9,          # Learning rate
  n.minobsinnode = 5,        # Minimum number of observations in terminal nodes
  bag.fraction = 0.9,        # Fraction of data used in each iteration
  cv.folds = 3,              # Use 3-fold cross-validation
  verbose = FALSE            # Suppress verbose output
)

# Check the optimal number of trees using cross-validation
optimal_trees <- gbm.perf(boosting_model, method = "cv")

# Generate predictions on the test data
boosting_pred <- predict(boosting_model, newdata = test.df, n.trees = optimal_trees)





#   10) Boosting with Tuning (recommended: run this individually) ---------------------------------------------
set.seed(543210)
# Ensure column names are clean
colnames(train.df) <- make.names(colnames(train.df))
colnames(test.df) <- make.names(colnames(test.df))

# Define grid for parameters to tune
shrinkage_values <- c(0.01, 0.05, 0.1, 0.5, 0.9)  # Learning rates to test
depth_values <- c(1, 3, 5, 7)  # Interaction depths to test
ntree_values <- c(100, 500)  # Number of trees to test

# Initialize a matrix to store results
boosting_rmse_results <- matrix(NA, nrow = length(shrinkage_values) * length(depth_values) * length(ntree_values), 
                                ncol = 3, 
                                dimnames = list(NULL, c("Shrinkage", "Depth", "RMSE")))


# Loop over combinations of shrinkage, depth, and n.trees
row_index <- 1
for (shrinkage in shrinkage_values) {
  for (depth in depth_values) {
    for (ntrees in ntree_values) {
      
      # Fit Boosting model with current combination of parameters
      boosting_model <- gbm(
        formula = y ~ ., 
        data = train.df, 
        distribution = "gaussian",  # For regression
        n.trees = ntrees,           # Number of boosting iterations
        interaction.depth = depth,  # Depth of each tree
        shrinkage = shrinkage,      # Learning rate
        n.minobsinnode = 5,        # Minimum number of observations in terminal nodes
        bag.fraction = 0.9,        # Fraction of data used in each iteration
        cv.folds = 3,              # Use 3-fold cross-validation
        verbose = FALSE            # Suppress verbose output
      )
      
      # Check the optimal number of trees using cross-validation
      optimal_trees <- gbm.perf(boosting_model, method = "cv")
      
      # Generate predictions on the test data
      boosting_pred <- predict(boosting_model, newdata = test.df, n.trees = optimal_trees)
      
      # Calculate RMSE for the current model
      boosting_mse <- mean((test.df$y - boosting_pred)^2)
      boosting_rmse <- sqrt(boosting_mse)
      
      # Store the RMSE along with the parameter values
      boosting_rmse_results[row_index, ] <- c(shrinkage, depth, boosting_rmse)
      
      # Increment row index
      row_index <- row_index + 1
    }
  }
}

# Find the best combination of parameters based on RMSE
best_params <- boosting_rmse_results[which.min(boosting_rmse_results[, 3]), ]

cat("Optimal Shrinkage (Learning Rate):", best_params[1], "\n")
cat("Optimal Interaction Depth:", best_params[2], "\n")
cat("Best RMSE:", best_params[3], "\n")


# Refit the final Boosting model with the best parameters
final_boosting_model <- gbm(
  formula = y ~ ., 
  data = train.df,  # Use cleaned data
  distribution = "gaussian", 
  n.trees = 100,  # We use 100 trees, 
  interaction.depth = as.numeric(best_params[2]),  # Optimal interaction depth
  shrinkage = as.numeric(best_params[1]),  # Optimal shrinkage
  n.minobsinnode = 5, 
  bag.fraction = 0.9, 
  cv.folds = 3, 
  verbose = FALSE
)


# Generate predictions using the final Boosting model
final_boosting_pred <- predict(final_boosting_model, newdata = train.df, n.trees = optimal_trees)



#########################################################################
# (5) Model Selection ---------------------------------------------------
#   Calculating RMSE ----------------------------------------------------------
# RMSE for Linear regression
fm_mse <- mean((test.df$y - fmPred)^2)
fm_rmse <- sqrt(fm_mse)

#  RMSE for Lasoo
lasso_mse <- mean((test.df$y - lasso_pred)^2)
lasso_rmse <- sqrt(lasso_mse)

#  RMSE for Ridge 
ridge_mse <- mean((test.df$y - ridgePred)^2)
ridge_rmse <- sqrt(ridge_mse)

#  RMSE for Elastic Net
elnet_mse <- mean((test.df$y - elnetPred)^2)
elnet_rmse <- sqrt(elnet_mse)

#RMSE for KNN with optimal k
knn_rmse <- sqrt(mse_values[optimal_k])

# RMSE for regression tree 
dTree_mse <- mean((test.df$y - dTreePred)^2)
dTree_rmse <- sqrt(dTree_mse)

#  RMSE for Random Forest 
rf_mse <- mean((test.df$y - rfPred)^2)  
rf_rmse <- sqrt(rf_mse)  

#  RMSE for Random Forest with feature selection
rf_mse1 <- mean((test.df_reduced$y - rfPred1)^2)
rf_rmse1 <- sqrt(rf_mse1)

#  RMSE for Boosting
boosting_mse <- mean((test.df$y - boosting_pred)^2)
boosting_rmse <- sqrt(boosting_mse)

# RMSE for Boosting with tuning
final_boosting_mse <- mean((test.df$y - final_boosting_pred)^2)
final_boosting_rmse <- sqrt(final_boosting_mse)

#   Extracting Variable Importance -----------------------------------------------------

# Variable importance for Regression Tree (dTree)
dTree_importance <- dTree$variable.importance
dTree_importance_df <- data.frame(
  Variable = names(dTree_importance),
  Importance = dTree_importance
)

# Variable importance for Random Forest (rf)
rf_importance <- importance(rf)
rf_importance_df <- data.frame(
  Variable = rownames(rf_importance),
  `%IncMSE` = rf_importance[, 1],
  IncNodePurity = rf_importance[, 2]
)

# Variable importance for Reduced Random Forest (rf_model1)
rf_importance1 <- importance(rf_final)
rf_importance1_df <- data.frame(
  Variable = rownames(rf_importance1),
  `%IncMSE` = rf_importance1[, 1],
  IncNodePurity = rf_importance1[, 2]
)

# Variable importance for Boosting Model (boosting_model) without plotting
boosting_importance <- summary(boosting_model, plot = FALSE)  # Suppress plotting
boosting_importance_df <- as.data.frame(boosting_importance)
colnames(boosting_importance_df) <- c("Variable", "RelativeInfluence")

# Variable importance for Tuned Boosting Model (final_boosting_model) without plotting
final_boosting_importance <- summary(final_boosting_model, plot = FALSE)  # Suppress plotting
final_boosting_importance_df <- as.data.frame(final_boosting_importance)
colnames(final_boosting_importance_df) <- c("Variable", "RelativeInfluence")

# Combine all variable importance data frames into a single list for comparison if needed
variable_importance_list <- list(
  "Regression Tree" = dTree_importance_df,
  "Random Forest" = rf_importance_df,
  "Reduced Random Forest" = rf_importance1_df,
  "Boosting" = boosting_importance_df,
  "Tuned Boosting" = final_boosting_importance_df
)

# Print each variable importance for review (optional)
print(variable_importance_list)

#   Comparing models ----------------------------------------------------------

#     RMSE --------------------------------------------------------------------
# Create a vector of RMSE values
rmse_values <- c(fm_rmse, lasso_rmse, ridge_rmse, elnet_rmse, knn_rmse, dTree_rmse, rf_rmse, rf_rmse1, boosting_rmse, final_boosting_rmse)

# Assign names to the RMSE values for identification
names(rmse_values) <- c("Linear regression", "LASSO", "Ridge", "Elastic Net", "KNN", "Regression Tree", "Random Forest", "Reduced Random Forest", "Boosting", "Tuned Boosting")

# Rank RMSE values from minimum to maximum
ranked_rmse <- rmse_values[order(rmse_values)]

# Print the ranked RMSE values
print(ranked_rmse)

# Find the model with the minimum RMSE
min_rmse_model <- names(rmse_values)[which.min(rmse_values)]
min_rmse_value <- min(rmse_values)

# Output the model with the minimum RMSE
cat("The model with the minimum RMSE is", min_rmse_model, "with a value of", min_rmse_value, "\n")


#     Variable Importance -----------------------------------------------------
# Compile variable importance values for each model
importance_values <- data.frame(
  Model = c("Regression Tree", "Random Forest", "Reduced Random Forest", "Boosting", "Tuned Boosting"),
  United_States_Humanitarian = c(0.00, 0.14, 0.00, 0.69, 1.90),
  United_States_Financial = c(0.00, 0.97, 0.00, 0.00, 4.03),
  United_States_Military = c(0.00, 0.07, 0.00, 0.41, 0.17)
)

# Print the updated table
print(importance_values)


#########################################################################
# (6) Making Predictions --------------------------------------------------
#   Making a prediction for 07-2024 -----------------------------------------------------------------

Predictions <- predict(dTree, newdata = pred.df)

# Extract the last row of pred.df
last_row <- tail(pred.df, 1)

# Make a prediction for 07.2024 using the trained random forest model
prediction_07_2024 <- predict(dTree, newdata = last_row)

# Print the prediction
print(prediction_07_2024*group_GDP_2023)

#   Making predictions if countries repeated their trends (dTree)  ---------------------------------------------------
# Extract the last 11 months of aid (last 11 rows from pred.df)
last_11_rows <- tail(pred.df, 11)

# Create a new data frame for the future months, filling with the trends from the past 11 months
future_months <- data.frame(
  Month.Year = c('2024-08', '2024-09', '2024-10', '2024-11', '2024-12',
                 '2025-01', '2025-02', '2025-03', '2025-04', '2025-05', '2025-06'),
  last_11_rows[, -1],  # Exclude the target column 'y' and keep the feature columns
  stringsAsFactors = FALSE
)
# Make predictions for the future months using the trained regression tree model (dTree)
dTree_predictions <- predict(dTree, newdata = future_months)


#   Predicted Series 2024-08 to 2025-06 (dTree) -------------------------------------------


# Combine dTree_predictions  with the future_months data
future_aid_dTree <- data.frame(
  Month.Year = future_months$Month.Year,
  Total_Aid = dTree_predictions
)

# Create a data frame for the July 2024 prediction
july_prediction <- data.frame(
  Month.Year = "2024-07", 
  Total_Aid = prediction_07_2024
)

# Bind the July 2024 prediction with the remaining months
standard_predictions <- rbind(july_prediction, future_aid_dTree)

# Ensure Month.Year remains as a character and sort if necessary
standard_predictions$Month.Year <- as.character(standard_predictions$Month.Year)
standard_predictions <- standard_predictions[order(standard_predictions$Month.Year), ]

# Remove 'Total Allocations' column from total_allocations
total_allocations_subset <- total_allocations[, c("Month.Year", "Total Allocations (% GDP)")]

# Rename 'Total Allocations (% GDP)' to 'Total_Allocations' for consistency
colnames(total_allocations_subset)[2] <- "Total_Allocations"

# Add a 'Type' column indicating 'Actual' for total_allocations
total_allocations_subset$Type <- "Actual"

# Prepare standard_predictions
# Rename 'Total_Aid' to 'Total_Allocations' for consistency
standard_predictions_renamed <- standard_predictions
colnames(standard_predictions_renamed)[2] <- "Total_Allocations"

# Add a 'Type' column indicating 'Prediction' for standard_predictions
standard_predictions_renamed$Type <- "Prediction"

# Combine the data frames
allocations_series_1 <- rbind(
  total_allocations_subset,
  standard_predictions_renamed
)

# Add a new column by multiplying Total_Allocations with group_GDP_2023
allocations_series_1$Total_Allocations_EUR <- allocations_series_1$Total_Allocations * group_GDP_2023

# Convert Month.Year to a date format
allocations_series_1$Month.Year <- as.Date(paste0(allocations_series_1$Month.Year, "-01"))


#   Introducing a US shock (Random Forest) ------------------------

#      1: Make baseline predictions using Random Forest ------------------------

# Make predictions for the future months using the trained random forest model
rf_predictions <- predict(rf, newdata = future_months)

# Combine the predictions with the future_months data
future_aid_rf <- data.frame(
  Month.Year = future_months$Month.Year,
  Total_Aid = rf_predictions
)

# Step 2: View the predictions
print(future_aid_rf)

#      3: Create the Trump scenario ---------------------------------------------

# Create the Trump scenario by replacing the last 6 rows of United_States variables with 0
Trump <- future_months
Trump$United_States_military[(nrow(Trump) - 5):nrow(Trump)] <- 0
Trump$United_States_humanitarian[(nrow(Trump) - 5):nrow(Trump)] <- 0
Trump$United_States_financial[(nrow(Trump) - 5):nrow(Trump)] <- 0

# Generate predictions for the Trump scenario using the random forest model
Trump_predictions <- predict(rf, newdata = Trump)

# Combine the predictions with the modified Trump data
Trump_scenario <- data.frame(
  Month.Year = Trump$Month.Year,
  Total_Aid = Trump_predictions
)

# Step 4: View the modified predictions
print(Trump_scenario)

#      5: Create the Biden_Trump scenario ---------------------------------------

# Create the Biden_Trump scenario by modifying the first 5 rows of United_States variables
Biden_Trump <- Trump
Biden_Trump$United_States_military[1:5] <- 0.024
Biden_Trump$United_States_financial[1:5] <- 0.024
Biden_Trump$United_States_humanitarian[1:5] <- 0.024
Biden_Trump$United_States_military[(nrow(Trump) - 5):nrow(Trump)] <- 0
Biden_Trump$United_States_humanitarian[(nrow(Trump) - 5):nrow(Trump)] <- 0
Biden_Trump$United_States_financial[(nrow(Trump) - 5):nrow(Trump)] <- 0

# Generate predictions for the Biden_Trump scenario using the random forest model
Biden_Trump_predictions <- predict(rf, newdata = Biden_Trump)

# Combine the predictions with the modified Biden_Trump data
Biden_Trump_scenario <- data.frame(
  Month.Year = Biden_Trump$Month.Year,
  Total_Aid = Biden_Trump_predictions
)

# View the modified predictions
print(Biden_Trump_scenario)

#      6: Combine the Results ---------------------------------------------------

# Combine all datasets for comparison
comparison_df <- data.frame(
  Month.Year = future_aid_rf$Month.Year,
  No_Shock = future_aid_rf$Total_Aid,
  Trump = Trump_scenario$Total_Aid,
  Trump_Biden = Biden_Trump_scenario$Total_Aid
)

# Step 8: View the comparison
print(comparison_df)

# Multiply each scenario by group_GDP_2023
comparison_df_scaled <- comparison_df %>%
  mutate(
    No_Shock_EUR = No_Shock * group_GDP_2023,
    Trump_EUR = Trump * group_GDP_2023,
    Trump_Biden_EUR = Trump_Biden * group_GDP_2023
  )

# Add columns for percentage change and absolute difference in EUR
comparison_df_scaled <- comparison_df_scaled %>%
  mutate(
    Percentage_Change = ((Trump_Biden_EUR - No_Shock_EUR) / No_Shock_EUR) * 100,
    Absolute_Difference_EUR = Trump_Biden_EUR - No_Shock_EUR
  )

# Print the updated dataframe
print(comparison_df_scaled)


# Step 2: Compute cumulative values for each scenario
comparison_df_scaled_cumulative <- comparison_df_scaled %>%
  mutate(
    Cumulative_No_Shock_EUR = cumsum(No_Shock_EUR),
    Cumulative_Trump_EUR = cumsum(Trump_EUR),
    Cumulative_Trump_Biden_EUR = cumsum(Trump_Biden_EUR)
  )

# Step 3: Add cumulative values for Actual and Prediction
allocations_series_cumulative <- allocations_series_1 %>%
  group_by(Type) %>%
  mutate(Cumulative_Allocations_EUR = cumsum(Total_Allocations_EUR)) %>%
  ungroup()

# Step 4: Reshape scenario data for combining
comparison_long <- comparison_df_scaled_cumulative %>%
  select(Month.Year, Cumulative_No_Shock_EUR, Cumulative_Trump_EUR, Cumulative_Trump_Biden_EUR) %>%
  pivot_longer(
    cols = c(Cumulative_No_Shock_EUR, Cumulative_Trump_EUR, Cumulative_Trump_Biden_EUR),
    names_to = "Type",
    values_to = "Cumulative_Allocations_EUR"
  ) %>%
  mutate(
    Type = case_when(
      Type == "Cumulative_No_Shock_EUR" ~ "No Shock",
      Type == "Cumulative_Trump_EUR" ~ "Trump",
      Type == "Cumulative_Trump_Biden_EUR" ~ "Trump_Biden",
      TRUE ~ Type
    )
  )

comparison_long <- comparison_long %>%
  mutate(Month.Year = as.Date(paste0(Month.Year, "-01"), format = "%Y-%m-%d"))

# Ensure Month.Year is in Date format for both datasets
allocations_series_cumulative <- allocations_series_cumulative %>%
  mutate(Month.Year = as.Date(Month.Year))

comparison_long <- comparison_long %>%
  mutate(Month.Year = as.Date(Month.Year))

# Merge Actual, Prediction, and Scenario Data
final_data <- bind_rows(
  allocations_series_cumulative %>% select(Month.Year, Cumulative_Allocations_EUR, Type),
  comparison_long
)


# View the final dataset to verify
print(final_data)

# Step 6: Convert Month.Year to Date format for plotting
final_data <- final_data %>%
  mutate(Month.Year = as.Date(Month.Year))

# Step 7: View the final dataset
print(final_data)


#   Fig. 4) Projected Allocations Plot ----------------------------------------------

# Step 1: Prepare the datasets
# Filter Actual and Trump, and ensure Trump continues from Actual
actual_data <- allocations_series_1 %>%
  filter(Type == "Actual")

trump_data <- comparison_long %>%
  filter(Type == "Trump") %>%
  mutate(Type = "Projection") # Rename Trump to Projection

# Add the last point of Actual to the Projection dataset
last_actual_point <- actual_data %>%
  slice_tail(n = 1) %>%
  select(Month.Year, Total_Allocations_EUR) %>%
  rename(Allocations_EUR = Total_Allocations_EUR)

trump_data <- bind_rows(last_actual_point, trump_data)

# Calculate the average value for the y-axis
average_y <- mean(c(actual_data$Total_Allocations_EUR, trump_data$Allocations_EUR), na.rm = TRUE)

# Step 2: Define custom colors
custom_colors <- c(
  "Actual" = "blue",
  "Projection" = "red"
)

# Step 3: Create the plot
plot <- plot_ly() %>%
  # Add Actual line (solid)
  add_trace(
    data = actual_data,
    x = ~Month.Year,
    y = ~Total_Allocations_EUR,
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "solid", width = 2, color = custom_colors["Actual"]),
    name = "Actual",
    text = ~paste(
      "Month:", Month.Year,
      "<br>Scenario: Actual",
      "<br>Allocations (EUR):", format(Total_Allocations_EUR, big.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  # Add Projection line (dotted)
  add_trace(
    data = trump_data,
    x = ~Month.Year,
    y = ~Allocations_EUR,
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "dot", width = 2, color = custom_colors["Projection"]),
    name = "Projection",
    text = ~paste(
      "Month:", Month.Year,
      "<br>Scenario: Projection",
      "<br>Allocations (EUR):", format(Allocations_EUR, big.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  # Add dotted line for the average on the y-axis
  add_trace(
    x = ~c(min(actual_data$Month.Year), max(trump_data$Month.Year)),
    y = ~rep(average_y, 2),
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "dot", width = 1, color = "black"),
    name = "Average",
    hoverinfo = "none"
  ) %>%
  # Layout settings
  layout(
    title = "",
    xaxis = list(title = ""),
    yaxis = list(title = "Monthly Allocations (EUR)"),
    legend = list(title = list(text = ""))
  )

# Display the plot
plot




#   Fig. 5) Projected Allocations Plot (Cumulative) ---------------------------------------------------------
# Step 1: Prepare Cumulative Data for Scenarios
comparison_long <- comparison_df_scaled %>%
  select(Month.Year, No_Shock_EUR, Trump_EUR, Trump_Biden_EUR) %>%
  pivot_longer(
    cols = starts_with("No_Shock_EUR"):ends_with("Trump_Biden_EUR"),
    names_to = "Type",
    values_to = "Allocations_EUR"
  ) %>%
  mutate(
    Month.Year = as.Date(paste0(Month.Year, "-01"), format = "%Y-%m-%d"),
    Type = case_when(
      Type == "No_Shock_EUR" ~ "No Shock",
      Type == "Trump_EUR" ~ "Trump",
      Type == "Trump_Biden_EUR" ~ "Trump_Biden",
      TRUE ~ Type
    )
  )

# Step 2: Add cumulative allocations for Actual and Prediction
allocations_series_cumulative <- allocations_series_1 %>%
  mutate(Month.Year = as.Date(Month.Year)) %>%
  group_by(Type) %>%
  mutate(Cumulative_Allocations_EUR = cumsum(Total_Allocations_EUR)) %>%
  ungroup()

# Step 3: Get the last Actual value
last_actual_point <- allocations_series_cumulative %>%
  filter(Type == "Actual") %>%
  slice_tail(n = 1)

# Step 4: Adjust Scenarios to Continue from Actual
comparison_long <- comparison_long %>%
  group_by(Type) %>%
  mutate(Cumulative_Allocations_EUR = cumsum(Allocations_EUR) + last_actual_point$Cumulative_Allocations_EUR) %>%
  ungroup()

# Step 5: Adjust Prediction to Continue from Actual
prediction_with_connection <- allocations_series_cumulative %>%
  filter(Type == "Prediction") %>%
  mutate(
    Cumulative_Allocations_EUR = Cumulative_Allocations_EUR + last_actual_point$Cumulative_Allocations_EUR
  ) %>%
  bind_rows(last_actual_point %>% mutate(Type = "Prediction")) %>%
  arrange(Month.Year)

# Step 6: Add Custom Data Point to Trump
trump_with_connection <- comparison_long %>%
  filter(Type == "Trump") %>%
  mutate(
    Cumulative_Allocations_EUR = cumsum(Allocations_EUR) + last_actual_point$Cumulative_Allocations_EUR
  ) %>%
  bind_rows(data.frame(
    Month.Year = as.Date("2024-07-01"),
    Type = "Trump",
    Cumulative_Allocations_EUR = 150771192572
  )) %>%
  arrange(Month.Year)  # Correctly sort by Month.Year to avoid connection issues

# Step 7: Merge All Data
final_data <- bind_rows(
  allocations_series_cumulative %>% filter(Type == "Actual") %>% select(Month.Year, Cumulative_Allocations_EUR, Type),
  prediction_with_connection %>% select(Month.Year, Cumulative_Allocations_EUR, Type),
  comparison_long %>% filter(Type != "Prediction"),
  trump_with_connection
)

# Step 8: Define Custom Colors
custom_colors <- c(
  "Actual" = "blue",
  "Prediction" = "blue",
  "Trump" = "red",
  "No Shock" = "green",
  "Trump_Biden" = "purple"
)

# Step 9: Plot the Data
plot <- plot_ly() %>%
  # Add Actual line
  add_trace(
    data = final_data %>% filter(Type == "Actual"),
    x = ~Month.Year,
    y = ~Cumulative_Allocations_EUR,
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "solid", width = 2, color = custom_colors["Actual"]),
    name = "Actual",
    text = ~paste(
      "Month:", Month.Year,
      "<br>Scenario: Actual",
      "<br>Cumulative Allocations (EUR):", format(Cumulative_Allocations_EUR, big.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  # Add Prediction line
  add_trace(
    data = final_data %>% filter(Type == "Prediction"),
    x = ~Month.Year,
    y = ~Cumulative_Allocations_EUR,
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "dot", width = 2, color = custom_colors["Prediction"]),
    name = "Prediction",
    text = ~paste(
      "Month:", Month.Year,
      "<br>Scenario: Prediction",
      "<br>Cumulative Allocations (EUR):", format(Cumulative_Allocations_EUR, big.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  # Add Trump line
  add_trace(
    data = final_data %>% filter(Type == "Trump"),
    x = ~Month.Year,
    y = ~Cumulative_Allocations_EUR,
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "dot", width = 2, color = custom_colors["Trump"]),
    name = "Trump",
    text = ~paste(
      "Month:", Month.Year,
      "<br>Scenario: Trump",
      "<br>Cumulative Allocations (EUR):", format(Cumulative_Allocations_EUR, big.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  # Add No Shock
  add_trace(
    data = final_data %>% filter(Type == "No Shock"),
    x = ~Month.Year,
    y = ~Cumulative_Allocations_EUR,
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "dot", width = 2, color = custom_colors["No Shock"]),
    name = "No Shock",
    text = ~paste(
      "Month:", Month.Year,
      "<br>Scenario: No Shock",
      "<br>Cumulative Allocations (EUR):", format(Cumulative_Allocations_EUR, big.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  # Add Trump_Biden
  add_trace(
    data = final_data %>% filter(Type == "Trump_Biden"),
    x = ~Month.Year,
    y = ~Cumulative_Allocations_EUR,
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "dot", width = 2, color = custom_colors["Trump_Biden"]),
    name = "Trump_Biden",
    text = ~paste(
      "Month:", Month.Year,
      "<br>Scenario: Trump_Biden",
      "<br>Cumulative Allocations (EUR):", format(Cumulative_Allocations_EUR, big.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  layout(
    title = "Cumulative Allocations in EUR Over Time (Corrected View)",
    xaxis = list(title = "Month"),
    yaxis = list(title = "Cumulative Allocations (EUR)"),
    legend = list(title = list(text = "Scenario"))
  )

# Display the plot
plot


# Filter final_data to keep only Actual, Prediction, and Trump
final_data_filtered <- final_data %>%
  filter(Type %in% c("Actual", "Prediction", "Trump"))

# Extract the last point from Actual
last_actual_point <- final_data_filtered %>%
  filter(Type == "Actual") %>%
  slice_tail(n = 1)

# Add the last Actual point as a separate row to Prediction
prediction_with_connection <- final_data_filtered %>%
  filter(Type == "Prediction") %>%
  bind_rows(last_actual_point %>% mutate(Type = "Prediction")) %>%
  arrange(Month.Year)

# Add a custom point to the Trump series and ensure it is cumulative
trump_with_connection <- final_data_filtered %>%
  filter(Type == "Trump") %>%
  bind_rows(data.frame(
    Month.Year = as.Date("2024-07-01"),
    Type = "Trump",
    Cumulative_Allocations_EUR = 150771192572
  )) %>%
  arrange(Month.Year)

# Combine Actual, Prediction (connected), and Trump (corrected)
connected_data <- final_data_filtered %>%
  filter(Type != "Prediction" & Type != "Trump") %>%
  bind_rows(prediction_with_connection, trump_with_connection)

# Define custom colors for lines
custom_colors <- c(
  "Actual" = "blue",
  "Prediction" = "blue",
  "Trump" = "red"
)

# Plot the filtered and corrected data
plot <- plot_ly() %>%
  # Add Actual line
  add_trace(
    data = connected_data %>% filter(Type == "Actual"),
    x = ~Month.Year,
    y = ~Cumulative_Allocations_EUR,
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "solid", width = 2, color = custom_colors["Actual"]),
    name = "Actual",
    text = ~paste(
      "Month:", Month.Year,
      "<br>Scenario: Actual",
      "<br>Cumulative Allocations (EUR):", format(Cumulative_Allocations_EUR, big.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  # Add Prediction line
  add_trace(
    data = connected_data %>% filter(Type == "Prediction"),
    x = ~Month.Year,
    y = ~Cumulative_Allocations_EUR,
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "dot", width = 2, color = custom_colors["Prediction"]),
    name = "Prediction",
    text = ~paste(
      "Month:", Month.Year,
      "<br>Scenario: Prediction",
      "<br>Cumulative Allocations (EUR):", format(Cumulative_Allocations_EUR, big.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  # Add Trump line
  add_trace(
    data = connected_data %>% filter(Type == "Trump"),
    x = ~Month.Year,
    y = ~Cumulative_Allocations_EUR,
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "dot", width = 2, color = custom_colors["Trump"]),
    name = "Trump",
    text = ~paste(
      "Month:", Month.Year,
      "<br>Scenario: Trump",
      "<br>Cumulative Allocations (EUR):", format(Cumulative_Allocations_EUR, big.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  layout(
    title = "Foreign Aid to Ukraine (Cumulative Projection)",
    xaxis = list(title = ""),
    yaxis = list(title = "Cumulative Allocations (EUR)"),
    legend = list(title = list(text = " "))
  )

# Display the plot
plot


# Plot the filtered and corrected data
plot <- plot_ly() %>%
  # Add Actual line
  add_trace(
    data = connected_data %>% filter(Type == "Actual"),
    x = ~Month.Year,
    y = ~Cumulative_Allocations_EUR,
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "solid", width = 2, color = custom_colors["Actual"]),
    name = "Actual",
    text = ~paste(
      "Month:", Month.Year,
      "<br>Scenario: Actual",
      "<br>Cumulative Allocations (EUR):", format(Cumulative_Allocations_EUR, big.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  # Add Status quo line (rename Prediction label to Status quo)
  add_trace(
    data = connected_data %>% filter(Type == "Prediction"),
    x = ~Month.Year,
    y = ~Cumulative_Allocations_EUR,
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "dot", width = 2, color = custom_colors["Prediction"]),
    name = "Status quo",  # Renamed label
    text = ~paste(
      "Month:", Month.Year,
      "<br>Scenario: Status quo",  # Updated hover label
      "<br>Cumulative Allocations (EUR):", format(Cumulative_Allocations_EUR, big.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  # Add Projection line (rename Trump label to Projection)
  add_trace(
    data = connected_data %>% filter(Type == "Trump"),
    x = ~Month.Year,
    y = ~Cumulative_Allocations_EUR,
    type = 'scatter',
    mode = 'lines',
    line = list(dash = "dot", width = 2, color = custom_colors["Trump"]),
    name = "Projection",  # Renamed label
    text = ~paste(
      "Month:", Month.Year,
      "<br>Scenario: Projection",  # Updated hover label
      "<br>Cumulative Allocations (EUR):", format(Cumulative_Allocations_EUR, big.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  layout(
    title = "",
    xaxis = list(title = ""),
    yaxis = list(title = "Cumulative Allocations (EUR)"),
    legend = list(title = list(text = " "))
  )

# Display the plot
plot




#   Fig. 6) Difference in Allocations between Baseline and Projection (% Change)  ---------------------------------------

# Create the data frame
comparison_table <- data.frame(
  Month.Year = c("2024-08", "2024-09", "2024-10", "2024-11", "2024-12", 
                 "2025-01", "2025-02", "2025-03", "2025-04", "2025-05", "2025-06"),
  Change = c(-0.14, 12.97, 14.78, 19.55, 6.63, -2.94, 0.00, 0.00, 0.00, -0.77, -1.82)
)

# Convert Month.Year to a Date type for better plotting
comparison_table$Month.Year <- as.Date(paste0(comparison_table$Month.Year, "-01"))

# Calculate the center of the y-range
y_center <- (min(comparison_table$Change) + max(comparison_table$Change)) / 2

# Create the Plotly plot with a red dotted line
fig <- plot_ly(
  data = comparison_table,
  x = ~Month.Year,
  y = ~Change,
  type = 'scatter',
  mode = 'lines',
  line = list(shape = "linear", color = "red", dash = "dash") # Red dotted line
)

# Add a vertical line in January 2025 with a label at the center
fig <- fig %>%
  layout(
    title = "",
    xaxis = list(title = ""),
    yaxis = list(title = "Percentage Change (%)"),
    shapes = list(
      list(
        type = "line",
        x0 = as.Date("2025-01-01"),
        x1 = as.Date("2025-01-01"),
        y0 = min(comparison_table$Change),
        y1 = max(comparison_table$Change),
        line = list(color = "black", dash = "dot")
      )
    ),
    annotations = list(
      list(
        x = as.Date("2025-01-01"),
        y = y_center,
        text = " Trump Administration begins",
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "middle"
      )
    ),
    showlegend = FALSE
  )

# Display the plot
fig
