# 00_generate_data.R
# Pre-requisite: Install necessary packages if missing
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")

library(dplyr)
library(lubridate)

set.seed(123)

# 1. Setup Parameters
n_rows <- 10000
start_date <- as.Date("2024-01-01")
end_date <- as.Date("2024-12-31")

# 2. Define Reference Data
items <- c("Chicken Biryani", "Veg Thali", "Pasta", "Coffee", "Tea", "Sandwich", "Fruit Bowl")
categories <- c("Main Course", "Main Course", "Main Course", "Beverages", "Beverages", "Snacks", "Healthy")
unit_costs <- c(70, 50, 90, 15, 8, 30, 25)
prices <- c(120, 80, 150, 40, 20, 60, 50)
menu <- data.frame(item_name = items, category = categories, unit_cost = unit_costs, price = prices)

# 3. Generate Base Data
dates <- sample(seq(start_date, end_date, by="day"), n_rows, replace = TRUE)
customer_types <- sample(c("Student", "Staff", "Visitor"), n_rows, replace = TRUE, prob = c(0.75, 0.20, 0.05))
weather_conditions <- sample(c("Sunny", "Rainy", "Cloudy"), n_rows, replace = TRUE, prob = c(0.6, 0.2, 0.2))

# 4. Create Transaction Dataframe
canteen_data <- data.frame(
  transaction_id = 1:n_rows,
  date = dates,
  customer_type = customer_types,
  weather = weather_conditions
)

# Assign Items based on random selection
item_indices <- sample(1:nrow(menu), n_rows, replace = TRUE)
canteen_data$item_name <- menu$item_name[item_indices]
canteen_data$category <- menu$category[item_indices]
canteen_data$unit_cost <- menu$unit_cost[item_indices]
canteen_data$price <- menu$price[item_indices]

# 5. Add Quantity and Time Logic
canteen_data <- canteen_data %>%
  mutate(
    day_of_week = wday(date, label = TRUE, abbr = FALSE),
    # Quantity is higher for students and snacks
    quantity = ifelse(category == "Snacks" & customer_type == "Student", sample(2:4, n(), replace=TRUE),
               ifelse(category == "Beverages", sample(1:3, n(), replace=TRUE), 1)),
    total_amount = price * quantity
  ) %>%
  arrange(date, transaction_id)

# 6. Save Data
output_file <- file.path(dirname(getwd()), "data", "synthetic_canteen_data.csv")
# To handle running from root vs scripts dir
if (!dir.exists("data") && dir.exists("../data")) {
    output_file <- "../data/synthetic_canteen_data.csv"
} else if (dir.exists("data")) {
    output_file <- "data/synthetic_canteen_data.csv"
} else {
    output_file <- "C:/Users/amits/Data WareHouse Model/Smart_Canteen_Analytics/data/synthetic_canteen_data.csv"
}

write.csv(canteen_data, output_file, row.names = FALSE)
print(paste("Data generation complete! Saved to:", output_file))
print("Preview:")
print(head(canteen_data))
