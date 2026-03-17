# ================================
# MERGE TWO CANTEEN DATASETS INTO TARGET SCHEMA
# Target schema:
# transaction_id, date, customer_type, weather, item_name, category,
# unit_cost, price, day_of_week, quantity, total_amount
# ================================

# Install packages if needed
# install.packages(c("dplyr", "lubridate", "readr", "stringr"))

library(dplyr)
library(lubridate)
library(readr)
library(stringr)

# ================================
# 1. FILE PATHS
# ================================
# Change these paths if needed
file1 <- "canteen_shop_data.csv"
file2 <- "college_canteen_dataset_v8.csv"

# ================================
# 2. READ DATASETS
# ================================
df1 <- read_csv(file1, show_col_types = FALSE)
df2 <- read_csv(file2, show_col_types = FALSE)

# Print column names for checking
cat("Dataset 1 columns:\n")
print(colnames(df1))

cat("\nDataset 2 columns:\n")
print(colnames(df2))

# ================================
# 3. HELPER FUNCTION: ITEM -> CATEGORY
# ================================
get_category <- function(item_name) {
    item <- tolower(trimws(item_name))

    case_when(
        str_detect(item, "tea|coffee|juice|smoothie|milkshake|soft drink|cold drink|water") ~ "Beverages",
        str_detect(item, "sandwich|samosa|puff|burger|roll|wrap|fries|chips|pakora|snack") ~ "Snacks",
        str_detect(item, "salad|fruit|fruit bowl|sprouts") ~ "Healthy",
        str_detect(item, "biryani|thali|rice|noodles|pasta|meal|pizza|paratha|dosa|idli|poha|upma|chole|paneer|curry") ~ "Main Course",
        TRUE ~ "Others"
    )
}

# ================================
# 4. CONVERT DATASET 1
# Expected columns:
# Date, Time, Item, Price, Quantity, Total, Customer ID, Payment Method,
# Employee ID, Customer Satisfaction, Weather, Special Offers
# ================================

# --- Standardize column names safely ---
# (Only if exact names exist; if not, adjust manually)
# Let's inspect first:
# print(colnames(df1))

# Convert date
# Try multiple formats safely
parse_date_safely <- function(x) {
    # Try common formats
    parsed <- suppressWarnings(as.Date(x))
    if (all(is.na(parsed))) parsed <- suppressWarnings(dmy(x))
    if (all(is.na(parsed))) parsed <- suppressWarnings(mdy(x))
    if (all(is.na(parsed))) parsed <- suppressWarnings(ymd(x))
    parsed
}

df1$Date_parsed <- parse_date_safely(df1$Date)

# Create customer_type:
# If Employee ID exists and is not NA/blank -> Staff, else Student
employee_exists <- if ("Employee ID" %in% colnames(df1)) {
    !is.na(df1$`Employee ID`) & trimws(as.character(df1$`Employee ID`)) != ""
} else {
    rep(FALSE, nrow(df1))
}

df1_converted <- df1 %>%
    mutate(
        transaction_id = 1000 + row_number(),
        date = as.Date(Date_parsed),
        customer_type = if_else(employee_exists, "Staff", "Student"),
        weather = if ("Weather" %in% colnames(.)) as.character(Weather) else "Unknown",
        item_name = as.character(Item),
        category = get_category(Item),
        # Dataset 1 has no true unit_cost -> estimate 50% of selling price
        # You can change 0.5 to 0.6 or use custom rules
        unit_cost = round(as.numeric(Price) * 0.5, 2),
        price = as.numeric(Price),
        day_of_week = if_else(
            !is.na(date),
            weekdays(date),
            if ("Date" %in% colnames(.)) as.character(Date) else "Unknown"
        ),
        quantity = as.integer(Quantity),
        total_amount = as.numeric(Total)
    ) %>%
    select(
        transaction_id, date, customer_type, weather, item_name, category,
        unit_cost, price, day_of_week, quantity, total_amount
    )

# ================================
# 5. CONVERT DATASET 2
# Expected columns:
# Date, Time, Meal_Time, Student_ID, Item, Quantity, Actual_Item_Price,
# Price, Total_Amount, Payment_Mode, Weather, Rating, Taste, DayOfWeek
# ================================

df2$Date_parsed <- parse_date_safely(df2$Date)

df2_converted <- df2 %>%
    mutate(
        transaction_id = 2000 + row_number(),
        date = as.Date(Date_parsed),
        customer_type = "Student", # Based on Student_ID dataset
        weather = if ("Weather" %in% colnames(.)) as.character(Weather) else "Unknown",
        item_name = as.character(Item),
        category = get_category(Item),
        unit_cost = as.numeric(Actual_Item_Price),
        price = as.numeric(Price),
        day_of_week = if ("DayOfWeek" %in% colnames(.)) {
            as.character(DayOfWeek)
        } else {
            if_else(!is.na(date), weekdays(date), "Unknown")
        },
        quantity = as.integer(Quantity),
        total_amount = as.numeric(Total_Amount)
    ) %>%
    select(
        transaction_id, date, customer_type, weather, item_name, category,
        unit_cost, price, day_of_week, quantity, total_amount
    )

# ================================
# 6. OPTIONAL DATA CLEANING / VALIDATION
# ================================

# Fix missing total_amount if any
df1_converted <- df1_converted %>%
    mutate(
        total_amount = if_else(
            is.na(total_amount),
            price * quantity,
            total_amount
        )
    )

df2_converted <- df2_converted %>%
    mutate(
        total_amount = if_else(
            is.na(total_amount),
            price * quantity,
            total_amount
        )
    )

# Fix missing day_of_week if any
df1_converted <- df1_converted %>%
    mutate(
        day_of_week = if_else(
            is.na(day_of_week) | day_of_week == "",
            if_else(!is.na(date), weekdays(date), "Unknown"),
            day_of_week
        )
    )

df2_converted <- df2_converted %>%
    mutate(
        day_of_week = if_else(
            is.na(day_of_week) | day_of_week == "",
            if_else(!is.na(date), weekdays(date), "Unknown"),
            day_of_week
        )
    )

# Fix negative / invalid values if any
df1_converted <- df1_converted %>%
    mutate(
        quantity = if_else(is.na(quantity) | quantity <= 0, 1L, quantity),
        price = if_else(is.na(price) | price < 0, 0, price),
        unit_cost = if_else(is.na(unit_cost) | unit_cost < 0, 0, unit_cost),
        total_amount = if_else(is.na(total_amount) | total_amount < 0, price * quantity, total_amount)
    )

df2_converted <- df2_converted %>%
    mutate(
        quantity = if_else(is.na(quantity) | quantity <= 0, 1L, quantity),
        price = if_else(is.na(price) | price < 0, 0, price),
        unit_cost = if_else(is.na(unit_cost) | unit_cost < 0, 0, unit_cost),
        total_amount = if_else(is.na(total_amount) | total_amount < 0, price * quantity, total_amount)
    )

# ================================
# 7. MERGE BOTH DATASETS
# ================================
merged_final <- bind_rows(df1_converted, df2_converted)

# ================================
# 8. SAVE OUTPUT FILES
# ================================
write_csv(df1_converted, "dataset1_converted.csv")
write_csv(df2_converted, "dataset2_converted.csv")
write_csv(merged_final, "merged_canteen_final.csv")

# ================================
# 9. PREVIEW RESULTS
# ================================
cat("\n====================\n")
cat("DATASET 1 CONVERTED PREVIEW\n")
cat("====================\n")
print(head(df1_converted, 10))

cat("\n====================\n")
cat("DATASET 2 CONVERTED PREVIEW\n")
cat("====================\n")
print(head(df2_converted, 10))

cat("\n====================\n")
cat("MERGED DATASET PREVIEW\n")
cat("====================\n")
print(head(merged_final, 15))

cat("\n====================\n")
cat("FINAL SHAPE\n")
cat("====================\n")
cat("Dataset 1 converted rows:", nrow(df1_converted), "\n")
cat("Dataset 2 converted rows:", nrow(df2_converted), "\n")
cat("Merged rows:", nrow(merged_final), "\n")
cat("Merged columns:", ncol(merged_final), "\n")

cat("\nFiles saved successfully:\n")
cat("- dataset1_converted.csv\n")
cat("- dataset2_converted.csv\n")
cat("- merged_canteen_final.csv\n")
