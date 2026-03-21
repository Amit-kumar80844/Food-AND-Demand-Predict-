# Data Analysis & Feature Roadmap

## 1. Current Data Analysis & Missing Aspects

Based on the existing schema (`Fact_Sales`, `Dim_Item`, `Dim_Date`, `Dim_Customer`), here is an analysis of what might be missing that limits deeper business intelligence:

### A. Missing Temporal Granularity
- **Current State:** The dimension is `Dim_Date` (daily level). 
- **Missing:** Time of day / Hour of transaction. 
- **Impact:** We cannot determine peak rush hours (e.g., Breakfast vs. Lunch rush vs. Evening snacks), which is critical for a canteen to manage staff shifts and fresh food preparation.

### B. Missing Customer Demographics
- **Current State:** `Dim_Customer` only tracks `CustomerType` (likely Student/Staff/Guest). 
- **Missing:** Unique identifiers (Customer ID), Age, Department/Major, Balance.
- **Impact:** True personalized recommendation systems and lifetime-value (LTV) cohort analysis are impossible without tracking individual repeat customers. 

### C. Missing Inventory & Supplier Tracking
- **Current State:** We track `UnitCost` and `Price`.
- **Missing:** Stock-on-hand, Expiry dates, Supplier IDs.
- **Impact:** Cannot calculate stock-out rates, wastage (food thrown away), or supplier profit margins. 

### D. Missing Payment Methods
- **Current State:** Not recorded.
- **Missing:** Cash, Card, Mobile Wallet, Campus ID Card deduction.
- **Impact:** Cannot audit transaction fees or optimize payment workflows.

---

## 2. How to Add New Features

To implement the missing features systematically, follow this pipeline upgrade process:

### Step 1: Update the Raw Dataset
Simulate or collect new columns in `data/rowData/merged_canteen_final.csv`. 
For example, adding: `transaction_time`, `customer_id`, `payment_method`.

### Step 2: Extend the Warehouse Schema (`schema.sql`)
Add new dimensions or update existing ones.
```sql
-- Example for new Time Dimension
CREATE TABLE IF NOT EXISTS Dim_Time (
    TimeKey INTEGER PRIMARY KEY AUTOINCREMENT,
    HourOfDay INTEGER,
    MealPeriod VARCHAR(50) -- 'Breakfast', 'Lunch', 'Dinner'
);

-- Example for extending Fact_Sales
ALTER TABLE Fact_Sales ADD COLUMN TimeKey INTEGER REFERENCES Dim_Time(TimeKey);
ALTER TABLE Fact_Sales ADD COLUMN PaymentMethod VARCHAR(50);
```

### Step 3: Update `01_etl_load.R`
Modify the ETL script to:
1. Parse the new `transaction_time` using `lubridate::hour()`.
2. Expand unique time records into `Dim_Time`.
3. Map the `TimeKey` while inserting into `Fact_Sales`.

### Step 4: Update Mining Models (`02_mining_models.R`)
- Include the new variables (like `HourOfDay` or `MealPeriod`) as predictors in the Random Forest formula.
- Use `customer_id` for actual Collaborative Filtering algorithms (Recommendation System) instead of just generic Apriori Market Basket.

### Step 5: Update the Shiny Dashboard (`dashboard/app.R`)
1. Create a **Heatmap** showing Sales Volume by `HourOfDay` vs. `DayOfWeek` to visualize peak rush hours.
2. Add drop-down filters for `MealPeriod` to allow users to slice data specifically for breakfast vs lunch.
3. Show Payment Method splits in a pie/donut chart.
