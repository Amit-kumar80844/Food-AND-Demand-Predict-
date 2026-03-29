# 🍽️ Smart Canteen Analytics and Food Demand Prediction System

**Repository Name:** `SmartCanteen_Team19`
**Repository Link:** [https://github.com/Amit-kumar80844/Food-AND-Demand-Predict-](https://github.com/Amit-kumar80844/Food-AND-Demand-Predict-)

---

## 👥 Team Members

| Name | Roll Number |
|:-----|:-----------|
| Amit Kumar | 2023BCS0210 |
| Gaurav Kumar | 2023BCS0114 |
| Rohit p singh | 2023BCS0138 |
| Ajeet Kumar | 2023BCS0177 |

---

## 🔍 Problem Statement

Canteen management in colleges and offices faces persistent challenges: over-stocking leads to food waste, under-stocking leads to lost revenue and dissatisfied customers, and managers have no systematic way to understand customer behavior. There is no data-driven mechanism to predict demand, understand purchasing patterns, or segment customers effectively.

This project addresses these real-world problems by building a complete end-to-end **Smart Canteen Analytics and Food Demand Prediction System** using R. Leveraging two real Kaggle datasets that were cleaned and merged, the system provides:
- Accurate **food demand forecasting** using Random Forest regression
- **Market Basket Analysis** to discover which items are frequently bought together
- **Customer segmentation** to understand different buyer profiles
- An **interactive Shiny dashboard** that makes all of this accessible to non-technical canteen administrators

---

## 🎯 Objectives

1. Source, clean, and merge two real-world Kaggle canteen datasets into a unified, high-quality dataset.
2. Design and implement a **Star Schema Data Warehouse** using SQLite for analytical querying.
3. Build an **ETL (Extract, Transform, Load) pipeline** to populate the warehouse from raw CSV data.
4. Develop a **Random Forest** model for predicting item demand based on weather, pricing, and time context.
5. Apply the **Apriori algorithm** for Market Basket Analysis to uncover cross-selling opportunities.
6. Use **K-Means Clustering** to identify and profile distinct customer segments.
7. Perform comprehensive **Exploratory Data Analysis (EDA)** to validate model assumptions.
8. Evaluate model performance using RMSE, MAE, and association rule metrics.
9. Build an interactive **R Shiny web dashboard** integrating all analytical outputs.

---

## 📦 Dataset

This project uses **two Kaggle datasets** that were downloaded separately and merged into one unified dataset for analysis.

---

### Dataset 1 — Canteen Shop Dataset

| Attribute | Value |
|:----------|:------|
| **Dataset Name** | Canteen Shop Dataset |
| **Source** | Kaggle |
| **Local Path** | `data/rowData/canteen_shop_data.csv` |
| **Number of Instances** | 200 rows |
| **Number of Attributes** | 12 columns |

**Attribute Descriptions:**

| Column | Type | Description |
|:-------|:-----|:------------|
| `Date` | Date | Date of the transaction (YYYY-MM-DD) |
| `Time` | Time | Time of the transaction |
| `Item` | Character | Name of the food/drink item purchased |
| `Price` | Numeric | Per-unit selling price (₹) |
| `Quantity` | Integer | Number of units purchased |
| `Total` | Numeric | Total transaction value (Price × Quantity) |
| `Customer ID` | Character | Unique identifier for the customer |
| `Payment Method` | Character | Mode of payment (Cash, Card, UPI) |
| `Employee ID` | Character | Staff member handling the transaction |
| `Customer Satisfaction` | Integer | Satisfaction rating (1–5 scale) |
| `Weather` | Character | Weather condition (Sunny, Rainy, Cloudy) |
| `Special Offers` | Character | Whether a promotional offer applied (Yes/No) |

---

### Dataset 2 — College Canteen Dataset

| Attribute | Value |
|:----------|:------|
| **Dataset Name** | College Canteen Dataset v8 |
| **Source** | Kaggle |
| **Local Path** | `data/rowData/college_canteen_dataset_v8.csv` |
| **Number of Instances** | 2,320 rows |
| **Number of Attributes** | 14 columns |

**Attribute Descriptions:**

| Column | Type | Description |
|:-------|:-----|:------------|
| `Date` | Date | Date of the transaction |
| `Time` | Time | Time of the transaction |
| `Meal_Time` | Character | Meal period: Breakfast, Lunch, or Snacks |
| `Student_ID` | Integer | Unique student identifier |
| `Item` | Character | Food item purchased |
| `Quantity` | Integer | Number of units purchased |
| `Actual_Item_Price` | Numeric | Original price before any discount |
| `Price` | Numeric | Final per-unit price paid |
| `Total_Amount` | Numeric | Total transaction amount |
| `Payment_Mode` | Character | Payment method (Cash, Card, UPI) |
| `Weather` | Character | Weather at time of purchase |
| `Rating` | Numeric | Customer satisfaction (out of 5) |
| `Taste` | Character | Qualitative rating: Good / Average / Poor |
| `DayOfWeek` | Character | Day name (Monday, Tuesday, etc.) |

---

### Merged Dataset — Final Analysis Dataset

| Attribute | Value |
|:----------|:------|
| **Local Path** | `data/rowData/merged_canteen_final.csv` |
| **Total Instances** | 2,520 rows (200 + 2,320) |
| **Merge Script** | `data/rowData/dataSetMerger.R` |

Both datasets were harmonized by standardizing column names, aligning data types, filling missing attributes with `NA`, and stacking them into a single unified dataset. This merged file is the input to all ETL, modeling, and visualization scripts.

> **Note on Dataset Availability:** These datasets are sourced from Kaggle and are available locally in `data/rowData/`. They are **not uploaded** directly to GitHub to respect licensing terms. The merging logic is fully documented and reproducible using `data/rowData/dataSetMerger.R`.

---

## 🔧 Methodology

### 1. Data Preprocessing (`01_etl_load.R`)
- Loaded both raw Kaggle CSV files into R.
- Standardized column names to a consistent snake_case format.
- Parsed date/time strings into proper `Date` and `POSIXct` objects using `lubridate`.
- Handled missing values: `NA` introduced where attributes were absent in one dataset.
- Derived new features: `day_of_week`, `month`, `hour_of_day`, `is_weekend`.
- Normalized price and quantity fields to a consistent numeric format.
- Created the **Star Schema** tables in SQLite (`canteen_dw.sqlite`):
  - `dim_item` — Item catalog with categories.
  - `dim_customer` — Customer types and IDs.
  - `dim_date` — Full date dimension with week/month/quarter breakdowns.
  - `dim_weather` — Weather lookup table.
  - `fact_sales` — Central fact table joining all dimensions with sales quantities and amounts.

### 2. Exploratory Data Analysis (`03_visual_analysis.R`)
- Plotted sales trends over time using line charts.
- Generated histograms for price, quantity, and total transaction amount.
- Used boxplots to examine distributions by weather and category.
- Computed a correlation heatmap across numeric features.
- Identified the top-selling item categories with bar plots.
- Examined scatter relationships between price, quantity, and revenue.

### 3. Models Used

| Model | Script | Purpose |
|:------|:-------|:--------|
| **Random Forest** | `02_mining_models.R` | Predicts item quantity demand |
| **Apriori Algorithm** | `02_mining_models.R` | Market Basket Analysis |
| **K-Means Clustering** | `02_mining_models.R` | Customer Segmentation |

**Random Forest (Demand Prediction):**
- Target variable: `quantity` (number of units sold per transaction).
- Features: `price`, `weather`, `day_of_week`, `is_weekend`, `special_offers`, `meal_time`, `category`.
- Dataset split: 80% training / 20% testing.
- Hyperparameter tuned using `caret` with 5-fold cross-validation.

**Apriori (Market Basket Analysis):**
- Transformed transaction data into a basket format (one row per `transaction_id`).
- Applied Apriori with `support = 0.01`, `confidence = 0.4`.
- Mined top rules ranked by `lift`.

**K-Means Clustering (Customer Segmentation):**
- Features: `total_spend` (sum of Total_Amount) and `purchase_frequency` (transaction count) per customer.
- Applied the Elbow Method to determine optimal k.
- Final model: `k = 3` clusters representing high-value, mid-tier, and budget customers.

### 4. Evaluation Methods (`04_model_evaluation.R`)
- **Random Forest:** Root Mean Squared Error (RMSE) and Mean Absolute Error (MAE) on test set.
- **Apriori:** Evaluated rules by support, confidence, and lift thresholds.
- **K-Means:** Within-cluster sum of squares (WCSS) plotted via Elbow Method.

---

## 📊 Results

### Demand Forecasting (Random Forest)

| Metric | Value |
|:-------|:------|
| RMSE | ~0.65 |
| MAE | ~0.48 |
| R² | ~0.84 |

The model accurately captured demand patterns. `weather`, `price`, and `day_of_week` were identified as the top predictors of item demand. Demand spikes were correctly anticipated on rainy days (hot beverages) and during lunch on weekdays.

### Market Basket Analysis (Apriori)

| Rule | Support | Confidence | Lift |
|:-----|:--------|:-----------|:-----|
| Coffee → Sandwich | 0.04 | 0.62 | 2.40 |
| Tea → Snack | 0.03 | 0.55 | 2.10 |
| Juice → Burger | 0.02 | 0.48 | 1.85 |

Key insight: Beverages are strong drivers of food item purchases, especially during Breakfast and Snack time.

### Customer Segmentation (K-Means)

| Cluster | Profile | Avg. Spend | Avg. Visits |
|:--------|:--------|:-----------|:------------|
| Cluster 1 | High-Value Regulars | High | High |
| Cluster 2 | Occasional Buyers | Medium | Medium |
| Cluster 3 | Budget/Infrequent | Low | Low |

---

## 🖼️ Key Visualizations

All plots are saved in `results/figures/`. They are fully embedded below:

### 📈 Sales Trend Over Time
![Sales Trend](results/figures/sales_trend_linechart.png)

### 🏷️ Top Selling Categories
![Top Categories](results/figures/top_categories_barplot.png)

### 🔥 Correlation Heatmap
![Correlation Heatmap](results/figures/correlation_heatmap.png)

### 🌦️ Quantity vs. Weather (Boxplot)
![Quantity vs Weather](results/figures/boxplot_quantity_vs_weather.png)

### 💰 Price vs. Total Amount (Scatter)
![Price vs Total Amount](results/figures/scatter_price_vs_total_amount.png)

### 📦 Quantity Distribution (Histogram)
![Quantity Histogram](results/figures/histogram_quantity.png)

### 💵 Price Distribution (Histogram)
![Price Histogram](results/figures/histogram_price.png)

### 🧮 Total Amount Distribution (Histogram)
![Total Amount Histogram](results/figures/histogram_total_amount.png)

### 📊 Quantity vs. Price (Boxplot)
![Quantity vs Price Boxplot](results/figures/boxplot_quantity_vs_price.png)

### 🤖 Random Forest — Actual vs. Predicted
![RF Actual vs Predicted](results/figures/rf_actual_vs_predicted.png)

### ⭐ Random Forest — Feature Importance
![RF Feature Importance](results/figures/rf_feature_importance.png)

### 👥 K-Means Customer Segments
![Customer Segments](results/figures/kmeans_customer_segments.png)

---

## 📺 Dashboard

The interactive Shiny dashboard (`app/app.R`) integrates all analytical components into a single web interface for non-technical users.

![Shiny Dashboard](results/figures/1.jpeg)
![Shiny Dashboard](results/figures/2.jpeg)
![Shiny Dashboard](results/figures/3.jpeg)
![Shiny Dashboard](results/figures/4.jpeg)
![Shiny Dashboard](results/figures/5.jpeg)

### Dashboard Features:
- **Overview Tab:** Summary KPIs — total revenue, top item, peak hour, number of transactions.
- **Demand Forecasting Tab:** Select an item, enter weather and price conditions, get the predicted daily demand.
- **Market Basket Tab:** Browse Apriori rules — filter by confidence and lift thresholds.
- **Customer Insights Tab:** Scatter plot of K-Means clusters with interactive tooltips via `plotly`.
- **Data Explorer Tab:** Browse the full merged warehouse fact table with `DT` interactive table.

---

## 🗂️ Project Structure

```
SmartCanteen_TeamX/
├── README.md                         # Full project documentation (this file)
├── requirements.R                    # All required R packages
│
├── data/
│   ├── dataset_description.md        # Dataset info (Kaggle sources)
│   ├── canteen_dw.sqlite             # SQLite Data Warehouse
│   └── rowData/
│       ├── canteen_shop_data.csv     # Kaggle Dataset 1 (200 rows)
│       ├── college_canteen_dataset_v8.csv  # Kaggle Dataset 2 (2,320 rows)
│       ├── merged_canteen_final.csv  # Final merged dataset (2,520 rows)
│       └── dataSetMerger.R           # Script to merge both datasets
│
├── scripts/
│   ├── 00_generate_data.R            # (Legacy) Synthetic data generator
│   ├── 01_etl_load.R                 # ETL: Load & transform into star schema
│   ├── 02_mining_models.R            # ML Models: RF, Apriori, K-Means
│   ├── 03_visual_analysis.R          # EDA plots & visualizations
│   └── 04_model_evaluation.R         # Model evaluation & metrics
│
├── app/
│   └── app.R                         # Shiny Dashboard application
│
├── models/
│   ├── demand_model.rds              # Trained Random Forest model
│   ├── basket_rules.rds              # Apriori association rules object
│   └── kmeans_model.rds             # K-Means clustering result
│
├── results/
│   ├── figures/                      # All generated plot images
│   │   ├── sales_trend_linechart.png
│   │   ├── correlation_heatmap.png
│   │   ├── rf_feature_importance.png
│   │   ├── kmeans_customer_segments.png
│   │   └── ... (18 total plots)
│   └── tables/
│       └── model_performance.csv     # Evaluation metrics table
│
├── presentation/
│   └── project_presentation.pptx    # 10–15 slide presentation
│
└── docs/
    └── README_PROJECT_FLOW.md        # End-to-end workflow documentation
```

---

## 🔄 Diagram

The following diagram illustrates the end-to-end data flow of the system:

```
┌─────────────────────────────────────────────┐
│              DATA ACQUISITION               │
│  Kaggle Dataset 1  +  Kaggle Dataset 2      │
│  (canteen_shop_data.csv)                    │
│  (college_canteen_dataset_v8.csv)           │
└─────────────────┬───────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────┐
│             DATA MERGING                    │
│  dataSetMerger.R                            │
│  → Standardize columns                      │
│  → Align data types                         │
│  → Stack & output merged_canteen_final.csv  │
└─────────────────┬───────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────┐
│           ETL PIPELINE  (01_etl_load.R)     │
│  → Extract from merged CSV                  │
│  → Transform (clean, derive features)       │
│  → Load into SQLite Star Schema             │
│     ┌────────────────────────────────────┐  │
│     │ dim_item | dim_customer | dim_date │  │
│     │ dim_weather | fact_sales           │  │
│     └────────────────────────────────────┘  │
└─────────────────┬───────────────────────────┘
                  │
        ┌─────────┴──────────┐
        ▼                    ▼
┌───────────────┐  ┌────────────────────────────┐
│  EDA & PLOTS  │  │   ML MODEL TRAINING         │
│ (03_visual_   │  │  (02_mining_models.R)        │
│  analysis.R)  │  │  → Random Forest (Demand)   │
│  → 18 plots   │  │  → Apriori (Market Basket)  │
│  saved to     │  │  → K-Means (Segmentation)   │
│  results/     │  │  → Save .rds to models/     │
│  figures/     │  └────────────┬───────────────┘
└───────┬───────┘               │
        │               ┌───────┴────────────┐
        │               │  MODEL EVALUATION  │
        │               │ (04_model_eval.R)  │
        │               │  → RMSE, MAE, R²   │
        │               │  → Lift, Conf.     │
        │               │  → WCSS Elbow Plot │
        │               └───────┬────────────┘
        │                       │
        └──────────┬────────────┘
                   ▼
┌─────────────────────────────────────────────┐
│          SHINY DASHBOARD  (app/app.R)        │
│  → Overview | Demand Forecast               │
│  → Market Basket | Customer Insights        │
│  → Data Explorer                            │
└─────────────────────────────────────────────┘
```
![Activity Diagram](docs/activity_diagram.jpeg)
![Project Structure](docs/project_structure.jpeg)
![Star Schema](docs/schema.jpeg)
---

## ▶️ How to Run the Project

### Prerequisites
- R (version ≥ 4.0)
- RStudio (recommended for interactive use)

### Step 1 — Install Required Packages

Run the following in your R console or terminal:
```r
source("requirements.R")
```

Or install individually:
```r
install.packages(c("dplyr", "lubridate", "DBI", "RSQLite", "caret",
                   "randomForest", "arules", "ggplot2", "corrplot",
                   "shiny", "shinydashboard", "plotly", "DT"))
```

### Step 2 — ETL: Load Data into Warehouse
```r
Rscript scripts/01_etl_load.R
```
For R studio
```r
source("scripts/01_etl_load.R")
```
> This reads from `data/rowData/merged_canteen_final.csv`, cleans and transforms the data, and loads it into the SQLite star schema in `data/canteen_dw.sqlite`.

### Step 3 — Train ML Models
```r
Rscript scripts/02_mining_models.R
```
For R studio
```r
source("scripts/02_mining_models.R")
```
> Trains the Random Forest, Apriori, and K-Means models. Saves results to `models/`.

### Step 4 — Run Exploratory Analysis
```r
Rscript scripts/03_visual_analysis.R
```
For R studio
```r
source("scripts/03_visual_analysis.R")
```
> Generates 18 plots and saves them to `results/figures/`.

### Step 5 — Evaluate Models
```r
Rscript scripts/04_model_evaluation.R
```
For R studio
```r
source("scripts/04_model_evaluation.R")
```

> Prints metrics to console and saves `results/tables/model_performance.csv`.

### Step 6 — Launch Shiny Dashboard
```powershell
Rscript -e "shiny::runApp('app', launch.browser=TRUE)"
```
For R studio
```r
# Launch Shiny app
shiny::runApp("app", launch.browser = TRUE)
```
> Starts the local web server and opens the dashboard automatically in your default browser.

---

### 📁 Folder Organization Summary

| Folder/File | Purpose |
|:------------|:--------|
| `data/rowData/` | Raw Kaggle CSVs and merging script |
| `data/canteen_dw.sqlite` | Final SQLite Data Warehouse |
| `data/dataset_description.md` | Dataset metadata and source info |
| `scripts/` | All R scripts (ETL → EDA → Modeling → Evaluation) |
| `app/app.R` | Shiny interactive dashboard |
| `models/` | Saved `.rds` model files |
| `results/figures/` | All 18 generated plots |
| `results/tables/` | Evaluation metrics CSV |
| `presentation/` | PowerPoint presentation slides |
| `requirements.R` | Package installation script |

---

## 🏁 Conclusion

This project successfully demonstrates a complete end-to-end data pipeline for Smart Canteen Analytics:

1. **Data Integration:** Two heterogeneous Kaggle datasets were merged into a robust unified dataset of 2,520 transactions with harmonized attributes.
2. **Data Warehousing:** A Star Schema in SQLite provides an efficient foundation for analytical querying and model training.
3. **Demand Forecasting:** The Random Forest model achieves strong predictive accuracy (R² ≈ 0.84), making it practical for daily inventory planning.
4. **Market Basket Analysis:** Apriori rules identify high-confidence combinations like Coffee → Sandwich (lift 2.4x), enabling targeted cross-selling.
5. **Customer Segmentation:** K-Means identifies three operationally meaningful customer segments, enabling personalized promotions.
6. **Dashboard:** The Shiny application democratizes access to these insights, providing real-time prediction and exploration tools to non-technical canteen staff.

The system reduces guesswork in inventory management and provides a scalable template for canteen analytics in any institutional setting.

---

## 🤝 Contribution

| 2023BCS0210| Contribution |
|:------------|:-------------|
| **001** | ETL pipeline (`01_etl_load.R`) |
| **002** | Random Forest model development, hyperparameter tuning, model evaluation (`04_model_evaluation.R`) |
| **003** | Exploratory Data Analysis (`03_visual_analysis.R`), writing  README |
| **004** | Apriori Market Basket Analysis, K-Means clustering (`02_mining_models.R`),Coding and Implementation |

| 2023BCS0144| Contribution |
|:------------|:-------------|
| **001** | Dataset sourcing (Kaggle), Star Schema design |
| **002** | visualization design|
| **003** | Shiny dashboard (`app/app.R`) |

| 2023BCS0138| Contribution |
|:------------|:-------------|
| **001** | Implemented data visualization in R (03_visual_analysis.R) |
| **002** | Created box plots, scatter plots, and histograms for analysis |
| **003** | Performed Exploratory Data Analysis (EDA) to identify patterns and trends |
| **004** | Analyzed relationships between price, quantity, weather, and revenue |


| 2023BCS0177| Contribution |
|:------------|:-------------|
| **001** | Dataset sourcing (Kaggle), data merging script (`dataSetMerger.R`). Star Schema design |
---

## 📚 References

1. Kaggle — *Canteen Shop Dataset* — [https://www.kaggle.com/datasets/susanta21/canteen-shop-transaction-data](https://www.kaggle.com/datasets/susanta21/canteen-shop-transaction-data)
2. Kaggle — *College Canteen Dataset v8* — [https://www.kaggle.com/datasets/reachout2meera/canteen-food-dataset](https://www.kaggle.com/datasets/reachout2meera/canteen-food-dataset)
---

## 📋 R Package Requirements

The following packages are required. Run `source("requirements.R")` to install them all:

```r
install.packages("dplyr")        # Data manipulation
install.packages("lubridate")    # Date/time parsing
install.packages("DBI")          # Database interface
install.packages("RSQLite")      # SQLite backend
install.packages("caret")        # ML training framework
install.packages("randomForest") # Random Forest algorithm
install.packages("arules")       # Apriori association rules
install.packages("ggplot2")      # Data visualization
install.packages("corrplot")     # Correlation heatmaps
install.packages("shiny")        # Web dashboard framework
install.packages("shinydashboard") # Dashboard UI components
install.packages("plotly")       # Interactive plots
install.packages("DT")           # Interactive data tables
```
