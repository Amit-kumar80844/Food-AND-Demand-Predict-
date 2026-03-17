# Smart Canteen Analytics Project Guide

This document outlines how to run the end-to-end Smart Canteen Analytics and Food Demand Prediction System in R.

## Project Structure
The project uses SQLite as the Data Warehouse and R for all data manipulation, modeling, and visualization.

- `data/`: Contains the raw synthetic CSV and the `canteen_dw.sqlite` database.
- `scripts/`: Contains the R scripts for the pipeline.
- `warehouse/`: Contains the Star Schema SQL definition.
- `models/`: Contains the trained machine learning `.rds` files.
- `dashboard/`: Contains the Shiny `app.R` file.

## Execution Steps

If you are running this for the first time, follow these steps in order:

### 1. Generate Synthetic Data
This script creates a realistic transactional dataset (`synthetic_canteen_data.csv`).
**Run from the root directory (`Smart_Canteen_Analytics/`):**
```R
Rscript scripts/00_generate_data.R
```

### 2. ETL Process & Data Warehouse Load
This script cleans the data, creates the Star Schema tables in SQLite (if they don't exist), and loads the Facts and Dimensions into `data/canteen_dw.sqlite`.
**Run from the root directory:**
```R
Rscript scripts/01_etl_load.R
```

### 3. Data Mining & Machine Learning
This script trains the Random Forest model for Demand Prediction, the Apriori algorithm for Market Basket Analysis, and K-Means clustering. It saves the resulting outputs into the `models/` directory.
**Run from the root directory:**
```R
Rscript scripts/02_mining_models.R
```

### 4. Launch the Dashboard
The Shiny application integrates the database and the models into a real-time web interface.
**Run this command directly in your Windows terminal (PowerShell/Command Prompt):**
```powershell
Rscript -e "shiny::runApp('dashboard', launch.browser=TRUE)"
```
This will start the local server and automatically open the dashboard in your default web browser.

## Key Features
- **Overview Area**: View summary statistics and top-selling items.
- **Demand Forecasting**: Select an item, provide the weather/price conditions, and get tomorrow's predicted unit demand.
- **Market Basket**: View Apriori rules to see which items are frequently bought together.
- **Customer Insights**: Analyze customer purchasing behavior by type.
