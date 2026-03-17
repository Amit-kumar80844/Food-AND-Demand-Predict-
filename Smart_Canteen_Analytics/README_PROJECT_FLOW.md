# Smart Canteen Analytics & Demand Forecasting System
## Software Architecture & Workflow Documentation

### A. Project Purpose
The Smart Canteen Analytics software provides an automated data warehousing and analytics pipeline for a college or enterprise canteen. It ingests raw transaction records, cleans the data, transforms it into a dimensional star schema (Data Warehouse), and applies data mining models (Random Forest, Apriori) to uncover insights. A visually rich, interactive Shiny dashboard enables the user to drill down into revenue trends, historical sales data, forecasting, market basket rules, and customer segments.

### B. Folder Structure
- **`data/`**: Stores data files.
  - `rowData/merged_canteen_final.csv`: The new raw dataset source for the ETL pipeline.
  - `canteen_dw.sqlite`: The SQLite data warehouse containing fact and dimension tables.
- **`warehouse/`**:
  - `schema.sql`: Data Definition Language (DDL) for creating tables (`Fact_Sales`, `Dim_Item`, `Dim_Date`, `Dim_Customer`).
- **`scripts/`**: Core automated processes.
  - `01_etl_load.R`: Execution script for Extraction, Transformation, and Loading pipeline.
  - `02_mining_models.R`: Script that trains Random Forest models, Market Basket Analysis rules, and clustering.
  - `03_visual_analysis.R`: Generates high-quality static visualizations of the data.
- **`models/`**: Stores the output `.rds` artifacts produced by `02_mining_models.R` (e.g. `rf_demand_model.rds`, `association_rules.rds`).
- **`outputs/plots/`**: Stores static images (pngs) built by the visual analysis script, such as boxplots, scatter plots, and heatmaps.
- **`dashboard/`**:
  - `app.R`: The interactive Shiny web application displaying data overviews, dynamic visual charts (EDA), and forecast prediction tools.

### C. Data Flow
1. **Raw Data Input:** Ingestion of `data/rowData/merged_canteen_final.csv`.
2. **Cleaning & Transformation:** Cleaning NA quantities, parsing various irregular date strings into standard shapes, detecting duplicates and mapping standard names.
3. **Warehouse Loading (ETL):** Expanding records into `Dim_Item`, `Dim_Date`, `Dim_Customer`, and inserting mapped transactions with foreign keys into `Fact_Sales`.
4. **Modeling:** The script extracts a consolidated flat-view from `Fact_Sales` joining dimensions to train algorithms on. Output models are preserved to `models/`.
5. **Visualization:** Creation of standalone graphical static plots like `boxplot_total_amount.png` to map relationships.
6. **Dashboard Reusability:** Starts up local web server to dynamically query SQLite and render visual panels and tables.

### D. Script-by-Script Explanation
1. **`01_etl_load.R`**: This connects to the `merged_canteen_final.csv` dataset, processes mixed-format dates (using lubridate), normalizes the dimensions (e.g. keeping distinct price-item bindings), clears out the old data warehouse values, connects to the SQLite engine, reads `warehouse/schema.sql`, and pumps cleanly inserted data into dimensions and facts.
2. **`02_mining_models.R`**: Joins table keys via a SQL standard query to fetch the training corpus. It trains a Random Forest algorithm predicting `DailyQuantity` using fields `ItemName`, `Price`, `Weather`, and `DayOfWeek`. It also utilizes `apriori` algorithms to discover association rules (Market Baskets).
3. **`03_visual_analysis.R`**: Pulls the most updated data from SQLite to create correlation matrices, heatmaps, boxplot distributions, scattered relationships, time serial curves, and outputs graphic images (`.png`) internally to `outputs/plots/`.

### E. How to Run the Project
To run the full end-to-end software pipeline sequentially, use the following Rscript executions in powershell at the root project directory:

```powershell
Rscript scripts/01_etl_load.R
Rscript scripts/02_mining_models.R
Rscript scripts/03_visual_analysis.R
Rscript -e "shiny::runApp('dashboard', launch.browser=TRUE)"
```

### F. Output Explanation
- **Processed Datasets**: Stored dynamically within `data/canteen_dw.sqlite`. The database enforces clean keys ensuring no data corruption.
- **Model Files**: `models/rf_demand_model.rds` (Predictive Model), `models/association_rules.rds` (Market Baskets rules), and `models/customer_segments.rds` (K-Means groups).
- **Chart Objects**: Generated and saved as standalone high resolution graphical pngs located within `outputs/plots/`. 

### G. Dashboard Explanation
Run `shiny::runApp('dashboard')` to access:
- **Dashboard Overview**: Immediate glance at global KPIs, such as Total Revenue, Units Sold, and Average Unit Revenue, along with interactive recent transactions table (`datatable` component). Trends plotted across days using local curve fits.
- **Visual Analysis (EDA)**: Embedded dashboard view evaluating Boxplot distributions (Amount spread), Histogram density (Quantity frequency), Scatter mapping (Quantity vs Total Amount colored conceptually by category), and numeric parameter correlation matrix. 
- **Demand Forecasting**: Control Panel selecting specific item variants and simulated environmental variables (Price/Weather). Outputs prediction derived directly from the trained historical Random Forest model dynamically.
- **Market Basket Analysis & Customers**: Apriori calculated metric grids showing item purchasing coincidences.

### H. Troubleshooting Section
- **Missing Packages:** A prerequisite block is included at the top of the scripts (e.g., `lubridate`, `corrplot`, `DT`). Check network connection if R tries installing but fails. Ensure CRAN mirrors are permitted. 
- **Invalid dashboard folder warning:** Be mindful of where PowerShell's current working directory (cwd) is placed. You need to be inside the `Smart_Canteen_Analytics/` folder before firing `shiny::runApp('dashboard')`.
- **CSV Parsing Issues:** If the CSV features unrecognized dates, check `lubridate::parse_date_time` execution on script 1 (`01_etl_load.R`). Make sure the new CSV format (`transaction_id, date, customer_type, weather...`) hasn't drastically changed headers.
- **Shiny app not starting / Already running:** If a previous Shiny app is blocking Port/Address, close the running terminal and launch a fresh one, or reload your IDE session. Stop the currently running `Rscript` command before starting a new one.
- **Plot errors:** When running `scripts/03_visual_analysis.R`, if headless without graphics support, the default R plotting engine might hiccup; run on a standard Windows graphical interface.
