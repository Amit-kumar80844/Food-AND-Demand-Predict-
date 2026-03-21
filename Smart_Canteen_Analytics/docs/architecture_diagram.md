# System Architecture Diagram

This PlantUML diagram illustrates the high-level architecture of the Smart Canteen Analytics system, showing how data flows from the source CSV through the ETL process into the data warehouse, gets processed by R scripts to generate models/visuals, and finally surfaces on the Shiny dashboard.

```plantuml
@startuml
!theme plain
skinparam componentStyle rectangle

package "Data Sources" {
  [Raw CSV Data\n(merged_canteen_final.csv)] as RawCSV
}

package "Automated Pipeline (R Scripts)" {
  [01_etl_load.R\n(Clean, Transform, Load)] as ETL
  [02_mining_models.R\n(RF, Apriori, K-Means)] as Models
  [03_visual_analysis.R\n(Static EDA Plots)] as Visuals
}

database "Data Warehouse\n(canteen_dw.sqlite)" as DW {
  [Star Schema Tables\n(Fact & Dimensions)] as Schema
}

folder "Outputs" {
  [ML Models (.rds)] as MLModels
  [Static Plots (.png)] as Plots
}

package "User Interface" {
  [Shiny Dashboard\n(app.R)] as Dashboard
}

RawCSV --> ETL : Ingests
ETL --> Schema : Loads Clean Data
Schema --> Models : Fetches Training Data
Schema --> Visuals : Fetches Data for Plots
Models --> MLModels : Saves Trained Models
Visuals --> Plots : Saves Visuals
Dashboard --> Schema : Queries Live Data
Dashboard --> MLModels : Uses for Predictions
@enduml
```
