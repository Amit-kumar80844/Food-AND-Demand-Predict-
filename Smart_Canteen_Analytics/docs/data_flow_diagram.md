# Data Retrieval Flow Diagram

This PlantUML sequence diagram explains how the Shiny Dashboard retrieves and processes data from the SQLite Data Warehouse and the Machine Learning models during runtime.

```plantuml
@startuml
!theme plain

actor User
participant "Shiny Dashboard\n(app.R)" as UI
database "SQLite Data Warehouse\n(canteen_dw.sqlite)" as DB
participant "Predictive Models\n(.rds files)" as ML

User -> UI : Opens Dashboard / Applies Filters
activate UI

UI -> DB : Executes SQL Query\n(e.g., SELECT * FROM Fact_Sales JOIN...)
activate DB
DB --> UI : Returns Filtered Result Set
deactivate DB

UI -> UI : Processes Data\n(Aggregations, Metrics)

alt Demand Forecasting Requested
    UI -> ML : Loads rf_demand_model.rds
    activate ML
    ML --> UI : Model Object
    deactivate ML
    UI -> ML : Predict(Item, Weather, Price)
    activate ML
    ML --> UI : Predicted Quantity
    deactivate ML
end

UI --> User : Renders Plots, KPI Cards,\nand Data Tables
deactivate UI

@enduml
```
