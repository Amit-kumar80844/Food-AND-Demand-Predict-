# Database Schema Diagram

This PlantUML diagram details the Entity-Relationship (ER) model of the `canteen_dw.sqlite` Data Warehouse. It uses a Star Schema design with one central Fact table and three Dimension tables.

```plantuml
@startuml
!theme plain

entity "Fact_Sales" as Fact_Sales {
  * SalesKey : INTEGER <<PK>>
  --
  * DateKey : INTEGER <<FK>>
  * ItemKey : INTEGER <<FK>>
  * CustomerKey : INTEGER <<FK>>
  Quantity : INTEGER
  TotalAmount : DECIMAL(10,2)
  UnitCost : DECIMAL(10,2)
  Weather : VARCHAR(50)
}

entity "Dim_Item" as Dim_Item {
  * ItemKey : INTEGER <<PK>>
  --
  ItemName : VARCHAR(255)
  Category : VARCHAR(100)
  UnitCost : DECIMAL(10,2)
  Price : DECIMAL(10,2)
}

entity "Dim_Date" as Dim_Date {
  * DateKey : INTEGER <<PK>>
  --
  FullDate : DATE
  DayOfWeek : VARCHAR(20)
  IsWeekend : BOOLEAN
  IsHoliday : BOOLEAN
}

entity "Dim_Customer" as Dim_Customer {
  * CustomerKey : INTEGER <<PK>>
  --
  CustomerType : VARCHAR(50)
}

Dim_Date ||--o{ Fact_Sales : "1:N"
Dim_Item ||--o{ Fact_Sales : "1:N"
Dim_Customer ||--o{ Fact_Sales : "1:N"

@enduml
```
