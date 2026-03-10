-- DDL for Canteen Data Warehouse SQLite
CREATE TABLE IF NOT EXISTS Dim_Item (
    ItemKey INTEGER PRIMARY KEY AUTOINCREMENT,
    ItemName VARCHAR(255),
    Category VARCHAR(100),
    UnitCost DECIMAL(10,2),
    Price DECIMAL(10,2)
);

CREATE TABLE IF NOT EXISTS Dim_Date (
    DateKey INTEGER PRIMARY KEY AUTOINCREMENT,
    FullDate DATE,
    DayOfWeek VARCHAR(20),
    IsWeekend BOOLEAN,
    IsHoliday BOOLEAN
);

CREATE TABLE IF NOT EXISTS Dim_Customer (
    CustomerKey INTEGER PRIMARY KEY AUTOINCREMENT,
    CustomerType VARCHAR(50)
);

CREATE TABLE IF NOT EXISTS Fact_Sales (
    SalesKey INTEGER PRIMARY KEY AUTOINCREMENT,
    DateKey INTEGER,
    ItemKey INTEGER,
    CustomerKey INTEGER,
    Quantity INTEGER,
    TotalAmount DECIMAL(10,2),
    UnitCost DECIMAL(10,2),
    Weather VARCHAR(50),
    FOREIGN KEY(DateKey) REFERENCES Dim_Date(DateKey),
    FOREIGN KEY(ItemKey) REFERENCES Dim_Item(ItemKey),
    FOREIGN KEY(CustomerKey) REFERENCES Dim_Customer(CustomerKey)
);
