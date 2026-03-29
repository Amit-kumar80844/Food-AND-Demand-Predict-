# scripts/03_visual_analysis.R
if (!require("ggplot2")) install.packages("ggplot2", repos="http://cran.rstudio.com/")
if (!require("dplyr")) install.packages("dplyr", repos="http://cran.rstudio.com/")
if (!require("corrplot")) install.packages("corrplot", repos="http://cran.rstudio.com/")
if (!require("DBI")) install.packages("DBI", repos="http://cran.rstudio.com/")
if (!require("RSQLite")) install.packages("RSQLite", repos="http://cran.rstudio.com/")

library(ggplot2)
library(dplyr)
library(corrplot)
library(DBI)
library(RSQLite)

print("Starting Visual Analysis...")

# Create output dir
out_dir <- "outputs/plots"
if (!dir.exists("outputs")) dir.create("outputs")
if (!dir.exists(out_dir)) dir.create(out_dir)

db_file <- "data/canteen_dw.sqlite"
if (!file.exists(db_file) && file.exists("../data/canteen_dw.sqlite")) {
    db_file <- "../data/canteen_dw.sqlite"
    out_dir <- "../outputs/plots"
    if (!dir.exists("../outputs")) dir.create("../outputs")
    if (!dir.exists(out_dir)) dir.create(out_dir)
}

# Connect
wh_con <- dbConnect(RSQLite::SQLite(), dbname = db_file)
query <- "
SELECT 
  f.Quantity, f.TotalAmount, f.UnitCost, f.Weather,
  i.Price, i.ItemName, i.Category,
  c.CustomerType, d.DayOfWeek, d.FullDate
FROM Fact_Sales f
JOIN Dim_Item i ON f.ItemKey = i.ItemKey
JOIN Dim_Customer c ON f.CustomerKey = c.CustomerKey
JOIN Dim_Date d ON f.DateKey = d.DateKey
"
df <- dbGetQuery(wh_con, query)
dbDisconnect(wh_con)

print(paste("Data loaded for visualization:", nrow(df), "rows."))

# 1. Boxplots
print("Generating Boxplots...")
png(file.path(out_dir, "boxplot_total_amount.png"), width=800, height=600)
boxplot(df$TotalAmount, main="Boxplot of Total Amount", ylab="Total Amount (₹)", col="lightblue")
dev.off()

png(file.path(out_dir, "boxplot_quantity.png"), width=800, height=600)
boxplot(df$Quantity, main="Boxplot of Quantity", ylab="Quantity", col="lightgreen")
dev.off()

png(file.path(out_dir, "boxplot_price_cost.png"), width=800, height=600)
boxplot(df[, c("Price", "UnitCost")], main="Boxplot of Price and Unit Cost", col=c("orange", "yellow"), ylab="Amount (₹)")
dev.off()

png(file.path(out_dir, "boxplot_quantity_vs_weather.png"), width=800, height=600)
boxplot(Quantity ~ Weather, data=df, main="Boxplot of Quantity by Weather", col="cyan", xlab="Weather", ylab="Quantity")
dev.off()

png(file.path(out_dir, "boxplot_quantity_vs_price.png"), width=800, height=600)
boxplot(Quantity ~ as.factor(Price), data=df, main="Boxplot of Quantity by Price", col="pink", xlab="Price (₹)", ylab="Quantity")
dev.off()

# 2. Scatter Plots
print("Generating Scatter Plots...")
png(file.path(out_dir, "scatter_quantity_vs_total_amount.png"), width=800, height=600)
plot(df$Quantity, df$TotalAmount, main="Quantity vs Total Amount", xlab="Quantity", ylab="Total Amount (₹)", col="blue", pch=16, cex=1.5)
dev.off()

png(file.path(out_dir, "scatter_price_vs_total_amount.png"), width=800, height=600)
plot(df$Price, df$TotalAmount, main="Price vs Total Amount", xlab="Price", ylab="Total Amount (₹)", col="red", pch=16, cex=1.5)
dev.off()

png(file.path(out_dir, "scatter_unit_cost_vs_price.png"), width=800, height=600)
plot(df$UnitCost, df$Price, main="Unit Cost vs Price", xlab="Unit Cost", ylab="Price", col="darkgreen", pch=16, cex=1.5)
dev.off()

png(file.path(out_dir, "scatter_quantity_vs_price.png"), width=800, height=600)
plot(df$Price, df$Quantity, main="Quantity vs Price", xlab="Price (₹)", ylab="Quantity", col="magenta", pch=16, cex=1.5)
dev.off()

# 3. Histograms
print("Generating Histograms...")
png(file.path(out_dir, "histogram_total_amount.png"), width=800, height=600)
hist(df$TotalAmount, main="Histogram of Total Amount", xlab="Total Amount (₹)", col="purple", breaks=30)
dev.off()

png(file.path(out_dir, "histogram_quantity.png"), width=800, height=600)
hist(df$Quantity, main="Histogram of Quantity", xlab="Quantity", col="darkorange", breaks=20)
dev.off()

png(file.path(out_dir, "histogram_price.png"), width=800, height=600)
hist(df$Price, main="Histogram of Price", xlab="Price (₹)", col="brown", breaks=20)
dev.off()

# 4. Correlation Heatmap
print("Generating Correlation Heatmap...")
num_df <- df %>% select(Quantity, TotalAmount, UnitCost, Price) %>% na.omit()
cor_matrix <- cor(num_df)
png(file.path(out_dir, "correlation_heatmap.png"), width=800, height=600)
corrplot(cor_matrix, method="color", type="upper", addCoef.col="black", tl.col="black", tl.srt=45, main="Correlation Heatmap", mar=c(0,0,2,0))
dev.off()

# 5. Additional High-Value Charts (Bar Chart & Line Chart)
print("Generating Additional Charts (Bar Chart and Line Chart)...")
top_cat <- df %>% group_by(Category) %>% summarise(Revenue = sum(TotalAmount)) %>% arrange(desc(Revenue))

p1 <- ggplot(top_cat, aes(x=reorder(Category, Revenue), y=Revenue, fill=Category)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme_minimal() +
  labs(title="Top Categories by Revenue", x="Category", y="Total Revenue (₹)") +
  theme(legend.position="none", text = element_text(size=14))
ggsave(file.path(out_dir, "top_categories_barplot.png"), plot = p1, width = 8, height = 6)

daily_trend <- df %>% group_by(FullDate) %>% summarise(Revenue = sum(TotalAmount)) %>%
  mutate(FullDate = as.Date(FullDate)) %>% arrange(FullDate)
gg_trend <- ggplot(daily_trend, aes(x=FullDate, y=Revenue)) +
  geom_line(color="steelblue", size=1.2) +
  theme_minimal() +
  labs(title="Daily Revenue Trend", x="Date", y="Total Revenue (₹)") +
  theme(text = element_text(size=14))
ggsave(file.path(out_dir, "sales_trend_linechart.png"), plot = gg_trend, width = 8, height = 6)

print("Visual Analysis Complete! Check the outputs/plots folder.")
