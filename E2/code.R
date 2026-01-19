print("Santhiya Joe Harson J 23BAD102")

library(ggplot2)
library(dplyr)
library(lubridate)

# Load the data - NOTE: Update this path to where your file is located
df <- read.csv("D:/Documents/SEM6/EDA/LAB/EX2/2.ecommerce_transactions.csv")

# Convert Transaction_Date to Date format
df$Transaction_Date <- as.Date(df$Transaction_Date)

# 1. Histogram
ggplot(df, aes(x = Transaction_Amount)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Transaction Amounts",
       x = "Transaction Amount",
       y = "Frequency") +
  theme_minimal()

# 2. Boxplot
ggplot(df, aes(y = Transaction_Amount)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Transaction Amounts",
       y = "Transaction Amount") +
  theme_minimal()

# 3. Data Preparation for Heatmap
heatmap_data <- df %>%
  mutate(Month = month(Transaction_Date, label = TRUE, abbr = FALSE)) %>%
  group_by(Product_Category, Month) %>%
  summarise(Total_Sales = sum(Transaction_Amount), .groups = 'drop')

# 4. Heatmap
ggplot(heatmap_data, aes(x = Month, y = Product_Category, fill = Total_Sales)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "darkblue") +
  labs(title = "Heatmap of Monthly Sales Intensity",
       x = "Month",
       y = "Product Category",
       fill = "Total Sales") +
  theme_minimal()
