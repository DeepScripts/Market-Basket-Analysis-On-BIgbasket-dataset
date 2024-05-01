# Load the necessary library
library(ggplot2)
library(dplyr)
# Assuming your dataset is stored in a CSV file named 'data.csv'
data <- read.csv("C:\\Users\\shriv\\Downloads\\r project\\BigBasket Products.csv")

# Aggregate sales data by category and sum up the sale_price
category_sales <- aggregate(sale_price ~ category, data = data, FUN = sum)
category_sales <- category_sales[order(category_sales$sale_price, decreasing = TRUE),]
# Plot the top-selling categories
top_categories <- head(category_sales, 10)  # You can adjust the number as needed
# Plot the bar chart
barplot(top_categories$sale_price, names.arg = top_categories$category, 
        main = "Top Selling Categories", xlab = "Category", ylab = "Total Sale Price",
        col = "skyblue", las = 2, cex.names = 0.4)

# Distribution of sale_price
ggplot(data, aes(x = sale_price)) +
  geom_histogram(binwidth = 50, fill = "skyblue") +
  labs(title = "Distribution of Sale Price", x = "Sale Price", y = "Frequency")
# Distribution of market_price
ggplot(data, aes(x = market_price)) +
  geom_histogram(binwidth = 50, fill = "red") +
  labs(title = "Distribution of Market Price", x = "Market Price", y = "Frequency")

library(ggplot2)

ggplot(data, aes(x = sale_price)) +
  geom_histogram(binwidth = 50, fill = alpha("skyblue", 1)) + # Adjust transparency
  geom_histogram(aes(x = market_price), binwidth = 50, fill = alpha("red", 1)) + # Adjust transparency
  labs(title = "Distribution of Sale Price and Market Price", x = "Price", y = "Frequency")



# Correlation matrix
correlation_matrix <- cor(data[, c("sale_price", "market_price", "rating")], use = "pairwise.complete.obs")
correlation_matrix

summary(data)

# Count the frequency of each brand
brand_frequency <- table(data$brand)
# Convert the frequency table to a data frame
brand_freq_df <- data.frame(brand = names(brand_frequency),
                            frequency = as.numeric(brand_frequency))
# Sort the data frame by frequency in descending order
brand_freq_df <- brand_freq_df[order(brand_freq_df$frequency, decreasing = TRUE),]
# Select only the top 10 brands
top_10_brands <- head(brand_freq_df, 10)
print(top_10_brands)
# Plot brand vs frequency for top 10 brands
ggplot(top_10_brands, aes(x = reorder(brand, -frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Brands by Frequency", x = "Brand", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Extract the brand names from the top_10_brands data frame
top_brands <- top_10_brands$brand
# Filter the original data for the top brands and calculate average rating
brand_avg_rating <- data %>%
  filter(brand %in% top_brands) %>%
  group_by(brand) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE))
# Print the average rating for the top brands
print(brand_avg_rating)
# Plot brand vs average rating
ggplot(brand_avg_rating, aes(x = reorder(brand, -avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Rating for Each Brand", x = "Brand", y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(brand_avg_rating$avg_rating) + 0.5, by = 0.5))

# Calculate the difference in prices
data$diff_in_prices <- data$market_price - data$sale_price
# Filter for products with discounts
discount <- data[data$diff_in_prices != 0, ]
# Create a density plot for ratings
ggplot() +
  geom_density(data = discount, aes(x = rating, color = "Discounted"), fill = 'blue', alpha = 0.5) +
  geom_density(data = data, aes(x = rating, color = "All"), fill = 'gold', alpha = 0.7) +
  labs(x = "Ratings", y = "Density", title = "Relative distribution of all products with discounted products") +
  scale_color_manual(values = c("Discounted" = "blue", "All" = "gold")) +
  theme_minimal()



######################################################################




