# Load required libraries
install.packages(c("factoextra", "cluster", "NbClust"))
library(factoextra)
library(cluster)
library(NbClust)

# Read the data
dfile_path <- "C:\\Users\\shriv\\Downloads\\r project\\BigBasket Data.csv"
data <- read.csv(file_path)

# Select relevant columns
data <- data[, c("Sales_price", "Market_price", "Rating")]

# Remove rows with missing values
data <- na.omit(data)
View(data)
# Standardize the data
scaled_data <- scale(data)

# Compute the dissimilarity matrix
dist_matrix <- dist(scaled_data, method = "euclidean")

# Hierarchical clustering
hierarchical_clustering <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hierarchical_clustering, main = "Dendrogram of Hierarchical Clustering")

# K-means clustering
kmeans_results <- kmeans(scaled_data, centers = 3, nstart = 25)

# Add cluster numbers to the original dataset
data$Cluster <- kmeans_results$cluster

# Visualize clusters
fviz_cluster(kmeans_results, data = scaled_data, geom = "point")

# Evaluate the optimal number of clusters
# Elbow method
fviz_nbclust(scaled_data, FUNcluster, method = "wss")

# Silhouette method
fviz_nbclust(scaled_data, FUNcluster, method = "silhouette")

# Gap statistic method
gap_statistic <- clusGap(scaled_data, FUNcluster, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_statistic)

# Print cluster means
aggregate(scaled_data, by = list(Cluster = kmeans_results$cluster), mean)

# Print cluster sizes
kmeans_results$size

# Cluster Interpretation:

# Cluster 1: This cluster contains 2,821 observations (products). 
# These products have moderate sales and market prices, but they tend to have lower ratings compared to other clusters. 
# This cluster may represent products that are priced affordably but may not have high customer satisfaction or perceived value.

# Cluster 2: This cluster contains 584 observations. 
# Products in this cluster have high sales and market prices, and their ratings are relatively higher compared to other clusters. 
# These products are likely premium or high-end products with higher perceived value and customer satisfaction.

# Cluster 3: This cluster is the largest, containing 15,524 observations. 
# Products in this cluster have lower sales and market prices compared to cluster 2, but their ratings are generally higher than those in cluster 1. 
# This cluster may represent products that are priced competitively and offer good value for customers.

# Cluster Sizes:

# Cluster 1: 2,821 observations
# Cluster 2: 584 observations
# Cluster 3: 15,524 observations

# The cluster sizes indicate the distribution of products among the clusters. 
# Cluster 3 is the largest, followed by cluster 1, and then cluster 2.

# Business Implications:

# Understanding the characteristics of each cluster can help in strategic decision-making for marketing, pricing, and product development.
# For example, products in cluster 2 (high sales and market prices, high ratings) may warrant special attention in terms of marketing efforts or product promotion due to their premium nature.
# Products in cluster 1 (moderate prices, lower ratings) may benefit from initiatives to improve product quality or customer satisfaction to enhance their competitiveness in the market.
# Cluster 3 represents a large segment of products with competitive pricing and relatively good ratings, indicating a potential opportunity for volume sales or market penetration strategies.

# Further Analysis:

# It's important to further analyze the clusters in conjunction with other relevant factors such as customer demographics, market trends, and competition to derive actionable insights and refine business strategies accordingly.
# Overall, clustering analysis provides a structured approach to understanding the diversity within product offerings and can guide businesses in tailoring their strategies to different customer segments and market dynamics.



