# Load the dataset
pharma_data <- read.csv("Pharmaceuticals.csv")

# Step 1: Basic scatter plot 
plot(pharma_data$Market_Cap ~ pharma_data$PE_Ratio, main = "Scatter plot of Market Cap vs PE Ratio")

# Step 2: Quantitative variables only 
pharma_quantitative <- pharma_data[, c("Market_Cap", "Beta", "PE_Ratio", "ROE", "ROA", "Asset_Turnover", "Leverage", "Rev_Growth", "Net_Profit_Margin")]
means <- apply(pharma_quantitative, 2, mean)
sds <- apply(pharma_quantitative, 2, sd)
normalized_data <- scale(pharma_quantitative, center = means, scale = sds)

# Step 3: Perform K-means clustering with 3 clusters 
set.seed(123)  # Set seed for reproducibility
kmeans_result <- kmeans(normalized_data, centers = 3, nstart = 25)

# Step 4: Summarize the K-means clusters
print(kmeans_result$centers)  # Centers of clusters
print(table(kmeans_result$cluster))  # Size of each cluster

# Step 5: Scatter plot of clusters using K-means result
plot(normalized_data[, "Market_Cap"], 
     normalized_data[, "Beta"], 
     col = kmeans_result$cluster, 
     pch = 19, 
     xlab = "Market Capitalization (normalized)", 
     ylab = "Beta (normalized)", 
     main = "K-means Clusters of Companies Based on Financial Metrics",
     cex = 1.5)  # Increase point size for better visibility

# Add text labels for each point
text(normalized_data[, "Market_Cap"], 
     normalized_data[, "Beta"], 
     labels = rownames(normalized_data), 
     pos = 4, 
     cex = 0.7)  # Adjust text size for readability

# Add a legend
legend("topright", legend = unique(kmeans_result$cluster), col = unique(kmeans_result$cluster), pch = 19, title = "Cluster")

# Step 6: Calculate and plot silhouette width for K-means
library(cluster)
sil_width_kmeans <- silhouette(kmeans_result$cluster, dist(normalized_data))
plot(sil_width_kmeans, main = "Silhouette Plot for K-means Clustering (3 clusters)")

# Scree plot to find the optimal number of clusters
wss <- (nrow(normalized_data) - 1) * sum(apply(normalized_data, 2, var))
for (i in 2:10) wss[i] <- sum(kmeans(normalized_data, centers = i, nstart = 25)$tot.withinss)
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares", main = "Scree Plot for K-means")

