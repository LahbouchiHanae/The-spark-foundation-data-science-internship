#import required libraries
library(ggplot2)
library(purrr)
library(cluster)

#load data
dataframe <- iris 

# See the first 5 rows
head(dataframe)

#removig initial label of Species from original dataset
dataframe_1 <- iris[, -5]

#scatter plot
ggplot(dataframe, aes(Petal.Length, Petal.Width, color=Species))+
  geom_point()

# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = dataframe_1, centers = k, nstart = 100)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10 ,
  tot_withinss = tot_withinss
)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)
#The optimum clusters is when the elbow occurs, so we choose the number of cluster as 3
# Build the kmeans model with k = 3
model_km <- kmeans(dataframe_1, centers = 3, nstart=100)

# Extract the cluster assignment vector from the kmeans model
clust_km <- model_km$cluster

#visualize clusters
clusplot(dataframe_1,
         clust_km,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 0,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of Iris Flowers')
)
