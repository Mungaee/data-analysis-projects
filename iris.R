library(readr)
library(dplyr)
library(ggplot2)

data(iris)
iris

# summary statistics of the dataset
summary (iris)
iris
sd_sepal_length <- sd(iris$Sepal.Length)
sd_sepal_width <- sd(iris$Sepal.Width)
sd_petal_length <- sd(iris$Petal.Length)
sd_petal_width <- sd(iris$Petal.Width)
cat("Standard Deviation Of Sepal Length:", sd_sepal_length,"\n")
cat("Standard Deviation Of Sepal Width:", sd_sepal_width,"\n")
cat("Standard Deviation Of Sepal Length:", sd_petal_length,"\n")
cat("Standard Deviation Of Sepal Length:", sd_petal_width,"\n")

#Visualise distribution of sepal length and width using histograms
#Distribution of Sepal Length
ggplot(iris, aes(x= Sepal.Length))+
  geom_histogram(binwidth=0.2,fill="lightblue",color="black")+
  labs(title="Distribution Of Sepal Length",
       x="Sepal Length",
       y="Frequency")

#Distribution Of Sepal Width
ggplot(iris, aes(x= Sepal.Width))+
  geom_histogram(binwidth=0.2,fill="lightblue",color="black")+
  labs(title="Distribution Of Sepal Width",
       x="Sepal Width",
       y="Frequency")

install.packages("gridExtra")

#Scatter Plots to explore relationships between variables
scatter_sepal <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width))+
  geom_point(aes(color=Species), size=3)+
  labs(title="Scatter Plot of Sepal Length vs Sepal Width",
       x="Sepal Length",
       y="Sepal Width")
scatter_sepal

scatter_petal <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width))+
  geom_point(aes(color=Species), size=3)+
  labs(title="Scatter Plot of Petal Length vs Petal Width",
       x="Petal Length",
       y="Petal Width")
scatter_petal

#Perform cluster analysis to identify distinct groups of flowers based on their characteristics.
install.packages("cluster")
library(cluster)

iris_columns <- iris[,1:4]

k <- 6
set.seed(123456)
kmeans_result <- kmeans(iris_columns, centers = k)

cluster_assignments <- kmeans_result$cluster
cat("Cluster Assignments:\n",cluster_assignments)

iris_clustered <- cbind(iris, Cluster= cluster_assignments)

cluster_means <- aggregate(. ~ Cluster, data=iris_clustered[, -5],mean)
cluster_means
