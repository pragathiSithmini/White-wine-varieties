#Import libraries
library(readxl)
library(dplyr)
library(fpc)
library(MASS)
library(caret)
library(flexclust)
library(NbClust)

#Read the file
original <- read_xlsx("./whitewine_v2.xlsx")
boxplot(original)

# Remove the extreme outliers
original_summary <- summary(original$`residual sugar`)
original_summary

# Estimate interquartile range
# (3rd quartile minus 1st quartile)
iqr <- original_summary[[5]] - original_summary[[2]]


# Identify bounds for outliers
lower_bound <- original_summary[[2]] - (1.5 * iqr)
upper_bound <- original_summary[[5]] + (1.5 * iqr)

# Identify outlier(s)
outliers <- original %>% 
  filter( `residual sugar`> upper_bound | `residual sugar`< lower_bound)

# Remove outliers from dataframe, but store as new dataframe "no_outliers"
no_outliers <- original %>%
  filter(`residual sugar` < upper_bound & `residual sugar` > lower_bound)

original_summary <- summary(original$`free sulfur dioxide`)
iqr <- original_summary[[5]] - original_summary[[2]]

# The bounds are established with the original data
lower_bound <- original_summary[[2]] - (1.5 * iqr)
upper_bound <- original_summary[[5]] + (1.5 * iqr)

outliers <- rbind(outliers,original %>% 
                    filter(`free sulfur dioxide` > upper_bound | `free sulfur dioxide` < lower_bound))

no_outliers <- no_outliers %>%
  filter(`free sulfur dioxide` < upper_bound & `free sulfur dioxide` > lower_bound)

# Repeat for fixed accidity
original_summary <- summary(original$`total sulfur dioxide`)
iqr <- original_summary[[5]] - original_summary[[2]]

# Remember that the bounds are based on the original data
lower_bound <- original_summary[[2]] - (1.5 * iqr)
upper_bound <- original_summary[[5]] + (1.5 * iqr)

# Removing fixed acidity outliers from the no_outliers data, not the original
outliers <- rbind(outliers,original %>% 
                    filter(`total sulfur dioxide` > upper_bound | `total sulfur dioxide` < lower_bound))

no_outliers <- no_outliers %>%
  filter(`total sulfur dioxide` < upper_bound & `total sulfur dioxide` > lower_bound)

boxplot(no_outliers)

#Scaling 
wine_stand<- scale(no_outliers[-12])
summary(wine_stand)

#NbClust()
set.seed(1234)
nc <- NbClust(wine_stand,
              min.nc=2, max.nc=8,
              method="kmeans")

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 9 Criteria")

ws <- 0
for (i in 1:9){
  ws[i] <-
    sum(kmeans(wine_stand, centers=i)$withinss)}

plot(1:9,
     ws,
     type="b",    
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#kmeans=2
fit.km2 <- kmeans(wine_stand,2)
plotcluster(wine_stand, fit.km2$cluster)
confuse <- table(no_outliers$quality,fit.km2$cluster)
confuse
parcoord(wine_stand, fit.km2$cluster)

#kmeans=3
fit.km3 <- kmeans(wine_stand , 3)
fit.km3
confuse3 <- table(no_outliers$quality,fit.km3$cluster)
confuse
parcoord(wine_stand, fit.km2$cluster)

#kmeans=4
fit.km4 <- kmeans(wine_stand, 4)
table(no_outliers$quality,fit.km4$cluster)
confuse
parcoord(wine_stand, fit.km2$cluster)

#kmeans=5
fit.km5 <- kmeans(wine_stand, 5)
table(no_outliers$quality,fit.km5$cluster)
confuse
parcoord(wine_stand, fit.km2$cluster)

plotcluster(wine_stand,fit.km5$cluster)

#Evaluation with ARI for k=2 
randIndex(confuse)

#NbClust() with Manhattan distance
clusters_manhattan <- NbClust(wine_stand,distance="manhattan",min.nc=2,max.nc=5,method="kmeans",index="all")