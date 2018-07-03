---
title: "Kmeans"
author: "Julien Jaber"
date: "7/2/2018"
output: 
  html_document: 
    keep_md: yes
---


```r
library(DataComputing)
```

```
## Loading required package: dplyr
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Loading required package: ggplot2
```

**Manual implementation of K-means**

1. Randomly select rows to be centroids
2. Measure the distance of each datapoint to centroid and see which centroid it is the closest o
3. Repeat the process for all datapoints
4. Update centroid
5. Repeat Steps 2 -> 4



```r
set.seed(32)

#1 randomly select rows
irisCols <- iris[, 1:4]
X = irisCols

X$group = 0

my_kmeans <- function(X, k) {
   select_rows = sample.int(nrow(X), k, replace = FALSE)
   random_means = X[select_rows ,]
   distancesVector <- c()
   #now calculate distance from every point to the three centroids
   for (Q in 1:10) {
      for (i in 1:nrow(X)) {
          for (z in 1:k) {
              distance = sqrt(sum((X[i,] - random_means[z,])^2))
              distancesVector <- c(distancesVector, distance)
          }
     
     X$group[i] = which.min(distancesVector)
     distancesVector <- c()
      }
     
     #update centroids
     levs <- unique(X$group)
     for (z in 1:k) {
        random_means[z,] <- colMeans(X[X$group == levs[z], ])
     }
    
   }
   
   LISTER <- list(random_means, X$group)
   names(LISTER) = c("Final Centroids after 10 iterations", "Selected group")
   return(LISTER)
   
}


my_kmeans(X, 3)
```

```
## $`Final Centroids after 10 iterations`
##     Sepal.Length Sepal.Width Petal.Length Petal.Width group
## 76      5.006000    3.428000     1.462000    0.246000     1
## 89      5.917308    2.780769     4.269231    1.351923     2
## 120     6.635417    2.970833     5.595833    2.027083     3
## 
## $`Selected group`
##   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [36] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
##  [71] 2 2 3 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3
## [106] 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 3 3 3 3 3 3 3 3 3 3 2 3
## [141] 3 3 3 3 3 3 3 3 3 3
```


### Compare with the real dataset




```
##      Species Mean_Sepal.Length Mean_Sepal.Width Mean_Petal.Length
## 1     setosa             5.006            3.428             1.462
## 2 versicolor             5.936            2.770             4.260
## 3  virginica             6.588            2.974             5.552
##   Mean_Petal.Width
## 1            0.246
## 2            1.326
## 3            2.026
```
