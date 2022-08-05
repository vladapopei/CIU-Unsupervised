library(clue)
library(ciu)
require(datasets)
library(readr)
library("ggplot2")
library("dplyr")
library("ggfortify")
library(dplyr)
library(cluster)
library(fclust)
library(factoextra)
library(Rtsne)
library(e1071)


#------------GERMAN CREDIT TESTING------------
#Setting initial data

x <- read.csv("german_credit (1).csv", header = TRUE)
german_data <- scale(x, center = FALSE)   #scaling
german_data_test <- as.data.frame(german_data[,-1])
german_data_test
german_data_instance <- german_data_test[450,1:20]
german_data_instance

#Clustering model
model = kmeans(german_data_test, 4)
model = cmeans(german_data_test, 4, 300, verbose=TRUE, method="cmeans", m=2)
model$membership

#Plot clustered data
autoplot(model, german_data_test, frame = TRUE)
#Method seems to work better for cmeans
clusplot(german_data_test[,-6], paste("test", german_data_test$clusters),  color=TRUE, shade=FALSE, 
         labels=4, lines=0)


#Check clustering distribution
table(model$cluster)
hist(model$cluster)
model$centers


#Prediction function, assigns proximity to cluster,
#simulating membership
predict.fclust <- function(model,data) {
  dm <- sapply(seq_len(nrow(data)),
               function(i) apply(model$centers, 1, function(v) sqrt(sum((data[i, ]-v)^2))))
  m <- 2
  ms <- t(apply(dm, 2,
                function(x) {
                  tmp <- 1/((x/sum(x))^(2/(m-1)))
                  
                  #Temporary fix for testing; to be addressed (?)
                  #This is not a great way of doing it...
                  for (i in 1:length(tmp)) {
                    if (is.infinite(tmp[i])) {
                      tmp[i] <- 1.0e+15
                    }
                  }
                  
                  tmp/sum(tmp)
                }))
  return(ms)
}


#Extracting on centroids
centers_df = as.data.frame(model$centers)

#Testing prediction function on instance
##print(predict.fclust(model, german_data_instance))
##print(predict.fclust(model, centers_df[i,]))

#Adding clusters and ordering
german_data_test$clusters <- model$cluster
german_data_test = german_data_test[order(german_data_test$clusters),]


#CIU object initialization
ciu <- ciu.new(model, clusters~., german_data_test, 
               abs.min.max = cbind(matrix(0, max(german_data_test$clusters), 1),
                                   matrix(1, max(german_data_test$clusters), 1)), 
               predict.function = predict.fclust, 
               output.names = unique(german_data_test$clusters))



#Outputting explanations per input index on test instance
for ( i in 1:ncol(german_data_instance)) {
  cat("Input Index",i," ",colnames(german_data_instance[i]),"\n")
  print(ciu$explain(german_data_instance, ind.inputs.to.explain=c(i)))
}


#Manual settings for plot size, not needed for ggplot
par(mar = c(8, 14, 7, 10))

#Plotting centroids CIU
for ( i in 1:max(model$cluster) ) {
  print(ciu$ggplot.col.ciu(centers_df[i,]))
}

