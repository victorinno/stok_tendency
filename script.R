library(dplyr)
library(clusterSim)
library(class)
library(gmodels)

petr3 <- read.csv("petr3.csv", stringsAsFactor = FALSE)

data <- data.Normalization(petr3[, 2:7], type="n4", normalization="column")
data <- tbl_df(data) %>%
        mutate(up_10_days = FALSE) 

total <- nrow(data)
for(indice in 1:total){
  a <- data[indice,4]
  b <- data[indice + 10 ,4]
  if(!is.na(b[1,1])){
    if(a[1,1] < b[1,1]){
      data[indice,7] = TRUE
    }
  }else{
    data[indice,7] = NA
  }
}
data <- data[complete.cases(data),]
data_train <- data[1:3034,]
data_test <- data[3035:3834,]

data_train_label <- data[1:3034,7]
data_train_label <- as.factor(data_train_label$up_10_days)
data_test_label <- data[3035:3834,7]
data_test_label <- as.factor(data_test_label$up_10_days)

pred <- knn(train = data_train, test = data_test, cl = data_train_label, k = 61)

CrossTable(x = data_test_label, y = pred, prop.chisq = FALSE)