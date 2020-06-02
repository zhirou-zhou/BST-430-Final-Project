---
title: "Prediction"
author: "Zhirou Zhou"
date: "12/20/2018"
output: html_document
---

# Extract the data for prediction
```{r}
# select.pc.data: prepare all the data needed for prediction
# Inputs:
# - df: dataframe containing traits and PC scores. Default as `df.merge`
# - ntraits: the order of the trait to be plotted
# Outputs: 
# - dataframe `df.select`, containing the first 30 PC scores and the specific trait score

select.pc.data = function(df = df.merge, ntraits) {
  ntraits = ntraits + 481
  df.reorder = df[order(df[,ntraits]),] # sort all subjects according to nth trait
  df.part = df.reorder[, c(122:151, ntraits)] # subset the df with 3 PC scores and trait
  df.select = df.part[complete.cases(df.part),] # remove the uncomplete cases
  colnames(df.select) = c("U1", "U2", "U3", "U4", "U5", "U6", "U7", "U8", "U9", "U10", 
                          "U11", "U12", "U13", "U14", "U15", "U16", "U17", "U18", "U19", 
                          "U20", "U21", "U22", "U23", "U24", "U25", "U26", "U27", "U28", 
                          "U29", "U30", "trait_score")
  return(df.select = df.select)
}

# select the data regarding trait: 'Substance use: alcohol'
df.alc = select.pc.data(ntraits = 58)
save(df.alc, file = "df.alc.rdata") # save the dataset as `df.alc.rdata`
```

# Prediction analysis
```{r,  message = FALSE}
library("MASS")
library("class")

# generate the sub-dataset containing trait score 1, 2, 5, 6
alc.sub.1 = rbind(df.alc[df.alc[,31] == 1, ], df.alc[df.alc[,31] == 2, ], 
                   df.alc[df.alc[,31] == 5, ], df.alc[df.alc[,31] == 6, ])

# generate the sub-dataset containing trait score 1 and 6
alc.sub.2 = rbind(df.alc[df.alc[,31] == 1, ], df.alc[df.alc[,31] == 6, ])

# separate the data into training set and test set
set.seed(0)
inds = sample(rep(1:5, length = nrow(df.alc)))
alc.tr = df.alc[inds != 5, ] # Training data
alc.te = df.alc[inds == 5, ] # Test data

set.seed(1)
inds = sample(rep(1:5, length = nrow(alc.sub.1)))
alc.tr.sub.1 = alc.sub.1[inds != 5, ] # Training data
alc.te.sub.1 = alc.sub.1[inds == 5, ] # Test data

set.seed(2)
inds = sample(rep(1:5, length = nrow(alc.sub.2)))
alc.tr.sub.2 = alc.sub.2[inds != 5, ] # Training data
alc.te.sub.2 = alc.sub.2[inds == 5, ] # Test data

# perform LDA on `alc.sub.2`
lda.fit = lda(trait_score ~ ., data = alc.tr.sub.2)
lda.pred = predict(lda.fit, alc.te.sub.2[, 1:30])
summary(lda.fit)
table(alc.te.sub.2$trait_score, lda.pred$class, dnn = c('Actual Group','Predicted Group'))
mean(lda.pred$class == alc.te.sub.2$trait_score) # calculate the classification accuracy

# perform KNN on `df.alc`
set.seed(3)
knn.pred = knn(alc.tr[, 1:30], alc.te[, 1:30], alc.tr[, 31], k = 7)
table(alc.te[, 31], knn.pred, dnn = c('Actual Group','Predicted Group'))
mean(knn.pred == alc.te[, 31])
sqrt((sum((as.numeric(knn.pred) - as.numeric(alc.te[, 31]))^2))/
       length(as.numeric(knn.pred))) # calculate the RMSE

# perform KNN on `alc.sub.1`
set.seed(4)
knn.pred.1 = knn(alc.tr.sub.1[, 1:30], alc.te.sub.1[, 1:30], alc.tr.sub.1[, 31], k = 12)
table(alc.te.sub.1[, 31], knn.pred.1, dnn = c('Actual Group','Predicted Group'))
mean(knn.pred.1 == alc.te.sub.1[, 31])
sqrt((sum((as.numeric(knn.pred.1) - as.numeric(alc.te.sub.1[, 31]))^2))/
       length(as.numeric(knn.pred.1))) # calculate the RMSE
```
