---
title: "Merge Data"
author: "Zhirou Zhou"
date: "12/20/2018"
output: html_document
---

# Merge all data into one data frame
```{r, message = FALSE}
library(R.matlab)
library(glmnet)

# load the original data set
func.con = readMat("~/Documents/Rochester/BST430_Intro_Statistical_Computing/Final_Project/Option1/final_project1_files/PCA_Coeff_HCP_Functional_Connectome.mat")
stru.con = readMat("~/Documents/Rochester/BST430_Intro_Statistical_Computing/Final_Project/Option1/final_project1_files/PCA_Coeff_HCP_Structural_Connectome.mat")
hcp.trait = readMat("~/Documents/Rochester/BST430_Intro_Statistical_Computing/Final_Project/Option1/final_project1_files/HCP_175Traits.mat")

# transform the 3-d array into 2-d data frame
func.df.1 = data.frame(matrix(func.con[[1]], nrow = dim(func.con[[1]])[2]))
trait.df.1 = data.frame(matrix(t(hcp.trait[[2]]), nrow = dim(hcp.trait[[2]])[2]))
stru.df.0 = stru.con[[1]]
stru.df.1 = as.data.frame(stru.df.0[1,,])
stru.df.2 = as.data.frame(stru.df.0[2,,])
stru.df.3 = as.data.frame(stru.df.0[3,,])
stru.df.4 = as.data.frame(stru.df.0[4,,])
stru.df.5 = as.data.frame(stru.df.0[5,,])
stru.df.6 = as.data.frame(stru.df.0[6,,])
stru.df.7 = as.data.frame(stru.df.0[7,,])
stru.df.all = cbind(stru.df.1, stru.df.2, stru.df.3, stru.df.4, stru.df.5, stru.df.6, stru.df.7)

# extract subject ids
func.id = t(data.frame(func.con[[2]]))
stru.id = data.frame(stru.con[[2]])
trait.id = data.frame(hcp.trait[[1]])

# merge the data frame with ids
func.df.2 = cbind(func.id, func.df.1)
stru.df = cbind(stru.id, stru.df.all)
trait.df = cbind(trait.id, trait.df.1)

# merge functional connectome PC scores with structural connectome PC scores
diff = stru.df[!(stru.df[,1] %in% func.df.2[,1]),]
diff = diff[,1]
diff.df = data.frame(matrix(NA, 7, 61))
diff.df[,1] = diff
func.df = data.frame(matrix(0, 1065, 61))
func.df[1:1058,] = func.df.2
func.df[1059:1065,] = diff.df
func.df = func.df[order(func.df[,1]),]
stru.df = stru.df[order(stru.df[,1]),]
df.1 = data.frame(matrix(0, 1065, 481))
df.1[,1] = func.df[,1]
df.1[,2:61] = func.df[,2:61]
df.1[,62:481] = stru.df[,2:421]

# merge PC scores with traits data
diff = trait.df[!(trait.df[,1] %in% df.1[,1]),]
diff = diff[,1]
diff.df = data.frame(matrix(NA, 141, 481))
diff.df[,1] = diff
df = data.frame(matrix(0, 1206, 481))
df[1:1065,] = df.1
df[1066:1206,] = diff.df
trait.df = trait.df[order(trait.df[,1]),]
df = df[order(df[,1]),]
df.merge = data.frame(matrix(NA, 1206, 656))
df.merge[,1] = trait.df[,1]
df.merge[,2:481] = df[,2:481]
df.merge[,482:656] = trait.df[,2:176]

# save the result data frame
save(df.merge, file = "df.merge.rdata")

# data frame description
# each row for one subject
# df.merge[,1]: subject ids
# df.merge[,2:61]: 60 PC scores from the functional connectome
# df.merge[,62:481]: 60*7 PC scores from 7 different structural connectome features
# df.merge[,482:656]: 175 traits
```
