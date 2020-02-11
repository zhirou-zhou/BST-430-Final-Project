---
title: "Hypothesis Test"
author: "Zhirou Zhou"
date: "12/20/2018"
output: html_document
---

# Hypothesis test
```{r}
# test.pc.data.alc: prepare the data needed for hypothesis test (alcohol related traits)
# Inputs:
# - df: dataframe containing traits and PC scores. Default is `df.merge`
# - ntraits: the order of the trait to be plotted
# Outputs: 
# - dataframe `df.test`, containing the first 3 PC scores, the specific trait score and the group of first and last 100 subjects

test.pc.data.alc = function(df = df.merge, ntraits) {
  ntraits = ntraits + 481
  df.reorder = df[order(df[,ntraits]),] # sort all subjects according to nth trait
  df.part = df.reorder[, c(122:124, ntraits)] # subset the df with 3 PC scores and trait
  df.remove.na = df.part[complete.cases(df.part),] # remove the uncomplete cases
  df.test = rbind(df.remove.na[df.remove.na[,4] == 1,], 
                  df.remove.na[df.remove.na[,4] == 6,]) # take out the subjects with low value and high value
  df.test$group[1:341] = 0
  df.test$group[52:341] = 1
  colnames(df.test) = c("U1", "U2", "U3", "trait_score", "group")
  return(df.test = df.test)
}
```

```{r,  message = FALSE}
library("MVN")
library("mvnormtest")

# perform hypothesis test for trait: 'Substance use: alcohol'
df.test = test.pc.data.alc(ntraits = 58)

# perform multivariate normality test over first 3 PC scores
nor.test = mvn(data = df.test[,1:3], mvnTest = "hz", 
               multivariateOutlierMethod = "adj", showNewData = TRUE, 
               univariatePlot = "qqplot")
nor.test$multivariateNormality
df.test.new = merge(nor.test$newData, df.test, by.x = c("U1", "U2", "U3"), 
                    by.y = c("U1", "U2", "U3")) # derive the new data set without outliers

nor.test.1 = mvn(data = df.test.new[,1:3], mvnTest = "hz", 
                 multivariateOutlierMethod = "adj", showNewData = TRUE) # redo the multivariate normality test over the new data set without outliers
nor.test.1$multivariateNormality

# perform homogeneity of variance and covariance test over first 3 PC scores
bartlett.test(U1 + U2 + U3 ~ group, data = df.test.new)
bartlett.test(interaction(U1, U2) ~ group, data = df.test.new)
bartlett.test(interaction(U2, U3) ~ group, data = df.test.new)
bartlett.test(interaction(U1, U3) ~ group, data = df.test.new)

# compute three-way ANOVA test
aov.full = aov(group ~ U1 * U2 * U3, data = df.test.new) # perform the full model
aov.part = update(aov.full, . ~ . - U1:U2:U3) # remove interaction
summary(aov.full)
anova(aov.full, aov.part)
plot(aov.part)
```
