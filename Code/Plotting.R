---
title: "Plotting"
author: "Zhirou Zhou"
date: "12/20/2018"
output: html_document
---

# Plotting
```{r, message = FALSE}
library("plot3D")
library("colorspace")
```

# Function
```{r}
# plot.pc: plot 3D scatterplot of traits and connectome PC scores
# Inputs:
# - df: dataframe containing traits and PC scores. Default as `df.merge`
# - ntraits: the order of the trait to be plotted
# - theta, phi: the angles defining the viewing direction. theta gives the azimuthal direction and phi the colatitude
# - colorpal: color palette to be used for coloring the colvar variable
# - main: an overall title for the plot
# Outputs:
# - dataframe `df.plot`, containing the first 3 PC scores and the specific trait score of first and last 100 subjects

plot.pc = function(df = df.merge, ntraits, theta, phi, colorpal, main) {
  ntraits = ntraits + 481 # locate the trait in `df.merge`
  df.reorder = df[order(df[,ntraits]),] # sort all subjects according to nth trait
  df.part = df.reorder[, c(122:124, ntraits)] # subset the df with 3 PC scores and trait
  df.remove.na = df.part[complete.cases(df.part),] # remove the uncomplete cases
  df.plot = rbind(head(df.remove.na, 100), tail(df.remove.na, 100)) # take out the first and last 100 subjects
  colnames(df.plot) = c("U1", "U2", "U3", "trait_score")
  plot.pc = scatter3D(df.plot$U1, df.plot$U2, df.plot$U3, colvar = df.plot$trait_score, 
                      theta = theta, phi = phi, col = colorpal, pch = 19, bty = "g", 
                      clab = c("Trait Score"), 
                      colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75), 
                      xlab = "U1", ylab = "U2", zlab = "U3", cex = 0.6, main = main,
                      ticktype = "detailed")
  #return(df.plot = df.plot)
}

# plot.pc.alc: plot 3D scatterplot of alcohol related traits and connectome PC scores
# Inputs:
# - df: dataframe containing traits and PC scores. Default as `df.merge`
# - ntraits: the order of the trait to be plotted
# - theta, phi: the angles defining the viewing direction. theta gives the azimuthal direction and phi the colatitude
# - colorpal: color palette to be used for coloring the colvar variable
# - main: an overall title for the plot
# Outputs: 
# - dataframe `df.plot`, containing the first 3 PC scores and the specific trait score of first and last 100 subjects

plot.pc.alc = function(df = df.merge, ntraits, theta, phi, colorpal, main) {
  ntraits = ntraits + 481 # locate the trait in `df.merge`
  df.reorder = df[order(df[,ntraits]),] # sort all subjects according to nth trait
  df.part = df.reorder[, c(122:124, ntraits)] # subset the df with 3 PC scores and trait
  df.remove.na = df.part[complete.cases(df.part),] # remove the uncomplete cases
  df.plot = rbind(df.remove.na[df.remove.na[,4] == 1,], 
                  df.remove.na[df.remove.na[,4] == 6,]) # take out the subjects with low value and high value
  colnames(df.plot) = c("U1", "U2", "U3", "trait_score")
  plot.pc = scatter3D(df.plot$U1, df.plot$U2, df.plot$U3, colvar = df.plot$trait_score, 
                      theta = theta, phi = phi, col = colorpal, pch = 19, bty = "g", 
                      xlab = "U1", ylab = "U2", zlab = "U3", cex = 0.6, main = main, 
                      ticktype = "detailed")
  #return(df.plot = df.plot)
}
```

```{r}
# plot: `Motor: Endurance age adjusted`
par(cex.axis = 0.35, cex.lab = 0.6, cex.main = 0.9, lwd = 0.5, col.lab = "darkgrey", 
    mar=c(2, 2, 1, 1))
plot.pc(ntraits = 49, theta = 10, phi = 15, 
        main = "Motor: 2-minute Walk Endurance Test (Age-Adjusted Scale Score)", 
        colorpal = diverge_hcl(12, c = 100, l = c(50, 90), power = 1))

# plot: `Health and Family History: Height`
par(cex.axis = 0.35, cex.lab = 0.6, cex.main = 0.9, lwd = 0.5, col.lab = "darkgrey", 
    mar=c(1, 2, 1, 1))
plot.pc(ntraits = 172, theta = 40, phi = 10, main = "Health and Family History: Height", 
        colorpal = diverge_hcl(12, h = c(246, 40), c = 96, power = 1))

# plot: `Substance use: alcohol`
par(cex.axis = 0.35, cex.lab = 0.6, cex.main = 0.9, lwd = 0.5, col.lab = "darkgrey", 
    mar=c(2, 2, 3, 1))
plot.pc.alc(ntraits = 58, theta = 60, phi = 20, 
            main = "Substance use: Frequency of any alcohol use in past 12 months",
        colorpal = diverge_hcl(12, h = c(130, 43), l = c(70, 90), power = 1))
legend("bottomleft", legend = c("4-7 days/week (male)", "1-11 days/year"), 
       col = c("green3", "darkorange"), pch = 19, border = "white")
```

```{r,  message = FALSE}
# save the plot with white background
png(filename = "1-1.png", units = "in", width = 5, height = 4, pointsize = 12, res = 500)
par(cex.axis = 0.35, cex.lab = 0.6, cex.main = 0.9, lwd = 0.5, col.lab = "darkgrey", 
    mar=c(2, 2, 1, 1))
plot.pc(ntraits = 49, theta = 10, phi = 15, 
        main = "Motor: 2-minute Walk Endurance Test (Age-Adjusted Scale Score)", 
        colorpal = diverge_hcl(12, c = 100, l = c(50, 90), power = 1))
dev.off()

# save the plot with transparent background
png(filename = "1-2.png", units = "in", width = 5, height = 4, pointsize = 12, res = 500)
par(cex.axis = 0.35, cex.lab = 0.6, cex.main = 0.9, lwd = 0.5, col.lab = "darkgrey", 
    mar=c(2, 2, 1, 1), bg = "transparent")
plot.pc(ntraits = 49, theta = 10, phi = 15, 
        main = "Motor: 2-minute Walk Endurance Test (Age-Adjusted Scale Score)", 
        colorpal = diverge_hcl(12, c = 100, l = c(50, 90), power = 1))
dev.off()

# save the plot with white background
png(filename = "3-1.png", units = "in", width = 5, height = 4, pointsize = 12, res = 500)
par(cex.axis = 0.35, cex.lab = 0.6, cex.main = 0.9, lwd = 0.5, col.lab = "darkgrey", 
    mar=c(1, 2, 1, 1))
plot.pc(ntraits = 172, theta = 40, phi = 10, main = "Health and Family History: Height", 
        colorpal = diverge_hcl(12, h = c(246, 40), c = 96, power = 1))
dev.off()

# save the plot with transparent background
png(filename = "3-2.png", units = "in", width = 5, height = 4, pointsize = 12, res = 500)
par(cex.axis = 0.35, cex.lab = 0.6, cex.main = 0.9, lwd = 0.5, col.lab = "darkgrey", 
    mar=c(1, 2, 1, 1), bg = "transparent")
plot.pc(ntraits = 172, theta = 40, phi = 10, main = "Health and Family History: Height", 
        colorpal = diverge_hcl(12, h = c(246, 40), c = 96, power = 1))
dev.off()

# save the plot with white background
png(filename = "2-1.png", units = "in", width = 5, height = 4, pointsize = 12, res = 500)
par(cex.axis = 0.35, cex.lab = 0.6, cex.main = 0.9, lwd = 0.5, col.lab = "darkgrey", 
    mar=c(2, 2, 1, 1))
plot.pc.alc(ntraits = 58, theta = 60, phi = 20, 
            main = "Substance use: Frequency of any alcohol use in past 12 months",
        colorpal = diverge_hcl(12, h = c(130, 43), l = c(70, 90), power = 1))
legend("bottomleft", legend = c("4-7 days/week (male)", "1-11 days/year"), 
       col = c("green3", "darkorange"), pch = 19, border = "white")
dev.off()

# save the plot with transparent background
png(filename = "2-2.png", units = "in", width = 5, height = 4, pointsize = 12, res = 500)
par(cex.axis = 0.35, cex.lab = 0.6, cex.main = 0.9, lwd = 0.5, col.lab = "darkgrey", 
    mar=c(2, 2, 1, 1), bg = "transparent")
plot.pc.alc(ntraits = 58, theta = 60, phi = 20, 
            main = "Substance use: Frequency of any alcohol use in past 12 months",
        colorpal = diverge_hcl(12, h = c(130, 43), l = c(70, 90), power = 1))
legend("bottomleft", legend = c("4-7 days/week (male)", "1-11 days/year"), 
       col = c("green3", "darkorange"), pch = 19, border = "white")
dev.off()
```
