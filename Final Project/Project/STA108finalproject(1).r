#STA 108 Final Project

#Import Data CDI.txt
install.packages("tidyverse")
library(tidyverse)
CDI <- read_table(file.choose(), col_names = FALSE)

#Part I Question 1 

colnames(CDI) = c(
  "ID", "County", "State", "Area", "Population",
  "Per_18_34", "Per_65plus", "MDs", "Beds",
  "Crimes", "HS", "BA", "Poverty", "Unemp",
  "PCI", "TPI", "Region"
)


data = CDI[,c(8,5,16,4,7,9)]
colnames(data) = c("Y", "X1", "X2", "X3", "X4", "X5")

Density = data$X1 / data$X3

summary(data)

hist(data$X1, main="Population")
hist(data$X3, main="Land Area")
hist(data$X2, main="Total Income")
hist(data$X4, main="Percent 65+")
hist(Density, main="Population Density")



#Part I Question 2 
data$Density = Density
pairs(data[, c("Y", "X1", "X3", "X2")],
      main = "Scatterplot Matrix - Model A")

pairs(data[, c("Y", "Density", "X4", "X2")],
      main = "Scatterplot Matrix - Model B")

cor_matrix_A = cor(data[, c("Y", "X1", "X3", "X2")])
cor_matrix_A

cor_matrix_B = cor(data[, c("Y", "Density", "X4", "X2")])
cor_matrix_B

#Part I Question 3 
modelA = lm(Y ~ X1 + X3 + X2, data=data)
modelB = lm(Y ~ Density + X4 + X2, data=data)

summary(modelA)
summary(modelB)


#Part I Question 4 
#Answer in the doc

#Part I Question 5 

par(mfrow = c(1, 3))
 
   plot(data$X1, resid(modelA),
               xlab = "X1: Population",
               ylab = "Residuals",
               main = "Model A: Residuals vs X1")
 abline(h = 0, lty = 2)
 
   plot(data$X3, resid(modelA),
               xlab = "X3: Land Area",
               ylab = "Residuals",
               main = "Model A: Residuals vs X3")
 abline(h = 0, lty = 2)
 
 plot(data$X2, resid(modelA),
               xlab = "X2: Total Income",
               ylab = "Residuals",
               main = "Model A: Residuals vs X2")
 abline(h = 0, lty = 2)
 
   par(mfrow = c(1, 3))
 
   plot(data$Density, resid(modelB),
               xlab = "Density",
               ylab = "Residuals",
               main = "Model B: Residuals vs Density")
 abline(h = 0, lty = 2)

   plot(data$X4, resid(modelB),
               xlab = "X4: Percent 65+",
               ylab = "Residuals",
               main = "Model B: Residuals vs X4")
 abline(h = 0, lty = 2)
 
   plot(data$X2, resid(modelB),
               xlab = "X2: Total Income",
               ylab = "Residuals",
               main = "Model B: Residuals vs X2")
 abline(h = 0, lty = 2)
 
#Part I Question 6
modelA_int = lm(Y ~ X1*X2 + X1*X3 + X2*X3, data=data)
summary(modelA_int)

modelB_int = lm(Y ~ Density*X4 + Density*X2 + X4*X2, data=data)
summary(modelB_int)



#Part II Question 1
model_base = lm(Y ~ X1 + X2, data=data)

model_X3 = lm(Y ~ X1 + X2 + X3, data=data) 
model_X4 = lm(Y ~ X1 + X2 + X4, data=data) 
model_X5 = lm(Y ~ X1 + X2 + X5, data=data) 

sse_base   = deviance(model_base)
sse_X3 = deviance(model_X3)
sse_X4 = deviance(model_X4)
sse_X5 = deviance(model_X5)

r2_X3_X1X2 = ((sse_base - sse_X3)/ sse_base)
r2_X4_X1X2 = ((sse_base - sse_X4) / sse_base)
r2_X5_X1X2 = ((sse_base - sse_X5) / sse_base)

r2_X3_X1X2 
r2_X4_X1X2 
r2_X5_X1X2 

#Part II Question 2 
anova(model_base, model_X5)

#Part II Question 3 
model_X3X4 = lm(Y ~ X1 + X2 + X3 + X4, data=data)
model_X3X5 = lm(Y ~ X1 + X2 + X3 + X5, data=data)
model_X4X5 = lm(Y ~ X1 + X2 + X4 + X5, data=data)

sse_X3X4 = deviance(model_X3X4)
sse_X3X5 = deviance(model_X3X5)
sse_X4X5 = deviance(model_X4X5)

r2_X3X4_X1X2 = (sse_base - sse_X3X4) / sse_base
r2_X3X5_X1X2 = (sse_base - sse_X3X5) / sse_base
r2_X4X5_X1X2 = (sse_base - sse_X4X5) / sse_base

r2_X3X4_X1X2
r2_X3X5_X1X2 
r2_X4X5_X1X2

#Part II Question 4 
anova(model_base, model_X3X5)
