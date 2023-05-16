# Practical 9

# Q1
states <- as.data.frame(state.x77)
str(states)

# Renaming Life Exp and HS Grad variables as 
# these will cause possible issues when referring to
# them since they contain a space.
colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"

# Q3a
# Examine initial linearity between variables in the dataset
install.packages("psych")
library(psych)
pairs.panels(states,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# Examine linearity in more detail
scatter.smooth(x = states$Population,
               y = states$Murder,
               xlab = "Population (,000)",
               ylab = "Murder %", main = "Correlation of murder ~ population")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(states$Murder, states$Population)

scatter.smooth(x = states$Illiteracy,
               y = states$Murder,
               main = "Correlation of Murder ~ Illiteracy",
               xlab = "Illiteracy %",
               ylab = "Murder %")

# Examining correlation between murder and illiteracy
cor(states$Murder, states$Illiteracy)

# This is a better correlation value between both variables.
# Lets examine murder and frost variables for correlation.
scatter.smooth(x = states$Frost,
               y = states$Murder,
               main = "Correlation of Murder ~ Frost",
               xlab = "Frost",
               ylab = "Murder %")
cor(states$Murder, states$Frost)

# Examining the other variables
paste("Correlation for Murder and Frost: ", cor(states$Murder, states$Frost))
paste("Correlation for Murder and Illiteracy: ", cor(states$Murder, states$Illiteracy))
paste("Correlation for Murder and Population: ", cor(states$Murder, states$Population))
paste("Correlation for Murder and HS Grad: ", cor(states$Murder, states$HS_Grad))
paste("Correlation for Murder and Income: ", cor(states$Murder, states$Income))
paste("Correlation for Murder and Life Exp: ", cor(states$Murder, states$Life_Exp))
paste("Correlation for Murder and Area: ", cor(states$Murder, states$Area))

# It appears that the variable Area has a vary low correlation with Murder. 
# Therefore I am going to remove it from the dataset. 
# Alternatively we can choose to exclude these independent variables when
# we are constructing the MLR model.

# Q3b
# Check for outliers
opar <- par(no.readonly = TRUE)
par(mfrow = c(4, 2)) # divide graph area in 3 rows by 2 columns
attach(states)
boxplot(Murder,
        main = "Murder",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Murder)$out)) # box plot for 'murder'
boxplot(Population,
        main = "Population",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Population)$out)) # box plot for 'Population'
boxplot(states$HS_Grad,
        main = "Graduation",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$HS_Grad)$out)) # box plot for 'HS Grad'
boxplot(Illiteracy,
        main = "Illiteracy",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Illiteracy)$out)) # box plot for 'HS Grad'
boxplot(Income,
        main = "Income",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Income)$out)) # box plot for 'HS Grad'
boxplot(Frost,
        main = "Frost",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Frost)$out)) # box plot for 'HS Grad'
boxplot(states$Life_Exp,
        main = "Life Exp",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$Life_Exp)$out)) # box plot for 'HS␣Grad'
detach(states)
par(opar)

# Both the population and Income variables contain outliers.
# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Population)$out # outlier values.
paste("Population outliers: ", paste(outlier_values, collapse=", "))

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Income)$out # outlier values.
paste("Income outliers: ", paste(outlier_values, collapse=", "))

outlier_values <- boxplot.stats(states$Illiteracy)$out # outlier values.
paste("Illiteracy outliers: ", paste(outlier_values, collapse=", "))

outlier_values <- boxplot.stats(states$Life_Exp)$out # outlier values.
paste("Life_Exp outliers: ", paste(outlier_values, collapse=", "))

outlier_values <- boxplot.stats(states$Murder)$out # outlier values.
paste("Murder outliers: ", paste(outlier_values, collapse=", "))


states <- subset(states,states$Population != 21198 )
states <- subset(states,states$Population != 11197 )
states <- subset(states,states$Population != 18076 )
states <- subset(states,states$Population != 11860 )
states <- subset(states,states$Population != 12237 )
states <- subset(states,states$Income != 6315 )


# check for normality and skewness
install.packages("e1071")
library(e1071)
par(mfrow = c(4,2))

plot(density(states$Population),
     main = "Density plot for population"
     ,sub = paste("Skewness : ", round(e1071::skewness(states$Population),2)
                  ,polygon(density(states$Population),col = "red")))


#* skewness of < -1 or > 1 , highly skewed
#* -1 to -0.5  or 1 to 0.5 , moderately skewed
#* -0.5  0.5 then approx symmetrical


shapiro.test(states$Population)

#*not normally distributed
#*option 1 : drop the variable and do not use the model
#*option 2: use the variable and ignore MLR assumptions
#*option 3: transform the variable to make it more normaly distributed
#*

install.packages("MASS")
library(MASS)
box_cox_transformation <- boxcox(states$Murder ~ states$Population)

lamda <- box_cox_transformation$x[which.max(box_cox_transformation$y)]
lamda


normalised_population <- ((states$Population ^ lamda)/ lamda)
hist(normalised_population)
shapiro.test(normalised_population)















library(e1071)
par(mfrow = c(4,2))

plot(density(states$Income),
     main = "Density plot for income"
     ,sub = paste("Skewness : ", round(e1071::skewness(states$Income),2)
                  ,polygon(density(states$Income),col = "red")))


#* skewness of < -1 or > 1 , highly skewed
#* -1 to -0.5  or 1 to 0.5 , moderately skewed
#* -0.5  0.5 then approx symmetrical


shapiro.test(states$Income)

#*not normally distributed
#*option 1 : drop the variable and do not use the model
#*option 2: use the variable and ignore MLR assumptions
#*option 3: transform the variable to make it more normaly distributed
#*

install.packages("MASS")
library(MASS)
box_cox_transformation <- boxcox(states$Murder ~ states$Income)

lamda <- box_cox_transformation$x[which.max(box_cox_transformation$y)]
lamda


normalised_population <- ((states$Population ^ lamda)/ lamda)
hist(normalised_population)
shapiro.test(normalised_population)


  
  
  
shapiro.test(states$Income)  #0.3246
shapiro.test(states$Illiteracy)   #8.297e-05 , not normally distributed
shapiro.test(states$`Life Exp`)  # 0.3608
shapiro.test(states$Murder)  #0.06601
shapiro.test(states$`HS Grad`)  #0.02194 , , not normally distributed
shapiro.test(states$Frost) #0.0928 
shapiro.test(states$Area)  #0.1876



library(MASS)
box_cox_transformation <- boxcox(states$Murder ~ states$Illiteracy)

lamda <- box_cox_transformation$x[which.max(box_cox_transformation$y)]
lamda


normalised_Illiteracy <- ((states$Illiteracy ^ lamda)/ lamda)
hist(normalised_Illiteracy)
shapiro.test(normalised_Illiteracy)



library(MASS)
box_cox_transformation <- boxcox(states$Murder ~ states$`HS Grad`)

lamda <- box_cox_transformation$x[which.max(box_cox_transformation$y)]
lamda


normalised_hsGrad <- ((states$`HS Grad` ^ lamda)/ lamda)
hist(normalised_hsGrad)
shapiro.test(normalised_hsGrad)
