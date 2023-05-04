str(mtcars)
str(cars)

#distance = dependent
#speed = independent

scatter.smooth(x = cars$speed, y = cars$dist 
               , main = "distance ~ speed"
               ,xlab = "stopping speed"
               ,ylab = "stopping distance")


# seems to be a positive correlation between speed and stopping distance

cor(cars$dist , cars$speed)


#check for outliers
#genarally  any data that is outside the 1.5x interquartile range then it is outlier

opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))

attach(cars)
boxplot(dist
        , main = "Distance"
        , sub = paste("Outlier rows: "
                      , boxplot.stats(dist)$out))

boxplot(speed, main = "Speed", sub = paste("Outlier rows: ", boxplot.stats(speed)$out))

par(opar)


# seems there is an outlier where distance is 120
nrow(cars)


# remove the outlier row

cars <- subset(cars,cars$dist != 120)


# check for normality and skewness
install.packages("e1071")
library(e1071)
par(mfrow = c(1,2))

#density plot for speed
attach(cars)
plot(density(speed),main = "density plot for speed"
     ,ylab = "frequency"
     ,sub = paste("Skewness: ",(e1071::skewness(speed))))


# skewness <-1 or >1 then highly skewed
#-1 to -0.5 and 0.5 to 1 = moderately skewed
# -0.5 to 0.5 the approx symmetric


polygon(density(speed),col = "red")




library(e1071)
par(mfrow = c(1,2))

#density plot for speed
attach(cars)
plot(density(dist),main = "density plot for distance"
     ,ylab = "frequency"
     ,sub = paste("Skewness: ",(e1071::skewness(dist))))


# skewness <-1 or >1 then highly skewed
#-1 to -0.5 and 0.5 to 1 = moderately skewed
# -0.5 to 0.5 the approx symmetric


polygon(density(dist),col = "red")

