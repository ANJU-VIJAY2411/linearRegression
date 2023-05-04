women_data <- women


#predit weight from height
# height = continuous independent
# weight =  continuous dependent

# syntax is y ~ x + z + w
attach(women_data)
simple_linear_model <- lm(weight ~ height , women_data)

simple_linear_model

attach(women_data)
#plot the dependent variable on the y axis and independent variable on x axis
plot(height
     ,weight
     ,xlab = "Height in inches"
     ,ylab = "weight in lbs"
     ,main = "Scatter plot showing regression line for weight prediction"
     ,abline(simple_linear_model)
     )


   
# h0 = there is no relationship between x and y 
# h1 = there is relation ship between x and y
# here p value is < 0.05 and hence there is relation between x and y


# corelation coefficient between height and weight
# -1 = perfect negative correlation
# -0.02 < x < 0.2 = most of the variation is not explainable by the independent variabl
# +1 = perfect positive correlation
confint(simple_linear_model)

cor(height,weight)

#model accuracy - goodness of fit
summary(simple_linear_model)

