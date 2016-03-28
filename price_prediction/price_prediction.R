# IMPORT DATA
House_Price = read.csv("house_price.csv", stringsAsFactors = FALSE)
View(House_Price)  # manual check on anomal values, var4 seems to be a categorical variable
str(House_Price)
House_Price$var4 = as.factor(House_Price$var4)

# LOOK AT DATA: watch for outliers, skewness, heteroskedasticity, variance
summary(House_Price)
    # Distribution
hist(House_Price$house_price) # nearly normal
hist(House_Price$var1) # right skewed
hist(House_Price$var2) # bimodal
hist(House_Price$var3) # nearly normal
    # Outliers 
boxplot(House_Price$house_price) # outliers detected
boxplot(House_Price$var1)
boxplot(House_Price$var2) 
boxplot(House_Price$var3) # outliers detected
barplot(prop.table(table(House_Price$var4)))

    # Correlation: how variables are related to one another graphically
        # Numeric vars
plot(House_Price[c("house_price", "var1", "var2", "var3")]) # Linearity exists for var3, isn't obvious with var1 and var2. 
        # Log transformation
plot(log(House_Price$var1), log(House_Price$house_price)) # still not obvious. More than one peak appeared
plot(log(House_Price$var2), log(House_Price$house_price)) # Might consider converting them into categorical vars
     
        # Categorical var: ANOVA
aov.var4 = aov(house_price ~ var4, data = House_Price)
summary(aov.var4) # with no other estimator present, var4 is a statistically significant predictor
boxplot(house_price ~ var4, data = House_Price)
print(model.tables(aov.var4, "means"), digits = 3) # report the means and the number of subjects/cell

    # Deal with anomal data
library(plyr)
library(dplyr)
HP = filter(House_Price, var1 > 0) # exclude zero value observations in var1

# MODELLING
    # Multiple linear regression 
        # Model 1
r1 = lm(log(house_price) ~ var3 + var4, data = HP)
summary(r1) # var4 gets insignificant but adjusted R-squared is higher than just having var4: 0.6685
        # Step regression
r1.step = step(r1) # Keeping var3 as the only predictor gives the highest AIC

        # Model 2: keeping var3 only makes it unnecessary to apply log transformation
r2 = lm(house_price ~ var3, data = HP)
summary(r2) # Adjusted R-squared: 0.72

        # Model 3: Adding transformed var1 
HP$var1_cat = ifelse(HP$var1 <= 300, 1, 
                              ifelse(HP$var1 > 300 && HP$var1 <= 500, 2, 3)) # breaking down var1
HP$var1_cat = as.factor(HP$var1_cat)
r3 = lm(log(house_price) ~ var3 + var1_cat, data = HP) 
summary(r3) # predicting power does not get improved, hence Model 2 will be used as the final model
      
    # Price prediction
newdata = data.frame(var3 = 1650)
print(predict(r2, newdata)) # based on the regression model and the given parameters, this gives the predicted house price

