require(mosaic)
trellis.par.set(theme=col.mosaic())
options(digits = 3)

View(ex11_53cheese)

# Question 11.53
ds = ex11_53cheese

names(ds)

favstats(ds$taste)
stem(ds$taste)
qqnorm(ds$taste)
qqline(ds$taste)
  # The taste Distribution is skewely right but is approximately Normal

favstats(ds$acetic)
stem(ds$acetic)
qqnorm(ds$acetic)
qqline(ds$acetic)
  # The acetic Distribution is approximately Normal

favstats(ds$h2s)
stem(ds$h2s)
qqnorm(ds$h2s)
qqline(ds$h2s)
  # The h2s Distribution is skewely right but is approximately Normal

favstats(ds$lactic)
stem(ds$lactic)
qqnorm(ds$lactic)
qqline(ds$lactic)
  # The lactic Distribution is approximately Normal





# Question 11.54

smallds = subset(ds, select=c("taste", "acetic", "h2s", "lactic"))
with(ds, cor(smallds))
  # output is correlation matrix 
pairs(smallds, pch=".")

# alt (long) way to find P
cor.test(ds$taste,ds$acetic)
cor.test(ds$taste,ds$h2s)
cor.test(ds$taste,ds$lactic)
cor.test(ds$acetic,ds$h2s)
cor.test(ds$acetic,ds$lactic)
cor.test(ds$h2s,ds$lactic)

#Outcome Summary:
  # Pair          Correlation  P-Values (closer to 1 = stronger)    
  # taste + acetic  = 0.550   = 0.002        the weakest linear relationship
  # taste + h2s     = 0.756   = 0.000001     the strongest positive linear relationship
  # taste + lactic  = 0.704   = 0.00001      positive linear relationship
  # acetic + h2s    = 0.618   = 0.0003       positive linear relationship
  # acetic + lactic = 0.604   = 0.0004       positive linear relationship
  # h2s + lactic    = 0.645   = 0.0001       positive linear relationship

    # p-values of zero pop correlation are very very small, can reject hypothesis
    # that the correlations on pop are not zero



# Question 11.55
    # simple linear regression 
    # taste - response var
    # acetic - explanatory var

  #SLR
acetic = ds$acetic
taste = ds$taste

lm1 = lm(taste~acetic, data = ds)
coef(lm1)
rsquared(lm1)
summary(lm1)  # hatTaste = -61.5 + 15.6*Acetic
anova(lm1)

  # plot of the data 

plot(acetic, taste, xlab = "Acetic", ylab = "Taste", main = "Plot Taste vs Acetic")
  # add the found least-squares regression line
abline(coef(lm1), lwd =2, lty = 2, col="red")

    # The residuals.................SUMMARIZE
plot(lm1, which=2)
histogram(~ residuals(lm1), fit="normal")
plot(lm1, which =1)

  # Plot of acetic residuals vs h2s
h2s = ds$h2s
xyplot(residuals(lm1) ~ h2s, type=c("p", "r", "smooth"), main = "Residuals v H2S")

  # Plot of acetic residuals vs lactic
lactic = ds$lactic
xyplot(residuals(lm1) ~ lactic, type=c("p", "r", "smooth"),  main = "Residuals v Lactic")

  # include t and P values
  # any patterns?




# Question 11.56
  # simple linear regression 
  # taste - response var
  # h2s - explanatory var

    #SLR
taste = ds$taste
h2s = ds$h2s

lm2 = lm(taste~h2s, data = ds)
coef(lm2)
rsquared(lm2)
summary(lm2)  # hatTaste = -9.79 + 5.78*h2s
anova(lm2)

  # plot of the data 

plot(h2s, taste, xlab = "H2S", ylab = "Taste", main = "Plot Taste vs H2S")
  # add the found least-squares regression line
abline(coef(lm2), lwd =2, lty = 2, col="red")

  # The residuals.................SUMMARIZE
plot(lm2, which=2)
histogram(~ residuals(lm2), fit="normal")
plot(lm2, which =1)

  # Plot of H2S residuals vs acetic
acetic = ds$acetic
xyplot(residuals(lm2) ~ acetic, type=c("p", "r", "smooth"), main = "Residuals v Acetic")

  # Plot of H2S residuals vs lactic
lactic = ds$lactic
xyplot(residuals(lm2) ~ lactic, type=c("p", "r", "smooth"),  main = "Residuals v Lactic")

  # include t and P values
  # any patterns?



# Question 11.57
  # simple linear regression 
  # taste - response var
  # lactic - explanatory var

  #SLR
taste = ds$taste
lactic = ds$lactic

lm3 = lm(taste~lactic, data = ds)
coef(lm3)
rsquared(lm3)
summary(lm3)  # hatTaste = -29.9 + 37.7*lactic
anova(lm3)

  # plot of the data 
plot(lactic, taste, xlab = "Lactic", ylab = "Taste", main = "Plot Taste vs Lactic")
  # add the found least-squares regression line
abline(coef(lm3), lwd =2, lty = 2, col="red")

  # The residuals.................SUMMARIZE!
plot(lm3, which=2)
histogram(~ residuals(lm3), fit="normal")
plot(lm3, which =1)

  # Plot of Lactic residuals vs acetic
acetic = ds$acetic
xyplot(residuals(lm3) ~ acetic, type=c("p", "r", "smooth"), main = "Residuals v Acetic")

  # Plot of Lactic residuals vs H2S
h2s = ds$h2s
xyplot(residuals(lm3) ~ h2s, type=c("p", "r", "smooth"),  main = "Residuals v H2S")

  # include t and P values
  # any patterns?



# Question 11.58
  # MAKE A TABLE TO COMPARE the previous 3 simple linear regressions



#Question 11.59 - mult linear reg Taste using acetic + h2s
acetic = ds$acetic
taste = ds$taste
h2s = ds$h2s

lm4 = lm(taste~ acetic + h2s, data = ds)
coef(lm4)
rsquared(lm4)
summary(lm4)  
anova(lm4)
plot(lm4)

  #Summary of anlysis
  #Compare the statistical significance of Acetic in this model 
  # with its significance alone as a predictor
  #Which model do you prefer?
  #Give simple explanation fo rthe fact acetic alone appears to 
  #  be a good predictor of tast but with HS@ in the model, it is not.


#Question 11.60- mult linear reg Taste using Lactic + h2s
lactic = ds$lactic
taste = ds$taste
h2s = ds$h2s

lm5 = lm(taste~ h2s + lactic, data = ds)
coef(lm5)
rsquared(lm5)
summary(lm5)  
anova(lm5)
plot(lm5)

  #Summary of anlysis
  #Compared to the results to the simple linear regressions using each 
  #   of these variables alone, it is evident that a better result 
  #   is obtained by using both predictors in a model. 
  #Support this statement with explicit information obtained from 
  #   your analysis


# Question 11.61 - mult lin reg all
lactic = ds$lactic
taste = ds$taste
h2s = ds$h2s
acetic = ds$acetic

lmALL = lm(taste~ acetic+ h2s + lactic, data = ds)
coef(lmALL)
rsquared(lmALL)
summary(lmALL)  
anova(lmALL)
plot(lmALL)
    #write a short summary of your results, including an examination 
    # of the residuals. Based on all the regression analyses you 
    # have carried out on these data, which model do you prefer and 
    # why?








