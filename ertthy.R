setwd("/cloud/project")

#All packages that are installed should be placed at the top of the code.
install.packages("leaps")
library(leaps)

#Assign the data to an object name.
SleepHealth <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

#Allows us to call data headings without using $ every time.
attach(SleepHealth)

#Justification for performing linear regression needs the data of focus to be
#normally distributed. Using the Shapiro Wilks test, we will evaluate whether 
#or not the data is normally distributed.
shapiro.test(Sleep.Duration)

#The test gives back a p-value of 1.268e-11. This number is a very small number
#and importantly less than .05, therefore we can reject the null hypothesis 
#stating that the data is not normal and therefore the data is normal.

#Blood Pressure variable needs to be split into two different variables, 
#diastolic and systolic
SleepHealth$Systolic = substr(Blood.Pressure, 1, 3)
SleepHealth$Systolic = as.numeric(SleepHealth$Systolic)
SleepHealth$Diastolic = substr(Blood.Pressure, 5, 6)
SleepHealth$Diastolic = as.numeric(SleepHealth$Diastolic)
#Now there are two more columns in the data, one with the systolic value and 
#the other with the diastolic value.

##IMPORTANT
#Remember when analyzing the blood pressure variable we will no longer use the
#Blood.Pressure column. Now we will use the two new columns instead.

#Next lines of code help us see what are the best variables for predicting sleep
#duration.
output <- regsubsets(Sleep.Duration ~ Gender + Age + Occupation +
                       Quality.of.Sleep + Physical.Activity.Level + 
                       Stress.Level + BMI.Category + Heart.Rate + Daily.Steps +
                       Sleep.Disorder + Systolic + Diastolic, data=SleepHealth, 
                     nvmax=12)

summOut <- summary(output)                
summOut

#To read this output, you will be looking for asterisks(*). For example,
#according to this output, the best singular variable to predict sleep duration
#is quality of sleep. The best two variables combined to predict sleep duration
# are Occupation Doctor and Quality of Sleep. 




#Part 3 - Take the code from second person (which should already include 
#person's 1), and output the plot that will help to determine #the best model 
#among all best models (AIC vs p) plot. Determine which model is the best model. 
#List the variables in this best model


n1 <- length(Sleep.Duration)
n1

p1 <- apply(summOut$which, 1, sum)

summOut$which

p1
#This shows us how many trues each variable has in a numeric form, and the models
#also include the intercept. 

aic1 <- summOut$bic - log(n1) * p1 + 2 * p1

plot(p1, aic1, ylab = "AIC1")
#based on the plotted data, it helps us determine the best model out of all
#the models which includes all 12 predictors because the lowest dot is 12.  

summOut
#We are re-running summOut to find the 12 variables that also has the lowest AIC
model1 <- lm(Sleep.Duration ~ Gender + Age + Occupation+ Quality.of.Sleep + 
               Physical.Activity.Level + Stress.Level + BMI.Category + 
               Heart.Rate+ Daily.Steps + Sleep.Disorder + Systolic + 
               Diastolic, data=SleepHealth)

summary(model1)

#Reviewing Occupations categoires

table(Occupation)

#Summary of significant levels
#Occupation Accountant is the reference level
#Coefficients for other occupations indicate 
#how each group's sleep duration differs from the Accountant group.


#Reviewing BMI Category

table(BMI.Category)

#Interpretation of results:
#"Normal" is the benchmark, and coefficients for "Overweight" and "Obese" 
#show the difference in sleep duration compared to "Normal."

#Reviewing Sleep Disorder categories

table(Sleep.Disorder)

#Interpretation of results:
#"Insomnia" serves as the benchmark for comparison.
#Coefficients for "None" and "Sleep Apnea" 
#show how these conditions affect sleep duration compared to Insomnia.

