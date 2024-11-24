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

