rm(list=ls())
library(rio)
library(dplyr)
d = import("HRanalytics.xlsx")
str(d)
dim(d)
#Checking for missing values
colSums(is.na(d)) # 8 NA's in manager ID. #104 in DateOfTermination.means 207 are still working.
#Feature Engineering.
d <- d[d$Salary < 220000, ] #CEO and President are the outliers.
#Factor conversion
d$Termd              =    as.factor(d$Termd)
d$Sex                =    as.factor(d$Sex)
d$Sex                =    relevel(d$Sex, "F")
d$MaritalDesc        =    as.factor(d$MaritalDesc)
d$MaritalDesc        =    relevel(d$MaritalDesc, "Widowed")
d$CitizenDesc        =    as.factor(d$CitizenDesc)
d$HispanicLatino     =    as.factor(d$HispanicLatino)
d$HispanicLatino     =    relevel(d$HispanicLatino, "No")
d$RaceDesc           =    as.factor(d$RaceDesc)
d$RaceDesc           =    relevel(d$RaceDesc, "Black or African American")
d$EmploymentStatus   =    as.factor(d$EmploymentStatus)
d$Department         =    as.factor(d$Department)
d$Department         =    relevel(d$Department, "Software Engineering")
d$ManagerName        =    as.factor(d$ManagerName)
d$RecruitmentSource  =    as.factor(d$RecruitmentSource)
d$RecruitmentSource  =    relevel(d$RecruitmentSource, "On-line Web application")
d$Position           =    as.factor(d$Position)
d$Position           =    relevel(d$Position, "Accountant I")
d$EmpSatisfaction    =    as.factor(d$EmpSatisfaction)

str(d)
attach(d)
#Descriptive analysis 

table(Termd) # 104 Employees terminated and 207 are active
table(PerfScoreID) #243 Employees are rated as "Fully meets"
table(EmpSatisfaction) #Most of the employees have 3 or higher satisfaction score.
hist(EngagementSurvey) #Looks good
hist(Absences) #Not normal
hist(log(Absences)) #Left skewed
summary(Absences) #The average number of absence days in the last 30 days is 10 days which is too high.
aggregate(Absences ~ Department, data=d, mean) #Sales has the highest avg. absences.
hist(Salary)
hist(log(Salary)) #Looks approx. normal
summary(Salary)
table(RecruitmentSource)
table(SpecialProjectsCount)
hist(SpecialProjectsCount)
summary(SpecialProjectsCount)

#Correlation Analysis
library(PerformanceAnalytics)
numeric_cols <- d %>%
  select_if(is.numeric)
chart.Correlation(numeric_cols, histogram=TRUE, pch=19)


#Models creation

#We have 3 DV's to analyze.employee performance(PerfScoreID), 
#satisfaction(EmpSatisfaction), and Absences

#Performace score and EmpSatisfaction are  ordered factors

#For the Performance Score and Satisfaction Score variables, 
#which are ordinal variables, you can use an ordered logistic regression model (also known as an ordinal regression model) instead of a 
#linear regression model. This model will take into account the ordinal nature of the outcome variable and provide more accurate estimates 
#of the relationships between the predictor variables and the outcome variables.

library("nnet")
#Model for performance score
#1. Multinomial model.
d$PerformanceScore = as.factor(d$PerformanceScore)
d$PerformanceScore = relevel(d$PerformanceScore, "PIP") #Releveing it to the lowest performance rating.

per_ml <- multinom(PerformanceScore ~ Salary +  EmpSatisfaction + Department * ManagerName 
                      + Absences  + RecruitmentSource + RaceDesc + Sex, data=d, trace=FALSE)

sat_ml = multinom(EmpSatisfaction ~ Salary + PerformanceScore + Department * ManagerName  + 
                    RecruitmentSource + RaceDesc + Sex , data=d, trace=FALSE)
#The odds ratio of these models is close to Inf and cannot be interpretable. 

#Model for Absences. Negbin because pois had high dispersion.
library(MASS)
ab_nb = glm.nb(Absences ~ Department + PerformanceScore +  Salary  + ManagerName + RecruitmentSource + RaceDesc + Sex , data = d,link = log)
summary(ab_nb)
library(stargazer)

stargazer(ab_nb, type="html", single.row = TRUE, out="absences.html")

#Let's try polr() models on Performance score and Emp satisfaction.
d$PerformanceScore <- factor(d$PerformanceScore, levels=c("PIP",
                                                          "Needs Improvement", "Fully Meets", "Exceeds"), ordered=TRUE)
per_ol <- polr(PerformanceScore ~ Salary +  EmpSatisfaction + Department * ManagerName 
               + Absences  + RecruitmentSource + RaceDesc + Sex, data=d
                , Hess=TRUE)
summary(per_ol)

#Model is not converging. Hence we need to try other approaches.

#Since the DV's are bounded, we can try using Tobit models.
library(AER)
tobi_per = tobit(PerfScoreID ~  Salary + EmpSatisfaction + Department + ManagerName  + RecruitmentSource, left=1, right=4, data=d)
summary(tobi_per)

d$EmpSatisfaction = as.numeric(d$EmpSatisfaction)
tobi_sat = tobit(EmpSatisfaction ~ PerformanceScore + Department + ManagerName  + RecruitmentSource, left=1, right=5, data=d)
summary(tobi_sat)

#TEsting Linearity 
plot(fitted(tobi_per), d$PerfScoreID, xlab = "Fitted values", ylab = "Perf.score")
# Add LOESS line to scatterplot
lines(lowess(fitted(tobi_per), d$PerfScoreID), col = "red")

#Testing Normality
# Extract residuals from the model
resid <- residuals(tobi_per)
# Create Q-Q plot of residuals
qqnorm(resid)
qqline(resid, col='red') #Fails on the edges but lets give it a pass.

#Testing Independence
library(lmtest)
dwtest(resid ~ seq_along(resid)) #Passed
#Testing Multicollinearity
library('car')
vif(tobi_per) #Passed


stargazer(tobi_per,tobi_sat, type="html", single.row=TRUE, out="tobits.htm")
stargazer(ab_nb, type="html", single.row=TRUE, out="nb.htm")


# Extract the coefficients
coefficients <- coef(ab_nb)

# Find the top three factors that contribute positively or negatively to employee performance
top_pos_factors <- head(sort(coefficients, decreasing = TRUE), n = 3)
top_neg_factors <- head(sort(coefficients), n = 3)





#Q5 
hist(log(Salary))
library(AER)
pois <- glm(Salary ~ Department * (RaceDesc + Sex) + PerformanceScore, data = d,family = poisson(link = "log"))
summary(pois)
library(AER)
dispersiontest(pois) #FAIL
#Lets try nb 
nb = glm.nb(Salary ~ Department * (RaceDesc + Sex + PerformanceScore +EmpSatisfaction ) , data = d)
summary(nb)
library('car')
vif(nb)
stargazer(nb, type="html", single.row = TRUE, out="sal.html")

# Create a new variable "days_of_work" from DateofTermination and DateOfHire only if the employee is terminated.
library(lubridate)
d$time = ifelse( d$Termd == 0,as.numeric(difftime(ymd(d$LastPerformanceReview_Date), ymd(d$DateofHire), units = "days")), 
                 
                 as.numeric(difftime(ymd(d$DateofTermination), ymd(d$DateofHire),units = "days")))

d <- d[d$time >= 0,]
library(survival)

# Fit Kaplan-Meier survival curves for each department
km_fit <- survfit(Surv(time, Termd) ~ Department, data = d)

# Plot Kaplan-Meier curves for each department
plot(km_fit, col=c("blue", "red", "green", "orange", "purple", "black"), 
     xlab="Time (days)", ylab="Survival Probability", main="Kaplan-Meier Curves by Department")
legend("topleft", legend=levels(d$Department), col=c("blue", "red", "green", "orange", "purple", "black"), lty=1, cex = 0.8)


# Estimate the survival probability at 5 years for each department
summary(km_fit, times=1825)


