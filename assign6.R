#load the data
rm(list=ls())
setwd("E:/2Sem_all_study_material/SDM/week7/assign")
library(readxl)
d<- read.table("LungCancer.txt", skip=15)
str(d)
colnames(d) <- c("med_treatemnt", "cell_type", "survival","status", "karn_score",
                 "month_diagosis", "age", "prior_chemo")

#feature engineering

d$med_treatemnt = factor(d$med_treatemnt)
d$cell_type = factor(d$cell_type)
d$prior_chemo = factor(d$prior_chemo)
attach(d)
d$cancer <- ifelse(d$cell_type=="2", "small-cell", "non-small-cell")
d$cancer <- factor(d$cancer)

# Data visulization 
length(unique(survival))                 # Count of unique values in the Time Column
unique(survival)                         # 20 unique times in the sample: 1-28 
summary(survival)
table(status)
table(prior_chemo)
hist(survival)
hist(age)

# Correlation Test

library("PerformanceAnalytics")
d_temp <- d[, c(5,6,7)]
chart.Correlation(d_temp)                    # there is no correlation 



library(survival)
y <- Surv(survival, status) 
km <- survfit(y~1)
summary(km)

km2 <- survfit(y~med_treatemnt)
summary(km2)
plot(km2, xlab="Days", ylab="Survival Probability")


summary(km2, times = 365)

# Cox proportional hazard model - coefficients and hazard rates
cox <- coxph(y~med_treatemnt+karn_score+month_diagosis+age+prior_chemo+cancer+
               med_treatemnt*age, data=d)
summary(cox)

# Exponential, Weibull, and log-logistic parametric model coefficients

exp <- survreg(y~med_treatemnt+karn_score+month_diagosis+age+prior_chemo+cancer+
                 med_treatemnt*age, data=d, dist="exponential")
summary(exp)

weibull <- survreg(y~med_treatemnt+karn_score+month_diagosis+age+prior_chemo+cancer+
                     med_treatemnt*age, data=d, dist="weibull")
summary(weibull)

loglogistic <- survreg(y ~ med_treatemnt+karn_score+month_diagosis+age+prior_chemo+cancer+
                         med_treatemnt*age, data = d, dist="loglogistic")
summary(loglogistic)


library(stargazer)
stargazer(cox, exp, weibull, loglogistic, type="text")



