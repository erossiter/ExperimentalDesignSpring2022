# Replication Code for "Public Attitudes toward Young Men"
# Author: Dalston G. Ward
# Date: September, 2018

library(data.table)
library(lmtest)
library(multiwayvcov)
library(texreg)
library(car)

data <- fread("YoungImmMenConjoint.csv", na.strings = "")

# 36 respondents who were outside the target age range (18-75)
# or had missing values on state, gender, or education
# were dropped from the data before calculating weights.
# This data is used for all weighted estimation.
datw <- data[!is.na(ps_weights)]

# Model for Figure 1
main <- lm(settle_binary ~ factor(group_edu) + men0 + men25 + men75 + men100,
           weights = ps_weights,
           data = datw)
main_vcov <- cluster.vcov(main, datw[ , "id"])
(main_coefs <- coeftest(main, main_vcov))


# Figure 2
job_mod <- lm( jobs_binary ~ factor(group_edu) + men0 + men25 + men75 + men100 , weights = ps_weights, data = datw)
job_vcov <- cluster.vcov(job_mod, datw[, id])
(job_coef <- coeftest(job_mod, job_vcov))

sec_mod <- lm(security_binary ~ factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw)
sec_vcov <- cluster.vcov(sec_mod, datw[ , id])
(sec_coef <- coeftest(sec_mod, sec_vcov))

culture_mod <- lm(culture_binary ~ factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw)
culture_vcov <- cluster.vcov(culture_mod, datw[ , id])
(culture_coef <- coeftest(culture_mod, culture_vcov))
