#ASSIGNMENT 1
#LOADING PACKAGES
library(tidyverse)
library(psych)
library(car)
library(lmtest)
library(sandwich)
library(boot)
library(lmboot)
library(gridExtra)
library(lm.beta)
library(lme4)
library(lmerTest)
library(cAIC4)
library(r2glmm)
library(MuMIn)

#LOADING THE DATA

data1 = read.csv("https://tinyurl.com/ha-dataset1")

View(data1)

data1 <- data1 %>%
  mutate(sex = factor(sex))

data1 <- data1 %>%
  rename(gender = sex)

data1 <- data1 %>%
  mutate(gender_r = recode_factor(gender,
                                  "male" = 1,
                                  "female" = 2))

#SPOTTING INPUT ERRORS

datasummary1 <- data1 %>%
  select(age, gender_r, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva, pain) %>%
  summary()

datasummary1

hist_age <- data1 %>%
  ggplot() +
  aes(x = age) +
  geom_histogram(binwidth = 2)

hist_age

hist_STAI <- data1 %>%
  ggplot() +
  aes(x = STAI_trait) +
  geom_histogram(binwidth = 2)

hist_STAI

data2 <- data1 %>%
  filter(age != 444, STAI_trait != 3.9)

View(data2)

datasummary2 <- data2 %>%
  select(age, gender_r, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva, pain) %>%
  summary()

datasummary2

#BUILDING MODELS AND ANALYSING LEVERAGE

mod1 <- lm(pain ~ age + gender_r, data = data2)

mod2 <- lm(pain ~ age + gender_r + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data2)

graph1 <- data2 %>%
  mutate(rownum = row.names(data2)) %>%
  ggplot() +
  aes(x = age, y = pain, label = rownum) +
  geom_point() +
  geom_label() +
  geom_smooth(method = "lm")

graph1

graph2 <- mod2 %>%
  plot(which = 5)

graph3 <- mod2 %>%
  plot(which = 4)

#TESTING THE NORMALITY ASSUMPTION

qqplot <- mod2 %>%
  plot(which = 2)

res_mod2 = enframe(residuals(mod2))

hist_res <- res_mod2 %>%
  ggplot() +
  aes(x = value) +
  geom_histogram(binwidth = 0.1)

hist_res

describe(residuals(mod2))

#TESTING THE LINEARITY ASSUMPTION

plot_res <- mod2 %>%
  residualPlots()

plot_res

#TESTING THE HOMOSCEDASTICITY ASSUMPTION

plot_homo <- mod2 %>%
  plot(which = 3)

NCV_mod2 <- mod2 %>%
  ncvTest()

NCV_mod2

BP_mod2 <- mod2 %>%
  bptest()

BP_mod2

#TESTING THE MULTICOLLINEARITY ASSUMPTION

VIF_mod2 <- mod2 %>%
  vif()

VIF_mod2

multi_mod2 <- data2 %>%
  select(pain, age, gender_r, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva) %>%
  pairs.panels(col = "red", lm = T)

data3 <- data2 %>%
  mutate(cortisol_level = (cortisol_serum + cortisol_saliva)/2)

View(data3)

mod2.2 <- lm(pain ~ age + gender_r + STAI_trait + pain_cat + mindfulness + cortisol_level, data = data3)

#CHECKING ALL ASSUMPTIONS AGAIN WITH THE NEW MODEL (ie. mod2.2)

normality_mod2 <- describe(residuals(mod2.2))

normality_mod2

linearity_mod2 <- mod2.2 %>%
  residualPlots()

homoscedasticity_NCV_mod2 <- mod2.2 %>%
  ncvTest()

homoscedasticity_NCV_mod2

homoscedasticity_BP_mod2 <- mod2.2 %>%
  bptest()

homoscedasticity_BP_mod2

multicollinearlity_mod2 <- mod2.2 %>%
  vif()

multicollinearlity_mod2

#MODEL RESULTS

summary_mod1 <- mod1 %>%
  summary()

summary_mod1

summary_mod2 <- mod2.2 %>%
  summary()

summary_mod2

CI_mod1 <- confint(mod1)

CI_mod1

CI_mod2 <- confint(mod2.2)

CI_mod2

beta_mod1 <- lm.beta(mod1)

beta_mod1

beta_mod2 <- lm.beta(mod2.2)

beta_mod2

AIC_mod1 <- AIC(mod1)

AIC_mod1

AIC_mod2 <- AIC(mod2.2)

AIC_mod2

ANOVA_12 <- anova(mod1, mod2.2)

ANOVA_12


#ASSIGNMENT 2
#SPOTTING INPUT ERRORS

datasummary3 <- data3 %>%
  select(age, gender_r, STAI_trait, pain_cat, mindfulness, cortisol_serum, weight, IQ, household_income, pain) %>%
  summary()

datasummary3

data3_hist <- data3 %>%
  ggplot() +
  aes(x = household_income) +
  geom_histogram()

data3_hist

data4 <- data3 %>%
  filter(household_income != -3732)

View(data4)

datasummary4 <- data4 %>%
  select(age, gender_r, STAI_trait, pain_cat, mindfulness, cortisol_serum, weight, IQ, household_income, pain) %>%
  summary()

datasummary4

graph4 <- data4 %>%
  mutate(rownum = row.names(data4)) %>%
  ggplot() +
  aes(x = age, y = pain, label = rownum) +
  geom_point() +
  geom_label() +
  geom_smooth(method = "lm")

graph4

#BUILDING MODELS AND ANALYSING LEVERAGE

mod2.3 <- lm(pain ~ age + gender_r + STAI_trait + pain_cat + mindfulness + cortisol_level, data = data4)

mod3 <- lm(pain ~ age + gender_r + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data4)

summary(mod3)

graph5 <- mod3 %>%
  plot(which = 5)

graph6 <- mod3 %>%
  plot(which = 4)

#CHECKING THE NORMALITY ASSUMPTION

qqplot2 <- mod3 %>%
  plot(which = 2)

res_mod3 = enframe(residuals(mod3))

hist_res3 <- res_mod3 %>%
  ggplot() +
  aes(x = value) +
  geom_histogram(binwidth = 0.1)

hist_res3

describe(residuals(mod3))

#CHECKING THE LINEARITY ASSUMPTION

plot_res3 <- mod3 %>%
  residualPlots()

plot_res3

#TESTING THE HOMOSCEDASTICITY ASSUMPTION

plot_homo3 <- mod3 %>%
  plot(which = 3)

NCV_mod3 <- mod3 %>%
  ncvTest()

NCV_mod3

BP_mod3 <- mod3 %>%
  bptest()

BP_mod3

#TESTING THE MULTICOLLINEARITY ASSUMPTION

VIF_mod3 <- mod3 %>%
  vif()

VIF_mod3

#BACKWARDS REGRESSION

mod3_back <- step(mod3, direction = "backward")

summary(mod3_back)

AIC_mod3 <- AIC(mod3)

AIC(mod3)

AIC_mod3_back <- AIC(mod3_back)

AIC_mod3_back

ANOVAq2 <- anova(mod3_back, mod3)

ANOVAq2

ANOVAq2.2 <- anova(mod2.3, mod3_back)

ANOVAq2.2

#BACKWARDS REGRESSION - MODEL RESULTS

summary_mod3 <- mod3 %>%
  summary()

summary_mod3

summary_mod3_back <- mod3_back %>%
  summary()

summary_mod3_back

CI_mod3_back <- confint(mod3_back)

CI_mod3_back

beta_mod3_back <- lm.beta(mod3_back)

beta_mod3_back

AIC_mod3 <- AIC(mod3)

AIC_mod3

AIC_mod2.3 <- AIC(mod2.3)

AIC_mod2.3

ANOVA_mod3 <- anova(mod3)

ANOVA_mod3

#TESTING THE MODELS ON A NEW DATA SET

newdata = read.csv("https://tinyurl.com/ha-dataset2")

View(newdata)

newdata <- newdata %>%
  mutate(sex = factor(sex))

newdata <- newdata %>%
  rename(gender = sex)

newdata <- newdata %>%
  mutate(gender_r = recode_factor(gender,
                                  "male" = 1,
                                  "female" = 2))

newdata <- newdata %>%
  mutate(cortisol_level = ((cortisol_serum + cortisol_saliva)/2))

newdata <- newdata %>%
  filter(mindfulness != 7.17)

predict_mod2 <- predict(mod2.3, newdata)

predict_mod3 <- predict(mod3, newdata)

RSS_mod2 = sum((newdata[, "pain"] - predict_mod2)^2)

RSS_mod2

RSS_mod3 = sum((newdata[, "pain"] - predict_mod3)^2)

RSS_mod3

#ASSIGNMENT 3
#LOADING THE DATA

q3data1 = read.csv("https://tinyurl.com/ha-dataset3")

View(q3data1)

summary(q3data1)

q3data1 = q3data1 %>%
  rename(gender = sex)

q3data1 <- q3data1 %>%
  mutate(gender = recode_factor(gender,
                                "femlae" = "female"))

q3data1 = q3data1 %>%
  mutate(gender_r = factor(gender))

q3data1 = q3data1 %>%
  mutate(gender_r = recode_factor(gender_r,
                                  "male" = 1,
                                  "female" = 2))

q3data1 = q3data1 %>%
  mutate(hospital = factor(hospital))

q3data1 = q3data1 %>%
  mutate(hospital_r = recode_factor(hospital,
                                    "hospital_1" = 1,
                                    "hospital_2" = 2,
                                    "hospital_3" = 3,
                                    "hospital_4" = 4,
                                    "hospital_5" = 5,
                                    "hospital_6" = 6,
                                    "hospital_7" = 7,
                                    "hospital_8" = 8,
                                    "hospital_9" = 9,
                                    "hospital_10" = 10))
                                  
summary(q3data1)

#BUILDING THE MODELS

mod4 = lmer(pain ~ gender_r + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness + (1|hospital_r), data = q3data1)

summary_mod4 <- mod4 %>%
	summary()
	
summary_mod4

CI_mod4 <- confint(mod4)

CI_mod4

AIC_mod4 <- AIC(mod4)

AIC_mod4

ANOVA_mod4 <- anova(mod4)

ANOVA_mod4

R2b_mod4 <- r2beta(mod4, method = "nsj", data = q3data1)

R2b_mod4

marg_cond_mod4 <- r.squaredGLMM(mod4)

marg_cond_mod4

#APPLYING THE MODEL TO A NEW DATA SET

q3data2 = read.csv("https://tinyurl.com/ha-dataset4")

View(q3data2)

q3data2 = q3data2 %>%
	rename(gender = sex)

q3data2 = q3data2 %>%
	mutate(gender = factor(gender))

q3data2 = q3data2 %>%
	mutate(gender_r = recode_factor(gender,
					"male" = 1,
					"female" = 2))

q3data2 = q3data2 %>%
	mutate(hospital = factor(hospital))

q3data2 = q3data2 %>%
	mutate(hospital_r = recode_factor(hospital,
					"hospital_11" = 11,
					"hospital_12" = 12,
					"hospital_13" = 13,
					"hospital_14" = 14,
					"hospital_15" = 15,
					"hospital_16" = 16,
					"hospital_17" = 17,
					"hospital_18" = 18,
					"hospital_19" = 19,
					"hospital_20" = 20))

summary(q3data2)

q3data2 = q3data2 %>%
	filter(mindfulness != 6.05)

#FINDING THE R^2 VALUE OF THE MODEL

mod_mean <- lm(pain ~ 1, data = q3data2)

RSS_mod4 <- sum(residuals(mod4)^2)

TSS_mod4 <- sum((q3data2$pain - predict(mod_mean))^2)

mod4_r2 <- (1 - (RSS_mod4/TSS_mod4))

mod4_r2

r2beta_mod4 <- r2beta(mod4)

r2beta_mod4

#BUILDING A RANDOM SLOPE MIXED MODEL USING ONLY THE MOST INFLUENTIAL PREDICTOR

mod5 = lmer(pain ~ cortisol_saliva + (cortisol_saliva|hospital_r), data = q3data1)

q3plot_all <- q3data1 %>%
  ggplot() +
  aes(y = pain, x = cortisol_saliva, color = hospital_r) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = F, fullrange=TRUE)

q3plot_all

q3data1 <- q3data1 %>%
	mutate(q3pred = predict(mod5))

q3plot_sep <- q3data1 %>%
  ggplot() +
  aes(y = pain, x = cortisol_saliva, group = hospital_r) +
  geom_point(aes(color = hospital_r), size = 4) +
  geom_line(color='red', aes(y = q3pred, x = cortisol_saliva)) +
  facet_wrap( ~ hospital_r, ncol = 2)

q3plot_sep