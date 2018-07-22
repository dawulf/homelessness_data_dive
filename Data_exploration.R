# Data Exploration
library(readr)
X2017_Sheltered <- read_csv("~/Documents/Other/Homelessness Data Dive/Sheltered (HMIS)/2017_Sheltered.csv", 
                            col_types = cols(Application_ID = col_character()))
colnames(X2017_Sheltered)
factor_cols_2017_shel <- c("Gender","Sexual_Orientation","Ethnicity","Race_Full","Race_Recode",
                      "Times_Homeless_3yrs","Current_Stint_Duration","Total_Months_Homeless_3yrs",
                      "Prior_Living_Situation","Prior_Living_Situation_Duration","Family_Structure",
                      "Relation_To_HOH","School_Complete","Program_Name","Program_Type")
X2017_Sheltered[,factor_cols_2017_shel] <- lapply(X2017_Sheltered[,factor_cols_2017_shel], factor)
summary(X2017_Sheltered)

X2017_Unsheltered <- read_csv("~/Documents/Other/Homelessness Data Dive/Unsheltered (Demographic)/2017_Unsheltered.csv")
colnames(X2017_Unsheltered)
factor_cols_2017_un <- c("Current_Stint_Duration",
                      "Times_Homeless_3yrs","Times_Homeless_Past_Year")
X2017_Unsheltered[,factor_cols_2017_un] <- lapply(X2017_Unsheltered[,factor_cols_2017_un], factor)
summary(X2017_Unsheltered[,c("Current_Stint_Duration_Detailed","Current_Stint_Duration",
                             "Times_Homeless_3yrs","Times_Homeless_Past_Year")])

Multi_Year_Homeless <- read_csv("~/Documents/Other/Homelessness Data Dive/Multi-year dataset/Multi_Year_Homeless.csv")
colnames(Multi_Year_Homeless)
factor_cols_mult <- c("Survey_Year","Gender","Ethnicity","Race_Full","Race_Recode",
                           "Times_Homeless_3yrs","Times_Homeless_Past_Year",
                           "Current_Stint_Duration","Census_Tract")
Multi_Year_Homeless[,factor_cols_mult] <- lapply(Multi_Year_Homeless[,factor_cols_mult], factor)
summary(Multi_Year_Homeless)
Multi_Year_Homeless$Sheltered <- Multi_Year_Homeless$Survey_Year %in% c("Sheltered 2013","Sheltered 2015",
                                                                        "Sheltered 2016","Sheltered 2017")
table(Multi_Year_Homeless$Survey_Year,Multi_Year_Homeless$Sheltered)
Sheltered_vec <- Multi_Year_Homeless$Sheltered

install.packages("fastDummies")
library(fastDummies)
Multi_Year_Dummies <- dummy_cols(Multi_Year_Homeless[,-c(1,2,7,18,30)])
Multi_Year_Dummies <- Multi_Year_Dummies[,!sapply(Multi_Year_Dummies, is.factor)]
Multi_Year_Dummies_noNA <- Multi_Year_Dummies
Multi_Year_Dummies_noNA[is.na(Multi_Year_Dummies)] <- 0
Multi_Year_Dummies_noNA <- Multi_Year_Dummies_noNA[,-c(23,30,35,46)]
Multi_Year_Dummies_noNA <- as.matrix(Multi_Year_Dummies_noNA)
summary(Multi_Year_Dummies_noNA)

library(glmnet)
lasso <- glmnet(x=Multi_Year_Dummies_noNA, y=Sheltered_vec, family = "binomial", alpha=1)
plot(lasso, xvar="lambda")
coef(lasso)[,12:19]
#Top two are Unemployed_looking and Drug_Alcohol_History
tab_quads <- table(Multi_Year_Homeless$Unemployed_Looking, Multi_Year_Homeless$Drug_Alcohol_History, 
                   Multi_Year_Homeless$Sheltered, useNA = "ifany")

prop.table(tab_quads)
prop.table(tab_quads,1)
prop.table(tab_quads,2)


library(dplyr)
X2011_Unsheltered <- read_csv("~/Documents/Other/Homelessness Data Dive/Unsheltered (Demographic)/2011_Unsheltered.csv")
X2013_Unsheltered <- read_csv("~/Documents/Other/Homelessness Data Dive/Unsheltered (Demographic)/2013_Unsheltered.csv")
X2015_Unsheltered <- read_csv("~/Documents/Other/Homelessness Data Dive/Unsheltered (Demographic)/2015_Unsheltered.csv")
X2016_Unsheltered <- read_csv("~/Documents/Other/Homelessness Data Dive/Unsheltered (Demographic)/2016_Unsheltered.csv")
Unshelt11 <- X2011_Unsheltered %>% 
  select(c(Total_Time_Homeless_Life,Times_Homeless_3yrs,Times_Homeless_Past_Year,
           Current_Stint_Duration,Current_Stint_Duration_Detailed, Unemployed_Looking,
           Drug_Alcohol_History))
Unshelt11$Current_Stint_Duration <- as.factor(Unshelt11$Current_Stint_Duration)
prop.table(table(Unshelt11$Current_Stint_Duration))
Unshelt13 <- X2013_Unsheltered %>% 
  select(c(Total_Time_Homeless_Life,Times_Homeless_3yrs,Times_Homeless_Past_Year,
           Current_Stint_Duration,Current_Stint_Duration_Detailed, Unemployed_Looking,
           Drug_Alcohol_History))
Unshelt13$Current_Stint_Duration <- as.factor(Unshelt13$Current_Stint_Duration)
prop.table(table(Unshelt13$Current_Stint_Duration))
Unshelt15 <- X2015_Unsheltered %>% 
  select(c(Times_Homeless_3yrs,Times_Homeless_Past_Year,
           Current_Stint_Duration,Current_Stint_Duration_Detailed, Unemployed_Looking,
           Drug_Alcohol_History))
Unshelt15$Current_Stint_Duration <- as.factor(Unshelt15$Current_Stint_Duration)
prop.table(table(Unshelt15$Current_Stint_Duration))
Unshelt16 <- X2016_Unsheltered %>% 
  select(c(Times_Homeless_3yrs,
           Current_Stint_Duration,Current_Stint_Duration_Detailed, Unemployed_Looking,
           Drug_Alcohol_History))
Unshelt16$Current_Stint_Duration <- as.factor(Unshelt16$Current_Stint_Duration)
prop.table(table(Unshelt16$Current_Stint_Duration))
Unshelt17 <- X2017_Unsheltered %>% 
  select(c(Times_Homeless_3yrs,Times_Homeless_Past_Year,
           Current_Stint_Duration,Current_Stint_Duration_Detailed, Unemployed_Looking,
           Drug_Alcohol_History))
Unshelt17$Current_Stint_Duration <- as.factor(Unshelt17$Current_Stint_Duration)
prop.table(table(Unshelt17$Current_Stint_Duration))

Unshelt17$month_recode <- if_else(Unshelt17$Current_Stint_Duration_Detailed < 31, 1,
                          if_else(Unshelt17$Current_Stint_Duration_Detailed < 61, 2,
                          if_else(Unshelt17$Current_Stint_Duration_Detailed < 92, 3,
                          if_else(Unshelt17$Current_Stint_Duration_Detailed < 122, 4,
                          if_else(Unshelt17$Current_Stint_Duration_Detailed < 153, 5,
                          if_else(Unshelt17$Current_Stint_Duration_Detailed < 183, 6,
                          if_else(Unshelt17$Current_Stint_Duration_Detailed < 213, 7,
                          if_else(Unshelt17$Current_Stint_Duration_Detailed < 244, 8,
                          if_else(Unshelt17$Current_Stint_Duration_Detailed < 274, 9,
                          if_else(Unshelt17$Current_Stint_Duration_Detailed < 305, 10,
                          if_else(Unshelt17$Current_Stint_Duration_Detailed < 335, 11, 12
                                  )))))))))),12)

Unshelt13$month_recode <- if_else(Unshelt13$Current_Stint_Duration_Detailed < 31, 1,
                                  if_else(Unshelt13$Current_Stint_Duration_Detailed < 61, 2,
                                          if_else(Unshelt13$Current_Stint_Duration_Detailed < 91, 3,
                                                  if_else(Unshelt13$Current_Stint_Duration_Detailed < 121, 4,
                                                          if_else(Unshelt13$Current_Stint_Duration_Detailed < 151, 5,
                                                                  if_else(Unshelt13$Current_Stint_Duration_Detailed < 181, 6,
                                                                          if_else(Unshelt13$Current_Stint_Duration_Detailed < 211, 7,
                                                                                  if_else(Unshelt13$Current_Stint_Duration_Detailed < 241, 8,
                                                                                          if_else(Unshelt13$Current_Stint_Duration_Detailed < 271, 9,
                                                                                                  if_else(Unshelt13$Current_Stint_Duration_Detailed < 301, 10,
                                                                                                          if_else(Unshelt13$Current_Stint_Duration_Detailed < 331, 11, 12
                                                                                                          )))))))))),12)

Unshelt16$month_recode <- if_else(Unshelt16$Current_Stint_Duration_Detailed < 31, 1,
                                  if_else(Unshelt16$Current_Stint_Duration_Detailed < 61, 2,
                                          if_else(Unshelt16$Current_Stint_Duration_Detailed < 91, 3,
                                                  if_else(Unshelt16$Current_Stint_Duration_Detailed < 121, 4,
                                                          if_else(Unshelt16$Current_Stint_Duration_Detailed < 151, 5,
                                                                  if_else(Unshelt16$Current_Stint_Duration_Detailed < 181, 6,
                                                                          if_else(Unshelt16$Current_Stint_Duration_Detailed < 211, 7,
                                                                                  if_else(Unshelt16$Current_Stint_Duration_Detailed < 241, 8,
                                                                                          if_else(Unshelt16$Current_Stint_Duration_Detailed < 271, 9,
                                                                                                  if_else(Unshelt16$Current_Stint_Duration_Detailed < 301, 10,
                                                                                                          if_else(Unshelt16$Current_Stint_Duration_Detailed < 331, 11, 12
                                                                                                          )))))))))),12)
monthly_combined <- c(Unshelt13$month_recode,Unshelt16$month_recode,Unshelt17$month_recode)
library(MASS)
monthly_combined_to_11 <- (monthly_combined[monthly_combined < 12] - 0.999)/10.1
fitdistr(monthly_combined_to_11, "beta", list(shape1=1,shape2=1)) #0.3022, 0.7683
fitdistr(monthly_combined_to_11, "exponential") #3.046
exp_vec_11 <- dexp(seq(0,1,length.out = 11),3.046)
exp_vec_11 <- (exp_vec_11/sum(exp_vec_11))*0.3
