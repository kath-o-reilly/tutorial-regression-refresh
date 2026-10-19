load("C:/Users/EmilyGranger/OneDrive - London School of Hygiene and Tropical Medicine/Documents/LSHTM/Teaching/2024/SHDS/Lesson 14 - Linear Regression 3/CFdata.RData")
library(table1)

#--------------------------------------------------------------------------------------
#Part 1A: data exploration 

#Baseline table
baseline_table<-table1(~ factor(Sex) + factor(Genotype) + Age + BMI + Baseline_FEV +
                         factor(Lung_Infection) + factor(HSS)
                       | Dnase, data=CFdata)

baseline_table

#Part 2: Univariable relationship between treatment and outcome
boxplot(CFdata$FEV~CFdata$Dnase)
summary(univariable_model<-lm(CFdata$FEV~CFdata$Dnase))
confint(univariable_model, parm=2, level=0.95)

#Part 3: Multivariable analyses
summary(multivariable_model<-lm(CFdata$FEV~CFdata$Dnase+CFdata$Age+CFdata$BMI+CFdata$Sex+CFdata$Baseline_FEV+CFdata$Genotype+CFdata$Lung_Infection+CFdata$HSS))
confint(multivariable_model, parm=2, level=0.95)

#Part 4: Investigate the assumptions
#Plot the residuals against the fitted values
plot(multivariable_model, which=1)

#Normal Q-Q plot of the standardised residuals 
plot(multivariable_model, which=2)

#Plot of Cook's distance
plot(multivariable_model, which=4)

#Part 5:Explore interaction terms
summary(multivariable_model<-lm(CFdata$FEV~CFdata$Dnase*CFdata$Sex+CFdata$Age+CFdata$BMI+CFdata$Baseline_FEV+CFdata$Genotype+CFdata$Lung_Infection+CFdata$HSS))
summary(multivariable_model<-lm(FEV~Dnase*Baseline_FEV+Age+BMI+Genotype+Lung_Infection+HSS, data=CFdata))

#Predicted average outcome in the treated (low FEV)
treated<-CFdata
treated$Dnase<-"Yes"
treated$Baseline_FEV<-80
predicted_treated<-mean(predict(multivariable_model, newdata=treated))
predicted_treated

untreated<-CFdata
untreated$Dnase<-"No"
untreated$Baseline_FEV<-80
predicted_untreated<-mean(predict(multivariable_model, newdata=untreated))
predicted_untreated

CE_low<-predicted_treated-predicted_untreated
CE_high<-predicted_treated-predicted_untreated
