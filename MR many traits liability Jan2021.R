

##Clean up R

rm(list=ls(all=TRUE))




####################################Install  packages
install.packages("devtools")
library(devtools)

install.packages("tidyr")

install.packages("dplyr")

install.packages("AER")
install.packages("coefplot")

install.packages("broom")
library(broom)

library(devtools)
install_github("WSpiller/RadialMR")

library(devtools)
install_github("qingyuanzhao/mr.raps")

install.packages(c("ggplot2", "ggrepel", "igraph"))

install.packages(scales)

install.packages("rms")

library(devtools)
install_github("MRCIEU/TwoSampleMR")
library(TwoSampleMR)
library(RadialMR)
library(mr.raps)
library(ggplot2)
library(AER)
library(dplyr)
library(rms)

library(tidyverse)

##

getwd()

setwd("...p1/013/working/data/Many traits analysis/")



######################## Initial data summaries ########################




individual.data <- read.csv("neid_pca_excl_pheno_grs.csv")

##Proportion of variance explained by each exposure

non.missing.cost<-sum(!is.na(individual.data$cost_person_year))
non.missing.cost


analysis.individual.data<-subset(individual.data,!is.na(individual.data$cost_person_year))
nrow(analysis.individual.data)

tabulate(analysis.individual.data$sex)



##Proportion of cases and controls in the pheno data


asthma.cases<-table(analysis.individual.data$phe_asthma)
addmargins(asthma.cases)
prop.table(asthma.cases)

eczema.cases<-table(analysis.individual.data$phe_eczema)
addmargins(eczema.cases)
prop.table(eczema.cases)


migraine.cases<-table(analysis.individual.data$phe_migraine)
addmargins(migraine.cases)
prop.table(migraine.cases)

coronary_heart_disease.cases<-table(analysis.individual.data$phe_coronary_heart_disease)
addmargins(coronary_heart_disease.cases)
prop.table(coronary_heart_disease.cases)

type_2_diabetes.cases<-table(analysis.individual.data$phe_type_2_diabetes)
addmargins(type_2_diabetes.cases)
prop.table(type_2_diabetes.cases)

depression.cases<-table(analysis.individual.data$phe_depression)
addmargins(depression.cases)
prop.table(depression.cases)


##Age and sex distribution

summary(analysis.individual.data$sex)
summary(analysis.individual.data$age_at_recruitment)


###Summary Statistics

#Mean age, sex, cost , associations of allele scores with principal components 

non.missing.phe_asthma<-sum(!is.na(analysis.individual.data$phe_asthma))
non.missing.phe_asthma

non.missing.grs_asthma<-sum(!is.na(analysis.individual.data$grs_asthma))
non.missing.grs_asthma


non.missing.phe_eczema<-sum(!is.na(analysis.individual.data$phe_eczema))
non.missing.phe_eczema

non.missing.grs_eczema<-sum(!is.na(analysis.individual.data$grs_eczema))
non.missing.grs_eczema

non.missing.phe_migraine<-sum(!is.na(analysis.individual.data$phe_migraine))
non.missing.phe_migraine

non.missing.grs_migraine<-sum(!is.na(analysis.individual.data$grs_migraine))
non.missing.grs_migraine

non.missing.phe_coronary_heart_disease<-sum(!is.na(analysis.individual.data$phe_coronary_heart_disease))
non.missing.phe_coronary_heart_disease

non.missing.grs_coronary_heart_disease<-sum(!is.na(analysis.individual.data$grs_coronary_heart_disease))
non.missing.grs_coronary_heart_disease

non.missing.phe_type_2_diabetes<-sum(!is.na(analysis.individual.data$phe_type_2_diabetes))
non.missing.phe_type_2_diabetes

non.missing.grs_type_2_diabetes<-sum(!is.na(analysis.individual.data$grs_type_2_diabetes))
non.missing.grs_type_2_diabetes

non.missing.phe_depression<-sum(!is.na(analysis.individual.data$phe_depression))
non.missing.phe_depression

non.missing.grs_depression<-sum(!is.na(analysis.individual.data$grs_depression))
non.missing.grs_depression



prin.comp<-paste("pc",1:40,sep="")
paste(prin.comp, collapse=" + ")


Formula <- formula(paste("grs_asthma ~ ", 
                         paste(prin.comp, collapse=" + ")))
asthma.pc.regression<-lm(Formula, analysis.individual.data)
summary(asthma.pc.regression)

Formula <- formula(paste("grs_eczema ~ ", 
                         paste(prin.comp, collapse=" + ")))
eczema.pc.regression<-lm(Formula, analysis.individual.data)
summary(eczema.pc.regression)

Formula <- formula(paste("grs_migraine ~ ", 
                         paste(prin.comp, collapse=" + ")))
migraine.pc.regression<-lm(Formula, analysis.individual.data)
summary(migraine.pc.regression)

Formula <- formula(paste("grs_coronary_heart_disease ~ ", 
                         paste(prin.comp, collapse=" + ")))
chd.pc.regression<-lm(Formula, analysis.individual.data)
summary(chd.pc.regression)


Formula <- formula(paste("grs_type_2_diabetes ~ ", 
                         paste(prin.comp, collapse=" + ")))
t2d.pc.regression<-lm(Formula, analysis.individual.data)
summary(t2d.pc.regression)

Formula <- formula(paste("grs_depression ~ ", 
                         paste(prin.comp, collapse=" + ")))
depression.pc.regression<-lm(Formula, analysis.individual.data)
summary(depression.pc.regression)

Formula <- formula(paste("grs_depression ~ ", 
                         paste(prin.comp, collapse=" + ")))
t2d.pc.regression<-lm(Formula, analysis.individual.data)
summary(t2d.pc.regression)

######sex and age individually 

asthma.sex.regression<-lm(grs_asthma~sex+age, analysis.individual.data)
summary(asthma.sex.regression)

eczema.sex.regression<-lm(grs_eczema~sex, analysis.individual.data)
summary(eczema.sex.regression)

migraine.sex.regression<-lm(grs_migraine~sex, analysis.individual.data)
summary(migraine.sex.regression)

chd.sex.regression<-lm(grs_coronary_heart_disease~sex, analysis.individual.data)
summary(chd.sex.regression)

t2d.sex.regression<-lm(grs_type_2_diabetes~sex, analysis.individual.data)
summary(t2d.sex.regression)

depress.sex.regression<-lm(grs_depression~sex, analysis.individual.data)
summary(depress.sex.regression)

##


asthma.age_at_recruitment.regression<-lm(grs_asthma~age_at_recruitment, analysis.individual.data)
summary(asthma.age_at_recruitment.regression)

eczema.age_at_recruitment.regression<-lm(grs_eczema~age_at_recruitment, analysis.individual.data)
summary(eczema.age_at_recruitment.regression)

migraine.age_at_recruitment.regression<-lm(grs_migraine~age_at_recruitment, analysis.individual.data)
summary(migraine.age_at_recruitment.regression)

chd.age_at_recruitment.regression<-lm(grs_coronary_heart_disease~age_at_recruitment, analysis.individual.data)
summary(chd.age_at_recruitment.regression)

t2d.age_at_recruitment.regression<-lm(grs_type_2_diabetes~age_at_recruitment, analysis.individual.data)
summary(t2d.age_at_recruitment.regression)

depress.age_at_recruitment.regression<-lm(grs_depression~age_at_recruitment, analysis.individual.data)
summary(depress.age_at_recruitment.regression)



######################## Observational regressions, condition by condition ########################

lm.asthma <- lm(cost_person_year ~ sex + age_at_recruitment + phe_asthma+as.factor(assessment_centre), data = individual.data)
summary(lm.asthma)

#extract data for a forest plot

lm.asthma.beta<-coef(lm.asthma)["phe_asthma"]
lm.asthma.upper<-lm.asthma.beta+1.96*coef(summary(lm.asthma))[4, "Std. Error"]
lm.asthma.lower<-lm.asthma.beta-1.96*coef(summary(lm.asthma))[4, "Std. Error"]


lm.eczema <- lm(cost_person_year ~ sex + age_at_recruitment +phe_eczema+as.factor(assessment_centre), data = individual.data)
summary(lm.eczema)


#extract data for a forest plot

lm.eczema.beta<-coef(lm.eczema)["phe_eczema"]
lm.eczema.upper<-lm.eczema.beta+1.96*coef(summary(lm.eczema))[4, "Std. Error"]
lm.eczema.lower<-lm.eczema.beta-1.96*coef(summary(lm.eczema))[4, "Std. Error"]

lm.migraine <- lm(cost_person_year ~ sex + age_at_recruitment + phe_migraine+as.factor(assessment_centre), data = individual.data)
summary(lm.migraine)

#extract data for a forest plot

lm.migraine.beta<-coef(lm.migraine)["phe_migraine"]
lm.migraine.upper<-lm.migraine.beta+1.96*coef(summary(lm.migraine))[4, "Std. Error"]
lm.migraine.lower<-lm.migraine.beta-1.96*coef(summary(lm.migraine))[4, "Std. Error"]

lm.chd <- lm(cost_person_year ~ sex + age_at_recruitment +phe_coronary_heart_disease+as.factor(assessment_centre),data = individual.data)
summary(lm.chd)

#extract data for a forest plot

lm.chd.beta<-coef(lm.chd)["phe_coronary_heart_disease"]
lm.chd.upper<-lm.chd.beta+1.96*coef(summary(lm.chd))[4, "Std. Error"]
lm.chd.lower<-lm.chd.beta-1.96*coef(summary(lm.chd))[4, "Std. Error"]

lm.t2d <- lm(cost_person_year ~ sex + age_at_recruitment +phe_type_2_diabetes+as.factor(assessment_centre), data = individual.data)
summary(lm.t2d)

#extract data for a forest plot

lm.t2d.beta<-coef(lm.t2d)["phe_type_2_diabetes"]
lm.t2d.upper<-lm.t2d.beta+1.96*coef(summary(lm.t2d))[4, "Std. Error"]
lm.t2d.lower<-lm.t2d.beta-1.96*coef(summary(lm.t2d))[4, "Std. Error"]

lm.depression <- lm(cost_person_year ~ sex + age_at_recruitment +phe_depression+as.factor(assessment_centre), data = individual.data)
summary(lm.depression)

#extract data for a forest plot

lm.depression.beta<-coef(lm.depression)["phe_depression"]
lm.depression.upper<-lm.depression.beta+1.96*coef(summary(lm.depression))[4, "Std. Error"]
lm.depression.lower<-lm.depression.beta-1.96*coef(summary(lm.depression))[4, "Std. Error"]

##Add all coefficients and CI to a data frame for forest plot

obs.results<-data.frame("Condition"=c("Asthma", "Eczema","Migraine", 
                                      "CHD","Type 2 Diabetes","Depression"),
                        "Beta"=c(lm.asthma.beta,lm.eczema.beta,lm.migraine.beta,
                                 lm.chd.beta, lm.t2d.beta, 
                                 lm.depression.beta),
                        "Lower"=c(lm.asthma.lower,lm.eczema.lower,lm.migraine.lower,
                                  lm.chd.lower, lm.t2d.lower, 
                                  lm.depression.lower),
                        "Upper"=c(lm.asthma.upper,lm.eczema.upper,lm.migraine.upper,
                                  lm.chd.upper, lm.t2d.upper, 
                                  lm.depression.beta)
                        
)



######################## GRS ivreg   ########################


#Asthma

grs.ivreg.asthma <- ivreg(cost_person_year ~ sex + age_at_recruitment + as.factor(assessment_centre)+ 
                            pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                            pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                            pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                            pc37 + pc38 + pc39 + pc40+phe_asthma| 
                            grs_asthma +sex+ age_at_recruitment + as.factor(assessment_centre)+ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                            pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                            pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                            pc37 + pc38 + pc39 + pc40,
                          data = analysis.individual.data)

summary(grs.ivreg.asthma)
asthma.beta<-summary(grs.ivreg.asthma)$coefficients["phe_asthma","Estimate"]
asthma.se<-summary(grs.ivreg.asthma)$coefficients["phe_asthma","Std. Error"]



asthma.upper<-asthma.beta+1.96*asthma.se
asthma.lower<-asthma.beta-1.96*asthma.se


#Eczema
grs.ivreg.eczema <- ivreg(cost_person_year ~ sex + age_at_recruitment + as.factor(assessment_centre) +
                            pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                            pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                            pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                            pc37 + pc38 + pc39 + pc40+phe_eczema| 
                            grs_eczema +sex+ age_at_recruitment + as.factor(assessment_centre) +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                            pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                            pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                            pc37 + pc38 + pc39 + pc40,
                          data = individual.data)

summary(grs.ivreg.eczema)
eczema.beta<-summary(grs.ivreg.eczema)$coefficients["phe_eczema","Estimate"]
eczema.se<-summary(grs.ivreg.eczema)$coefficients["phe_eczema","Std. Error"]


eczema.upper<-eczema.beta+1.96*eczema.se
eczema.lower<-eczema.beta-1.96*eczema.se

#Migraine

grs.ivreg.migraine <- ivreg(cost_person_year ~ sex + age_at_recruitment +  as.factor(assessment_centre) +
                              pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                              pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                              pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                              pc37 + pc38 + pc39 + pc40+phe_migraine| 
                              grs_migraine +sex+ age_at_recruitment  + as.factor(assessment_centre) + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                              pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                              pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                              pc37 + pc38 + pc39 + pc40,
                            data = individual.data)

summary(grs.ivreg.migraine)
migraine.beta<-summary(grs.ivreg.migraine)$coefficients["phe_migraine","Estimate"]
migraine.se<-summary(grs.ivreg.migraine)$coefficients["phe_migraine","Std. Error"]


migraine.upper<-migraine.beta+1.96*migraine.se
migraine.lower<-migraine.beta-1.96*migraine.se

##CHD

grs.ivreg.coronary_heart_disease <- ivreg(cost_person_year ~ sex + age_at_recruitment + as.factor(assessment_centre) +
                                            pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                            pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                            pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                            pc37 + pc38 + pc39 + pc40+phe_coronary_heart_disease| 
                                            grs_coronary_heart_disease +sex+ age_at_recruitment + as.factor(assessment_centre) +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                            pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                            pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                            pc37 + pc38 + pc39 + pc40,
                                          data = individual.data)

summary(grs.ivreg.coronary_heart_disease)
coronary_heart_disease.beta<-summary(grs.ivreg.coronary_heart_disease)$coefficients["phe_coronary_heart_disease","Estimate"]
coronary_heart_disease.se<-summary(grs.ivreg.coronary_heart_disease)$coefficients["phe_coronary_heart_disease","Std. Error"]


coronary_heart_disease.upper<-coronary_heart_disease.beta+1.96*coronary_heart_disease.se
coronary_heart_disease.lower<-coronary_heart_disease.beta-1.96*coronary_heart_disease.se

##Type 2 diabetes

grs.ivreg.type_2_diabetes <- ivreg(cost_person_year ~ sex + age_at_recruitment + as.factor(assessment_centre) + 
                                     pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                     pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                     pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                     pc37 + pc38 + pc39 + pc40+phe_type_2_diabetes| 
                                     grs_type_2_diabetes +sex+ age_at_recruitment + as.factor(assessment_centre) +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                     pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                     pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                     pc37 + pc38 + pc39 + pc40,
                                   data = individual.data)

summary(grs.ivreg.type_2_diabetes, diagnostics = TRUE)
type_2_diabetes.beta<-summary(grs.ivreg.type_2_diabetes)$coefficients["phe_type_2_diabetes","Estimate"]
type_2_diabetes.se<-summary(grs.ivreg.type_2_diabetes)$coefficients["phe_type_2_diabetes","Std. Error"]



type_2_diabetes.upper<-type_2_diabetes.beta+1.96*type_2_diabetes.se
type_2_diabetes.lower<-type_2_diabetes.beta-1.96*type_2_diabetes.se

#Depression


grs.ivreg.depression <- ivreg(cost_person_year ~ sex + age_at_recruitment + as.factor(assessment_centre) + 
                                pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                pc37 + pc38 + pc39 + pc40+phe_depression| 
                                grs_depression +sex+ age_at_recruitment + as.factor(assessment_centre) +  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                pc37 + pc38 + pc39 + pc40,
                              data = individual.data)

summary(grs.ivreg.depression)

depression.beta<-summary(grs.ivreg.depression)$coefficients["phe_depression","Estimate"]
depression.se<-summary(grs.ivreg.depression)$coefficients["phe_depression","Std. Error"]


depression.upper<-depression.beta+1.96*depression.se
depression.lower<-depression.beta-1.96*depression.se



summary(grs.ivreg.asthma, diagnostics = TRUE)
summary(grs.ivreg.coronary_heart_disease, diagnostics = TRUE)
summary(grs.ivreg.depression, diagnostics = TRUE)
summary(grs.ivreg.eczema, diagnostics = TRUE)
summary(grs.ivreg.migraine, diagnostics = TRUE)
summary(grs.ivreg.type_2_diabetes, diagnostics = TRUE)





########Sex stratified GRS analysis
####First, create substs of the main data by sex

male.analysis.individual.data<-subset(analysis.individual.data,sex=="Male")
female.analysis.individual.data<-subset(analysis.individual.data,sex=="Female")




male.grs.ivreg.asthma <- ivreg(cost_person_year ~  age_at_recruitment + as.factor(assessment_centre)+ 
                                 pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                 pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                 pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                 pc37 + pc38 + pc39 + pc40+phe_asthma| 
                                 grs_asthma + age_at_recruitment + as.factor(assessment_centre)+ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                 pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                 pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                 pc37 + pc38 + pc39 + pc40,
                               data = male.analysis.individual.data)

summary(male.grs.ivreg.asthma)
male.asthma.beta<-summary(male.grs.ivreg.asthma)$coefficients["phe_asthma","Estimate"]
male.asthma.se<-summary(male.grs.ivreg.asthma)$coefficients["phe_asthma","Std. Error"]



male.asthma.upper<-male.asthma.beta+1.96*male.asthma.se
male.asthma.lower<-male.asthma.beta-1.96*male.asthma.se




female.grs.ivreg.asthma <- ivreg(cost_person_year ~  age_at_recruitment + as.factor(assessment_centre)+ 
                                   pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                   pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                   pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                   pc37 + pc38 + pc39 + pc40+phe_asthma| 
                                   grs_asthma + age_at_recruitment + as.factor(assessment_centre)+ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                   pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                   pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                   pc37 + pc38 + pc39 + pc40,
                                 data = female.analysis.individual.data)

summary(female.grs.ivreg.asthma)
female.asthma.beta<-summary(female.grs.ivreg.asthma)$coefficients["phe_asthma","Estimate"]
female.asthma.se<-summary(female.grs.ivreg.asthma)$coefficients["phe_asthma","Std. Error"]



female.asthma.upper<-female.asthma.beta+1.96*female.asthma.se
female.asthma.lower<-female.asthma.beta-1.96*female.asthma.se

#####Eczema

male.grs.ivreg.eczema <- ivreg(cost_person_year ~  age_at_recruitment + as.factor(assessment_centre)+ 
                                 pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                 pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                 pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                 pc37 + pc38 + pc39 + pc40+phe_eczema| 
                                 grs_eczema + age_at_recruitment + as.factor(assessment_centre)+ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                 pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                 pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                 pc37 + pc38 + pc39 + pc40,
                               data = male.analysis.individual.data)

summary(male.grs.ivreg.eczema)
male.eczema.beta<-summary(male.grs.ivreg.eczema)$coefficients["phe_eczema","Estimate"]
male.eczema.se<-summary(male.grs.ivreg.eczema)$coefficients["phe_eczema","Std. Error"]



male.eczema.upper<-male.eczema.beta+1.96*male.eczema.se
male.eczema.lower<-male.eczema.beta-1.96*male.eczema.se




female.grs.ivreg.eczema <- ivreg(cost_person_year ~  age_at_recruitment + as.factor(assessment_centre)+ 
                                   pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                   pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                   pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                   pc37 + pc38 + pc39 + pc40+phe_eczema| 
                                   grs_eczema + age_at_recruitment + as.factor(assessment_centre)+ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                   pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                   pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                   pc37 + pc38 + pc39 + pc40,
                                 data = female.analysis.individual.data)

summary(female.grs.ivreg.eczema)
female.eczema.beta<-summary(female.grs.ivreg.eczema)$coefficients["phe_eczema","Estimate"]
female.eczema.se<-summary(female.grs.ivreg.eczema)$coefficients["phe_eczema","Std. Error"]



female.eczema.upper<-female.eczema.beta+1.96*female.eczema.se
female.eczema.lower<-female.eczema.beta-1.96*female.eczema.se

###Migraine

male.grs.ivreg.migraine <- ivreg(cost_person_year ~  age_at_recruitment + as.factor(assessment_centre)+ 
                                   pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                   pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                   pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                   pc37 + pc38 + pc39 + pc40+phe_migraine| 
                                   grs_migraine + age_at_recruitment + as.factor(assessment_centre)+ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                   pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                   pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                   pc37 + pc38 + pc39 + pc40,
                                 data = male.analysis.individual.data)

summary(male.grs.ivreg.migraine)
male.migraine.beta<-summary(male.grs.ivreg.migraine)$coefficients["phe_migraine","Estimate"]
male.migraine.se<-summary(male.grs.ivreg.migraine)$coefficients["phe_migraine","Std. Error"]



male.migraine.upper<-male.migraine.beta+1.96*male.migraine.se
male.migraine.lower<-male.migraine.beta-1.96*male.migraine.se




female.grs.ivreg.migraine <- ivreg(cost_person_year ~  age_at_recruitment + as.factor(assessment_centre)+ 
                                     pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                     pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                     pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                     pc37 + pc38 + pc39 + pc40+phe_migraine| 
                                     grs_migraine + age_at_recruitment + as.factor(assessment_centre)+ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                     pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                     pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                     pc37 + pc38 + pc39 + pc40,
                                   data = female.analysis.individual.data)

summary(female.grs.ivreg.migraine)
female.migraine.beta<-summary(female.grs.ivreg.migraine)$coefficients["phe_migraine","Estimate"]
female.migraine.se<-summary(female.grs.ivreg.migraine)$coefficients["phe_migraine","Std. Error"]



female.migraine.upper<-female.migraine.beta+1.96*female.migraine.se
female.migraine.lower<-female.migraine.beta-1.96*female.migraine.se


##CHD

male.grs.ivreg.coronary_heart_disease <- ivreg(cost_person_year ~  age_at_recruitment + as.factor(assessment_centre)+ 
                                                 pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                                 pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                                 pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                                 pc37 + pc38 + pc39 + pc40+phe_coronary_heart_disease| 
                                                 grs_coronary_heart_disease + age_at_recruitment + as.factor(assessment_centre)+ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                                 pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                                 pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                                 pc37 + pc38 + pc39 + pc40,
                                               data = male.analysis.individual.data)

summary(male.grs.ivreg.coronary_heart_disease)
male.coronary_heart_disease.beta<-summary(male.grs.ivreg.coronary_heart_disease)$coefficients["phe_coronary_heart_disease","Estimate"]
male.coronary_heart_disease.se<-summary(male.grs.ivreg.coronary_heart_disease)$coefficients["phe_coronary_heart_disease","Std. Error"]



male.coronary_heart_disease.upper<-male.coronary_heart_disease.beta+1.96*male.coronary_heart_disease.se
male.coronary_heart_disease.lower<-male.coronary_heart_disease.beta-1.96*male.coronary_heart_disease.se




female.grs.ivreg.coronary_heart_disease <- ivreg(cost_person_year ~  age_at_recruitment + as.factor(assessment_centre)+ 
                                                   pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                                   pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                                   pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                                   pc37 + pc38 + pc39 + pc40+phe_coronary_heart_disease| 
                                                   grs_coronary_heart_disease + age_at_recruitment + as.factor(assessment_centre)+ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                                   pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                                   pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                                   pc37 + pc38 + pc39 + pc40,
                                                 data = female.analysis.individual.data)

summary(female.grs.ivreg.coronary_heart_disease)
female.coronary_heart_disease.beta<-summary(female.grs.ivreg.coronary_heart_disease)$coefficients["phe_coronary_heart_disease","Estimate"]
female.coronary_heart_disease.se<-summary(female.grs.ivreg.coronary_heart_disease)$coefficients["phe_coronary_heart_disease","Std. Error"]



female.coronary_heart_disease.upper<-female.coronary_heart_disease.beta+1.96*female.coronary_heart_disease.se
female.coronary_heart_disease.lower<-female.coronary_heart_disease.beta-1.96*female.coronary_heart_disease.se


##Type 2 diabetes

male.grs.ivreg.type_2_diabetes <- ivreg(cost_person_year ~  age_at_recruitment + as.factor(assessment_centre)+ 
                                          pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                          pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                          pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                          pc37 + pc38 + pc39 + pc40+phe_type_2_diabetes| 
                                          grs_type_2_diabetes + age_at_recruitment + as.factor(assessment_centre)+ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                          pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                          pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                          pc37 + pc38 + pc39 + pc40,
                                        data = male.analysis.individual.data)

summary(male.grs.ivreg.type_2_diabetes)
male.type_2_diabetes.beta<-summary(male.grs.ivreg.type_2_diabetes)$coefficients["phe_type_2_diabetes","Estimate"]
male.type_2_diabetes.se<-summary(male.grs.ivreg.type_2_diabetes)$coefficients["phe_type_2_diabetes","Std. Error"]



male.type_2_diabetes.upper<-male.type_2_diabetes.beta+1.96*male.type_2_diabetes.se
male.type_2_diabetes.lower<-male.type_2_diabetes.beta-1.96*male.type_2_diabetes.se




female.grs.ivreg.type_2_diabetes <- ivreg(cost_person_year ~  age_at_recruitment + as.factor(assessment_centre)+ 
                                            pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                            pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                            pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                            pc37 + pc38 + pc39 + pc40+phe_type_2_diabetes| 
                                            grs_type_2_diabetes + age_at_recruitment + as.factor(assessment_centre)+ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                            pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                            pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                            pc37 + pc38 + pc39 + pc40,
                                          data = female.analysis.individual.data)

summary(female.grs.ivreg.type_2_diabetes)
female.type_2_diabetes.beta<-summary(female.grs.ivreg.type_2_diabetes)$coefficients["phe_type_2_diabetes","Estimate"]
female.type_2_diabetes.se<-summary(female.grs.ivreg.type_2_diabetes)$coefficients["phe_type_2_diabetes","Std. Error"]



female.type_2_diabetes.upper<-female.type_2_diabetes.beta+1.96*female.type_2_diabetes.se
female.type_2_diabetes.lower<-female.type_2_diabetes.beta-1.96*female.type_2_diabetes.se


##Depression

male.grs.ivreg.depression <- ivreg(cost_person_year ~  age_at_recruitment + as.factor(assessment_centre)+ 
                                     pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                     pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                     pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                     pc37 + pc38 + pc39 + pc40+phe_depression| 
                                     grs_depression + age_at_recruitment + as.factor(assessment_centre)+ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                     pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                     pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                     pc37 + pc38 + pc39 + pc40,
                                   data = male.analysis.individual.data)

summary(male.grs.ivreg.depression)
male.depression.beta<-summary(male.grs.ivreg.depression)$coefficients["phe_depression","Estimate"]
male.depression.se<-summary(male.grs.ivreg.depression)$coefficients["phe_depression","Std. Error"]



male.depression.upper<-male.depression.beta+1.96*male.depression.se
male.depression.lower<-male.depression.beta-1.96*male.depression.se




female.grs.ivreg.depression <- ivreg(cost_person_year ~  age_at_recruitment + as.factor(assessment_centre)+ 
                                       pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                       pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                       pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                       pc37 + pc38 + pc39 + pc40+phe_depression| 
                                       grs_depression + age_at_recruitment + as.factor(assessment_centre)+ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + 
                                       pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + pc21 + pc22 + pc23 + pc24 + 
                                       pc25 + pc26 + pc27 + pc28 + pc29 + pc30 + pc31 + pc32 + pc33 + pc34 + pc35 + pc36 + 
                                       pc37 + pc38 + pc39 + pc40,
                                     data = female.analysis.individual.data)

summary(female.grs.ivreg.depression)
female.depression.beta<-summary(female.grs.ivreg.depression)$coefficients["phe_depression","Estimate"]
female.depression.se<-summary(female.grs.ivreg.depression)$coefficients["phe_depression","Std. Error"]



female.depression.upper<-female.depression.beta+1.96*female.depression.se
female.depression.lower<-female.depression.beta-1.96*female.depression.se


##################



data = read.csv("Sex stratified results 190520.csv",stringsAsFactors = FALSE)

tabletext = as.vector(unique(data[[1]]))

#Comment 19 May - code not working - 4 boxes when three required...
#Check for extraneous boxes or where code not working because need 6 not four

#Update xticks

height = 10
width = 8
xlab_size = 1
xtick_size = 1
label_size = 1
#hrzl_lines = list("1"=gpar(lty=1),"2"=gpar(lty=2),"3"=gpar(lty=2),"4"=gpar(lty=1))
xlab = "Cost (£) per person per year"


tiff(file="Asthma Results.tiff", res=600, width = width, height = height,units = "in",compression="lzw") 

forestplot( title="Asthma",
            tabletext, 
            mean =data$beta[data$type == "Asthma"], 
            lower=data$lower[data$type == "Asthma"], 
            upper = data$upper[data$type == "Asthma"], 
            
            #clip = xticks,
            
            boxsize = 0.05,
            line.margin = 0.2,
            
            #hrzl_lines=hrzl_lines,
            #txt_gp = fpTxtGp(xlab=gpar(cex=xlab_size),
            #ticks = gpar(cex=xtick_size),
            #label = gpar(cex=label_size)),
            
            xticks=seq(-350,350,by=50),
            grid = structure(seq(-350,350,by=50), 
                             gp = gpar(lty = 2, col = "#CCCCFF")),
            lwd.zero=5,
            col=fpColors(box="black", lines="black",
                         zero = "lightgrey"),
            xlab = xlab,
            #xticks=xticks
)
dev.off()

#Eczema

tiff(file="Eczema Results.tiff", res=600, width = width, height = height,units = "in",compression="lzw") 


forestplot( title="Eczema",
            tabletext, 
            mean =data$beta[data$type == "Eczema"], 
            lower=data$lower[data$type == "Eczema"], 
            upper = data$upper[data$type == "Eczema"], 
            
            #clip = xticks,
            
            boxsize = 0.05,
            line.margin = 0.2,
            
            #hrzl_lines=hrzl_lines,
            #txt_gp = fpTxtGp(xlab=gpar(cex=xlab_size),
            #ticks = gpar(cex=xtick_size),
            #label = gpar(cex=label_size)),
            
            xticks=seq(-1200,3000,by=200),
            grid = structure(seq(-1200,3000,by=200), 
                             gp = gpar(lty = 2, col = "#CCCCFF")),
            lwd.zero=5,
            col=fpColors(box="black", lines="black",
                         zero = "lightgrey"),
            xlab = xlab,
            #xticks=xticks
)
dev.off()

#Migraine


tiff(file="Migraine Results.tiff", res=600, width = width, height = height,units = "in",compression="lzw") 


forestplot(title="Migraine",
           tabletext, 
           mean =data$beta[data$type == "Migraine"], 
           lower=data$lower[data$type == "Migraine"], 
           upper = data$upper[data$type == "Migraine"], 
           
           #clip = xticks,
           
           boxsize = 0.05,
           line.margin = 0.2,
           
           #hrzl_lines=hrzl_lines,
           #txt_gp = fpTxtGp(xlab=gpar(cex=xlab_size),
           #ticks = gpar(cex=xtick_size),
           #label = gpar(cex=label_size)),
           
           xticks=seq(-1200,3200,by=400),
           grid = structure(seq(-1200,3200,by=200), 
                            gp = gpar(lty = 2, col = "#CCCCFF")),
           lwd.zero=5,
           col=fpColors(box="black", lines="black",
                        zero = "lightgrey"),
           xlab = xlab,
           #xticks=xticks
)
dev.off()


##CHD

tiff(file="CHD Results.tiff", res=600, width = width, height = height,units = "in",compression="lzw") 

forestplot(title="Coronary heart disease",
           tabletext, 
           mean =data$beta[data$type == "CHD"], 
           lower=data$lower[data$type == "CHD"], 
           upper = data$upper[data$type == "CHD"], 
           
           #clip = xticks,
           
           boxsize = 0.05,
           line.margin = 0.2,
           
           #hrzl_lines=hrzl_lines,
           #txt_gp = fpTxtGp(xlab=gpar(cex=xlab_size),
           #ticks = gpar(cex=xtick_size),
           #label = gpar(cex=label_size)),
           
           xticks=seq(-800,2300,by=400),
           grid = structure(seq(-800,2300,by=200), 
                            gp = gpar(lty = 2, col = "#CCCCFF")),
           lwd.zero=5,
           col=fpColors(box="black", lines="black",
                        zero = "lightgrey"),
           xlab = xlab,
           #xticks=xticks
)
dev.off()

#T2D

tiff(file="T2D Results.tiff", res=600, width = width, height = height,units = "in",compression="lzw") 


forestplot(title="Type 2 diabetes",
           tabletext, 
           mean =data$beta[data$type == "Type 2 diabetes"], 
           lower=data$lower[data$type == "Type 2 diabetes"], 
           upper = data$upper[data$type == "Type 2 diabetes"], 
           
           #clip = xticks,
           
           boxsize = 0.05,
           line.margin = 0.2,
           
           #hrzl_lines=hrzl_lines,
           #txt_gp = fpTxtGp(xlab=gpar(cex=xlab_size),
           #ticks = gpar(cex=xtick_size),
           #label = gpar(cex=label_size)),
           
           xticks=seq(-800,800,by=400),
           grid = structure(seq(-800,800,by=200), 
                            gp = gpar(lty = 2, col = "#CCCCFF")),
           lwd.zero=5,
           col=fpColors(box="black", lines="black",
                        zero = "lightgrey"),
           xlab = xlab,
           #xticks=xticks
)
dev.off()

#Depression

tiff(file="Depression Results.tiff", res=600, width = width, height = height,units = "in",compression="lzw") 



forestplot(title="Depression",
           tabletext, 
           mean =data$beta[data$type =="Depression "], 
           lower=data$lower[data$type =="Depression "], 
           upper = data$upper[data$type =="Depression "], 
           
           
           
           #clip = xticks,
           
           boxsize = 0.05,
           line.margin = 0.2,
           
           #hrzl_lines=hrzl_lines,
           #txt_gp = fpTxtGp(xlab=gpar(cex=xlab_size),
           #ticks = gpar(cex=xtick_size),
           #label = gpar(cex=label_size)),
           
           xticks=seq(-1600,800,by=400),
           grid = structure(seq(-1600,800,by=200), 
                            gp = gpar(lty = 2, col = "#CCCCFF")),
           lwd.zero=5,
           col=fpColors(box="black", lines="black",
                        zero = "lightgrey"),
           xlab = xlab,
           #xticks=xticks
)

dev.off()

##########################################################################

####################### SUMMARY METHODS ##################################

##########################################################################


######################## summary two-sample Mendelian Randomization, condition by condition ########################

######################## Asthma ########################


###################################

#Variant level analysis

###################################

##First, read in my file name with the non-standard headings

exp_dat<-read_exposure_data("...p1/013/working/data/Many traits analysis/combined_cost_1_betas.csv",
                            sep = ",",
                            snp_col = "snp",
                            beta_col = "betaexposure",
                            se_col = "seexposure",
                            effect_allele_col = "effect_alleleexposure",
                            other_allele_col = "other_alleleexposure",
                            eaf_col = "eafexposure",
                            pval_col = "pvalexposure",
                            units_col = "unitsexposure",
                            #gene_col = "Gene",
                            samplesize_col = "samplesizeexposure"
                            
)

##Clump this file - 

exp_dat <- clump_data(exp_dat)




cost_out_dat<-read_outcome_data("...p1/013/working/data/Many traits analysis/combined_cost_1_betas.csv",
                                snps = exp_dat$SNP,
                                sep = ",",
                                snp_col = "snp",
                                beta_col = "beta_1_outcome",
                                se_col = "se_1_outcome",
                                effect_allele_col = "effect_alleleexposure",
                                other_allele_col = "other_alleleexposure",
                                eaf_col = "eafexposure",
                                pval_col = "p",
                                units_col = "unit_output",
                                #gene_col = "Gene",
                                samplesize_col = "samplesize_out"
)


##Harmonise data 

dat <- harmonise_data(
  exposure_dat = exp_dat, 
  outcome_dat = cost_out_dat
)







asthma.res.summary <- mr(dat, method_list=c("mr_ivw", "mr_egger_regression","mr_penalised_weighted_median","mr_weighted_mode"))
asthma.resegger <- mr(dat, method_list=c("mr_ivw", "mr_egger_regression"))
asthma.resmedian <- mr(dat, method_list=c("mr_ivw","mr_simple_median", "mr_weighted_median", "mr_penalised_weighted_median"))	
asthma.resmode <- mr(dat, method_list=c("mr_ivw", "mr_simple_mode", "mr_weighted_mode"))			
asthma.resraps <- mr(dat, method_list = c("mr_ivw","mr_raps"), parameters = list(over.dispersion = FALSE, loss.function = "l2"))
asthma.resivw <- mr(dat, method_list = "mr_ivw")





##Steiger test for directionality 

out <- directionality_test(dat)


# Heterogeneity and Egger intercept
asthma.mr_het <- mr_heterogeneity(dat)
asthma.mr_het
asthma.mr_egger_int <- mr_pleiotropy_test(dat)
asthma.mr_egger_int




# single SNP analyses
asthma.res_single <- mr_singlesnp(dat, all_method=c("mr_ivw", "mr_egger_regression", "mr_penalised_weighted_median", "mr_weighted_mode"))
asthma.res_single

# leave one out analyses
asthma.res_loo <- mr_leaveoneout(dat)
asthma.res_loo

#Asthma radial analysis
asthma_radial<-format_radial(dat[,6],dat[,7],dat[,21],dat[,14],dat[,1])

#Then run analysis

asthma.radial<-ivw_radial(asthma_radial,0.05,1,0.0001) ##default parameters
asthma.radial

#One outlying SNP: rs1837253




# re-run analysis without outlying SNPs



asthma.radial.dat<- subset(dat, SNP!="rs1837253")
asthma.res.radial <- mr(t2d.radial.dat, method_list=c("mr_ivw", "mr_egger_regression","mr_penalised_weighted_median","mr_weighted_mode"))
asthma.res.radial

#######GRAPHICS 

##Plain scatter 


#Scatter plot

asthma.main.scatter<- mr_scatter_plot(asthma.res.summary, dat)
asthma.main.scatter
ggsave(asthma.main.scatter[[1]], file="main_scatter.png", width=7, height=7)

asthma.ivw.scatter.only<-mr_scatter_plot(asthma.resivw,dat)
asthma.ivw.scatter.only
ggsave(asthma.ivw.scatter.only[[1]], file="ivw_scatter.png", width=7, height=7)



#FOREST PLOT 


asthma.res_single <- mr_singlesnp(dat)
asthma.forest.plot <- mr_forest_plot(asthma.res_single)
asthma.forest.plot[[1]]
ggsave(asthma.forest.plot[[1]], file="asthmaforest.png", width=7, height=7)




#10% genetic liability change


asthma.res_single.10<-data.frame(asthma.res_single)
asthma.res_single.10$b<-asthma.res_single.10$b*scale.10
asthma.res_single.10$se<-asthma.res_single.10$se*scale.10

asthma.forest.plot.10 <- mr_forest_plot(asthma.res_single.10)
asthma.forest.plot.10[[1]]
ggsave(asthma.forest.plot.10[[1]], file="asthmaforest10.png", width=7, height=7)


######################## CHD ########################


######################## CHD ########################


##First, read in my file name with the non-standard headings

exp_dat<-read_exposure_data("...p1/013/working/data/Many traits analysis/combined_cost_5_betas.csv",
                            sep = ",",
                            snp_col = "snp",
                            beta_col = "betaexposure",
                            se_col = "seexposure",
                            effect_allele_col = "effect_alleleexposure",
                            other_allele_col = "other_alleleexposure",
                            eaf_col = "eafexposure",
                            pval_col = "pvalexposure",
                            units_col = "unitsexposure",
                            #gene_col = "Gene",
                            samplesize_col = "samplesizeexposure"
)

##Clump this file - 

exp_dat <- clump_data(exp_dat)




cost_out_dat<-read_outcome_data("...p1/013/working/data/Many traits analysis/combined_cost_5_betas.csv",
                                snps = exp_dat$SNP,
                                sep = ",",
                                snp_col = "snp",
                                beta_col = "beta_5_outcome",
                                se_col = "se_5_outcome",
                                effect_allele_col = "effect_alleleexposure",
                                other_allele_col = "other_alleleexposure",
                                eaf_col = "eafexposure",
                                pval_col = "p",
                                units_col = "unit_output",
                                #gene_col = "Gene",
                                samplesize_col = "samplesize_out"
)


##Harmonise data 

dat <- harmonise_data(
  exposure_dat = exp_dat, 
  outcome_dat = cost_out_dat
)







chd.res.summary <- mr(dat, method_list=c("mr_ivw", "mr_egger_regression","mr_penalised_weighted_median","mr_weighted_mode"))
chd.resegger <- mr(dat, method_list=c("mr_ivw", "mr_egger_regression"))
chd.resmedian <- mr(dat, method_list=c("mr_ivw","mr_simple_median", "mr_weighted_median", "mr_penalised_weighted_median"))	
chd.resmode <- mr(dat, method_list=c("mr_ivw", "mr_simple_mode", "mr_weighted_mode"))			
chd.resraps <- mr(dat, method_list = c("mr_ivw","mr_raps"), parameters = list(over.dispersion = FALSE, loss.function = "l2"))
chd.resivw <- mr(dat, method_list = "mr_ivw")





##Steiger test for directionality

out <- directionality_test(dat)


# Heterogeneity and Egger intercept
chd.mr_het <- mr_heterogeneity(dat)
chd.mr_het
chd.mr_egger_int <- mr_pleiotropy_test(dat)
chd.mr_egger_int




# single SNP analyses
chd.res_single <- mr_singlesnp(dat, all_method=c("mr_ivw", "mr_egger_regression", "mr_penalised_weighted_median", "mr_weighted_mode"))
chd.res_single

# leave one out analyses
chd.res_loo <- mr_leaveoneout(dat)
chd.res_loo


#RADIAL MR

#First, convert data to radial format
chd_radial<-format_radial(dat[,6],dat[,7],dat[,21],dat[,14],dat[,1])

#Then run analysis

chd.radial<-ivw_radial(chd_radial,0.05,1,0.0001) ##default parameters
chd.radial

#rs12190287 

# re-run analysis without outlying SNPs

chd.radial.dat<- subset(dat, SNP!="rs12190287" & SNP!="rs2351524")
chd.res.radial <- mr(chd.radial.dat, method_list=c("mr_ivw", "mr_egger_regression","mr_penalised_weighted_median","mr_weighted_mode"))
chd.res.radial




#######GRAPHICS 

##Plain scatter 


#Scatter plot

chd.main.scatter<- mr_scatter_plot(chd.res.summary, dat)
chd.main.scatter
ggsave(chd.main.scatter[[1]], file="main_scatter.png", width=7, height=7)

chd.ivw.scatter.only<-mr_scatter_plot(chd.resivw,dat)
chd.ivw.scatter.only
ggsave(chd.ivw.scatter.only[[1]], file="ivw_scatter.png", width=7, height=7)

#FOREST PLOT 


chd.res_single <- mr_singlesnp(dat)
chd.forest.plot <- mr_forest_plot(chd.res_single)
chd.forest.plot[[1]]
ggsave(chd.forest.plot[[1]], file="chdforest.png", width=7, height=7)


##10% liability change


chd.res_single.10<-data.frame(chd.res_single)
chd.res_single.10$b<-chd.res_single.10$b*scale.10
chd.res_single.10$se<-chd.res_single.10$se*scale.10

chd.forest.plot.10 <- mr_forest_plot(chd.res_single.10)
chd.forest.plot.10[[1]]
ggsave(chd.forest.plot.10[[1]], file="chdforest10.png", width=7, height=7)


######################## Eczema ########################


######################## Eczema ########################


##First, read in my file name with the non-standard headings

exp_dat<-read_exposure_data("...p1/013/working/data/Many traits analysis/combined_cost_7_betas.csv",
                            sep = ",",
                            snp_col = "snp",
                            beta_col = "betaexposure",
                            se_col = "seexposure",
                            effect_allele_col = "effect_alleleexposure",
                            other_allele_col = "other_alleleexposure",
                            eaf_col = "eafexposure",
                            pval_col = "pvalexposure",
                            units_col = "unitsexposure",
                            #gene_col = "Gene",
                            samplesize_col = "samplesizeexposure"
)

##Clump this file - 

exp_dat <- clump_data(exp_dat)





cost_out_dat<-read_outcome_data("...p1/013/working/data/Many traits analysis/combined_cost_7_betas.csv",
                                snps = exp_dat$SNP,
                                sep = ",",
                                snp_col = "snp",
                                beta_col = "beta_7_outcome",
                                se_col = "se_7_outcome",
                                effect_allele_col = "effect_alleleexposure",
                                other_allele_col = "other_alleleexposure",
                                eaf_col = "eafexposure",
                                pval_col = "p",
                                units_col = "unit_output",
                                #gene_col = "Gene",
                                samplesize_col = "samplesize_out"
)


##Harmonise data 

dat <- harmonise_data(
  exposure_dat = exp_dat, 
  outcome_dat = cost_out_dat
)







eczema.res.summary <- mr(dat, method_list=c("mr_ivw", "mr_egger_regression","mr_penalised_weighted_median","mr_weighted_mode"))
eczema.resegger <- mr(dat, method_list=c("mr_ivw", "mr_egger_regression"))
eczema.resmedian <- mr(dat, method_list=c("mr_ivw","mr_simple_median", "mr_weighted_median", "mr_penalised_weighted_median"))	
eczema.resmode <- mr(dat, method_list=c("mr_ivw", "mr_simple_mode", "mr_weighted_mode"))			
eczema.resraps <- mr(dat, method_list = c("mr_ivw","mr_raps"), parameters = list(over.dispersion = FALSE, loss.function = "l2"))
eczema.resivw <- mr(dat, method_list = "mr_ivw")





##Steiger test for directionality  

out <- directionality_test(dat)


# Heterogeneity and Egger intercept
eczema.mr_het <- mr_heterogeneity(dat)
eczema.mr_het
eczema.mr_egger_int <- mr_pleiotropy_test(dat)
eczema.mr_egger_int




# single SNP analyses
eczema.res_single <- mr_singlesnp(dat, all_method=c("mr_ivw", "mr_egger_regression", "mr_penalised_weighted_median", "mr_weighted_mode"))
eczema.res_single

# leave one out analyses
eczema.res_loo <- mr_leaveoneout(dat)
eczema.res_loo




#RADIAL MR

#First, convert data to radial format
eczema_radial<-format_radial(dat[,6],dat[,7],dat[,21],dat[,14],dat[,1])

#Then run analysis

eczema.radial<-ivw_radial(eczema_radial,0.05,1,0.0001) ##default parameters
eczema.radial


# re-run analysis without outlying SNPs

eczema.radial.dat<- subset(dat, SNP!="rs10790275" & SNP!=" rs8066625")
eczema.res.radial <- mr(eczema.radial.dat, method_list=c("mr_ivw", "mr_egger_regression","mr_penalised_weighted_median","mr_weighted_mode"))
eczema.res.radial



#######GRAPHICS 

##Plain scatter 


#Scatter plot

eczema.main.scatter<- mr_scatter_plot(eczema.res.summary, dat)
eczema.main.scatter
ggsave(eczema.main.scatter[[1]], file="main_scatter.png", width=7, height=7)

eczema.ivw.scatter.only<-mr_scatter_plot(eczema.resivw,dat)
eczema.ivw.scatter.only
ggsave(eczema.ivw.scatter.only[[1]], file="ivw_scatter.png", width=7, height=7)

#FOREST PLOT 


eczema.res_single <- mr_singlesnp(dat)
eczema.forest.plot <- mr_forest_plot(eczema.res_single)
eczema.forest.plot[[1]]
ggsave(eczema.forest.plot[[1]], file="eczemaforest.png", width=7, height=7)


#10% liability change

eczema.res_single.10<-data.frame(eczema.res_single)
eczema.res_single.10$b<-eczema.res_single.10$b*scale.10
eczema.res_single.10$se<-eczema.res_single.10$se*scale.10

eczema.forest.plot.10 <- mr_forest_plot(eczema.res_single.10)
eczema.forest.plot.10[[1]]
ggsave(eczema.forest.plot.10[[1]], file="eczemaforest10.png", width=7, height=7)



######################## Migraine ########################


######################## Migraine ########################


##First, read in my file name with the non-standard headings

exp_dat<-read_exposure_data("...p1/013/working/data/Many traits analysis/combined_cost_8_betas.csv",
                            sep = ",",
                            snp_col = "snp",
                            beta_col = "betaexposure",
                            se_col = "seexposure",
                            effect_allele_col = "effect_alleleexposure",
                            other_allele_col = "other_alleleexposure",
                            eaf_col = "eafexposure",
                            pval_col = "pvalexposure",
                            units_col = "unitsexposure",
                            #gene_col = "Gene",
                            samplesize_col = "samplesizeexposure"
)

##Clump this file - 

exp_dat <- clump_data(exp_dat)




cost_out_dat<-read_outcome_data("...p1/013/working/data/Many traits analysis/combined_cost_8_betas.csv",
                                snps = exp_dat$SNP,
                                sep = ",",
                                snp_col = "snp",
                                beta_col = "beta_8_outcome",
                                se_col = "se_8_outcome",
                                effect_allele_col = "effect_alleleexposure",
                                other_allele_col = "other_alleleexposure",
                                eaf_col = "eafexposure",
                                pval_col = "p",
                                units_col = "unit_output",
                                #gene_col = "Gene",
                                samplesize_col = "samplesize_out"
)


##Harmonise data 

dat <- harmonise_data(
  exposure_dat = exp_dat, 
  outcome_dat = cost_out_dat
)







migraine.res.summary <- mr(dat, method_list=c("mr_ivw", "mr_egger_regression","mr_penalised_weighted_median","mr_weighted_mode"))
migraine.resegger <- mr(dat, method_list=c("mr_ivw", "mr_egger_regression"))
migraine.resmedian <- mr(dat, method_list=c("mr_ivw","mr_simple_median", "mr_weighted_median", "mr_penalised_weighted_median"))	
migraine.resmode <- mr(dat, method_list=c("mr_ivw", "mr_simple_mode", "mr_weighted_mode"))			
migraine.resraps <- mr(dat, method_list = c("mr_ivw","mr_raps"), parameters = list(over.dispersion = FALSE, loss.function = "l2"))
migraine.resivw <- mr(dat, method_list = "mr_ivw")





##Steiger test for directionality 

out <- directionality_test(dat)


# Heterogeneity and Egger intercept
migraine.mr_het <- mr_heterogeneity(dat)
migraine.mr_het
migraine.mr_egger_int <- mr_pleiotropy_test(dat)
migraine.mr_egger_int




# single SNP analyses
migraine.res_single <- mr_singlesnp(dat, all_method=c("mr_ivw", "mr_egger_regression", "mr_penalised_weighted_median", "mr_weighted_mode"))
migraine.res_single

# leave one out analyses
migraine.res_loo <- mr_leaveoneout(dat)
migraine.res_loo


#RADIAL MR

#First, convert data to radial format
migraine_radial<-format_radial(dat[,6],dat[,7],dat[,21],dat[,14],dat[,1])

#Then run analysis

migraine.radial<-ivw_radial(migraine_radial,0.05,1,0.0001) ##default parameters
migraine.radial


# re-run analysis without outlying SNPs



migraine.radial.dat<- subset(dat, SNP!="rs4081947" & SNP!="rs75213074")
migraine.res.radial <- mr(migraine.radial.dat, method_list=c("mr_ivw", "mr_egger_regression","mr_penalised_weighted_median","mr_weighted_mode"))
migraine.res.radial
migraine.res.summary



#######GRAPHICS 

##Plain scatter 


#Scatter plot

migraine.main.scatter<- mr_scatter_plot(migraine.res.summary, dat)
migraine.main.scatter
ggsave(migraine.main.scatter[[1]], file="main_scatter.png", width=7, height=7)

migraine.ivw.scatter.only<-mr_scatter_plot(migraine.resivw,dat)
migraine.ivw.scatter.only
ggsave(migraine.ivw.scatter.only[[1]], file="ivw_scatter.png", width=7, height=7)

#FOREST PLOT 


migraine.res_single <- mr_singlesnp(dat)
migraine.forest.plot <- mr_forest_plot(migraine.res_single)
migraine.forest.plot[[1]]
ggsave(migraine.forest.plot[[1]], file="migraineforest.png", width=7, height=7)


##10% genetic liability

migraine.res_single.10<-data.frame(migraine.res_single)
migraine.res_single.10$b<-migraine.res_single.10$b*scale.10
migraine.res_single.10$se<-migraine.res_single.10$se*scale.10

migraine.forest.plot.10 <- mr_forest_plot(migraine.res_single.10)
migraine.forest.plot.10[[1]]
ggsave(migraine.forest.plot.10[[1]], file="migraineforest10.png", width=7, height=7)


######################## Type 2 Diabetes ########################


######################## Type 2 Diabetes ########################




##First, read in my file name with the non-standard headings

exp_dat<-read_exposure_data("...p1/013/working/data/Many traits analysis/combined_cost_10_betas.csv",
                            sep = ",",
                            snp_col = "snp",
                            beta_col = "betaexposure",
                            se_col = "seexposure",
                            effect_allele_col = "effect_alleleexposure",
                            other_allele_col = "other_alleleexposure",
                            eaf_col = "eafexposure",
                            pval_col = "pvalexposure",
                            units_col = "unitsexposure",
                            #gene_col = "Gene",
                            samplesize_col = "samplesizeexposure"
)

##Clump this file - 

exp_dat <- clump_data(exp_dat)




cost_out_dat<-read_outcome_data("...p1/013/working/data/Many traits analysis/combined_cost_10_betas.csv",
                                snps = exp_dat$SNP,
                                sep = ",",
                                snp_col = "snp",
                                beta_col = "beta_10_outcome",
                                se_col = "se_10_outcome",
                                effect_allele_col = "effect_alleleexposure",
                                other_allele_col = "other_alleleexposure",
                                eaf_col = "eafexposure",
                                pval_col = "p",
                                units_col = "unit_output",
                                #gene_col = "Gene",
                                samplesize_col = "samplesize_out"
)


##Harmonise data 

dat <- harmonise_data(
  exposure_dat = exp_dat, 
  outcome_dat = cost_out_dat
)







t2d.res.summary <- mr(dat, method_list=c("mr_ivw", "mr_egger_regression","mr_penalised_weighted_median","mr_weighted_mode"))
t2d.resegger <- mr(dat, method_list=c("mr_ivw", "mr_egger_regression"))
t2d.resmedian <- mr(dat, method_list=c("mr_ivw","mr_simple_median", "mr_weighted_median", "mr_penalised_weighted_median"))	
t2d.resmode <- mr(dat, method_list=c("mr_ivw", "mr_simple_mode", "mr_weighted_mode"))			
t2d.resraps <- mr(dat, method_list = c("mr_ivw","mr_raps"), parameters = list(over.dispersion = FALSE, loss.function = "l2"))
t2d.resivw <- mr(dat, method_list = "mr_ivw")





##Steiger test for directionality 

out <- directionality_test(dat)


# Heterogeneity and Egger intercept
t2d.mr_het <- mr_heterogeneity(dat)
t2d.mr_het
t2d.mr_egger_int <- mr_pleiotropy_test(dat)
t2d.mr_egger_int




# single SNP analyses
t2d.res_single <- mr_singlesnp(dat, all_method=c("mr_ivw", "mr_egger_regression", "mr_penalised_weighted_median", "mr_weighted_mode"))
t2d.res_single

# leave one out analyses
t2d.res_loo <- mr_leaveoneout(dat)
t2d.res_loo



#RADIAL MR

#First, convert data to radial format
t2d_radial<-format_radial(dat[,6],dat[,7],dat[,21],dat[,14],dat[,1])

#Then run analysis

t2d.radial<-ivw_radial(t2d_radial,0.05,1,0.0001) ##default parameters
t2d.radial




# re-run analysis without outlying SNPs



t2d.radial.dat<- subset(dat, SNP!="rs2383208" & SNP!="rs864745" & SNP!="rs9936385")
t2d.res.radial <- mr(t2d.radial.dat, method_list=c("mr_ivw", "mr_egger_regression","mr_penalised_weighted_median","mr_weighted_mode"))
t2d.res.radial



#######GRAPHICS 

##Plain scatter 


#Scatter plot

t2d.main.scatter<- mr_scatter_plot(t2d.res.summary, dat)
t2d.main.scatter
ggsave(t2d.main.scatter[[1]], file="main_scatter.png", width=7, height=7)

t2d.ivw.scatter.only<-mr_scatter_plot(t2d.resivw,dat)
t2d.ivw.scatter.only
ggsave(t2d.ivw.scatter.only[[1]], file="ivw_scatter.png", width=7, height=7)

#FOREST PLOT 


t2d.res_single <- mr_singlesnp(dat)
t2d.forest.plot <- mr_forest_plot(t2d.res_single)
t2d.forest.plot[[1]]
ggsave(t2d.forest.plot[[1]], file="t2dforest.png", width=7, height=7)

##for a 10% increase in genetic liability

t2d.res_single.10<-data.frame(t2d.res_single)
t2d.res_single.10$b<-t2d.res_single.10$b*scale.10
t2d.res_single.10$se<-t2d.res_single.10$se*scale.10

t2d.forest.plot.10 <- mr_forest_plot(t2d.res_single.10)
t2d.forest.plot.10[[1]]
ggsave(t2d.forest.plot.10[[1]], file="t2dforest10.png", width=7, height=7)


######################## Depression ########################


######################## Depression ########################


##First, read in my file name with the non-standard headings

exp_dat<-read_exposure_data("...p1/013/working/data/Many traits analysis/combined_cost_14_betas.csv",
                            sep = ",",
                            snp_col = "snp",
                            beta_col = "betaexposure",
                            se_col = "seexposure",
                            effect_allele_col = "effect_alleleexposure",
                            other_allele_col = "other_alleleexposure",
                            eaf_col = "eafexposure",
                            pval_col = "pvalexposure",
                            units_col = "unitsexposure",
                            #gene_col = "Gene",
                            samplesize_col = "samplesizeexposure"
)

##Clump this file - 

exp_dat <- clump_data(exp_dat)




cost_out_dat<-read_outcome_data("...p1/013/working/data/Many traits analysis/combined_cost_14_betas.csv",
                                snps = exp_dat$SNP,
                                sep = ",",
                                snp_col = "snp",
                                beta_col = "beta_14_outcome",
                                se_col = "se_14_outcome",
                                effect_allele_col = "effect_alleleexposure",
                                other_allele_col = "other_alleleexposure",
                                eaf_col = "eafexposure",
                                pval_col = "p",
                                units_col = "unit_output",
                                #gene_col = "Gene",
                                samplesize_col = "samplesize_out"
)


##Harmonise data 

dat <- harmonise_data(
  exposure_dat = exp_dat, 
  outcome_dat = cost_out_dat
)







major.depress.res.summary <- mr(dat, method_list=c("mr_ivw", "mr_egger_regression","mr_penalised_weighted_median","mr_weighted_mode"))
major.depress.resegger <- mr(dat, method_list=c("mr_ivw", "mr_egger_regression"))
major.depress.resmedian <- mr(dat, method_list=c("mr_ivw","mr_simple_median", "mr_weighted_median", "mr_penalised_weighted_median"))	
major.depress.resmode <- mr(dat, method_list=c("mr_ivw", "mr_simple_mode", "mr_weighted_mode"))			
major.depress.resraps <- mr(dat, method_list = c("mr_ivw","mr_raps"), parameters = list(over.dispersion = FALSE, loss.function = "l2"))
major.depress.resivw <- mr(dat, method_list = "mr_ivw")





##Steiger test for directionality 

out <- directionality_test(dat)


# Heterogeneity and Egger intercept
major.depress.mr_het <- mr_heterogeneity(dat)
major.depress.mr_het
major.depress.mr_egger_int <- mr_pleiotropy_test(dat)
major.depress.mr_egger_int




# single SNP analyses
major.depress.res_single <- mr_singlesnp(dat, all_method=c("mr_ivw", "mr_egger_regression", "mr_penalised_weighted_median", "mr_weighted_mode"))
major.depress.res_single

# leave one out analyses
major.depress.res_loo <- mr_leaveoneout(dat)
major.depress.res_loo


#RADIAL MR

#First, convert data to radial format
major.depress_radial<-format_radial(dat[,6],dat[,7],dat[,21],dat[,14],dat[,1])

#Then run analysis

major.depress.radial<-ivw_radial(major.depress_radial,0.05,1,0.0001) ##default parameters
major.depress.radial


##

# re-run analysis without outlying SNPs

major.depress.radial.dat<- subset(dat, SNP!="rs11663393" & SNP!="rs7198928" & SNP!="rs915057")
major.depress.res.radial <- mr(major.depress.radial.dat, method_list=c("mr_ivw", "mr_egger_regression","mr_penalised_weighted_median","mr_weighted_mode"))
major.depress.res.radial


#######GRAPHICS 

##Plain scatter 


#Scatter plot

major.depress.main.scatter<- mr_scatter_plot(major.depress.res.summary, dat)
major.depress.main.scatter
ggsave(major.depress.main.scatter[[1]], file="main_scatter.png", width=7, height=7)

major.depress.ivw.scatter.only<-mr_scatter_plot(major.depress.resivw,dat)
major.depress.ivw.scatter.only
ggsave(major.depress.ivw.scatter.only[[1]], file="ivw_scatter.png", width=7, height=7)

#FOREST PLOT 


major.depress.res_single <- mr_singlesnp(dat)
major.depress.forest.plot <- mr_forest_plot(major.depress.res_single)
major.depress.forest.plot[[1]]
ggsave(major.depress.forest.plot[[1]], file="major.depressforest.png", width=7, height=7)

##Express in terms of 10% increase in genetic liability

major.depress.res_single.10<-data.frame(major.depress.res_single)
major.depress.res_single.10$b<-major.depress.res_single.10$b*scale.10
major.depress.res_single.10$se<-major.depress.res_single.10$se*scale.10

major.depress.forest.plot.10 <- mr_forest_plot(major.depress.res_single.10)
major.depress.forest.plot.10[[1]]
ggsave(major.depress.forest.plot.10[[1]], file="major.depressforest10.png", width=7, height=7)


######################## Main results for all exposures ########################


######################## Main results for all exposures ########################


asthma.res.summary
chd.res.summary
eczema.res.summary
migraine.res.summary
t2d.res.summary

major.depress.res.summary


##Heterogeneity tests for all exposures

asthma.mr_het
asthma.mr_egger_int
chd.mr_het
chd.mr_egger_int
eczema.mr_het
eczema.mr_egger_int
migraine.mr_het
migraine.mr_egger_int
t2d.mr_het
t2d.mr_egger_int
major.depress.mr_het
major.depress.mr_egger_int





##MR Radial, for selected exposures (contrasted with core results repeated from above)
asthma.res.summary
asthma.res.radial

chd.res.summary
chd.res.radial

eczema.res.summary
eczema.res.radial

migraine.res.summary
migraine.res.radial

t2d.res.summary
t2d.res.radial

major.depress.res.summary
major.depress.res.radial




################################################################

#Forest plot of results

###############################################################


##Condition-by-condition

##COMMENT#LN2 = 100% (relative increase)


scale.10<-0.69314718

boxsize=0.1



#Asthma




asthma.res.summary$upper<-asthma.res.summary$b+1.96*asthma.res.summary$se
asthma.res.summary$lower<-asthma.res.summary$b-1.96*asthma.res.summary$se


#Possible alternative to row names 
tabletext = as.vector(unique(eczema.res.radial[[5]])) ##Comment - this works, use earlier...

row_names <- c("IVW","MR Egger","Penalised Weighted Median","Weighted Mode")

#Forest plot on scale 10

forestplot(row_names, 
           mean=asthma.res.summary$b*scale.10,
           lower=asthma.res.summary$lower*scale.10,
           upper=asthma.res.summary$upper*scale.10,
           zero = 0,
           cex = 2,
           
           
           lineheight = "auto",
           xlab=xlab,
           grid=TRUE,
           
           
           
           boxsize = 0.1
           
)       



##Both radial and conventional on same graph

asthma.res.radial$upper<-asthma.res.radial$b+1.96*asthma.res.radial$se
asthma.res.radial$lower<-asthma.res.radial$b-1.96*asthma.res.radial$se






row_names <- c("IVW","MR Egger","Penalised Weighted Median","Weighted Mode")

forestplot(row_names,
           title="Asthma",
           legend = c("MR - all SNPs (n=8)","Radial MR - restricted SNPs (n=7)"),
           mean=cbind(asthma.res.summary$b*scale.10,asthma.res.radial$b*scale.10),
           lower=cbind(asthma.res.summary$lower*scale.10,asthma.res.radial$lower*scale.10),
           upper=cbind(asthma.res.summary$upper*scale.10,asthma.res.radial$upper*scale.10),
           zero = 0,
           cex = 2,
           lineheight = "auto",
           xlab=xlab,
           lwd.zero=5,
           xticks=seq(-120,120,by=20),
           grid = structure(seq(-120,120,by=20), 
                            gp = gpar(lty = 2, col = "#CCCCFF")), 
           boxsize = boxsize,
           col=fpColors(box=c("blue", "darkred"),lines=c("blue", "darkred"))
)   

############################################

#Eczema, just using conventional results 
eczema.res.summary$upper<-eczema.res.summary$b+1.96*eczema.res.summary$se
eczema.res.summary$lower<-eczema.res.summary$b-1.96*eczema.res.summary$se



row_names <- c("IVW","MR Egger","Penalised Weighted Median","Weighted Mode")

xlab="Cost increase per person per year per doubling of genetic liability"

forestplot(row_names,
           mean=eczema.res.summary$b*scale.10,
           lower=eczema.res.summary$lower*scale.10,
           upper=eczema.res.summary$upper*scale.10,
           zero = 0,
           cex = 2,
           
           
           lineheight = "auto",
           xlab=FALSE,
           grid=TRUE,
           
           
           
           boxsize = boxsize
           
)   

#############################################
#Eczema, including conventional and radial results 

eczema.res.radial$upper<-eczema.res.radial$b+1.96*eczema.res.radial$se
eczema.res.radial$lower<-eczema.res.radial$b-1.96*eczema.res.radial$se


row_names <- c("IVW","MR Egger","Penalised Weighted Median","Weighted Mode")

forestplot(row_names,
           title="Eczema",
           legend = c("MR - all SNPs (n=12)","Radial MR - restricted SNPs (n=11)"),
           mean=cbind(eczema.res.summary$b*scale.10,eczema.res.radial$b*scale.10),
           lower=cbind(eczema.res.summary$lower*scale.10,eczema.res.radial$lower*scale.10),
           upper=cbind(eczema.res.summary$upper*scale.10,eczema.res.radial$upper*scale.10),
           zero = 0,
           cex = 2,
           lineheight = "auto",
           xlab=xlab,
           lwd.zero=5,
           xticks=seq(-50,70,by=10),
           grid = structure(seq(-50,70,by=10), 
                            gp = gpar(lty = 2, col = "#CCCCFF")), 
           boxsize = boxsize,
           col=fpColors(box=c("blue", "darkred"),lines=c("blue", "darkred"))
)   






############################################



#Migraine

migraine.res.summary$upper<-migraine.res.summary$b+1.96*migraine.res.summary$se
migraine.res.summary$lower<-migraine.res.summary$b-1.96*migraine.res.summary$se


row_names <- c("IVW","MR Egger","Penalised Weighted Median","Weighted Mode")


forestplot(row_names,
           mean=migraine.res.summary$b*scale.10,
           lower=migraine.res.summary$lower*scale.10,
           upper=migraine.res.summary$upper*scale.10,
           zero = 0,
           cex = 2,
           
           
           lineheight = "auto",
           xlab=xlab,
           boxsize = 0.1
           
)   

##Both radial and MR on same graph

migraine.res.radial$upper<-migraine.res.radial$b+1.96*migraine.res.radial$se
migraine.res.radial$lower<-migraine.res.radial$b-1.96*migraine.res.radial$s



forestplot(row_names,
           title="Migraine",
           legend = c("MR - all SNPs (n=33)","Radial MR - restricted SNPs (n=27)"),
           mean=cbind(migraine.res.summary$b*scale.10,migraine.res.radial$b*scale.10),
           lower=cbind(migraine.res.summary$lower*scale.10,migraine.res.radial$lower*scale.10),
           upper=cbind(migraine.res.summary$upper*scale.10,migraine.res.radial$upper*scale.10),
           zero = 0,
           cex = 2,
           lineheight = "auto",
           xlab=xlab,
           lwd.zero=5,
           
           xticks=seq(-30,70,by=10),
           grid = structure(seq(-30,70,by=10), 
                            gp = gpar(lty = 2, col = "#CCCCFF")),
           boxsize = boxsize,
           col=fpColors(box=c("blue", "darkred"),lines=c("blue", "darkred"))
)   



#CHD

chd.res.summary$upper<-chd.res.summary$b+1.96*chd.res.summary$se
chd.res.summary$lower<-chd.res.summary$b-1.96*chd.res.summary$se


row_names <- c("IVW","MR Egger","Penalised Weighted Median","Weighted Mode")


forestplot(row_names,
           mean=chd.res.summary$b*scale.10,
           lower=chd.res.summary$lower*scale.10,
           upper=chd.res.summary$upper*scale.10,
           zero = 0,
           cex = 2,
           
           
           lineheight = "auto",
           xlab=FALSE,
           boxsize = 0.1
           
)    

###Both radial and conventional on same graph


chd.res.radial$upper<-chd.res.radial$b+1.96*chd.res.radial$se
chd.res.radial$lower<-chd.res.radial$b-1.96*chd.res.radial$se


row_names <- c("IVW","MR Egger","Penalised Weighted Median","Weighted Mode")

forestplot(row_names,
           title="Coronary heart disease",
           legend = c("MR - all SNPs (n=15)","Radial MR - restricted SNPs (n=13)"),
           mean=cbind(chd.res.summary$b*scale.10,chd.res.radial$b*scale.10),
           lower=cbind(chd.res.summary$lower*scale.10,chd.res.radial$lower*scale.10),
           upper=cbind(chd.res.summary$upper*scale.10,chd.res.radial$upper*scale.10),
           zero = 0,
           cex = 2,
           lwd.zero=5,
           
           lineheight = "auto",
           xlab=xlab,
           xticks=seq(-10,80,by=10),
           grid = structure(seq(-10,80,by=10), 
                            gp = gpar(lty = 2, col = "#CCCCFF")),
           boxsize = boxsize,
           col=fpColors(box=c("blue", "darkred"),lines=c("blue", "darkred"))
)   


#T2diabetes

t2d.res.summary$upper<-t2d.res.summary$b+1.96*t2d.res.summary$se
t2d.res.summary$lower<-t2d.res.summary$b-1.96*t2d.res.summary$se


row_names <- c("IVW","MR Egger","Penalised Weighted Median","Weighted Mode")


forestplot(row_names,
           mean=t2d.res.summary$b*scale.10,
           lower=t2d.res.summary$lower*scale.10,
           upper=t2d.res.summary$upper*scale.10,
           zero = 0,
           cex = 2,
           
           
           lineheight = "auto",
           xlab=FALSE,
           boxsize = 0.1
           
)    

##Both radial and conventional on same graph


##Both radial and conventional on same graph

t2d.res.radial$upper<-t2d.res.radial$b+1.96*t2d.res.radial$se
t2d.res.radial$lower<-t2d.res.radial$b-1.96*t2d.res.radial$s


forestplot(row_names,
           title="Type 2 diabetes",
           legend = c("MR - all SNPs (n=13)","Radial MR - restricted SNPs (n=7)"),
           mean=cbind(t2d.res.summary$b*scale.10,t2d.res.radial$b*scale.10),
           lower=cbind(t2d.res.summary$lower*scale.10,t2d.res.radial$lower*scale.10),
           upper=cbind(t2d.res.summary$upper*scale.10,t2d.res.radial$upper*scale.10),
           zero = 0,
           cex = 2,
           lineheight = "auto",
           xlab=xlab,
           xticks=seq(-120,120,by=20),
           grid = structure(seq(-120,120,by=20), 
                            gp = gpar(lty = 2, col = "#CCCCFF")),
           lwd.zero=5,
           boxsize = boxsize,
           col=fpColors(box=c("blue", "darkred"),lines=c("blue", "darkred"))
)    


#Depression

major.depress.res.summary$upper<-major.depress.res.summary$b+1.96*major.depress.res.summary$se
major.depress.res.summary$lower<-major.depress.res.summary$b-1.96*major.depress.res.summary$se


row_names <- c("IVW","MR Egger","Penalised Weighted Median","Weighted Mode")


forestplot(row_names,
           mean=major.depress.res.summary$b*scale.10,
           lower=major.depress.res.summary$lower*scale.10,
           upper=major.depress.res.summary$upper*scale.10,
           zero = 0,
           cex = 2,
           lwd.zero=5,
           
           lineheight = "auto",
           xlab=FALSE,
           boxsize = 0.1
           
)       

##Both radial and conventional on same graph

major.depress.res.radial$upper<-major.depress.res.radial$b+1.96*major.depress.res.radial$se
major.depress.res.radial$lower<-major.depress.res.radial$b-1.96*major.depress.res.radial$s


forestplot(row_names,
           title="Depression",
           legend = c("MR - all SNPs (n=36)","Radial MR - restricted SNPs (n=30)"),
           mean=cbind(major.depress.res.summary$b*scale.10,major.depress.res.radial$b*scale.10),
           lower=cbind(major.depress.res.summary$lower*scale.10,major.depress.res.radial$lower*scale.10),
           upper=cbind(major.depress.res.summary$upper*scale.10,major.depress.res.radial$upper*scale.10),
           zero = 0,
           cex = 2,
           lineheight = "auto",
           xlab=xlab,
           xticks=seq(-140,160,by=20),
           grid = structure(seq(-140,160,by=20), 
                            gp = gpar(lty = 2, col = "#CCCCFF")),
           lwd.zero=5,
           boxsize = boxsize,
           col=fpColors(box=c("blue", "darkred"),lines=c("blue", "darkred"),
                        zero = "lightgrey")
)   

###########################

