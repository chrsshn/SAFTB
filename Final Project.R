
library(dplyr)
### John Reading in data ###
#setwd("C:\\Users\\tsiri\\OneDrive\\Desktop\\BIOSTAT620\\Final Project")
#baseline<-read.csv(".\\baseline_full_deid.csv")
#crossover<-read.csv(".\\crossover_full_deid.csv")

### Spencer Reading in the data ###
crossover = read.csv(file = "crossover_full_deid.csv", header=TRUE)

baseline = read.csv(file = "baseline_full_deid.csv", header=TRUE)

attach(crossover)

#Column for treatment sequence
seq.df = as.data.frame(matrix(crossover$Treatment,ncol=24,nrow=25), stringsAsFactors = FALSE)
seq.vec = rep("AA", 24)

for(i in 1:24) {
  seq.vec[i] = paste(seq.df[15,i],seq.df[22,i],sep="")
}

crossover$Sequence = rep(seq.vec, each=25)

seq_transpose<-as.data.frame(t(as.matrix(seq.df)))
seq_transpose

row.names(seq_transpose)<-1:24

#merge both datasets together

df<-merge(crossover,baseline,all = T)

#Fix mistakes in DOW

df$Day = ifelse(df$Day == "Fi", "Fr", df$Day)

#create score variable

i=0
df$score=0

for (i in 1:length(df$Pickups)){
  df$score[i]=(0.6*df$Tot.Scr.Time[i]+0.4*df$Pickups[i])
  i=i+1
}

#Rename 'sex' variable to something easier to work with

names(df)[names(df)=="sex"]<-"male"

#Play around with regression models on score

null_model<-lm(score~1,data=df)

df$Treatment<-as.factor(df$Treatment)
df<-within(df,Treatment<-relevel(Treatment,ref="N"))

treatment_model<-lm(score~Treatment,data=df)
summary(treatment_model)

anova(null_model,treatment_model)#treatment model is obviously a better fit

#Lets include Group Level model

group_model<-lm(score~Treatment+Sequence,data=df)
summary(group_model)

anova(treatment_model,group_model)#group model is better

#DOW model

df$Day<-as.factor(df$Day)
df<-within(df,Day<-relevel(Day,ref="Mo")) #reference=Monday

DOW_model<-lm(score~Treatment+Sequence+Day,data=df)
summary(DOW_model)

anova(group_model,DOW_model) #p-value is 0.9635

#Age model

age_model<-lm(score~Treatment+Sequence+age,data=df)
summary(age_model)

anova(group_model,age_model) #p-value is 0.7556

#Gender model

gender_model<-lm(score~Treatment+Sequence+male,data=df)
summary(gender_model)

anova(group_model,gender_model)#keep gender


#Seq:gender interaction model
seqgender<-lm(score~Treatment+Sequence+male+Sequence*male,data=df)
summary(seqgender)

anova(gender_model,seqgender)#Although p-value<0.01, not very useful as one sequence 
                            #is an all-female group

#Plot model
plot(gender_model)

###########################################
###########RUN DIAGNOSTICS#################
###########################################

library(olsrr)

#Will give us strongly influential points
plot(gender_model,4) #Points 31, 493, and 548 are influential

ols_plot_cooksd_bar(gender_model) #several outliers, above points stick out

#Point 493 seems to be the most outlying, so lets see what regression looks like
#without it

test_df<-df[-c(493),]
test_model<-lm(score~Treatment+Sequence+male,data=test_df)
summary(test_model)

#STUDENTIZED RESIDUAL PLOT
ols_plot_resid_stud(gender_model)#Only outlier in this case is 510
test_df2<-df[-c(510),]
test_df3<-test_df[-c(509),]

test_model2<-lm(score~Treatment+Sequence+male,data=test_df2)
summary(test_model2)

test_model3<-lm(score~Treatment+Sequence+male,data=test_df3)
summary(test_model3)

#Lets try centering scores (probably doesn't do anything)
mean_score=mean(df$score)

j=0
df$test_score=0

for (j in 1:length(df$score)){
  df$test_score[j]=df$score[j]-mean_score
  j=j+1
}

#RERUN ANOTHER MODEL WITH TEST SCORE

testscore_lm<-lm(test_score~Treatment+Sequence+male,data=df)
summary(testscore_lm)

#RERUN SOME DIAGNOSTICS
plot(testscore_lm,4) #same outliers
ols_plot_cooksd_bar(testscore_lm)#still a bunch of outliers
ols_plot_resid_stud(testscore_lm)#510 is still an outlier

#Not worth centering


######### TABLES ############
library(gtsummary)

names(df)[names(df)=="male"]<-"Male"

table.model1 = lm(score~Treatment+Sequence, data=df)

table.model2 = lm(score~Treatment*Male+Sequence,data=df)


t1 = table.model1 %>%
  tbl_regression() %>%
  bold_p(t=0.05) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_footnote(p.value ~ "Significant values in bold")

t2 = table.model2 %>%
  tbl_regression() %>%
  bold_p(t=0.05)%>%
  modify_header(label ~ "**Variable**") %>%
  modify_footnote(p.value ~ "Significant values in bold")

tbl_merge(tbls = list(t1,t2),tab_spanner = c("**Model 1**", "**Model 2**")) %>%
  as_gt() %>%
  gt::tab_header(title = "Table 3. Regression Results") 
  
  
  
