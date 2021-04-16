### Libraries ###
library(tidyverse)
library(readxl)
library(lubridate)
library(gtsummary)



# Below is John's way to read in data
#df<-read.csv("C://Users//tsiri//OneDrive//Desktop//BIOSTAT620//Project2//SAFTB_Crossover_Data.csv")
#setwd("C://Users//tsiri//OneDrive//Desktop//BIOSTAT620//Project2//SAFTB")
#attach(df)

#Below is Spencer's way to read in the data
df = read_excel(path = "SAFTB_crossover_data.xlsx", col_types = 
                       c("text", "text", "text", "text", "numeric", "numeric", "numeric",
                         "date", "text"))

#Create score variable
i=0
df$score=0

for (i in 1:length(df$Pickups)){
  df$score[i]=round((0.4*df$Tot.Scr.Time[i])+(0.6*df$Pickups[i]),3)
  i=i+1
}


#model 1
model1<-lm(score~ID,data=df)
summary(model1)

#model 2
model2<-lm(score~ID+Period,data=df)
summary(model2)

#testing significance of added variable
(anova1<-anova(model1,model2)) #p-value is 0.02389, keep variable

#model3
model3<-lm(score~ID+Period+Day,data=df)
summary(model3)

#testing significance of added variable
(anova2<-anova(model2,model3)) #no significance

#adding age variable
df$age=0
j=0

for (j in 1:length(df$ID)){
  if (df$ID[j]=="tsirigoj"){
    df$age[j]=30
  }
  else if (df$ID[j]=="srhaup"|df$ID[j]=="shincd"){
    df$age[j]=25
  }
  else{
    df$age[j]=23
  }
  j=j+1
}
 #model 4
model4<-lm(score~ID+Period+age,data=df)
summary(model4)

#test with new variable
(anova3<-anova(model2,model4))

l=0
m=0
n=0
p=0
df$baseline=0
df$A=0
df$washout=0
df$B=0

for (l in 1:length(df$ID)){
  if(df$Period[l]=="P0"){
    df$baseline[l]=1
  }
  else{
    df$baseline[l]=0
  }
  l=l+1
}

for (m in 1:length(df$ID)){
  if(df$Period[m]=="P1"){
    df$A[m]=1
  }
  else{
    df$A[m]=0
  }
  m=m+1
}

for (n in 1:length(df$ID)){
  if(df$Period[n]=="W"){
    df$washout[n]=1
  }
  else{
    df$washout[n]=0
  }
  n=n+1
}

for (p in 1:length(df$ID)){
  if(df$Period[p]=="P2"){
    df$B[p]=1
  }
  else{
    df$B[p]=0
  }
  p=p+1
}

model5<-lm(score~ID+baseline+A+washout+B,data=df)
summary(model5)

(anova(model2,model5))

#Include interaction
model6<-lm(score~ID+Period+ID*Period,data=df)#THIS IS THE MODEL
summary(model6)
(anova(model2,model6))
(anova(model6))

### Renaming for tables ###
df2 = df %>%
  rename(Subject = ID) %>%
  mutate(
    Subject = case_when(Subject == "clarkbou" ~ "1",
                        Subject == "shincd" ~ "2",
                        Subject == "srhaup" ~ "3",
                        Subject == "tsirigoj" ~ "4"))

### Rerun models with new names ###
#model 2
model2a<-lm(score~Subject+Period,data=df2)


#model3
model3a<-lm(score~Subject+Period+Day,data=df2)


#testing significance of added variable
(anova2a<-anova(model2a,model3a)) #no significance


### Tables ###
t2 = model2a %>%
  tbl_regression() %>%
  bold_p(t=0.05) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_footnote(p.value ~ "Significant values in bold")

t3 = model3a %>%
  tbl_regression() %>%
  bold_p(t=0.05)%>%
  modify_header(label ~ "**Variable**") %>%
  modify_footnote(p.value ~ "Significant values in bold")

tbl_merge(tbls = list(t2,t3),tab_spanner = c("**Model 1**", "**Model 2**")) %>%
  as_gt() %>%
  gt::tab_header(title = "Table 3. Regression Results")


### Weekday vs Weekend ###
df2 = df2 %>%
  mutate(Weekend = ifelse(Day == "Sa" | Day == "Su","Yes", "No"))

model3b<-lm(score~Subject+Period+Weekend,data=df2)
summary(model3b)
  