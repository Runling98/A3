# *************************
#  Econ 613 A3 
#  Runling, Mar.2, 2022
#  Due Mar.16, 2022 
# ************************

setwd("~/Dropbox/Duke/Econ 613/Assignment/A3/Data")
getwd()

rm(list = ls())
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(xtable)
library(bayesm)
library(matrixStats)
library(mlogit)
library(stargazer)
library(texreg)
library(survival)
library(nnet)
library(stringr)
library(fastDummies)

# Introducing data 

datstu <- read.table("datstu_v2.csv", sep = ",", header = TRUE)
datjss <- read.table("datjss.csv", sep = ",", header = TRUE)
datsss <- read.table("datsss.csv", sep = ",", header = TRUE)

###################
# Exercise 1
###################

# (1) Number of students, schools, programs

nrow(datstu) # 340823 students 

# data tidying work 
datsss$schoolcode_name <- paste(datsss$schoolname,datsss$schoolcode)
length(unique(datsss$schoolcode_name))  # 1609 schools 

# number of programs (pull all the columns as a separate dataset)
program <- 
  pivot_longer(datstu, cols = choicepgm1:choicepgm6, 
               names_to = "Choice", 
               values_to = "Program")
length(unique(program$Program)) # 33 programs 

# (2) number of choices 
subset <- select(datstu, -agey, -male, -score)

schoolchoice = reshape(data = subset, 
                       idvar = c("V1"), 
                       varying = 2:13,
                       sep = "",
                       timevar= "choice",
                       times = c(1,2,3,4,5,6),
                       new.row.names= NULL,
                       direction = "long")

schoolchoice$school_program <- paste(schoolchoice$schoolcode,schoolchoice$choicepgm)
length(unique(schoolchoice$school_program))

# the total number of choices (account for repeated choices) is 2044938, if same school and same 
# program only accounts once, we have 3086 unqiue school-program choices. 

# (3) Number of students applying to at least one 
# senior high schools in the same district to home (merge using the schoolcode)
# datstu var jssdistrict = datsss var sssdistrict 

schoolocation = left_join(schoolchoice, datsss, "schoolcode") # could aslo use left_join 
head(schoolocation, 10)
schoolocation$same_district = ifelse(schoolocation$jssdistrict==
                                       schoolocation$sssdistrict,1,0)
head(schoolocation, 10)

# method 1 
schoolocation %>% 
  dplyr::group_by(V1.x) %>% 
  dplyr::filter (same_district == 1) %>% 
  dplyr::summarise(obs=n()) %>% 
  dplyr::count() # 265464 

# method 2
schoolocation %>% 
  dplyr::group_by(V1.x) %>% 
  dplyr::filter (same_district == 1) %>% 
  dplyr::summarise(obs=n()) %>%
  dplyr::filter (obs >0) %>%
  dplyr::group_by(V1.x) %>%
  dplyr::count()

# (4) Number of students each senior high school admitted

choice = datstu[,c(5:10)]  %>% 
  mutate(id=row_number())
choice_long=melt(choice, id.vars=c("id"))
rank=datstu %>% dplyr::select(V1, rankplace)

choice_long=left_join(choice_long, rank, by=c("id" = "V1"))
choice_long = choice_long %>% filter(!is.na(rankplace),rankplace!=99)
choice_long$rankplace=paste0("schoolcode",choice_long$rankplace)
choice_long$true=choice_long$variable==choice_long$rankplace
choice_long=choice_long %>%
  filter(true==TRUE) %>%
  dplyr::count(value)

xtable(choice_long)

# (5) The cutoff of senior high schools (the lowest score to be admitted)
# (6) The quality of senior high schools (the average score of students admitted)

score = datstu %>%
  dplyr::select(V1,score,schoolcode1,schoolcode2,schoolcode3,schoolcode4,schoolcode5,schoolcode6,rankplace) %>%
  filter(rankplace<7)

score = score %>% 
  mutate(school_choice = ifelse(rankplace==1,schoolcode1,
                         ifelse(rankplace==2,schoolcode2,
                         ifelse(rankplace==3,schoolcode3,
                                ifelse(rankplace==4,schoolcode4,
                                       ifelse(rankplace==5,schoolcode5,schoolcode6))))))
schinfo = score %>%
  dplyr::select(school_choice,score) %>%
  group_by(school_choice) %>% 
  summarise(cutoff=min(score),
            quality=mean(score),
            size=n())

xtable(schinfo)


###################
# Exercise 2 
###################
schoolocation = left_join(schoolchoice, datsss, "schoolcode") # could aslo use left_join 

schoolocation$admitted = ifelse(schoolocation$choice==schoolocation$rankplace,1,0)

score = select(datstu, score, V1)
admitseniorhigh= schoolocation %>% 
  dplyr::filter(admitted == 1)
cutoff <- merge(x=score, y=admitseniorhigh, by.x=c("V1"), 
                by.y=c("V1.x"), all.y=TRUE)

school_program_info <- cutoff %>%
  group_by(schoolcode, school_program) %>%
  summarise(
    size = sum(admitted==1),
    cutoff = min(score, na.rm = T),
    quality = mean(score, na.rm = T)
  )

school_info2 <- select(datsss, ssslong, sssdistrict, schoolcode)
school_info2 <- school_info2 %>% distinct()

school=merge(school_program_info, school_info2, by="schoolcode")
school <- school[!duplicated(school$school_program), ]

head_school <- head(school, 10) 
xtable(head_school)


###################
# Exercise 3 
###################

school_dist <- left_join(schoolocation, datjss, by="jssdistrict")
school_dist <- school_dist[!duplicated(school_dist$school_program), ]

school_dist$distance = sqrt((69.172*(school_dist$ssslong-school_dist$point_x)*
                               cos(school_dist$point_y/57.3))^2 + 
                              (69.172*(school_dist$ssslat-school_dist$point_y))^2)

head_school_dist <- select(school_dist, V1.x, distance, school_program)
head_school_dist <- head(head_school_dist, 10)

xtable(head_school_dist)


###################
# Exercise 4 
###################
# Method 1: 

# (1) Recode the schoolcode into its first three digits
schoolchoice$scode_rev = str_sub(schoolchoice$schoolcode,1,3)

# (2)  Recode the program variable into 4 categories
schoolchoice <- schoolchoice %>% mutate(pgm_rev = recode(choicepgm, 
                                                         "General Arts" = "Arts",
                                                         "Visual Arts" = "Arts",
                                                         "Business"  = "Economics",
                                                         "Home Economics" = "Economics", 
                                                         "General Science" = "Science",
                                                         .default = "Others"
))

# (3) Create a new choice variable choice rev.
schoolchoice$choice_rev <- paste(schoolchoice$scode_rev,schoolchoice$pgm_rev)

# (4) Recalculate the cutoff and the quality for each recoded choice.

schoolchoice$admitted = ifelse(schoolchoice$choice==schoolchoice$rankplace,1,0)

schoolchoice_clean= schoolchoice %>% 
  dplyr::filter(admitted == 1)

newdata <- merge(x=score, y=schoolchoice_clean, by.x=c("V1"), 
                 by.y=c("V1"), all.y=TRUE)

newdata2<- newdata %>%
  group_by(choice_rev,scode_rev, pgm_rev) %>%
  summarise(
    size = sum(admitted==1),
    cutoff = min(score, na.rm = T),
    quality = mean(score, na.rm = T)
  )

newdatset <- merge(x=schoolchoice, y=newdata2, by.x=c("choice_rev"),
                   by.y=c("choice_rev"), all.y=TRUE)  

newdataset <- merge(x=score, y=newdatset, by.x=c("V1"), 
                    by.y=c("V1"), all.y=TRUE)

recodedchoice <- select(newdataset, V1,choice,choice_rev, score,
                        size, cutoff, quality)

head_reco <- head(recodedchoice, 10)
xtable(head_reco)

# (5) Consider the 20,000 highest score students
recodedchoice_new <- recodedchoice %>%
  dplyr::filter(!is.na(score)) %>%
  mutate(rank_score = rank(score, ties.method = "min"))  %>%
  filter(rank_score<=124121)

#given we have a long format data, everyone student has 6 rows, then we limit 
# the obs number as <=124121
table(recodedchoice_new$rank_score)

# Method 2: 

select=dplyr::select
datstu_new=datstu[,c(1,5:16)]
rev <- datstu%>%select(-c(5:10))
rev2=sapply(datstu[,5:10], function(x) substr(x, 1, 3))
rev=cbind(rev,rev2)

choice_mod=c("General Arts"="Arts", "Visual Arts"="Arts", 
             "General Science"="Science", "Home Economics"="Economics", 
             "Business"="Economics" )

rev$choicepgm1=as.character(choice_mod[rev$choicepgm1])
rev$choicepgm1[is.na(rev$choicepgm1)] <- "Others"
rev$choicepgm2=as.character(choice_mod[rev$choicepgm2])
rev$choicepgm2[is.na(rev$choicepgm2)] <- "Others"
rev$choicepgm3=as.character(choice_mod[rev$choicepgm3])
rev$choicepgm3[is.na(rev$choicepgm3)] <- "Others"
rev$choicepgm4=as.character(choice_mod[rev$choicepgm4])
rev$choicepgm4[is.na(rev$choicepgm4)] <- "Others"
rev$choicepgm5=as.character(choice_mod[rev$choicepgm5])
rev$choicepgm5[is.na(rev$choicepgm5)] <- "Others"
rev$choicepgm6=as.character(choice_mod[rev$choicepgm6])
rev$choicepgm6[is.na(rev$choicepgm6)] <- "Others"


rev = rev %>%dplyr::mutate(choice1=paste0(schoolcode1,choicepgm1),
                                 choice2=paste0(schoolcode2,choicepgm2),
                                 choice3=paste0(schoolcode3,choicepgm3),
                                 choice4=paste0(schoolcode4,choicepgm4),
                                 choice5=paste0(schoolcode5,choicepgm5),
                                 choice6=paste0(schoolcode6,choicepgm6))

choice_df = rev %>% 
  dplyr::select(choice1,choice2,choice3,choice4,choice5,choice6) %>%
  gather('key','choice') %>%
  dplyr::select(choice)

choice_df = choice_df %>%
  mutate(schoolcode= gsub('\\D','', choice),pgm=gsub('\\d','', choice)) %>%
  distinct()

choice_column = choice_df$choice
choice_df = choice_df %>% mutate(ch_rev = as.numeric(factor(choice,levels=choice_column)))

schoolinfo = datsss %>%
  dplyr::select(schoolcode,sssdistrict,ssslong,ssslat) %>%
  mutate(schoolcode=as.character(schoolcode)) %>%
  drop_na() %>%
  distinct()

choice_df  = choice_df %>% left_join(schoolinfo)

choice_sub = rev %>%
  dplyr::select(score,choice1,choice2,choice3,choice4,choice5,choice6,rankplace) %>%
  filter(rankplace<7)

choice_sub = choice_sub %>%
  mutate(choice = ifelse(rankplace==1,choice1,
                  ifelse(rankplace==2,choice2,
                  ifelse(rankplace==3,choice3,
                  ifelse(rankplace==4,choice4,
                  ifelse(rankplace==5,choice5,choice6))))))

choice_sub = choice_sub %>%
  dplyr::filter(!is.na(score)) %>%
  dplyr::select(choice,score) %>%
  group_by(choice) %>%
  summarise(
    cutoff=min(score),
    quality=mean(score),
    size=n())


choice_df = choice_df %>% left_join(choice_sub)
schoolchoice_clean = choice_df %>% select(choice, cutoff, quality,size)
xtable(schoolchoice_clean)

subset <- rev %>%
  arrange(desc(score)) %>%
  filter(score >= score[20000])

ex5.data=subset[,c(2,19)] %>%
  rename(choice_rev=choice1)

choice_df %>% select(schoolcode,quality)
ex6.data=subset %>% left_join(choice_df,by=c("schoolcode1"="schoolcode"))
ex6.data=ex6.data[,c(13,32)]

#############################
# Exercise 5 : First Model 
#############################
# transfer the data to ideal format

choice_matrix <- ex5.data%>%select(choice_rev)%>%distinct()

matrix1 = as.matrix(ex5.data)
matrix2 = as.matrix(choice_matrix)

ni=nrow(matrix1)
nj=nrow(choice_matrix)

Y <- matrix(0, ni,nj)
for(i in 1:nj){
  for(j in 1:ni){
    if(matrix1[j,2]==matrix2[i]){
      Y[j,i]=1
    }
  }
}

X=cbind(int = rep(1,nrow(ex5.data)), ex5.data$score)

# use package to determine the starting values 
# ex5.model = multinom(choice_rev ~ score, data = ex5.data)
# write.csv(ex5.model$coefficients,'multi.csv')
# start.ex5=fread('multi.csv',head=T)

mlike=function(x,beta) {
  beta=mat.or.vec(2,nj)
  sum_exp=as.matrix(rowSums(exp(x %*% beta[,2:nj]))) 
  mat_pro=mat.or.vec(nrow(ex5.data),nj)
  mat_pro[,1]=1/(1+sum_exp)
  for(i in 1:(nj-1)){
    pr=exp(x %*% beta[,i])/(1+sum_exp)
    mat_pro[,i+1]=pr
  }
  return(mat_pro)
}

mllike=function(y,x,beta) {
  beta=mat.or.vec(2,nj)
  sum_exp=as.matrix(rowSums(exp(x %*% beta[,2:nj]))) 
  mat_pro=mat.or.vec(nrow(ex5.data),nj)
  mat_pro[,1]=1/(1+sum_exp)
  for(i in 1:(nj-1)){
    pr=exp(x %*% beta[,i])/(1+sum_exp)
    mat_pro[,i+1]=pr
  }
  fnlike=0
  for(i in 1:nj){
    fnlike=fnlike+colSums(as.matrix(y[,i]*log(mat_pro[,i])))
  }
  return(-fnlike)
}
model1=optim(function(beta) mllike(y=Y,x=X,b=beta),par=runif(490),
             control=list(trace=6,REPORT=1,maxit=1000),method="BFGS")
model1$par

# marginal effect
p=as.matrix(ex5.data$score, ncol=1)
pij_m2=mlike(X,model1$par)

mb=c(0,model1$par[246:490])
me_model2=array(0,dim=c(nrow(p),246))
for (i in 1:nrow(p)) {
  be=sum(pij_m2[i,]*mb)
  for (j in 1:246) {
    me_model2[i,j] <- pij_m2[i,j]*(mb[j]-be)
  }
}
me_model2=apply(me_model2, 2, mean)
me_model2

#############################
# Exercise 6 : Second Model 
#############################
# my computer cannot run full sample but only a subset 

# choice matrix (only for the first choice)
choice <- recodedchoice_new %>%
  dplyr::filter(choice == 1, !is.na(score),V1 <= 180988) 

# convert choice_rev as a numerical variable 
choice$choice_rev <- as.numeric(as.factor(choice$choice_rev)) 

choicematrix<- dummy_cols(choice,  select_columns = "choice_rev",remove_first_dummy = TRUE)
Y <- select(choicematrix, 9:58)
set.seed(100)
# generate school quality matrix
school.quality <- choice %>%
  mutate(index = choice_rev) %>%
  pivot_wider(names_from = choice_rev, values_from = quality)
school.quality <- school.quality %>% 
  mutate(across(everything(), ~replace_na(.x, max(.x, na.rm = TRUE))))

#Likelihood Function
quality <- school.quality[,8:421]

likelihood=function(x,beta) {
  coef=exp(matrix(rep(c(0,beta[1:414]),nrow(x)),byrow=TRUE,nrow(x))+x*beta[415])
  coef_sum=apply(coef,1,sum)
  return(coef/coef_sum)
}
llike=function(y1,x,beta) {
  lprob=log(likelihood(x,beta))
  return(-sum(Y*lprob))
}

#optimization
model2=optim((function(beta) llike(y,quality,beta)),par=runif(415),control=list(trace=6,REPORT=1,maxit=1000),method="BFGS")
model2.par=as.matrix(model2$par)
xtable(model2.par)

#marginal effect for model 1(conditional logit)
pij=likelihood(quality,model2$par)
mid=array(0,dim = c(nrow(quality),415,415))
for (i in 1:nrow(quality)) {
  diag(mid[i,,]) <- 1
}
llikem=array(0,dim=c(nrow(quality),415,415))
for (i in 1:nrow(quality)) {
  for (j in 1:415) {
    for (k in 1:415) {
      llikem[i,j,k]=pij[i,j]*(mid[i,j,k]-pij[i,k])*model2$par[415]
    }
  }
}
me_model2=apply(llikem,c(2,3),mean)

# use package to set the starting values (could use, but my computer cannot work. 
# take so long)
#start2=clogit(choice ~ quality + strata(V1), data=data, method = "exact")
#start2$coefficients

#############################
# Exercise 7 : Simulations 
#############################

# both models are appropriate to conduct this exercise
# ( I choose ex6's model to do the following questions)

# 7.2 same as ex6

# 7.3 the essence of this problem is to exclude those choice who choose others but 
# not excluding those observations 

# use the same method to contrusct the following data (very similiar to my way of constructing 
# school.quality) also upload to sakai. 

ex7.data <- read.table("ex7.data.csv", sep = ",", header = TRUE)
ex7.data <- select(ex7.data, -X)

exclude_others <- ex7.data %>%
  mutate(fil=ifelse(str_detect(choice_rev,'Others'),1,0)) %>%
  filter(fil==0) %>%
  select(1:250)

exclude_others<-exclude_others[,!str_detect(colnames(exclude_others),'Others')]

# cannot run mlogit.data package on my computer, the idea is to transfer wide to long 
# using the package
# start3=clogit(choice ~ quality + strata(V1), data=exclude_others_long, method = "exact")
# start3$coefficients

n2i <- nrow(exclude_others)
n2j <- ncol(exclude_others[,5:200])
Y2 <- matrix(0,n2i,n2j)
Y2=matrix(0, n2i,n2j)
for(i in 1:n2j){
  for(j in 1:n2i){
    if(exclude_others$choice_rev[j]==names(exclude_others)[i+4]){
      Y2[j,i]=1
    }
  }
}

X3 <- exclude_others %>% 
  group_by(choice_rev) %>% 
  summarise(quality = unique(rev_quality)) %>%
  ungroup()
X3 <- as.matrix(cbind(rep(1, n2j), X3[,2]))

mclike2=function(y,x,beta) {
  beta=mat.or.vec(1,n2j+1)
  beta[1]=0 
  mat_pro=mat.or.vec(1,n2j)
  mat_beta = mat.or.vec(2,n2j)
  mat_beta[1,]= beta[1:n2j]
  mat_beta[2,]= beta[n2j+1]
  sum_exp=sum(diag(exp(x %*% mat_beta)))
  mat_pro=diag(exp(x %*% mat_beta))/sum_exp
  fnlike=0
  for(i in 1:n2j){
    fnlike=fnlike+colSums(as.matrix(y[,i]*log(mat_pro[i])))
  }
  return(-fnlike)
}

model4=optim(function(beta) mclike2(y=Y2,x=X3,b=beta),par=runif(196),
             control=list(trace=6,REPORT=1,maxit=1000),
             method="BFGS")

as.matrix(model4$par)

