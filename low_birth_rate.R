# low birth weight
library(xlsx)
library(MASS)
library(plyr)
library(ggplot2)
library(usdm)
library(bestglm)
library(fmsb)
library(leaps)
library(pscl)
library(MKmisc)
library(caret)
library(pROC)
library(ROCR)
library(influence.measures)
data=read.csv("G:/ม๔ัง/Columbia/study/4201/project/low_birth.csv",sep=";")
head(data)
birth=data[complete.cases(data),]
attach(birth)
names(birth)
boxplot(tpounds)
hist(tpounds)
birth$sex=ifelse(birth$sex==1,0,1)
birth$sex=as.factor(birth$sex)
birth$marital=ifelse(birth$marital==1,0,1)
birth$marital=as.factor(birth$marital)
birth$racemom=as.factor(birth$racemom)
birth$hispmom=as.factor(birth$hispmom)
birth$smoke=as.factor(birth$smoke)
birth$Mature=as.factor(birth$Mature)
birth$Premie=as.factor(birth$Premie)
birth$lowbw=as.factor(birth$lowbw)
birth$plural=ifelse(birth$plural==1,0,1)
birth$plural=as.factor(birth$plural)


ggplot(birth,aes(tpounds,fill=smoke))+geom_histogram(alpha = 0.5, bins=20,
       position = 'identity')+ggtitle("smoke ")
birth$black=factor(1,levels = c(0,1))
birth$black[birth$racemom!="2"]=factor(0)

summary(lm(tpounds~plural+sex+fage+mage+weeks+visits+marital+racemom+hispmom+gained+lowbw+smoke+Mature+Premie))
#glm_weight=glm(LOW~AGE+LWT+RACE+SMOKE+PTL+HT+UI+FTV,family="binomial")
#seperate into 2 groups
normal=subset(birth,birth$lowbw==0)
low=subset(birth,birth$lowbw==1)
boxplot(normal$tpounds)
boxplot(low$tpounds)
#normal test
fitdistr(normal$tpounds,"normal")
ks.test(normal$tpounds,pnorm,7.53148352 ,0.99623108)

fitdistr(low$tpounds,"normal")
ks.test(low$tpounds,pnorm,4.0427027,1.3127867)
# cannot take log, or would be more skewed!
fitdistr(log(low$tpounds),"normal")
ks.test(log(low$tpounds),pnorm,1.32153637 ,0.42823520 )

lm_weight_low=lm(tpounds~plural+sex+fage+mage+weeks+visits+marital+racemom+hispmom+gained+smoke+Mature+Premie,data=low)
summary(lm_weight_low)
lm_weight_normal=lm(tpounds~plural+sex+fage+mage+weeks+visits+marital+racemom+hispmom+gained+smoke+Mature+Premie,data=normal)
summary(lm_weight_normal)
#+LWT+RACE+SMOKE+PTL+HT+UI+FTV+BWT,
#rank sum test
wilcox.test(tpounds~smoke,paired=F)
ggplot()
mean(subset(birth,racemom==1)$tpounds)
mean(subset(birth,racemom==2)$tpounds)

#VIF
r=brick(birth)# reading a RasterBrick object including 10 raster layers in Spain
VIF(lm(tpounds~plural+sex+fage+mage+weeks+visits+marital+racemom+hispmom+gained+smoke+Mature+Premie,data=birth))


#logistic regression part
#best subset selection of logistic regression
out <- summary(regsubsets(x=plural+sex+fage+mage+weeks
     +visits+marital+racemom+hispmom+gained+smoke+Mature+Premie, y =lowbw))
bestglm(as.data.frame(cbind(plural,sex,fage,mage,weeks,visits,marital,black,gained,smoke,Mature,lowbw)),
        IC="AIC",family = binomial(link = "logit"))
bestglm(as.data.frame(cbind(plural,sex,fage,mage,weeks,visits,marital,black,gained,smoke,Mature,lowbw)),
        IC="BIC",family = binomial(link = "logit"))
bestglm(as.data.frame(cbind(plural,sex,fage,mage,weeks,visits,marital,black,gained,smoke,Mature,lowbw)),
        IC="CV",family = binomial(link = "logit"))
glm_AIC=glm(lowbw~plural+weeks+marital+gained+smoke,data=birth,family="binomial")
glm_BIC=glm(lowbw~plural+weeks,data=birth,family="binomial")
glm_CV=glm(lowbw~weeks,family="binomial")
glm_weight=glm(lowbw~plural+sex+fage+mage+weeks+visits+marital+racemom+hispmom+gained+smoke+Mature+Premie,family="binomial")
summary(glm_weight)

#delete fage
glm_weight_m=glm(lowbw~plural+sex+mage+weeks+visits+marital+racemom+hispmom+gained+smoke+Mature+Premie,family="binomial")
summary(glm_weight_m)
#delete mage
glm_weight_f=glm(lowbw~plural+sex+fage+weeks+visits+marital+racemom+hispmom+gained+smoke+Mature+Premie,family="binomial")
summary(glm_weight_f~plural+weeks+marital+gained+smoke)

#split it into training and testing
set.seed (1)
x=sample(nrow(birth),size=floor(0.75*length(birth[,1])),replace=F)
all_number=1:nrow(birth)
y=subset(all_number,!(all_number %in% x))
train_sample=birth[x,]
test_sample=birth[y,]
head(train_sample)
train_sample$plural=as.factor(train_sample$plural)
train_sample$sex=as.factor(train_sample$sex)
train_sample$marital=as.factor(train_sample$marital)
train_sample$racemom=as.factor(train_sample$racemom)
train_sample$hispmom=as.factor(train_sample$hispmom)
train_sample$smoke=as.factor(train_sample$smoke)
train_sample$Mature=as.factor(train_sample$Mature)
train_sample$Premie=as.factor(train_sample$Premie)
test_sample$plural=as.factor(test_sample$plural)
test_sample$sex=as.factor(test_sample$sex)
test_sample$marital=as.factor(test_sample$marital)
test_sample$racemom=as.factor(test_sample$racemom)
test_sample$hispmom=as.factor(test_sample$hispmom)
test_sample$smoke=as.factor(test_sample$smoke)
test_sample$Mature=as.factor(test_sample$Mature)
test_sample$Premie=as.factor(test_sample$Premie)



#test for logistic regression
#Likelihood Ratio Test
anova(glm_AIC)
anova(glm_AIC)
anova(glm_BIC,glm_AIC)
summary(anova(glm_AIC))
deviance(glm_AIC)
1 - pchisq(deviance(glm_AIC), df.residual(glm_AIC),lower.tail = F)
1 - pchisq(deviance(glm_BIC), df.residual(glm_BIC),lower.tail = F)
#Pseudo R^2
pR2(glm_AIC)
pR2(glm_BIC)

#Hosmer-Lemeshow Test
HLgof.test(fit = fitted(glm_AIC), obs = train_sample$lowbw)
varImp(glm_weight)
order(varImp(glm_weight))
#ROC
#AIC
bestglm(as.data.frame(cbind(train_sample$plural,train_sample$sex,train_sample$fage,train_sample$mage,
train_sample$weeks,train_sample$visits,train_sample$marital,train_sample$black,train_sample$gained,
train_sample$smoke,train_sample$Mature,train_sample$lowbw)),
IC="AIC",family = binomial(link = "logit"))

glm_training_AIC=glm(lowbw~plural+weeks+marital+gained+smoke,family="binomial",data=train_sample)
prob <- predict(glm_training_AIC, newdata=test_sample, type="response") 
pred <- prediction(prob, test_sample$lowbw)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
title("ROC plot for AIC ")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
#BIC
bestglm(as.data.frame(cbind(train_sample$plural,train_sample$sex,train_sample$fage,train_sample$mage,
                            train_sample$weeks,train_sample$visits,train_sample$marital,train_sample$black,train_sample$gained,
                            train_sample$smoke,train_sample$Mature,train_sample$lowbw)),
        IC="BIC",family = binomial(link = "logit"))
glm_training_BIC=glm(lowbw~weeks,family="binomial",data=train_sample)
prob <- predict(glm_training_BIC, newdata=test_sample, type="response") 
pred <- prediction(prob, test_sample$lowbw)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
title("ROC plot for BIC ")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

# smoke_mother research------------------------------------------
smokemom=read.xlsx("G:/ม๔ัง/Columbia/study/4201/project/smoke_mother.xlsx",sheetIndex = 1)
head(smokemom)
attach(smokemom)
pairs(smokemom[,1:3],col=Smoking)
lm_smoke_length=lm(Length.in.cm~Smoking+Gestation.Period..Weeks.,data=smokemom)
lm_smoke_weight=lm(Weight.in.Grams~Smoking+Gestation.Period..Weeks.,data=smokemom)
summary(lm_smoke_weight)
plot(lm_smoke)
#length research
boxplot(Length.in.cm~Smoking)
table(Smoking)
hist(Length.in.cm[which(Smoking=="YES")])
clength <- ddply(smokemom, "Smoking", summarise, rating.mean=mean(Length.in.cm))
cgestation=ddply(smokemom, "Smoking", summarise, rating.mean=mean(Gestation.Period..Weeks.))
cweight=ddply(smokemom, "Smoking", summarise, rating.mean=mean(Weight.in.Grams))
ggplot(data=smokemom,aes(Length.in.cm,fill=Smoking))+geom_histogram(alpha = 0.5, bins=7,position = 'identity')+labs(x="Length/cm",y="Count",title="Hist of Length")+
geom_vline(data=clength, aes(xintercept=rating.mean,  colour=Smoking),linetype="dashed", size=1)

t.test(Length.in.cm~Smoking,var.equal=T)
t.test(Weight.in.Grams~Smoking,var.equal=T)
#gestation period
ggplot(data=smokemom,aes(Gestation.Period..Weeks.,fill=Smoking))+geom_histogram(alpha = 0.5, bins=7,position = 'identity')+labs(x="Gestation Period/weeks",y="Count",title="Hist of Gestation Period")+geom_vline(data=cgestation, aes(xintercept=rating.mean,  colour=Smoking),
                                                                                                                                                                                                              linetype="dashed", size=1)
ggplot(data=smokemom,aes(Smoking,Gestation.Period..Weeks.))+geom_boxplot(aes(fill=Smoking))
t.test(Gestation.Period..Weeks.~Smoking,var.equal=T) #there is no influence of smoking on gestation period
#weigt

ggplot(data=smokemom,aes(Weight.in.Grams,fill=Smoking))+geom_histogram(alpha = 0.5, bins=7,position = 'identity')+labs(x="Length/cm",y="Count",title="Hist of Weight")+
geom_vline(data=cweight, aes(xintercept=rating.mean,  colour=Smoking),linetype="dashed", size=1)

names(smokemom)
