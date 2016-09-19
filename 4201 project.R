#4201 project
movie_finacial=read.csv("G:/留学/Columbia/study/4201/project/data/movie_financial_summaries.csv")
#average_box_office=tapply(movie_finacial[,"movie_annual_summary_domestic_box_office"],movie_finacial[,"movie_annual_summary_year"],fun=mean)
#length(average_box_office)
library(dplyr)
library(doBy)
library(ggplot2)
library(xlsx)
library(stats)
grp=group_by(movie_finacial,movie_annual_summary_year)
average_box_office=summarise(grp, mean=mean(movie_annual_summary_domestic_box_office))
average_box_office=average_box_office[1:83,]
plot(log(mean)~(movie_annual_summary_year),data=average_box_office,type="l")
qplot((movie_annual_summary_year),log(mean),data=average_box_office,geom=c("point","line"),col="red")


#cleaning data

movie1=read.csv("G:/留学/Columbia/study/4201/project/data/1902~2005_part1.csv")
movie2=read.csv("G:/留学/Columbia/study/4201/project/data/2006~2012_part1.csv")
movie3=read.csv("G:/留学/Columbia/study/4201/project/data/2013~part1.csv")

movie4=read.csv("G:/留学/Columbia/study/4201/project/data/rating&budget_part1-2.csv")
movie5=read.csv("G:/留学/Columbia/study/4201/project/data/rating&budget_part2-2.csv")
movie6=read.csv("G:/留学/Columbia/study/4201/project/data/rating&budget_part3-2.csv")
data2=rbind(movie4,movie5,movie6)
colnames(data2)=c("name","year","international","budget","rating")
colnames(movie3)=colnames(movie1)

data=rbind(movie1,movie2,movie3)
dat=data[-which(is.na(data$movie_annual_summary_inflation_adjusted_domestic_box_office)),]
dat=dat[,-c(4,8,14,15)]
colnames(dat)=c("name","year","type","ticket","boxoffice","Adj_boxoffice","sequel","time","source","method","genre","blueray")

#matching data1 and data2
y=merge(dat,data2,by="name",all.x=T)
dat[,c(13,14,15)]=data2[match(dat$name,data2$name),]

yy=cbind(dat, data2[,"international"][match(dat$name, data2$name)])
yy=cbind(yy, data2[,"budget"][match(yy$name, data2$name)])
yy=cbind(yy, data2[,"rating"][match(yy$name, data2$name)])
colnames(yy)[c(13,14,15)]=c("international","budget","rating")
yy2=yy
write.csv(yy,"G:/留学/Columbia/study/4201/project/data/data_all.csv")


#cleaned full data
cleaned=read.xlsx("G:/留学/Columbia/study/4201/project/data/data_all.xlsx",sheetName="Sheet1")


attach(yy2)
detach(yy2)
names(yy2)
boxplot(log(Adj_boxoffice)~genre)
boxplot(log(Adj_boxoffice)~type)
boxplot(log(Adj_boxoffice)~source)
lm_year=lm(Adj_boxoffice~year)
boxplot(log(Adj_boxoffice)~method)
boxplot(log(Adj_boxoffice)~sequel)
boxplot(log(Adj_boxoffice)~rating)
plot(lm(log2(Adj_boxoffice)~as.numeric(budget)))
summary(lm(log(Adj_boxoffice)~budget))
lm(Adj_boxoffice~budget)
ggplot(data=yy2,aes(x=budget,y=log(Adj_boxoffice)))+geom_point()
#descriptive data
hist(log(Adj_boxoffice),breaks=500)
pairwise.t.test(genre,log(Adj_boxoffice))

#clean the final data without all NA
yy3=yy2[complete.cases(yy2),]

data_1014=read.csv("G:/留学/Columbia/study/4201/project/production2010to2014_total.csv")
names(data_1014)
head(data_1014)
data_10142=data_1014[complete.cases(data_1014),]
ggplot(data=NULL,aes(log(data_1014$annual_box_office/data_1014$MAX_theaters)))+geom_histogram(aes(y =..density..,fill=..density..),alpha = 0.5, 
            position = 'identity')+labs(title="Histogram for Average Box Office per theater")+geom_density(col=2)
+labs(x="Box Office",y="Frequency")

ggplot(data=NULL,aes(sample=log(data_1014$annual_box_office/data_1014$MAX_theaters)))+stat_qq()+labs(title="Q-Q plot for average box office")



hist(log(annual_box_office/MAX_theaters))
#colnames(data_1014)=c("movie","open weekend revenue","# theaters","maximum_theater","boxoffice")
attach(data_1014)
#hist(log(boxoffice/`maximum theater`))
#shapiro.test(log(boxoffice/`maximum theater`))#test for normality
qqnorm(log(annual_box_office/MAX_theaters))
summary(lm(log(annual_box_office/MAX_theaters)~genre+rating+running_time))
detach(data_1014)
boxplot(log(annual_box_office/MAX_theaters)~rating)

