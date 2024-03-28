install.packages("ISLR")
install.packages("caret")
install.packages("naivebayes")
install.packages("aod")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
library(tidyr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(caret)
library(naivebayes)
library(ISLR)

setwd('C:\\Users\\toftk\\OneDrive\\Documents\\COSC460\\R')
movies<- read.csv("movies_metadata.csv")
movies=subset(movies, select=c("vote_count", "revenue", "runtime", "vote_average"))

movies<- na.omit(movies)
movies<-movies[!(movies$runtime%in% 0),]
movies<-movies[!(movies$revenue%in% 0),]
movies<-movies[!(movies$vote_count%in% 0),]
movies<-movies[!(movies$vote_average%in% 0),]


movies<-movies%>%
  mutate(Success=if_else(vote_average>=6.30, "Rating Success", "Rating Failure"))
summary(movies$Success)
table(movies$Success)
Ind<- sample(2,nrow(movies), replace=T, prob=c(0.8,0.2))
train<- movies[Ind==1,]
test<- movies[Ind==2,]
model<- naive_bayes(Success~revenue+runtime+vote_count, data=train, laplace=1, usekernel=T)
pred_df<-data.frame(Success_pred=predict(model, newdata= test), Success=test$Success, revenue=test$revenue)
naivetabl<-table(pred_df$Success_pred, test$Success)
1-sum(diag(naivetabl))/sum(naivetabl)
ggplot(pred_df, aes(Success, color=Success_pred))+geom_bar(position='dodge')+
  labs(title='Ratings and Naive Bayes Predicted Ratings' ,x='Ratings',y='Num Guessed')

glm.fit<-glm(as.factor(Success)~revenue+runtime+vote_count, data=train, family=binomial)
glm.probs<-predict(glm.fit, test, type="response")
pred_df<-pred_df%>%
  mutate(glm_pred=glm.pred<-ifelse(glm.probs>0.5, "Predicted Success", "Predicted Flop"))
attach(test)
logtabl<-table(pred_df$glm_pred, Success)
1-sum(diag(logtabl))/sum(logtabl)
ggplot(pred_df, aes(Success, color=glm_pred))+geom_bar(position='dodge')+
  labs(title='Ratings and Logistic Predicted Ratings' ,x='Ratings',y='Num Guessed')
