# attach relevant libraries
library(httr)
library(jsonlite)
get1<-GET("https://s3-us-west-1.amazonaws.com/riot-developer-portal/seed-data/matches10.json")
alljson1<-fromJSON(content(get1, "text", encoding = "UTF-8"))
# get 100 chunks
lol <- data.frame(win=integer(),
                  kills=integer(),
                  deaths=integer(),
                  assists=integer(),
                  goldEarned=double(),
                  longestTimeSpentLiving=double(),
                  largestMultiKill=integer())

for (i in 1:100) { 
  this.row.i<-data.frame(win=ifelse(alljson1$matches$participants[[i]]$stats$win==TRUE,1,0),
                        kills=alljson1$matches$participants[[i]]$stats$kills,
                        deaths=alljson1$matches$participants[[i]]$stats$deaths,
                        assists=alljson1$matches$participants[[i]]$stats$assists,
                        goldEarned=alljson1$matches$participants[[i]]$stats$goldEarned,
                        longestTimeSpentLiving=alljson1$matches$participants[[i]]$stats$longestTimeSpentLiving,
                        largestMultiKill=alljson1$matches$participants[[i]]$stats$largestMultiKill)
  
# add condition: lol gets first iteration, binds to following iterations
  if (i == 1) {
    lol <- this.row.i
  } else {
    lol <- rbind(lol, this.row.i)
  }
}

# ensure data read in nicely
str(lol)
tail(lol)

#create boxplot of total 'skill score' vs wins
#boxplot(kills + deaths + assists + goldEarned + longestTimeSpentLiving + largestMultiKill ~ win, data = lol,
#        xlab = 'Win or no win', ylab = 'Skill score', main = 'League of Legends player skill score vs. Wins')

# whoops, I was supposed to create separate boxplots...now I understand
boxplot(kills ~ win, data = lol, main = 'Kills vs Wins')
boxplot(deaths ~ win, data = lol, main = 'Deaths vs Wins')
boxplot(assists ~ win, data = lol, main = 'Assists vs Wins')
boxplot(goldEarned ~ win, data = lol, main = 'Gold Earned vs Wins')
boxplot(longestTimeSpentLiving ~ win, data = lol, main = 'Longest Time Spent Living vs Wins')
boxplot(largestMultiKill ~ win, data = lol, main = 'Largest Multikill vs Wins')

# remove obs 175, 322, 374, 526, 792 and any obs with Kills >=20 and Deaths >= 15 (SHOULD BE 989 OBS)
lol <- subset(lol, kills < 20)
#str(lol)
lol <- subset(lol, deaths < 15)
#str(lol)
lol <- lol[-c(175, 322, 374, 526, 792),]
str(lol)

# response variable win = 1, loss = 0

# explanatory variables:
# offense components:  kills, gold earned
# 'error' components:  deaths
# team play components:  assists
# risk/reward components:  longest time spent living
# 'hot hand' components:  largest multikill

# create train and test datasets
set.seed(58)
train_ind <- sample(989, 700)

lol_train <- lol[train_ind,]
lol_test <- lol[-train_ind,]

# confirm train and test are similar
summary(lol_train)
summary(lol_test)

# Analysis

# FIT MODEL
# log( P(Win) / P(Loss))
#   = beta0 + beta1 * kills + beta2 * deaths + beta3 * assists + 
#      beta4 * goldEarned + beta5 * longestTimesSpentLiving + beta6 * largestMultiKill

lol_out <- glm(win ~ kills + deaths + assists + goldEarned + longestTimeSpentLiving + largestMultiKill,
               data = lol_train, 
               family = "binomial")

summary(lol_out)
#for some reason my nummbers are different even though I set the same seed..?

#Coeffiecients are close, though

#For each additional kill, *holding all else constant*, we estimate an
#   increase of 0.06 in the log odds ratio of winning

#change in odds interpretation

#exponentiating y-intercept coefficient has no interpretation
exp(coef(lol_out)[-1])
#the '-1' excludes the y-intercept

#***************************************************************
#Remember that with exponentiating transformations, the mu = 1.

#Therefore increasing coefficients are greater than 1, and
#   and decreasing coefficients are less than 1
#***************************************************************

#Interpretation:

#For each additional kill, *holding all else constant*, we estimate a
#   6% increase in the odds of winning

#For each additional death, *holding all else constant*, we estimate a
#   53% decrease in the odds of winning

#NOW, using the coefficients table from before, we can still infer significance

#There is no statistically significant kills effect on wins (p-value=0.278)
#There is a statistically significant deaths effect on wins (p-value < 0.0001)

#Confidence Intervals
#95% CI on log odds
confint(lol_out)

#95% CI on change in odds
exp(confint(lol_out)[-1,])

#kills CI: (-3%, +17%)

#GRAPHICS of effects
# kills

par(mfrow=c(2,2))
#in terms of log odds
x_star <- seq(0,10,length=100)
plot(x_star,  coef(lol_out)[2]*x_star,type="l",
     xlab="Kills", ylab="Partial logit(Win)")  #partial log odds of win

x_star<- seq(0,15,length=100)
plot(x_star, coef(lol_out)[3]*x_star,type="l",
     xlab="Deaths", ylab="Partial logit(Win)")
#par(mfrow=c(1,1))

#add for assists and gold earned

x_star <- seq(0,30,length=100)
plot(x_star, coef(lol_out)[4]*x_star,type="l",
     xlab="Assists", ylab="Partial logit(Win)")

x_star <- seq(0,25000,length=100)
plot(x_star, coef(lol_out)[5]*x_star, type="l",
     xlab="Gold Earned", ylab="Partial logit(Win)")

#I wonder if you can overlay plots using ggplot..?

#From a probability perspective
#demonstrate effect of kills
#set all other expl. vars to the median

#check medians
summary(lol)

x_star <- data.frame(goldEarned = seq(5000,17500,length=100),
                     kills=5, deaths=5, assists=7,
                     longestTimeSpentLiving=600, largestMultiKill=1)

#par(mfrow)

plot(x_star$kills,predict(lol_out,newdata=x_star,type="response"),
     type = "l", xlab = "Kills", ylab= "P(Win)", ylim=c(0,1))

#had to change x_star above for this plot
plot(x_star$goldEarned,predict(lol_out,newdata=x_star,type="response"),
     type = "l", xlab = "Gold Earned", ylab= "P(Win)", ylim=c(0,1))

#summarize statistical significance of model factors
#use sig. tests or 95% conf intervals
summary(lol_out)

#Test Ho: no effect on winning for an aggressive strategy
#     Ho: no kills or largestMultiKill or goldEarned

lol_reduced<- glm(win ~ deaths + assists + longestTimeSpentLiving,
                  data = lol_train, 
                  family = "binomial")

anova(lol_reduced,lol_out,test='Chisq')
#reject Ho; there is an effect on winning for aggressive strategy

#Predict P(Win) for a player with Faker (very skilled player)-like skills
predict(lol_out,newdata=data.frame(kills=2,goldEarned=15000,deaths=2,assists=8,
                                   longestTimeSpentLiving=600, largestMultiKill=2), 
        type="response")
#very high probability

#95% conf int on P(Win)
Faker_logit<-predict(lol_out,newdata=data.frame(kills=2,goldEarned=15000,deaths=2,assists=8,
                                                longestTimeSpentLiving=600, largestMultiKill=2), 
                     type="link",se.fit=TRUE)

Faker_logit

# 95% CI on logit(Win)
logit_L <- Faker_logit$fit - 1.96*Faker_logit$se.fit
logit_U <- Faker_logit$fit + 1.96*Faker_logit$se.fit

#transform logit to probability
Faker_phat_L <- exp(logit_L)/(1+exp(logit_L))
Faker_phat_U <- exp(logit_U)/(1+exp(logit_U))

#construct ROC (receiver operator characterisic) curve
library(ROCR)
train_pred<-prediction(predict(lol_out, type="response"), lol_train$win)
train_perf<-performance(train_pred,measure="tpr",x.measure="fpr")
plot(train_perf,xlab="1-specificity",ylab="sensitivity",main="ROC curve")
abline(0,1,col='grey')

test_pred<-prediction(predict(lol_out,newdata=lol_test,type="response"), lol_test$win)
#we expect test data to be worse; closer to 50/50 line on plot
test_perf<-performance(test_pred,measure="tpr",x.measure = "fpr")
plot(test_perf,add=TRUE,col="dodgerblue")

# AUC - area under curve
performance(train_pred,measure="auc")
performance(test_pred,measure="auc")

