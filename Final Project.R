library(readr)
library(alr3)
library(leaps)

top2018 <- read_csv("top2018.csv")
Top<-top2018[,c(4:14)]
model1<-lm(danceability~.,data=Top)
summary(model1)
plot(model1)

hat1<-hatvalues(model1)
print(2*(10+1)/100)
high1<-which(hat1>0.22)
print(high1)
r1<-rstandard(model1)
bad1<-which(abs(r1)>2)
print(bad1)

best<-regsubsets(danceability~.,data=Top,nvmax=9)
plot(summary(best)$bic)
summary(best)
aic1<-summary(best)$bic-(log(100)-2)*(1:9)
plot(1:9,aic1)
lines(1:9,aic1)
#AIC, 5 predictors: Energy, speechiness, acousticness, valence, and tempo.
#BIC, 4 predictors: Energy, speechiness, acousticness, and valence.

backward<-regsubsets(danceability~.,data=Top,method="backward",nvmax=9)
plot(summary(backward)$bic)
summary(backward)
aic2<-summary(backward)$bic-(log(100)-2)*(1:9)
plot(1:9,aic2)
lines(1:9,aic2)
#AIC, 5 predictors: Energy, speechiness, acousticness, valence, and tempo.
#BIC, 4 predictors: Energy, speechiness, acousticness, and valence.

forward<-regsubsets(danceability~.,data=Top,method="forward",nvmax=9)
plot(summary(forward)$bic)
summary(forward)
aic3<-summary(forward)$bic-(log(100)-2)*(1:9)
plot(1:9,aic3)
lines(1:9,aic3)
#AIC, 5 predictors: Energy, speechiness, acousticness, valence, and tempo.
#BIC, 4 predictors: Energy, speechiness, acousticness, and valence.

model2<-lm(danceability~energy+speechiness+acousticness+valence+tempo,data=Top)
model3<-lm(danceability~energy+speechiness+acousticness+valence,data=Top)
anova(model2,model3)

summary(model2)
plot(model2)
hat2<-hatvalues(model2)
print(2*(5+1)/100)
high2<-which(hat2>0.12)
print(high2)
r2<-rstandard(model2)
bad2<-which(abs(r2)>2)
print(bad2)
vif(model2)
mmps(model2)
invResPlot(model2)
summary(powerTransform(danceability~energy+speechiness+acousticness+valence+tempo,data=Top))
summary(powerTransform(cbind(Top$energy,Top$speechiness,Top$acousticness,Top$valence,Top$tempo)~1))

tenergy<-Top$energy^2
tspeechiness<-Top$speechiness^(-0.5)
tacousticness<-Top$acousticness^(0.33)
ttempo<-log(Top$tempo)
model4<-lm((danceability)^2~tenergy+tspeechiness+tacousticness+valence+ttempo,data=Top)
summary(model4)

model5<-lm((danceability)^2~tenergy+tspeechiness+tacousticness+valence,data=Top)
summary(model5)
plot(model5)
hat3<-hatvalues(model5)
print(2*(4+1)/100)
high3<-which(hat3>0.1)
print(high3)
r3<-rstandard(model5)
bad3<-which(abs(r3)>2)
print(bad3)
vif(model5)
mmps(model5)
mmp(model5)
