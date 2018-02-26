#Exploratory Data Analytics
attach(X50_Startups)
library("dummies")
requireddata <- X50_Startups[c("Profit","R&D Spend","Administration","Marketing Spend")]
summary(requireddata)

#Graphical representation
hist(requireddata$Profit)
hist(requireddata$`R&D Spend`)
hist(requireddata$Administration)
hist(requireddata$`Marketing Spend`)

boxplot(requireddata$Profit, horizontal = T)
boxplot(requireddata$`R&D Spend`, horizontal = T)
boxplot(requireddata$Administration, horizontal = T)
boxplot(requireddata$`Marketing Spend`, horizontal = T)

qqnorm(requireddata$Profit);qqline(requireddata$Profit)
qqnorm(requireddata$`R&D Spend`);qqline(requireddata$`R&D Spend`)
qqnorm(requireddata$Administration);qqline(requireddata$Administration)
qqnorm(requireddata$`Marketing Spend`);qqline(requireddata$`Marketing Spend`)

#2nd moment business decisions
var(requireddata$Profit)
sdev <- sqrt(var(requireddata$Profit));show(sdev)
var(requireddata$`R&D Spend`)
sdev <- sqrt(var(requireddata$`R&D Spend`));show(sdev)
var(requireddata$Administration)
sdev <- sqrt(var(requireddata$Administration));show(sdev)
var(requireddata$`Marketing Spend`)
sdev <- sqrt(var(requireddata$`Marketing Spend`));show(sdev)
library(moments)
#3rd and 4th moment business decisions
skewness(requireddata$Profit);kurtosis(requireddata$Profit)   
skewness(requireddata$`R&D Spend`);kurtosis(requireddata$`R&D Spend`)
skewness(requireddata$Administration);kurtosis(requireddata$Administration)
skewness(requireddata$`Marketing Spend`);kurtosis(requireddata$`Marketing Spend`)

#data seems to be normal and the exploratory data analytics are done so lets moven on with our model.
  #scatter plot matrix with corelations
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(requireddata, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

#for determining pure corelation
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(requireddata))

#even partial corelation shows colinearity problem between (profit & R&D Spend)
  attach(requireddata)
  model1 <- lm(Profit~`R&D Spend`+Administration+`Marketing Spend`)
  summary(model1) 
  model2 <- lm(log(Profit)~`R&D Spend`+Administration+`Marketing Spend`)
  summary(model2)
  model3 <- lm(log(Profit)~`R&D Spend`+log(Administration)+`Marketing Spend`)  
  summary(model3)  
  model4 <- lm(sqrt(Profit)~`R&D Spend`+Administration+`Marketing Spend`)  
  summary(model4)  
  model5 <- lm(sqrt(Profit)~ sqrt(`R&D Spend`)+Administration+`Marketing Spend`)  
  summary(model5)  
  model6 <- lm((Profit^2) ~ `R&D Spend`+Administration+`Marketing Spend`)
  summary(model6)  
  model7 <- lm((Profit^2) ~ ((`R&D Spend`)^2)+Administration+`Marketing Spend`)  
  summary(model7)
  model8 <- lm((Profit) ~ ((`R&D Spend`)^2)+Administration+`Marketing Spend`)  
  summary(model8)
  model9 <- lm((Profit^3) ~ ((`R&D Spend`))+Administration+`Marketing Spend`)  
  summary(model9)
  model10 <- lm((Profit^3) ~ ((`R&D Spend`)^3)+Administration+`Marketing Spend`)  
  summary(model10)

#no matter what transformation is applied there seems to be a p value >5% so lets start deletion diagnostics
  library(mvinfluence)
  influenceIndexPlot(model1, id.n=3)
  influencePlot(model1, id.n=3)   
  ##lets delete rows (50,49,46,15)
  exp.model1<-lm(Profit~`R&D Spend`+Administration+`Marketing Spend`, data = requireddata[-c(15,46,49,50),])
  summary(exp.model1)
  exp.model2<-lm((Profit^2) ~ ((`R&D Spend`)^2)+Administration+`Marketing Spend`, data = requireddata[-c(15,46,49,50),])
  summary(exp.model2)
  exp.model3<-lm(log(Profit)~`R&D Spend`+Administration+`Marketing Spend`, data = requireddata[-c(15,46,49,50),])
  summary(exp.model3)
  exp.model4<-lm(log(Profit)~`R&D Spend`+log(Administration)+`Marketing Spend`, data = requireddata[-c(15,46,49,50),])
  summary(exp.model4)
  ###nothing seems to work so lets delete administration
  newdata <-X50_Startups[c("Profit","R&D Spend","Marketing Spend")]   
  new.model1 <- lm(Profit~`R&D Spend`+`Marketing Spend`, data = newdata[-c(15,46,49,50),])
  summary(new.model1)
#checking for colinearity problem in final model
av.plots(new.model1, id.n=2, id.cex=0.7)
#checking our assumptions
plot(new.model1)
#everything's ok so lets proceed with CI and PI
confint(new.model1,level = 0.95)
predict(new.model1, interval = "predict")    
