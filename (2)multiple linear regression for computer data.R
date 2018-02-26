mydata <- Computer_Data[c("price","hd","speed","ram","screen","alt_cd","alt_multi","alt_prem","ads","trend")]
attach(mydata)
#exploratory data analytics
summary(mydata)
##graphical representation
  #histograms
  hist(mydata$price)
  hist(mydata$hd)
  hist(mydata$speed)
  hist(ram)
  hist(screen)
  hist(alt_cd)
  hist(alt_multi)
  hist(alt_prem)
  hist(ads)
  hist(trend)
  #boxplots
  boxplot(price)
  boxplot(hd)
  boxplot(speed)
  boxplot(ram)
  boxplot(screen)
  boxplot(alt_cd)
  boxplot(alt_multi)
  boxplot(alt_prem)
  boxplot(ads)
  boxplot(trend)
  #normal q-q plot
  qqnorm(price);qqline(price)
  qqnorm(hd);qqline(hd)  
  qqnorm(speed);qqline(speed)
  qqnorm(ram);qqline(ram)
  qqnorm(screen);qqline(screen)
  qqnorm(alt_cd);qqline(alt_cd)
  qqnorm(alt_multi);qqline(alt_multi)
  qqnorm(alt_prem);qqline(alt_prem)
  qqnorm(ads);qqline(ads)
  qqnorm(trend);qqline(trend)
##2nd moment Business decisions
  var(price);sd(price)
  var(hd);sd(hd)
  var(speed);sd(speed)
  var(ram);sd(ram)
  var(screen);sd(screen)
  var(alt_cd);sd(alt_cd)
  var(alt_multi);sd(alt_multi)
  var(alt_prem);sd(alt_prem)
  var(ads);sd(ads)
  var(trend);sd(trend)
##3rd and 4th moment business decisons
  library(moments)
  skewness(price);kurtosis(price)
  skewness(hd);kurtosis(hd)
  skewness(speed);kurtosis(speed)
  skewness(ram);kurtosis(ram)  
  skewness(screen);kurtosis(screen)
  skewness(alt_cd);kurtosis(alt_cd)
  skewness(alt_multi);kurtosis(alt_multi)
  skewness(alt_prem);kurtosis(alt_prem)
  skewness(ads);kurtosis(ads)
  skewness(trend);kurtosis(trend)
#data seems to be normal and in order so lets build the regression model
  #scatterplot with corelations
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
  pairs(mydata, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")
  #lets determine the pure corelation between the attributes
  library(corpcor)
  cor2pcor(cor(mydata))  
#no colinearity so lets build the model
  model1 <- lm(price~hd+speed+ram+screen+alt_cd+alt_multi+alt_prem+ads+trend)
  summary(model1)  
  model2 <- lm(sqrt(price) ~ log(hd)+speed+ram+screen+alt_cd+alt_multi+alt_prem+log(ads)+trend)
  summary(model2)  
#checking for influencing values
  library(mvinfluence)
  influenceIndexPlot(model2,id.n=3)  
  influencePlot(model2,id.n=3)  

  final.model<-lm(sqrt(price) ~ log(hd)+speed+ram+screen+alt_cd+alt_multi+alt_prem+log(ads)+trend, data = mydata[-c(28,80,1441,1701),])
  summary(final.model)
  #checking for colinearity problem in final model
  av.plots(final.model, id.n=2, id.cex=0.7)
  #checking our assumptions
  plot(final.model)
  #everything's ok so lets proceed with CI and PI
  confint(final.model,level = 0.95)
  predict(final.model, interval = "predict")    
  