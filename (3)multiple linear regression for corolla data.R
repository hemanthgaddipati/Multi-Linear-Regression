attach(ToyotaCorolla)
summary(ToyotaCorolla)

#Exploratory Data Analytics
requiredcolumns <- ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
summary(requiredcolumns)

  # Graphical exploration
    hist(requiredcolumns$Price)
    hist(requiredcolumns$Age_08_04)
    hist(requiredcolumns$KM)
    hist(requiredcolumns$HP)
    hist(requiredcolumns$cc)
    hist(requiredcolumns$Weight)
    hist(requiredcolumns$Doors)
    hist(requiredcolumns$Gears)
    hist(requiredcolumns$Quarterly_Tax)
    
    boxplot(requiredcolumns$Price, horizontal = T)
    boxplot(requiredcolumns$Age_08_04, horizontal = T)
    boxplot(requiredcolumns$KM, horizontal = T)
    boxplot(requiredcolumns$HP, horizontal = T)
    boxplot(requiredcolumns$cc, horizontal = T)
    boxplot(requiredcolumns$Doors, horizontal = T)
    boxplot(requiredcolumns$Gears, horizontal = T)
    boxplot(requiredcolumns$Quarterly_Tax, horizontal = T)
    boxplot(requiredcolumns$Weight, horizontal = T)
    
    qqnorm(requiredcolumns$Price);qqline(requiredcolumns$Price)
    qqnorm(requiredcolumns$Age_08_04); qqline(requiredcolumns$Age_08_04)
    qqnorm(requiredcolumns$KM);qqline(requiredcolumns$KM)
    qqnorm(requiredcolumns$HP);qqline(requiredcolumns$HP)
    qqnorm(requiredcolumns$cc);qqline(requiredcolumns$cc)
    qqnorm(requiredcolumns$Weight);qqline(requiredcolumns$Weight)
    qqnorm(requiredcolumns$Doors); qqline(requiredcolumns$Doors)
    qqnorm(requiredcolumns$Gears);qqline(requiredcolumns$Gears)
    qqnorm(requiredcolumns$Quarterly_Tax);qqline(requiredcolumns$Quarterly_Tax)
 
    
   #2nd moment business decisions
    var(requiredcolumns$Price)
    sdev_price <- sqrt(var(requiredcolumns$Price));show(sdev_price)
    var(requiredcolumns$Age_08_04)
    sdev_age <- sqrt(var(requiredcolumns$Age_08_04));show(sdev_age)
    var(requiredcolumns$KM)
    sdev_km <- sqrt(var(requiredcolumns$KM));show(sdev_km)
    var(requiredcolumns$HP)
    sdev_hp <- sqrt(var(requiredcolumns$HP));show(sdev_hp)
    var(requiredcolumns$cc)
    sdev_cc <- sqrt(var(requiredcolumns$cc));show(sdev_cc)
    var(requiredcolumns$Doors)
    sdev_doors <- sqrt(var(requiredcolumns$Doors));show(sdev_doors)
    var(requiredcolumns$Gears)
    sdev_gears <- sqrt(var(requiredcolumns$Gears));show(sdev_gears)
    var(requiredcolumns$Quarterly_Tax)
    sdev_qtax <- sqrt(var(requiredcolumns$Quarterly_Tax));show(sdev_qtax)
    var(requiredcolumns$Weight)
    sdev_weight <- sqrt(var(requiredcolumns$Weight));show(sdev_weight)
    
  # 3rd and 4th moment business decisions
    
    skewness(requiredcolumns$Price)
    kurtosis(requiredcolumns$Price)
    skewness(requiredcolumns$Age_08_04)
    kurtosis(requiredcolumns$Age_08_04)
    skewness(requiredcolumns$KM)
    kurtosis(requiredcolumns$KM)
    skewness(requiredcolumns$HP)
    kurtosis(requiredcolumns$HP)
    skewness(requiredcolumns$cc)
    kurtosis(requiredcolumns$cc)
    skewness(requiredcolumns$Doors)
    kurtosis(requiredcolumns$Doors)
    skewness(requiredcolumns$Gears)
    kurtosis(requiredcolumns$Gears)
    skewness(requiredcolumns$Quarterly_Tax)
    kurtosis(requiredcolumns$Quarterly_Tax)
    skewness(requiredcolumns$Weight)
    kurtosis(requiredcolumns$Weight)
  
#data seems to be in order so lets proceed with the regression model
    
    #Scatter plot matrix with Correlations inserted in graph
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
    pairs(requiredcolumns, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")
    
    #for determining pure corelation
    install.packages("corpcor")
    library(corpcor)
    cor2pcor(cor(requiredcolumns))
    
#there seems to be no colinearity problem so let's build our model
    attach(requiredcolumns)
    model1 <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
    summary(model1)    
  ##The p values of cc and doors is high so lets check influencers 
    # Deletion Diagnostics for identifying influential variable
        library(mvinfluence)
    influenceIndexPlot(model1, id.n=3)
    influencePlot(model1, id.n=3) 
  #According to the diagnostic plots row 81,961,222,602 are the influencers so lets delete these rows and plot the model
    final.model<-lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = requiredcolumns[-c(81,961,222,602),])
    summary(final.model)
  #checking for colinearity problem in final model
    av.plots(final.model, id.n=2, id.cex=0.7)
  #checking our assumptions
    plot(final.model)
  #everything's ok so lets proceed with CI and PI
    confint(final.model,level = 0.95)
    predict(final.model, interval = "predict")    

      
    
    