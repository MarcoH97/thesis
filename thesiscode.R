#### Installing and loading packages #### 
library(RedditExtractoR)
library(SentimentAnalysis)
library(quanteda)
library(tm)
library(stats)
library(tidyverse)
library(quantmod)
library(dplyr)
library(ggplot2)
library(sentimentr)
library(vrtest)
library(urca)
library(tseries)
library(forecast)
library(lmtest)
library(dynlm)
library(FinTS)
library(rugarch)
library(vars)
library(frequencyConnectedness)
library(egg)
library(ggpubr)
library(tsDyn)
library(data.table)


setwd("C:/Users/Marco/Documents/Thesis")

#### Downloading the daily closing stock prices for Intel and AMD from yahoo finance #### 
# Intel
INTC <- getSymbols("INTC", src="yahoo", from="2017-03-02", to="2022-03-06", auto.assign=FALSE)
INTC <- INTC$INTC.Close
# AMD
AMD <- getSymbols("AMD", src="yahoo", from="2017-03-02", to="2022-03-06", auto.assign=FALSE)
AMD <- AMD$AMD.Close

# Importing the files required for the models
INTCSentiment <- read.csv("C:/Users/Marco/Documents/Thesis/intelsentiment.csv")
AMD2Sentiment <-read.csv ("C:/Users/Marco/Documents/Thesis/AMDsentiment.csv")
INTC2 <- read.csv("C:/Users/Marco/Documents/Thesis/intelstock.csv")
AMD2 <- read.csv("C:/Users/Marco/Documents/Thesis/AMDstock.csv")
IntelSentiment <- read.csv("C:/Users/Marco/Documents/Thesis/TotalIntelSentiment.csv")
AMDSentiment <- read.csv("C:/Users/Marco/Documents/Thesis/TotalAMDSentiment.csv")
intelcommentdf <- fread("C:/Users/Marco/Documents/Thesis/intelcomments.csv")
amdcommentdf <- fread("C:/Users/Marco/Documents/Thesis/amdcomments.csv")



####  Adding the necessary variables to the main dataframe, removing NA variables #### 
IntelSentiment$Date <- intelcommentdf$date
IntelSentiment$Score <- intelcommentdf$score
# file saved after this
IntelSentiment <- IntelSentiment[,c(2,15,16)]
IntelSentiment <- drop_na(IntelSentiment)
IntelSentiment$Date <- as.Date(IntelSentiment$Date)
# Grouping up the SentimentGI values for each date
IntelSentiment <- aggregate(IntelSentiment['SentimentGI'], by=IntelSentiment['Date'], sum)
ts.plot(IntelSentiment$SentimentGI)


# Sentiment QDAP
IntelSentiment2 <- IntelSentiment[,c(12,15,16)]
IntelSentiment2 <- drop_na(IntelSentiment2)
IntelSentiment2$Date <- as.Date(IntelSentiment2$Date)
IntelSentiment2 <- aggregate(IntelSentiment2['SentimentQDAP'], by=IntelSentiment2['Date'], sum)
ts.plot(IntelSentiment2$SentimentQDAP)

# ScoreSentiment
IntelSentitest <- IntelSentiment[,c(15,17)]
IntelSentitest <- drop_na(IntelSentitest)
IntelSentitest$Date <- as.Date(IntelSentitest$Date)
# Grouping up the SentimentGI values for each date
IntelSentitest$SentScore <- as.numeric(IntelSentitest$SentScore)
IntelSentitest <- aggregate(IntelSentitest['SentScore'], by=IntelSentitest['Date'], sum)
ts.plot(IntelSentitest$SentScore)


INTC2 <- as.data.frame(INTC)
INTC2 <- cbind(Date=rownames(INTC2), INTC2)
rownames(INTC2) <- 1:nrow(INTC2)
colnames(INTC2)[colnames(INTC2)== 'INTC.Close'] <- 'INTC'
INTC2$Date <- as.Date(INTC2$Date)

INTCSentiment <- merge(x = INTC2, y = IntelSentiment, by = 'Date', all.x = TRUE, all.y = TRUE)
INTCSentiment <- merge(x = INTCSentiment, y = IntelSentiment2, by = 'Date', all.x = TRUE, all.y = TRUE)
INTCSentiment <- merge(x = INTCSentiment, y = IntelSentitest, by = 'Date', all.x = TRUE, all.y = TRUE)
INTCSentiment <- merge(x = INTCSentiment, y = RSent_INTC, by = 'Date', all.x = TRUE, all.y = TRUE)
INTCSentiment <- as.data.frame(INTCSentiment)
INTCSentiment <- INTCSentiment[!is.na(INTCSentiment$INTC),1:5]
INTCSentiment$Date <- as.Date(INTCSentiment$Date)
INTCSentiment[is.na(INTCSentiment)] <- 0

summary(INTCSentiment)

# SentimentR
# Intel
RSent_INTC <- sentiment_by(intelcommentdf$comment)
RSent_INTC$Date <- intelcommentdf$date
RSent_INTC <- RSent_INTC[,4:5]
RSent_INTC <- as.data.frame(RSent_INTC)
RSent_INTC <- aggregate(RSent_INTC['ave_sentiment'], by=RSent_INTC['Date'], sum)
RSent_INTC$Date <- IntelSentiment$Date

# Adding the necessary variables to the main dataframe, removing NA variables
AMDSentiment$Date <- amdcommentdf$date
AMDSentiment$Score <- amdcommentdf$score
# file saved after this
AMDSentiment <- AMDSentiment[,c(2,15,16)]
AMDSentiment <- drop_na(AMDSentiment)
AMDSentiment$Date <- as.Date(AMDSentiment$Date)
# Grouping up the SentimentGI values for each date
AMDSentiment <- aggregate(AMDSentiment['SentimentGI'], by=AMDSentiment['Date'], sum)
ts.plot(AMDSentiment$SentimentGI)



# SentimentQDAP
AMDSentiment2 <- AMDSentiment[,c(12,15,16)]
AMDSentiment2 <- drop_na(AMDSentiment2)
AMDSentiment2$Date <- as.Date(AMDSentiment2$Date)
AMDSentiment2 <- aggregate(AMDSentiment2['SentimentQDAP'], by=AMDSentiment2['Date'], sum)
ts.plot(AMDSentiment2$SentimentQDAP)

# ScoreSentiment
AMDSentitest <- AMDSentiment[,c(15,17)]
AMDSentitest <- drop_na(AMDSentitest)
AMDSentitest$Date <- as.Date(AMDSentitest$Date)
# Grouping up the SentimentGI values for each date
AMDSentitest$SentScore <- as.numeric(AMDSentitest$SentScore)
AMDSentitest <- aggregate(AMDSentitest['SentScore'], by=AMDSentitest['Date'], sum)
ts.plot(AMDSentitest$SentScore)



AMD2 <- as.data.frame(AMD)
AMD2 <- cbind(Date=rownames(AMD2), AMD2)
rownames(AMD2) <- 1:nrow(AMD2)
colnames(AMD2)[colnames(AMD2)== 'AMD.Close'] <- 'AMD'
AMD2$Date <- as.Date(AMD2$Date)


AMD2Sentiment <- merge(x = AMD2, y = AMDSentiment, by = 'Date', all.x = TRUE, all.y = TRUE)
AMD2Sentiment <- merge(x = AMD2Sentiment, y = AMDSentiment2, by = 'Date', all.x = TRUE, all.y = TRUE)
AMD2Sentiment <- merge(x = AMD2Sentiment, y = AMDSentitest, by = 'Date', all.x = TRUE, all.y = TRUE)
AMD2Sentiment <- merge(x = AMD2Sentiment, y = RSent_AMD, by = 'Date', all.x = TRUE, all.y = TRUE)
AMD2Sentiment <- as.data.frame(AMD2Sentiment)
AMD2Sentiment <- AMD2Sentiment[!is.na(AMD2Sentiment$AMD),1:5]
AMD2Sentiment$Date <- as.Date(AMD2Sentiment$Date)
AMD2Sentiment[is.na(AMD2Sentiment)] <- 0


# AMD
RSent_AMD <- sentiment_by(amdcommentdf$comment)
RSent_AMD$Date <- amdcommentdf$date
RSent_AMD <- RSent_AMD[,4:5]
RSent_AMD <- as.data.frame(RSent_AMD)
RSent_AMD <- aggregate(RSent_AMD['ave_sentiment'], by=RSent_AMD['Date'], sum)
RSent_AMD <- RSent_AMD[2:1412,]
RSent_AMD$Date <- AMDSentiment$Date


####  Model building #### 

#####  Intel #####
# Shapiro-Wilkins normality test for Intel
shapiro.test(INTC2$INTC) # H0 is rejected, time-series is not normal
# Variance test for log of Intel
Auto.VR(log(INTC2$INTC)) # H0 rejected Variance is not constant, ARCH/GARCH warranted
# ADF test 
T <- length(INTC2$INTC)
lag.max <- as.integer(12*(T/100)^0.25)
summary(ur.df(log(INTC2$INTC), type="trend", lags=lag.max, selectlags="BIC"))
#KPSS test
summary(ur.kpss(log(INTC2$INTC), type="tau", lags="long")) # Not stationary 

# Creating a new dataframe with the logdifference of Intel stock prices
INTClogdiff <- log(INTC2$INTC)
INTClogdiff <- diff(INTClogdiff)
INTClogdiff <- as.data.frame(INTClogdiff)
colnames(INTClogdiff)[colnames(INTClogdiff)=='INTClogdiff'] <- 'INTC'
INTClogdiff$Date <- INTC2$Date[2:1262]

# ADF/KPSS test again
# ADF test
T <- length(INTClogdiff$INTC)
lag.max <- as.integer(12*(T/100)^0.25)
summary(ur.df(INTClogdiff$INTC), type="trend", lags=lag.max, selectlags="BIC")
#KPSS test
summary(ur.kpss(INTClogdiff$INTC), type="tau", lags="long") # It is stationary

acf(log(INTC2$INTC))
pacf(log(INTC2$INTC))

acf(INTClogdiff$INTC)
pacf(INTClogdiff$INTC)

# ARIMA
auto.arima(INTClogdiff$INTC)

# ARIMA(1,1,3):
ARIMAINT <- Arima(INTClogdiff$INTC, order=c(1,0,3))
coeftest(ARIMAINT)
# modulus
abs(polyroot(c(1, -ARIMAINT$model$phi)))
abs(polyroot(c(1, ARIMAINT$model$theta)))
checkresiduals(ARIMAINT, lag=30)
plot(ARIMAINT)
tsdiag(ARIMAINT)
acf(ARIMAINT$residuals)
pacf(ARIMAINT$residuals)
BIC(ARIMAINT)
plot(ARIMAINT$residuals)
plot(ARIMA_AMD$residuals)



### ARCH model building for Intel
# GARCH
INTC_GARCH <- ugarchfit(ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(3, 3)), distribution.model = "sstd"), INTClogdiff$INTC)
plot(sigma(INTC_GARCH))
INTC_GARCH

plot(INTC_GARCH, which = 3)
plot(INTC_GARCH, which = 5)
plot(INTC_GARCH, which = 9)
plot(INTC_GARCH, which = 10)


#T-GARCH
INTC_TGARCH <- ugarchfit(ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", 
                                                            garchOrder = c(1, 1)),
                                     mean.model = list(armaOrder = c(3, 3)), distribution.model = "sstd"), 
                         INTClogdiff$INTC)

plot(sigma(INTC_TGARCH))
INTC_TGARCH

plot(INTC_TGARCH, which = 3)
plot(INTC_TGARCH, which = 5)
plot(INTC_TGARCH, which = 9)
plot(INTC_TGARCH, which = 10)

# Mean-GARCH
INTC_MEAN <- ugarchfit(ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1 )),
                                   mean.model = list(armaOrder = c(3, 3),
                                                      archm = TRUE, 
                                                      archpow = 2), distribution.model = "sstd"), 
                       INTClogdiff$INTC)

INTC_MEAN
plot(INTC_MEAN, which = 3)
plot(INTC_MEAN, which = 5)
plot(INTC_MEAN, which = 9)
plot(INTC_MEAN, which = 10)

# MEAN-TGARCH
INTC_TMEAN <- ugarchfit(ugarchspec(variance.model = list(model = "fGARCH", 
                                                            submodel = "TGARCH", 
                                                            garchOrder = c(1, 1)),
                                     mean.model = list( armaOrder = c(3, 3),
                                                        archm = TRUE, 
                                                        archpow = 2),
                                     distribution.model = "sstd"), 
                         INTClogdiff$INTC)


INTC_TMEAN 
plot(INTC_TMEAN , which = 3)
plot(INTC_TMEAN , which = 2)
plot(INTC_TMEAN , which = 9)
plot(INTC_TMEAN , which = 12)

# Comparing the models
# Likelihood
likelihood(INTC_GARCH)
likelihood(INTC_TGARCH)
likelihood(INTC_MEAN)
likelihood(INTC_TMEAN)

# AIC/BIC
infocriteria(INTC_GARCH)
infocriteria(INTC_TGARCH)
infocriteria(INTC_MEAN)
infocriteria(INTC_TMEAN)

# White noise test
LJTESTI <-c(NA,residuals(INTC_TGARCH,standardize=TRUE))
ArchTest(coredata(LJTESTI))
Box.test(LJTESTI, lag=30, type="Ljung-Box")
ArchTest(coredata(LJTESTI)^2)

##### AMD #####
# Shapiro-Wilkins normality test for AMD
shapiro.test(AMD2$AMD) # H0 is rejected, time-series is not normal
# Variance test for log of AMD
Auto.VR(log(AMD2$AMD)) # H0 rejected Variance is not constant, ARCH/GARCH warranted
# ADF test 
T <- length(AMD2$AMD)
lag.max <- as.integer(12*(T/100)^0.25)
summary(ur.df(log(AMD2$AMD), type="trend", lags=lag.max, selectlags="BIC"))
#KPSS test
summary(ur.kpss(log(AMD2$AMD), type="tau", lags="long")) # Not stationary 

# Creating a new dataframe with the logdifference of AMD stock prices
AMDlogdiff <- log(AMD2$AMD)
AMDlogdiff <- diff(AMDlogdiff)
AMDlogdiff <- as.data.frame(AMDlogdiff)
colnames(AMDlogdiff)[colnames(AMDlogdiff)=='AMDlogdiff'] <- 'AMD'
AMDlogdiff$Date <- AMD2$Date[2:1262]

# ADF/KPSS test again
# ADF test
T <- length(AMDlogdiff$AMD)
lag.max <- as.integer(12*(T/100)^0.25)
summary(ur.df(AMDlogdiff$AMD), type="trend", lags=lag.max, selectlags="BIC")
#KPSS test
summary(ur.kpss(AMDlogdiff$AMD), type="tau", lags="long") # It is stationary

acf(log(AMD2$AMD))
pacf(log(AMD2$AMD))

acf(AMDlogdiff$AMD)
pacf(AMDlogdiff$AMD)


# ARIMA
auto.arima(AMDlogdiff$AMD)

# ARIMA(1,1,1):
ARIMA_AMD <- Arima(AMDlogdiff$AMD, order=c(1,0,1))
coeftest(ARIMA_AMD)
# modulus
abs(polyroot(c(1, -ARIMA_AMD$model$phi)))
abs(polyroot(c(1, ARIMA_AMD$model$theta)))
checkresiduals(ARIMA_AMD, lag=30)
plot(ARIMA_AMD)
tsdiag(ARIMA_AMD)
acf(ARIMA_AMD$residuals)
pacf(ARIMA_AMD$residuals)
BIC(ARIMA_AMD)



# ARCH model building for AMD
# GARCH
AMD_GARCH <- ugarchfit(ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                   mean.model = list(armaOrder = c(1, 1)), distribution.model = "sstd"), AMDlogdiff$AMD)
plot(sigma(AMD_GARCH))
AMD_GARCH

plot(AMD_GARCH, which = 3)
plot(AMD_GARCH, which = 5)
plot(AMD_GARCH, which = 9)
plot(AMD_GARCH, which = 10)


#T-GARCH
AMD_TGARCH <- ugarchfit(ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", 
                                                          garchOrder = c(1, 1)),
                                    mean.model = list(armaOrder = c(1, 1)), distribution.model = "sstd"), 
                         AMDlogdiff$AMD)

plot(sigma(AMD_TGARCH))
AMD_TGARCH

plot(AMD_TGARCH, which = 3)
plot(AMD_TGARCH, which = 5)
plot(AMD_TGARCH, which = 9)
plot(AMD_TGARCH, which = 10)

# Mean-GARCH
AMD_MEAN <- ugarchfit(ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1 )),
                                  mean.model = list(armaOrder = c(1, 1),
                                                    archm = TRUE, 
                                                    archpow = 2), distribution.model = "sstd"), 
                       AMDlogdiff$AMD)

AMD_MEAN
plot(AMD_MEAN, which = 3)
plot(AMD_MEAN, which = 5)
plot(AMD_MEAN, which = 9)
plot(AMD_MEAN, which = 10)

# MEAN-TGARCH
AMD_TMEAN <- ugarchfit(ugarchspec(variance.model = list(model = "fGARCH", 
                                                         submodel = "TGARCH", 
                                                         garchOrder = c(1, 1)),
                                   mean.model = list( armaOrder = c(1, 1),
                                                      archm = TRUE, 
                                                      archpow = 2),
                                   distribution.model = "sstd"), 
                        AMDlogdiff$AMD)


AMD_TMEAN 
plot(AMD_TMEAN , which = 3)
plot(AMD_TMEAN , which = 6)
plot(AMD_TMEAN , which = 9)
plot(AMD_TMEAN , which = 12)

# Comparing the models
# Likelihood
likelihood(AMD_GARCH)
likelihood(AMD_TGARCH)
likelihood(AMD_MEAN)
likelihood(AMD_TMEAN)

# AIC/BIC
infocriteria(AMD_GARCH)
infocriteria(AMD_TGARCH)
infocriteria(AMD_MEAN)
infocriteria(AMD_TMEAN)

# White noise test
LJTESTA <-c(NA,sigma(AMD_TGARCH))
ArchTest(coredata(LJTESTA))
ArchTest(coredata(LJTESTA)^2)
Box.test(LJTESTA, lag=30, type="Ljung-Box")

#### Creating the dataframe for VAR model ####

INTCVOL <- as.data.frame(sigma(INTC_TGARCH))
AMDVOL <- as.data.frame(sigma(AMD_TGARCH))
colnames(INTCVOL) <- c("INTCVOL")
colnames(AMDVOL) <- c("AMDVOL")
VARdf <- cbind(INTCVOL, AMDVOL, INTCSentiment$SentimentGI[2:1262],AMD2Sentiment$SentimentGI[2:1262], INTC2$Date[2:1262])
colnames(VARdf)[colnames(VARdf)=='INTCSentiment$SentimentGI[2:1262]'] <- 'INTCSENTIMENTGI'
colnames(VARdf)[colnames(VARdf)=='AMD2Sentiment$SentimentGI[2:1262]'] <- 'AMDSENTIMENTGI'
colnames(VARdf)[colnames(VARdf)=='INTC2$Date[2:1262]'] <- 'Date'
rownames(VARdf) <- VARdf$Date
VARdf <- VARdf[,1:4]
# Creating separate dfs for Intel and AMD
VAR_INTC <- VARdf[,c(1,3)]
VAR_AMD <- VARdf[,c(2,4)]

VARdf2 <- VARdf[,c(2,1,3,4)]
#### VAR/VECM model building ####
# Testing for cointegration
jotest <- ca.jo(VARdf[,1:2], type ="eigen")
summary(jotest)
jotest2 <- ca.jo(VARdf, type = "trace")
summary(jotest2) # r=3, there is cointegration, we will use VECM model

# Selecting the appropriate lag
var_lag <- VARselect(VARdf[,1:2])
var_lag$selection # Lag = 2 will be used

# VECM model
model1 <- VECM(VARdf[,1:2], lag = 2, include = "const", estim = "ML", r = 1, exogen = VARdf[,3:4])
summary(model1)

model1VAR <- vec2var(jotest, r = 1)

model1cajo <- cajorls(jotest, r = 1)

# Testing the VECM model
vecmresid <- as.data.frame(resid(model1))
lapply(vecmresid, function(i)Box.test(i,lag=2,type = "Ljung-Box"))

# IVF
plot(irf(model1VAR, impulse = "AMDVOL", n.ahead = 2, ortho = F))

# Variance decomposition
fevd_total <- vars:: fevd(model1, n.ahead = 2)
plot(vars:: fevd(model1))


genfevd_total <- genFEVD(model1VAR, n.ahead = 2)
order_genfevd_total <- genFEVD(model1VAR , n.ahead = 2)

write_xlsx(as.data.frame(order_genfevd_total), "GENFEVD.xlsx")

# Granger causation
Granger <- grangertest(INTCVOL~INTCSENTIMENTGI,data=VARdf)
Granger
Granger2 <- grangertest(AMDVOL~AMDSENTIMENTGI,data=VARdf)
Granger2
Granger3 <- grangertest(AMDVOL~INTCSENTIMENTGI,data=VARdf)
Granger3
Granger4 <- grangertest(INTCVOL~AMDSENTIMENTGI,data=VARdf)
Granger4

Granger5 <- grangertest(INTCSENTIMENTGI~INTCVOL,data=VARdf)
Granger5
Granger6 <- grangertest(AMDSENTIMENTGI~AMDVOL,data=VARdf)
Granger6



## VAR order 2:
jotest3 <- ca.jo(VARdf2[,1:2], type ="eigen")
summary(jotest)
jotest4 <- ca.jo(VARdf2, type = "trace")
summary(jotest2) # r=3, there is cointegration, we will use VECM model

# Selecting the appropriate lag
var_lag <- VARselect(VARdf2[,1:2])
var_lag$selection # Lag = 2 will be used

# VECM model
model2 <- VECM(VARdf2[,1:2], lag = 2, include = "const", estim = "ML", r = 1, exogen = VARdf2[,3:4])
summary(model2)

model2VAR <- vec2var(jotest, r = 1)

model2cajo <- cajorls(jotest, r = 1)

# Testing the VECM model
vecmresid <- as.data.frame(resid(model2))
lapply(vecmresid, function(i)Box.test(i,lag=2,type = "Ljung-Box"))

# IVF
plot(irf(model2VAR, impulse = "AMDVOL", n.ahead = 2, ortho = F))

# Variance decomposition
fevd_total <- vars:: fevd(model2, n.ahead = 2)
plot(vars:: fevd(model2))


genfevd_total <- genFEVD(model2VAR, n.ahead = 2)
order_genfevd_total <- genFEVD(model2VAR , n.ahead = 2)

# INTC-VAR
# Testing for cointegration
jotest <- ca.jo(VAR_INTC, type ="eigen")
summary(jotest)
jotest2 <- ca.jo(VAR_INTC, type = "trace")
summary(jotest2) # r=1, there is cointegration, we will use VECM model

# Selecting the appropriate lag
var_lag <- VARselect(VAR_INTC)
var_lag$selection # Lag = 3 will be used

# VECM model
model2 <- VECM(VAR_INTC, lag = 3, include = "const", estim = "ML", r = 1)
summary(model2)

model2VAR <- vec2var(jotest, r = 1)

model2cajo <- cajorls(jotest, r = 1)

# Testing the VECM model
vecmresid <- as.data.frame(resid(model2))
lapply(vecmresid, function(i)Box.test(i,lag=3,type = "Ljung-Box"))

# IVF
plot(irf(model2, impulse = "INTCVOL", n.ahead = 3, ortho = F))

# Variance decomposition
fevd_total <- vars:: fevd(model2, n.ahead = 3)
plot(vars:: fevd(model2))

# Granger test
GrangerINTC <- grangertest(INTCSENTIMENTGI~INTCVOL,data=VARdf)
GrangerINTC
GrangerINTC2 <- grangertest(INTCVOL~INTCSENTIMENTGI,data=VARdf)
GrangerINTC2


# AMD-VAR
# Testing for cointegration
jotest <- ca.jo(VAR_AMD, type ="eigen")
summary(jotest)
jotest2 <- ca.jo(VAR_AMD, type = "trace")
summary(jotest2) # r=1, there is cointegration, we will use VECM model

# Selecting the appropriate lag
var_lag <- VARselect(VAR_AMD)
var_lag$selection # Lag = 3 will be used

# VECM model
model3 <- VECM(VAR_AMD, lag = 4, include = "const", estim = "ML", r = 1)
summary(model3)

model3VAR <- vec2var(jotest, r = 1)

model3cajo <- cajorls(jotest, r = 1)

# Testing the VECM model
vecmresid <- as.data.frame(resid(model3))
lapply(vecmresid, function(i)Box.test(i,lag=4,type = "Ljung-Box"))

# IVF
plot(irf(model3, impulse = "AMDVOL", n.ahead = 4, ortho = F))

# Variance decomposition
fevd_total <- vars:: fevd(model3, n.ahead = 4)
plot(vars:: fevd(model3))

# Granger causation
GrangerAMD <- grangertest(AMDVOL~AMDSENTIMENTGI,data=VARdf)
GrangerAMD
GrangerAMD2 <- grangertest(AMDSENTIMENTGI~AMDVOL,data=VARdf)
GrangerAMD2


#### VECM TEST ####
VARdf2 <- VARdf[550:1261,]

# Testing for cointegration
jotest <- ca.jo(VARdf2[,1:2], type ="eigen")
summary(jotest)
jotest2 <- ca.jo(VARdf2, type = "trace")
summary(jotest2) # r=2, there is cointegration, we will use VECM model

# Selecting the appropriate lag
var_lag <- VARselect(VARdf2[,1:2])
var_lag$selection # Lag = 1 will be used

# VECM model
model4 <- VECM(VARdf2[,1:2], lag = 2, include = "const", estim = "ML", r = 1, exogen = VARdf2[,3:4])
summary(model4)

model4VAR <- vec2var(jotest, r = 1)

model4cajo <- cajorls(jotest, r = 2)

# Testing the VECM model
vecmresid <- as.data.frame(resid(model4))
lapply(vecmresid, function(i)Box.test(i,lag=2,type = "Ljung-Box"))

# IVF
plot(irf(model4VAR, impulse = "INTCVOL", n.ahead = 1, ortho = F))

# Variance decomposition
fevd_total <- vars:: fevd(model4, n.ahead = 1)
plot(vars:: fevd(model4))

GrangerTest <- grangertest(INTCVOL~INTCSENTIMENTGI,data=VARdf2)
GrangerTest
GrangerTest2 <- grangertest(AMDVOL~AMDSENTIMENTGI,data=VARdf2)
GrangerTest2
GrangerTest3 <- grangertest(AMDVOL~INTCSENTIMENTGI,data=VARdf2)
GrangerTest3
GrangerTest4 <- grangertest(INTCVOL~AMDSENTIMENTGI,data=VARdf2)
GrangerTest4

GrangerTest5 <- grangertest(INTCSENTIMENTGI~INTCVOL,data=VARdf2)
GrangerTest5
GrangerTest6 <- grangertest(AMDSENTIMENTGI~AMDVOL,data=VARdf2)
GrangerTest6


#### Creating the graphs #### 
# Closing stock prices of Intel 
ggplot(data=INTC2, aes(Date,INTC)) +
  geom_line(aes(y=INTC), colour="#0071c5", size=0.5)+
  labs(title = "Daily Closing Price, Intel Corp. (INTC)",
       subtitle = "2017.03.02 - 2022.03.04",
       caption = "Source: Yahoo Finance")+
  ylab("INTC (USD)")+xlab("")+
  scale_x_date(date_breaks = "6 month" ,date_labels = "%Y %b")+
  theme_bw()+
  theme(plot.title = element_text(size = 14, hjust = 0.5 ,face = "bold"),
        plot.subtitle = element_text(size=10, hjust=0.5),
        axis.title.y = element_text(size=10, hjust = 0.5, face='bold'),
        axis.text.x = element_text(angle=45, hjust = 1))
        #panel.background = element_rect(fill="white", colour = "black"),
        #panel.grid.major = element_line(colour="grey"))

# Closing stock prices of AMD
ggplot(data=AMD2, aes(Date,AMD)) +
  geom_line(aes(y=AMD), colour="#ED1C24", size=0.5)+
  labs(title = "Daily Closing Price, Advanced Micro Devices, Inc. (AMD) ",
       subtitle = "2017.03.02 - 2022.03.04",
       caption = "Source: Yahoo Finance")+
  ylab("AMD (USD)")+xlab("")+
  scale_x_date(date_breaks = "6 month" ,date_labels = "%Y %b")+
  theme_bw()+
  theme(plot.title = element_text(size = 14, hjust = 0.5 ,face = "bold",),
        plot.subtitle = element_text(size=10, hjust=0.5),
        axis.title.y = element_text(size=10, hjust = 0.5, face="bold"),
        axis.text.x = element_text(angle=45, hjust = 1))


# Log-yield of the 2 stocks
I <- ggplot(data=INTClogdiff, aes(Date,INTC)) +
     geom_line(aes(y=INTC), colour="#0071c5", size=0.3)+
    ylab("INTC (Return)")+xlab("")+
    scale_x_date(date_breaks = "6 month" ,date_labels = "%Y %b")+
    theme_bw()+
    theme(plot.title = element_text(size = 10, hjust = 0.5 ,face = "bold"),
          plot.subtitle = element_text(size=8, hjust=0.5),
          axis.title.y = element_text(size=8, hjust = 0.5, face='bold'),
          axis.text.x = element_text(angle=45, hjust = 1))

A <- ggplot(data=AMDlogdiff, aes(Date,AMD)) +
     geom_line(aes(y=AMD), colour="#ED1C24", size=0.3)+
     labs(caption = "Source: Yahoo finance")+
     ylab("AMD (Return)")+xlab("")+
     scale_x_date(date_breaks = "6 month" ,date_labels = "%Y %b")+
     theme_bw()+
     theme(plot.title = element_text(size = 10, hjust = 0.5 ,face = "bold",),
           plot.subtitle = element_text(size=8, hjust=0.5),
           axis.title.y = element_text(size=8, hjust = 0.5, face="bold"),
           axis.text.x = element_text(angle=45, hjust = 1))

logyield <- ggarrange(I,A, nrow=2)
title <- expression(atop(bold("Logarithmic return of Intel Corp. and Advanced Micro Devices, Inc."), scriptstyle("2017.03.03 - 2022.03.04")))
annotate_figure(logyield, top=text_grob(title, color="black", face="bold", size = 14))






####  Saving the important dataframes into a csv file #### 
write.csv(INTC2, "C:/Users/Marco/Documents/Thesis/intelstock.csv")
write.csv(AMD2, "C:/Users/Marco/Documents/Thesis/AMDstock.csv")
write.csv(INTCSentiment, "C:/Users/Marco/Documents/Thesis/intelsentiment.csv")
write.csv(AMD2Sentiment, "C:/Users/Marco/Documents/Thesis/AMDsentiment.csv")
write.csv(intelcommentdf, "C:/Users/Marco/Documents/Thesis/intelcomments.csv")
write.csv(amdcommentdf, "C:/Users/Marco/Documents/Thesis/amdcomments.csv")
write.csv(IntelSentiment, "C:/Users/Marco/Documents/Thesis/TotalIntelSentiment.csv")
write.csv(AMDSentiment, "C:/Users/Marco/Documents/Thesis/TotalAMDSentiment.csv")



####  Reddit data download and SentimentAnalysing #### 

# Downloading the threads and comments from the Intel subreddit
intelthread <- find_thread_urls(subreddit = "Intel", sort_by ="top",period = "all")
intelcomments <- get_thread_content(intelthread$url)
intelcommentdf <- intelcomments$comments
intelthreaddf <- intelcomments$threads

# Downloading the threads and comments from the AMD subreddit
amdthread <- find_thread_urls(subreddit = "AMD", sort_by ="top",period = "all")
amdcomments <- get_thread_content(amdthread$url)
amdcommentdf <- amdcomments$comments
amdthreaddf <- amdcomments$threads


# Sentiment analysing the comments from r/Intel

# Due to RAM limitations I have to create several temporary dataframes
inteltemp1 <- intelcommentdf[1:40000,]

IntelCorpus1 <- Corpus(VectorSource(inteltemp1$comment))
IntelCorpus1 %>% tm_map(content_transformer(tolower)) %>% tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) -> IntelCorpus1

IntelSentiment1 <- analyzeSentiment(IntelCorpus1)

inteltemp2 <- intelcommentdf[40001:80000,]

IntelCorpus2 <- Corpus(VectorSource(inteltemp2$comment))
IntelCorpus2 %>% tm_map(content_transformer(tolower)) %>% tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) -> IntelCorpus2

IntelSentiment2 <- analyzeSentiment(IntelCorpus2)

inteltemp3 <- intelcommentdf[80001:89873,]

IntelCorpus3 <- Corpus(VectorSource(inteltemp3$comment))
IntelCorpus3 %>% tm_map(content_transformer(tolower)) %>% tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) -> IntelCorpus3

IntelSentiment3 <- analyzeSentiment(IntelCorpus3)

# Binding together the temporary dataframes
IntelSentiment <- bind_rows(IntelSentiment1, IntelSentiment2, IntelSentiment3)

# Sentiment analysing the comments from r/AMD

# Due to RAM limitations I have to create several temporary dataframes
AMDtemp1 <- amdcommentdf[1:40000,]

AMDCorpus1 <- Corpus(VectorSource(AMDtemp1$comment))
AMDCorpus1 %>% tm_map(content_transformer(tolower)) %>% tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) -> AMDCorpus1

AMDSentiment1 <- analyzeSentiment(AMDCorpus1)


AMDtemp2 <- amdcommentdf[40001:80000,]

AMDCorpus2 <- Corpus(VectorSource(AMDtemp2$comment))
AMDCorpus2 %>% tm_map(content_transformer(tolower)) %>% tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) -> AMDCorpus2

AMDSentiment2 <- analyzeSentiment(AMDCorpus2)



AMDtemp3 <- amdcommentdf[80001:120000,]

AMDCorpus3 <- Corpus(VectorSource(AMDtemp3$comment))
AMDCorpus3 %>% tm_map(content_transformer(tolower)) %>% tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) -> AMDCorpus3

AMDSentiment3 <- analyzeSentiment(AMDCorpus3)



AMDtemp4 <- amdcommentdf[120001:160000,]

AMDCorpus4 <- Corpus(VectorSource(AMDtemp4$comment))
AMDCorpus4 %>% tm_map(content_transformer(tolower)) %>% tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) -> AMDCorpus4

AMDSentiment4 <- analyzeSentiment(AMDCorpus4)


AMDtemp5 <- amdcommentdf[160001:200000,]

AMDCorpus5 <- Corpus(VectorSource(AMDtemp5$comment))
AMDCorpus5 %>% tm_map(content_transformer(tolower)) %>% tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) -> AMDCorpus5

AMDSentiment5 <- analyzeSentiment(AMDCorpus5)



AMDtemp6 <- amdcommentdf[200001:240000,]

AMDCorpus6 <- Corpus(VectorSource(AMDtemp6$comment))
AMDCorpus6 %>% tm_map(content_transformer(tolower)) %>% tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) -> AMDCorpus6

AMDSentiment6 <- analyzeSentiment(AMDCorpus6)



AMDtemp7 <- amdcommentdf[240001:280000,]

AMDCorpus7 <- Corpus(VectorSource(AMDtemp7$comment))
AMDCorpus7 %>% tm_map(content_transformer(tolower)) %>% tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) -> AMDCorpus7

AMDSentiment7 <- analyzeSentiment(AMDCorpus7)



AMDtemp8 <- amdcommentdf[280001:301435,]

AMDCorpus8 <- Corpus(VectorSource(AMDtemp8$comment))
AMDCorpus8 %>% tm_map(content_transformer(tolower)) %>% tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace) -> AMDCorpus8

AMDSentiment8 <- analyzeSentiment(AMDCorpus8)


#Binding the temporary dataframes together
AMDSentiment <-  bind_rows(AMDSentiment1, AMDSentiment2, AMDSentiment3, AMDSentiment4
                           , AMDSentiment5, AMDSentiment6, AMDSentiment7, AMDSentiment8)




# Saving the residuals of the ARIMA model
ARIMAINTRES <- ARIMAINT$residuals
ggtsdisplay(ARIMAINTRES)

# Estimating the mean equation r = beta + error
IntelMean <- dynlm(ARIMAINTRES ~ 1)
summary(IntelMean)


# Determining the ARCH effect
IntelMeanSquare <- ts(resid(IntelMean)^2)
IntelARCHef <- dynlm(IntelMeanSquare ~ L(IntelMeanSquare))
summary(IntelARCHef) # square of resid -> volatility, conditional volatility is present

# Chi-square test
t <- nobs(IntelMean)
q <- length(coef(IntelARCHef))-1
rsq <- broom::glance(IntelARCHef)[[1]]
lm <- (t-q)*rsq
alpha <- 0.05
chicr <- qchisq(1-alpha, q)
lm
chicr

# H0 rejected, there is an ARCH effect
IntelARCHef1 <- ArchTest(ARIMAINTRES, lags = 1, demean = TRUE)
IntelARCHef1

# GARCH(1), ARCH(1)
INTC_GARCH <- garch(ARIMAINTRES,c(1,1), control = garch.control(maxiter=500,grad="numerical"))
summary(INTC_GARCH)

# Estimating the volatility
INTCVOL <- ts(2*INTC_GARCH$fitted.values[-1,1]^2)
plot.ts(INTCVOL)
INTC_GARCH$fitted.values


# Saving the residuals of the ARIMA model
ARIMA_AMDRES <- ARIMA_AMD$residuals
ggtsdisplay(ARIMA_AMDRES)

# Estimating the mean equation r = beta + error
AMDMean <- dynlm(ARIMA_AMDRES ~ 1)
summary(AMDMean)


# Determining the ARCH effect
AMDMeanSquare <- ts(resid(AMDMean)^2)
AMDARCHef <- dynlm(AMDMeanSquare ~ L(AMDMeanSquare))
summary(AMDARCHef) # square of resid -> volatility, conditional volatility is present

# Chi-square test
t <- nobs(AMDMean)
q <- length(coef(AMDARCHef))-1
rsq <- broom::glance(AMDARCHef)[[1]]
lm <- (t-q)*rsq
alpha <- 0.05
chicr <- qchisq(1-alpha, q)
lm
chicr

# H0 rejected, there is an ARCH effect
AMDARCHef1 <- ArchTest(ARIMA_AMDRES, lags = 1, demean = TRUE)
AMDARCHef1


# GARCH(0), ARCH(2)
AMD_GARCH <- garch(ARIMA_AMDRES,c(0,2), control = garch.control(maxiter=500,grad="numerical"))
summary(AMD_GARCH)

# Estimating the volatility
AMDVOL <- ts(2*AMD_GARCH$fitted.values[-1,1]^2)
plot.ts(AMDVOL)
AMD_GARCH$fitted.values
