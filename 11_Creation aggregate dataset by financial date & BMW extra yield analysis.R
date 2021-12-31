#Load Libraries
library(xgboost)
library(ROSE)
library(caret)
library(tidyverse)
library(e1071)
library(quantmod)
library(stringr)
library(glmnet)
library(randomForest)
library(tree)
library(xts)
library(rmgarch)
library(parallel)
library(forecast)

#Function to aggregate dataset by financial date
aggregadate <- function(x){
  
  y <- matrix(nrow=0,ncol=ncol(x))
  colnames(y) <- colnames(x)  
  i_agg_r<- vector()
  i_agg<- rep(F, nrow(x))
  
  for (i in 1:nrow(x)) {
    if (i ==1) {
      i_agg[i] <- ifelse(is.na(x[i,26])==T ,T,F )
    } else i_agg[i] <- ifelse(is.na(x[i,26])==T |is.na(x[i-1,26])==T ,T,F )
    
    if ( (is.na(x[i-1,26])==T && is.na(x[i,26])==F) ) { 
      y <- rbind(y, cbind(rbind(apply(x[i_agg,-26],2,mean)),rend=x[i,26])) 
      rownames(y)[nrow(y)]<- rownames(x)[i]
      i_agg_r <- c(i_agg_r, i_agg[(length(i_agg_r)+1):i])
      i_agg<- rep(F, nrow(x))
    }
  }
  i_agg_r<- c(i_agg_r,rep(F,i-length(i_agg_r)))
  return(list(y=y,
              i_agg_r=i_agg_r,
              i_agg=i_agg))
}

#Function for create date
crea_data <- function (x) paste(str_sub(x,1,4),"-",sep="",str_sub(x,5,6),"-",str_sub(x,7,8))

#Create dataset aggregate by financial date
#Extract BMW prices  training set 
x1 <- getSymbols("BMW.DE", 
                 from="2018-12-28",
                 to="2020-01-01", 
                 auto.assign = F)

#yield calculation training set
ret1 <- as.numeric(ClCl(x1))[-1]
ret_day_1<-((x1[-1,6])-as.numeric(x1[-length(x1[,6]),6]))/as.numeric(x1[-length(x1[,6]),6])
ret1 <- as.data.frame(ret1)
row.names(ret1) <- index(ret_day_1) 

#Extract CARZ prices training set
xCarz <- getSymbols("CARZ", 
                    from="2018-12-28",
                    to="2020-01-01", 
                    auto.assign = F)

#CARZ yields calculation training set
retC <- as.numeric(ClCl(xCarz))[-1]
ret_day_C<-((xCarz[-1,6])-as.numeric(xCarz[-length(xCarz[,6]),6]))/as.numeric(xCarz[-length(xCarz[,6]),6])
retC <- as.data.frame(retC)
row.names(retC) <- index(ret_day_C) 

#BMW extra yields calculation training set
indC<-match(row.names(ret1),row.names(retC))
table(rownames(ret1)==rownames(retC)[indC],useNA = "always")
exret <- ret1 - retC[indC,]
na_exret <- rownames(exret)[is.na(exret)]
ro_exret <- rownames(exret)[is.na(exret)==F]
exret<- as.data.frame(exret[is.na(exret)==F])
rownames(exret)<- ro_exret
ret <- exret


#Load training dataset whit information of BMW Tweets (output 7_Create aggregate dataset by day.R)
load("####") 
DSagg <- DSagg_giorno  
rownames(DSagg) <- apply(as.data.frame(DSagg[,1]),2, crea_data )
DSagg<- DSagg[,-1]
DSagg<- DSagg[order(rownames(DSagg),decreasing = F),]
DSagg <-DSagg[-1,]

#Union whit extra yield
ind <- match(rownames(DSagg),rownames(ret))
ind2 <- ifelse(is.na(ind)==T,T,F)
DSagg_rend<-cbind(DSagg, rend=as.numeric(ret[ind,]))

#Extract BMW prices test set
xTest1 <- getSymbols("BMW.DE", 
                 from="2019-12-30",
                 to="2020-03-31", 
                 auto.assign = F)

#BMW yields calculation test set
retTest1 <- as.numeric(ClCl(xTest1))[-1]
ret_day_T1<-((xTest1[-1,6])-as.numeric(xTest1[-length(xTest1[,6]),6]))/as.numeric(xTest1[-length(xTest1[,6]),6])
retTest1 <- as.data.frame(retTest1)
row.names(retTest1) <- index(ret_day_T1) 

#Extract CARZ prices test set
xTest1C <- getSymbols("CARZ", 
                     from="2019-12-30",
                     to="2020-03-31", 
                     auto.assign = F)

##CARZ yields calculation test set
retTest1C <- as.numeric(ClCl(xTest1C))[-1]
ret_day_T1C<-((xTest1C[-1,6])-as.numeric(xTest1C[-length(xTest1C[,6]),6]))/as.numeric(xTest1C[-length(xTest1C[,6]),6])
retTest1C <- as.data.frame(retTest1C)
row.names(retTest1C) <- index(ret_day_T1C) 

#BMW extra yields calculation test set
indC1<-match(row.names(retTest1),row.names(retTest1C))
exretT <- retTest1 - retTest1C[indC1,]
na_exretT <- rownames(exretT)[is.na(exretT)]
ro_exretT <- rownames(exretT)[is.na(exretT)==F]
exretT<- as.data.frame(exretT[is.na(exretT)==F])
rownames(exretT)<- ro_exretT
retTest <- exretT

#Load test dataset whit information of BMW Tweets (output 7_Create aggregate dataset by day.R)
load("####") 
DSaggTest <- DSagg_giorno[1:91,]  
rownames(DSaggTest) <- apply(as.data.frame(DSaggTest[,1]),2, crea_data )
DSaggTest<- DSaggTest[,-1]
DSaggTest<- DSaggTest[order(rownames(DSaggTest),decreasing = F),]

#Union whit extra yield
ind_t <- match(rownames(DSaggTest),rownames(retTest))
ind2_t <- ifelse(is.na(ind_t)==T,T,F)
DSaggT_rend<-cbind(DSaggTest, rend=as.numeric(retTest[ind_t,]))

#Union Training set whit test set
DSaggTot_rend <- rbind(DSagg_rend,DSaggT_rend)

#Aggregate dataset by financial date
DS_gf1T<-aggregadate(DSaggTot_rend)$y
ind_agg_rT<-aggregadate(DSaggTot_rend)$i_agg_r
DS_finT <-  rbind(DSaggTot_rend[ind_agg_rT==F,],DS_gf1T)
DS_finT<- DS_finT[is.na(DS_finT[,26])==F,]
ind_finT <- order(rownames(DS_finT),decreasing = F)
DS_finT <- DS_finT[ind_finT,]
DS_finT[,c(1,5,6,7,9,11,15,16,17)] <- apply(DS_finT[,c(1,5,6,7,9,11,15,16,17)],2,floor)

#Create lag_1 dataset  
DS_fin_lag1T <- rbind( rep(NA , ncol(DS_finT)),DS_finT[-nrow(DS_finT),]) 
colnames(DS_fin_lag1T) <- apply( as.data.frame(colnames(DS_finT)[]),2,paste,"_lag1", sep="" )

#Create lag_2 dataset 
DS_fin_lag2T <- rbind( rep(NA , ncol(DS_finT)), rep(NA , ncol(DS_finT)) ,DS_finT[-c(nrow(DS_finT),nrow(DS_finT)-1),]) 
colnames(DS_fin_lag2T) <- apply( as.data.frame(colnames(DS_finT)[]),2,paste,"_lag2", sep="" )

#Union dataset
DS_fin_conLagT <- cbind(DS_finT,DS_fin_lag1T,DS_fin_lag2T)
DS_fin_conLagT <- DS_fin_conLagT[-c(1,2),]

#BMW extra yield analysis

#graph BMW extra yields time serie

plot(x=1:nrow(DS_fin_conLagT) ,y=DS_fin_conLagT[,"rend"],type="l"
     ,ylab="extrarendimenti_BMW",xlab="time",main="Serie storica extrarendimenti BMW",xaxt="n")

axis(1,
     c(1,20,39,60,80,101,120,142,164,184,206,226,244,265,284,304),
     as.Date(rownames(DS_fin_conLagT)[c(1,20,39,60,80,101,120,142,164,184,206,226,244,265,284,304)]))

#marginal mean and standard deviation
round(sd(DS_fin_conLagT[,"rend"]),3)
round(mean(DS_fin_conLagT[,"rend"]),3)
#standard deviation covid period
round(sd(DS_fin_conLagT[284:304,"rend"]),3)

#Graph autocorrelation and partial autocorrelation
acf(DS_fin_conLagT[,"rend"], main ="Extrarendimenti BMW")
pacf(DS_fin_conLagT[,"rend"], main ="Extrarendimenti BMW")

#Pearson Correlation
corre <-cor(DS_fin_conLagT)
ind3 <-  ifelse(corre[,26]>=0.08 | corre[,26]<=-0.08 ,T,F) 
Correlazione<-round(sort(corre[ind3,26],decreasing = T)[-1],2)
View(as.data.frame(Correlazione))

#Create matrix for analysis
XTot <- as.matrix(DS_fin_conLagT[,-26])
yTot <- DS_fin_conLagT[,"rend"]

indTrT<- 1:(length(yTot)-61)
indTeT<- (length(yTot)-60): length(yTot)

# Varaible selection AIC
fit.fullT <- lm(rend~.,data = as.data.frame(DS_fin_conLagT[indTrT,]))
fit.nullT <- lm(rend~1,data = as.data.frame(DS_fin_conLagT[indTrT,]))
fit.BackT <- step(fit.fullT,scope=list(lower=fit.nullT),direction = "backward", k=1,trace=0,step=3000)
namB <- names(sort(abs(coef(fit.BackT)),decreasing=T))[names(sort(abs(coef(fit.BackT)),decreasing=T))!="(Intercept)"]

#AR whit external regressor + eGARCH 
xspec <- ugarchspec(mean.model = list(armaOrder=c(1,0),
                                      external.regressors= XTot[,c(
                                      namB[-c(2)]
                                     )
                                      ],
                                      include.mean=F),
                    variance.model = list(model = "eGARCH",variance.targeting=F,garchOrder=c(0,1)),
                    distribution.model = "ged")


garch.mod <- ugarchfit(xspec,data= yTot , solver = "hybrid", out.sample = 61)
garch.for <- ugarchforecast(garch.mod,n.ahead = 1, n.roll = 61)
y.garch <- fitted(garch.for)

#Performance
RMSE1 <- sqrt(mean((yTot[indTeT]-y.garch[-length(y.garch)])^2))
RMSE0 <- sqrt(mean((yTot[indTeT]-mean(yTot[indTrT]))^2))

RRMSE1<-sqrt(mean((yTot[indTeT]-y.garch[-length(y.garch)])^2))/
  sqrt(mean((yTot[indTeT]-mean(yTot[indTrT]))^2))

RRMSE0<-1

R_21<- 1-(sum((yTot[indTeT]-y.garch[-length(y.garch)])^2)/
            sum((yTot[indTeT]-mean(yTot[indTeT]))^2))


R_20<- 1-(sum((yTot[indTeT]-mean(yTot[indTrT]))^2)/
            sum((yTot[indTeT]-mean(yTot[indTeT]))^2))

metriche <- matrix(0,2,3)
colnames(metriche) <- c("RMSE","RRMSE","R^2")
rownames(metriche)<- c("AR + eGARCH","Benchmark")

metriche["AR + eGARCH","RMSE"]<-round(RMSE1,4)
metriche["AR + eGARCH","RRMSE"]<-round(RRMSE1,2)
metriche["AR + eGARCH","R^2"]<-round(R_21,2)

metriche["Benchmark","RMSE"]<-round(RMSE0,4)
metriche["Benchmark","RRMSE"]<-round(RRMSE0,2)
metriche["Benchmark","R^2"]<-round(R_20,2)

View(metriche)

#Graph extra yield vs prediction
plot(x=1:nrow(XTot[indTeT,]),y=yTot[indTeT],col="red",type = "l", xaxt="n",xlab="time", ylab="extrarendimenti"
     ,main="Valori extrarendimenti vs previsioni - BMW")

axis(1,
     c(1,22,41,61),
     as.Date(rownames(DS_fin_conLagT[244:304,])[c(1,22,41,61)]))

lines(1:nrow(XTot[indTeT,]),y.garch[-length(y.garch)],col="blue")
lines(1:nrow(XTot[indTeT,]),rep(mean(yTot[indTrT]),nrow(XTot[indTeT,])),col="green")

legend("topleft", legend = c("BMW","AR+eGARCH", "Benchmark"),
       col=c("red","blue","green"),lty = 1, cex = 1, title="Serie",title.adj = 0.5)



#Analysis of the sign of BMW extra returns
#Create sign dataset
DSsegno <- as.data.frame(DS_fin_conLagT)
DSsegno[,"rend"]<- (ifelse(sign(DSsegno[,"rend"])==-1,0,1))


DSsegno <- cbind(DSsegno,sgn_lag1=(sign(DSsegno[,"rend_lag1"])),
                 sgn_lag2=(sign(DSsegno[,"rend_lag2"])))
colnames(DSsegno)[26]<- "sgn"

#Craete matrix for analysis
XTots <- as.matrix(DSsegno[,-26])
yTots <- (DSsegno[,"sgn"])

#Create index
indTrTs<- 1:(length(yTots)-61)
indTeTs<- (length(yTots)-60): length(yTots)


#Time series of the sign of BMW extra returns
plot(x=1:nrow(DSsegno) ,y=DSsegno[,"sgn"],type="l"
     ,ylab="segno_extrarendimenti_BMW",xlab="time",main="Serie storica segno extrarendimenti BMW",xaxt="n")

axis(1,
     c(1,20,39,60,80,101,120,142,164,184,206,226,244,265,284,304),
     as.Date(rownames(DSsegno)[c(1,20,39,60,80,101,120,142,164,184,206,226,244,265,284,304)]))

#Distribution of the signs of BMW extra returns
table(DSsegno[,"sgn"])

#Kendall correlation
corres <-cor(DSsegno, method = "kendall")
ind3s <-  ifelse(corres[,26]>=0.08 | corres[,26]<=-0.08 ,T,F) 
View(data.frame(Correlazione=round(sort(corres[ind3s,26],decreasing=T )[-1],2)))




#SVM
Xtrain <- apply(XTots[indTrTs,],2,function(x) (x-mean(x))/sd(x))
ytrain <- apply(as.data.frame(yTots[indTrTs]),2,function(x) (x-mean(x))/sd(x))


Xnew <- apply(XTots[indTeTs,],2,function(x) (x-mean(x))/sd(x))
ynew <- apply(as.data.frame(yTots[indTeTs]),2,function(x) (x-mean(x))/sd(x))

model.svms <- svm( x=Xtrain[,],y=as.factor(yTots[indTrTs]), 
                  scale=F, type="C-classification", kernel="polynomial"
)

y.svms <- predict( model.svms,Xnews[,])

#Performance
Accuracy1<- sum(diag(table(as.factor(yTots[indTeTs]),y.svms)))/sum(length(y.svms))
Accuracy1vs0<- Accuracy1-0.5

#ROC Curve
obj_roc <- roc.curve(as.factor(yTots[indTeTs]), y.svms, xlab="1-Specificità", ylab="Sensibilità", main="Curva Roc")

obj_roc$auc-0.5

metriche <-matrix (0,2,2)
colnames(metriche) <- c("Accuracy","AUC")
rownames(metriche) <- c("SVM","SVM vs benchmark")

metriche["SVM","Accuracy"] <- Accuracy1
metriche["SVM","AUC"] <- obj_roc$auc

metriche["SVM vs benchmark","Accuracy"] <- Accuracy1vs0
metriche["SVM vs benchmark","AUC"] <- obj_roc$auc-0.5

View(round(metriche,3))

#Confusion matrix

Confusion_matrix<-unclass(addmargins(table(True_value=as.factor(yTots[indTeTs]),prev=y.svms),
))
rownames(Confusion_matrix)<- c("True_value_0", "True_value_1","Total")
colnames(Confusion_matrix)<- c("pred_value_0", "pred_value_1","Total")

View(Confusion_matrix,title = "Confusion Matrix")






