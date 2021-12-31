#Load Libraries
library(xgboost)
library(ROSE)
library(caret)
library(MASS)
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
ret <- as.numeric(ClCl(x1))[-1]
ret_day<-((x1[-1,6])-as.numeric(x1[-length(x1[,6]),6]))/as.numeric(x1[-length(x1[,6]),6])
ret <- as.data.frame(ret)
row.names(ret) <- index(ret_day) 

#Load training dataset whit information of BMW Tweets (output 7_Create aggregate dataset by day.R)
load("####") 
DSagg <- DSagg_giorno
rownames(DSagg) <- apply(as.data.frame(DSagg[,1]),2, crea_data )
DSagg<- DSagg[,-1]
DSagg<- DSagg[order(rownames(DSagg),decreasing = F),]
DSagg <-DSagg[-1,]

#Union whit yield
ind <- match(rownames(DSagg),rownames(ret))
ind2 <- ifelse(is.na(ind)==T,T,F)
DSagg_rend<-cbind(DSagg, rend=as.numeric(ret[ind,]))

#Extract BMW prices test set
xTest <- getSymbols("BMW.DE", 
                 from="2019-12-30",
                 to="2020-03-31", 
                 auto.assign = F)
#yield calculation test set
retTest <- as.numeric(ClCl(xTest))[-1]
ret_day_T<-((xTest[-1,6])-as.numeric(xTest[-length(xTest[,6]),6]))/as.numeric(xTest[-length(xTest[,6]),6])
retTest <- as.data.frame(retTest)
row.names(retTest) <- index(ret_day_T) 


#Load test dataset whit information of BMW Tweets (output 7_Create aggregate dataset by day.R)
load("####") 
DSaggTest <- BMWagg_giorn[1:91,]  
rownames(DSaggTest) <- apply(as.data.frame(DSaggTest[,1]),2, crea_data )
DSaggTest<- DSaggTest[,-1]
DSaggTest<- DSaggTest[order(rownames(DSaggTest),decreasing = F),]

#Union whit yield
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

#BMW yield analysis 

#Pearson correlation 
corre <-cor(DS_fin_conLagT)
ind3 <-  ifelse(corre[,26]>=0.08 | corre[,26]<=-0.08 ,T,F) 
Correlazione<-round(sort(corre[ind3,26],decreasing = T)[-1],2)
View(as.data.frame(Correlazione))

#graph BMW sentiment time serie
DSaggTot_norend <- rbind(DSagg,DSaggTest)
plot(x=1:nrow(DSaggTot_norend),y=as.numeric(DSaggTot_norend[,"mean_sentiment_score_pond"])
     ,type="l", xaxt="n", xlab="time",ylab="mean_sentiment_score_pond", main="Serie storica livello di sentimento medio giornaliero BMW" )

axis(1,
     c(1,32,60,91,121,152,182,213,244,274,305,335,366,397,426,456),
     as.Date(rownames(DSaggTot_norend[,]))[c(1,32,60,91,121,152,182,213,244,274,305,335,366,397,426,456)])

#marginal mean and standard deviation
mean(as.numeric(DSaggTot_norend[,"mean_sentiment_score_pond"]))
sd(as.numeric(DSaggTot_norend[,"mean_sentiment_score_pond"]))
#standard deviation covid period
sd(as.numeric(DSaggTot_norend[441:456,"mean_sentiment_score_pond"]))


#graph BMW yields time serie

plot(x=1:nrow(DS_fin_conLagT) ,y=DS_fin_conLagT[,"rend"],type="l"
     ,ylab="rendimenti_BMW",xlab="time",main="Serie storica rendimenti BMW",xaxt="n")

axis(1,
     c(1,21,41,62,82,104,123,146,168,189,211,232,250,272,292,312),
     as.Date(rownames(DS_fin_conLagT)[c(1,21,41,62,82,104,123,146,168,189,211,232,250,272,292,312)]))

#marginal mean and standard deviation
round(sd(DS_fin_conLagT[,"rend"]),3)
round(mean(DS_fin_conLagT[,"rend"]),3)
#standard deviation covid period
round(sd(DS_fin_conLagT[292:311,"rend"]),3)

#Create matrix for analysis

XTot <- as.matrix(DS_fin_conLagT[,-26])
yTot <- DS_fin_conLagT[,"rend"]

indTrT<- 1:(length(yTot)-63)
indTeT<- (length(yTot)-62): length(yTot)

#lasso
lassobsT <-cv.glmnet(XTot[indTrT,],yTot[indTrT],alpha=1,nfolds = length(yTot[indTrT])) 
lmlT <- lassobsT$lambda.min
lassoT<- glmnet(XTot[indTrT,],yTot[indTrT],alpha=1 , lambda = lmlT)
VarSelT <- rownames(as.data.frame(as.matrix(lassoT$beta)[as.numeric(lassoT$beta)!=0,]))

y.lT<-predict(lassoT,newx = XTot[indTeT,])

#Graph cross-validation
plot(lassobsT)

#Performance
RMSE1 <- sqrt(mean((yTot[indTeT]-y.lT)^2))
RMSE0 <- sqrt(mean((yTot[indTeT]-mean(yTot[indTrT]))^2))

RRMSE1<-sqrt(mean((yTot[indTeT]-y.lT)^2))/
  sqrt(mean((yTot[indTeT]-mean(yTot[indTrT]))^2))

RRMSE0<-1

R_21<- 1-(sum((yTot[indTeT]-y.lT)^2)/
            sum((yTot[indTeT]-mean(yTot[indTeT]))^2))


R_20<- 1-(sum((yTot[indTeT]-mean(yTot[indTrT]))^2)/
            sum((yTot[indTeT]-mean(yTot[indTeT]))^2))

metriche <- matrix(0,2,3)
colnames(metriche) <- c("RMSE","RRMSE","R^2")
rownames(metriche)<- c("Lasso","Benchmark")

metriche["Lasso","RMSE"]<-round(RMSE1,4)
metriche["Lasso","RRMSE"]<-round(RRMSE1,2)
metriche["Lasso","R^2"]<-round(R_21,2)

metriche["Benchmark","RMSE"]<-round(RMSE0,4)
metriche["Benchmark","RRMSE"]<-round(RRMSE0,2)
metriche["Benchmark","R^2"]<-round(R_20,2)

View(metriche)

#Graph yield vs prediction
plot(x=1:nrow(XTot[indTeT,]),y=yTot[indTeT],col="red",type = "l", xaxt="n",xlab="time", ylab="rendimenti"
     ,main="Valori rendimenti vs previsioni - BMW")

axis(1,
     c(1,23,43,63),
     as.Date(rownames(DS_fin_conLagT[250:312,])[c(1,23,43,63)]))

lines(1:nrow(XTot[indTeT,]),y.lT,col="blue")
lines(1:nrow(XTot[indTeT,]),rep(mean(yTot[indTrT]),nrow(XTot[indTeT,])),col="green")

legend("bottomleft", legend = c("BMW","Lasso", "Benchmark"),
       col=c("red","blue","green"),lty = 1, cex = 1, title="Serie",title.adj = 0.5)




#Analysis of the sign of BMW returns
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
indTrTs<- 1:(length(yTots)-63)
indTeTs<- (length(yTots)-62): length(yTots)

#Distribution of the signs of BMW returns
table(DSsegno[,"sgn"])

#Time series of the sign of BMW returns
plot(x=1:nrow(DSsegno) ,y=DSsegno[,"sgn"],type="l"
     ,ylab="segno_rendimenti_BMW",xlab="time",main="Serie storica segno rendimenti BMW",xaxt="n")
axis(1,
     c(1,21,41,62,82,104,123,146,168,189,211,232,250,272,292,312),
     as.Date(rownames(DSsegno)[c(1,21,41,62,82,104,123,146,168,189,211,232,250,272,292,312)]))

#Kendall correlation
corres <-cor(DSsegno, method = "kendall")
ind3s <-  ifelse(corres[,26]>=0.08 | corres[,26]<=-0.08 ,T,F) 
View(data.frame(Correlazione=round(sort(corres[ind3s,26],decreasing=T )[-1],2)))


#RandomForest
#Modello con una miglior AUC =0.6

rfT <-randomForest(sgn~.,data = as.data.frame(cbind(DSsegno[indTrTs,-26],sgn=as.factor(DSsegno[indTrTs,26]))),
                   ntree=2500, importance=T)
y.rfT<- predict(rfT,newdata = DSsegno[indTeTs,])

#Variable importance
Importance <-as.data.frame(sort(rfT$importance[,3],decreasing=T))
colnames(Importance)<- "Mean Decrease Accuracy"
View(Importance)
varimp<- rownames(Importance)[which(Importance[,1]<0)]

#Performance
Accuracy1<- sum(diag(table(as.factor(yTots[indTeTs]),y.rfT)))/sum(length(y.rfT))
Accuracy1vs0<- Accuracy1-0.5

#ROC Curve
obj_roc <- roc.curve(as.factor(yTots[indTeTs]), y.rfT, xlab="1-Specificità", ylab="Sensibilità", main="Curva Roc")
obj_roc$auc-0.5

metriche <-matrix (0,2,2)
colnames(metriche) <- c("Accuracy","AUC")
rownames(metriche) <- c("Random Forest","Random Forest vs benchmark")
metriche["Random Forest","Accuracy"] <- Accuracy1
metriche["Random Forest","AUC"] <- obj_roc$auc
metriche["Random Forest vs benchmark","Accuracy"] <- Accuracy1vs0
metriche["Random Forest vs benchmark","AUC"] <- obj_roc$auc-0.5
View(round(metriche,3))

#Confusion Matrix

Confusion_matrix<-unclass(addmargins(table(True_value=as.factor(yTots[indTeTs]),prev=y.rfT),
))
rownames(Confusion_matrix)<- c("True_value_0", "True_value_1","Total")
colnames(Confusion_matrix)<- c("pred_value_0", "pred_value_1","Total")

View(Confusion_matrix,title = "Confusion Matrix")





