#Script for create aggregate dataset by day
library(stringr)
library(readr)

#load the matrix output from the file 6_Union FinBERT outputs.R
load(file="####")

DSpy_conRT <- Tweetpy_withRT



giorni <- ifelse(DSpy_conRT[,"is_retweet"]==F,
                 as.numeric(apply(data.frame(DSpy_conRT[,"data"]), 2, function(x) str_replace_all(substr(x,1,10),"-","") )),
                 as.numeric(apply(data.frame(DSpy_conRT[,"data_retweet"]), 2, function(x) str_replace_all(x,"-","") )))

index<- order(giorni,decreasing = T)


DSpy_conRT2 <- cbind(DSpy_conRT[index,],ind.giorni=giorni[index]) 

nomi.giorni <- names(table(DSpy_conRT2[,"ind.giorni"]))
nomi.giorni<- nomi.giorni[order(as.numeric(nomi.giorni),decreasing = T)]
RT <- ifelse(DSpy_conRT2[,"is_retweet"]==F,1,DSpy_conRT2[,"n_retweet"])
DSpy_conRT2<- cbind(DSpy_conRT2,tweet_totali=RT )

#Calculate variable weighted for retweet
DSpy_conRT2<-  cbind(DSpy_conRT2, sentiment_score_pond=DSpy_conRT2[,"sentiment_score"]*DSpy_conRT2[,"tweet_totali"])
DSpy_conRT2<-  cbind(DSpy_conRT2, logit_positive_pond=DSpy_conRT2[,"logit_positive"]*DSpy_conRT2[,"tweet_totali"])
DSpy_conRT2<-  cbind(DSpy_conRT2, logit_negative_pond=DSpy_conRT2[,"logit_negative"]*DSpy_conRT2[,"tweet_totali"])
DSpy_conRT2<-  cbind(DSpy_conRT2, logit_neutral_pond=DSpy_conRT2[,"logit_neutral"]*DSpy_conRT2[,"tweet_totali"])

DSpy_conRT2<-  cbind(DSpy_conRT2, n_positive_pond=ifelse(DSpy_conRT2[,"prediction"]=="positive",1,0)*DSpy_conRT2[,"tweet_totali"])
DSpy_conRT2<-  cbind(DSpy_conRT2, n_negative_pond=ifelse(DSpy_conRT2[,"prediction"]=="negative",1,0)*DSpy_conRT2[,"tweet_totali"])
DSpy_conRT2<-  cbind(DSpy_conRT2, n_neutral_pond=ifelse(DSpy_conRT2[,"prediction"]=="neutral",1,0)*DSpy_conRT2[,"tweet_totali"])

#Create matrix for aggregate dataset
DSagg <- matrix(nrow = 366,ncol=18)

colnames(DSagg)<- c("giorno","n_Tweet", "sum_positive","sum_negative","sum_neutral",
                       "n_positive","n_negative","n_neutral","sum_sentiment_score","n_retweet",
                     "sum_sentiment_score_pond","n_tweet_totali","sum_positive_pond","sum_negative_pond","sum_neutral_pond",
                     "n_positive_pond","n_negative_pond","n_neutral_pond")

I <- length(nomi.giorni)

#Aggregate for day
for(i in 1:I){

DSgio <- DSpy_conRT2[DSpy_conRT2[,"ind.giorni"]==nomi.giorni[i],]

DSagg[i,1]<- nomi.giorni[i]
DSagg[i,2]<- as.numeric(table(DSgio[DSgio[,"is_retweet"]==F,"ind.giorni"]))
DSagg[i,3]<- sum(DSgio[DSgio[,"is_retweet"]==F,"logit_positive"])
DSagg[i,4]<- sum(DSgio[DSgio[,"is_retweet"]==F,"logit_negative"])
DSagg[i,5]<- sum(DSgio[DSgio[,"is_retweet"]==F,"logit_neutral"])
DSagg[i,6]<- as.numeric(table(unlist(DSgio[DSgio[,"is_retweet"]==F,"prediction"]))["positive"])
DSagg[i,7]<- as.numeric(table(unlist(DSgio[DSgio[,"is_retweet"]==F,"prediction"]))["negative"])
DSagg[i,8]<- as.numeric(table(unlist(DSgio[DSgio[,"is_retweet"]==F,"prediction"]))["neutral"])
DSagg[i,9]<- sum(DSgio[DSgio[,"is_retweet"]==F,"sentiment_score"],na.rm=T)
DSagg[i,10]<- sum(DSgio[,"n_retweet"],na.rm = T)

DSagg[i,11]<- sum(DSgio[,"sentiment_score_pond"],na.rm = T)
DSagg[i,12]<- sum(DSgio[,"tweet_totali"],na.rm = T)
DSagg[i,13]<- sum(DSgio[,"logit_positive_pond"])
DSagg[i,14]<- sum(DSgio[,"logit_negative_pond"])
DSagg[i,15]<- sum(DSgio[,"logit_neutral_pond"])
DSagg[i,16]<- sum(DSgio[,"n_positive_pond"])
DSagg[i,17]<- sum(DSgio[,"n_negative_pond"])
DSagg[i,18]<- sum(DSgio[,"n_neutral_pond"])


print(i) 
}

#Create support matrix 
DSagg2 <- matrix(0, nrow= nrow(DSagg) , ncol=ncol(DSagg))
DSagg2[,] <- apply( DSagg,2,as.numeric)
colnames(DSagg2) <- colnames(DSagg)

#Create mean variable
DSagg2 <- cbind(DSagg2, mean_positive=DSagg2[,"sum_positive"]/DSagg2[,"n_Tweet"])
DSagg2 <- cbind(DSagg2, mean_negative=DSagg2[,"sum_negative"]/DSagg2[,"n_Tweet"])
DSagg2 <- cbind(DSagg2, mean_neutral=DSagg2[,"sum_neutral"]/DSagg2[,"n_Tweet"])
DSagg2 <- cbind(DSagg2, mean_sentiment_score=DSagg2[,"sum_sentiment_score"]/DSagg2[,"n_Tweet"])

DSagg2 <- cbind(DSagg2, mean_positive_pond=DSagg2[,"sum_positive_pond"]/DSagg2[,"n_tweet_totali"])
DSagg2 <- cbind(DSagg2, mean_negative_pond=DSagg2[,"sum_negative_pond"]/DSagg2[,"n_tweet_totali"])
DSagg2 <- cbind(DSagg2, mean_neutral_pond=DSagg2[,"sum_neutral_pond"]/DSagg2[,"n_tweet_totali"])
DSagg2 <- cbind(DSagg2, mean_sentiment_score_pond=DSagg2[,"sum_sentiment_score_pond"]/DSagg2[,"n_tweet_totali"])


DSAgg_giorno <- DSagg2

#Save output matrix
save(DSAgg_giorno, file ="####" )

