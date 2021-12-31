#This is a support script to write the datasets, from the matrix in which that are saved the extract tweets ,that has to be passed to the finbert model.  
library(stringr)
library(readr)
load("####")

Tweets<-Tweet
head(Tweets)
indexRT<- ifelse(is.na(Tweets[,"referenced_tweets.type_retweeted"])==T,F,T)
indexNOTRT<- indexRT==FALSE

Tweets1 <- Tweets[indexNOTRT,]
lim<-floor(nrow(Tweets[indexNOTRT,])/2)
Tweets1<- Tweets1[1:lim,]
Tweets2 <- Tweets[indexNOTRT,]
Tweets2<- Tweets2[(lim+1):nrow(Tweets[indexNOTRT,]),]

#Every matrix is divided i two equal-size datasets
write_excel_csv2(data.frame(Tweets1[,c(1,2,4)]),
                 file=paste("####",
                            gsub(x=substr(Tweets1[1,4],1,10),"-"," ")
                            ," ",
                            gsub(x=substr(Tweets1[nrow(Tweets1),4],1,10),"-"," "),
                            ".csv"))


write_excel_csv2(data.frame(Tweets2[,c(1,2,4)]),
                 file=paste("####",
                            gsub(x=substr(Tweets2[1,4],1,10),"-"," ")
                            ," ",
                            gsub(x=substr(Tweets2[nrow(Tweets2),4],1,10),"-"," "),
                            ".csv"))


