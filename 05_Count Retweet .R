library(stringr)

#Load Retweets matrix out of the file 4_Union Tweets_extraction datasets.R  
load(file="R/TweetBMWRT.R")

#Create a key for count the retweets shared every day, concatenating date of creation and original tweet id 

Key <- paste (substr(TweetRT[,"created_at"],1,10),TweetRT[,"referenced_tweets.id_retweeted"], sep=" - ")

Cont.Ret <- data.frame(table(Key))
Cont.Ret <- cbind(data=substr(Cont.Ret[,1],1,10),id=substr(Cont.Ret[,1],14,999),Cont.Ret)

#save the matrix
save(Cont.Ret, file="####")




