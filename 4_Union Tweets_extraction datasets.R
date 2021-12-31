#Support script for union the multiple matrixs in which are saved the tweets 
load("####")
Tweet1 <- Tweet

load("###")
Tweet2 <- Tweet

#There can be multiple matrix Tweet "n"

TweetTotal<- rbind(Tweet1,Tweet2)


save(TweetTotal, file="R/####")
#It's the same file that i saved in the line below 

#Split the tweets in retweets and other tweets 
indexRT <- ifelse(is.na(TweetTotal[,6])==T, F,T)
indexNOTRT <- ifelse(is.na(TweetTotal[,6])==T, T,F)


TweetRT <- TweetTotal[indexRT,]
TweetNOTRT <- TweetTotal[indexNOTRT,]


save(TweetRT, file="####")
save(TweetNOTRT, file="####")





