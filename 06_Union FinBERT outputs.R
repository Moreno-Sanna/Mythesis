#This is a support code for the union of the multiple outputs for the FinBERT analysis
library(stringr)
library(readr)

#Load the csv files
Tweetpy11 <- read_delim(file = "####", col_names  = TRUE,delim = ";"
                       , col_types  = cols(
                         id = col_character(),
                         data = col_character(),
                         sentence = col_character(),
                         logit_positive = col_double(),
                         logit_negative = col_double(),
                         logit_neutral = col_double(),
                         prediction = col_character(),
                         sentiment_score = col_double()
                       )
)



Tweetpy12 <- read_delim(file = "####", col_names  = TRUE,delim = ";"
                      , col_types  = cols(
                        id = col_character(),
                        data = col_character(),
                        sentence = col_character(),
                        logit_positive = col_double(),
                        logit_negative = col_double(),
                        logit_neutral = col_double(),
                        prediction = col_character(),
                        sentiment_score = col_double()
                      )
)
 


Tweetpy21 <- read_delim(file = "####", col_names  = TRUE,delim = ";"
                      , col_types  = cols(
                        id = col_character(),
                        data = col_character(),
                        sentence = col_character(),
                        logit_positive = col_double(),
                        logit_negative = col_double(),
                        logit_neutral = col_double(),
                        prediction = col_character(),
                        sentiment_score = col_double()
                      )
)


Tweetpy22 <- read_delim(file = "####", col_names  = TRUE,delim = ";"
                      , col_types  = cols(
                        id = col_character(),
                        data = col_character(),
                        sentence = col_character(),
                        logit_positive = col_double(),
                        logit_negative = col_double(),
                        logit_neutral = col_double(),
                        prediction = col_character(),
                        sentiment_score = col_double()
                      )
)


#There can be more of Tweetpy## files

#Union the matrix
Tweetpy <- rbind(Tweetpy11,Tweetpy21,
               Tweetpy12,Tweetpy22)


#Save the matrix

save(Tweetpy , file = "####")



####Count Retweet for day - id 

#load the matrix output from the file Count_retweet
load("###")

#Create index for matching retweet whit original tweets
index <-match(Cont.Ret[,2],as.character(unlist(Tweetpy[,1])))

#Create dataset whit Tweet and Retweet
TweetRE <- cbind(data_retweet=Cont.Ret[,1],n_retweet=Cont.Ret[,4],is_retweet=rep(T,nrow(Cont.Ret)),Tweetpy[index,])
TweetRE <- TweetRE[is.na(TweetRE[,"id"])==F,]
Tweetpy_withRT <- rbind(cbind(data_retweet=rep(NA,nrow(Tweetpy)),n_retweet=rep(NA,nrow(Tweetpy)),is_retweet=rep(F,nrow(Tweetpy)),Tweetpy),TweetRE)   
#Save the matrix
save(Tweetpy_withRT , file = "####")





