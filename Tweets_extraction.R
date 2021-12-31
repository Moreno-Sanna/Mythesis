library(httr)

# write bearer code
bearer_token = "####"
headers = c(
  `Authorization` = sprintf('Bearer %s', bearer_token)
)

#I create vectors fr the lists
v_reftweet <- vector()
d_reftweet <- vector()

#I create matrix for storage the tweets

Tweet<- matrix(nrow = 0,ncol =11 )
colnames(Tweet) <- c("id",
                          "text",
                          "author_id",
                          "created_at",
                          "lang",
                          "referenced_tweets.type_retweeted",
                          "referenced_tweets.type_quoted",
                          "referenced_tweets.type_replied_to",
                          "referenced_tweets.id_retweeted",
                          "referenced_tweets.id_quoted",
                          "referenced_tweets.id_replied_to"
)
#I create matrix for storage the referenced tweets
ReferencedTweet<- matrix(nrow = 0,ncol =11 )
colnames(ReferencedTweet) <- c("id",
                          "text",
                          "author_id",
                          "created_at",
                          "lang",
                          "referenced_tweets.type_retweeted",
                          "referenced_tweets.type_quoted",
                          "referenced_tweets.type_replied_to",
                          "referenced_tweets.id_retweeted",
                          "referenced_tweets.id_quoted",
                          "referenced_tweets.id_replied_to")


#i write the end date of the period of the extraction
starttm <- "2020-04-01T00:00:00.000Z"


for(i in 1:300){
  #I write Query parameter; Example whit BMW
params = list(
  `query` = '"Bayerische Motoren Werke" OR BayerischeMotorenWerke OR #BayerischeMotorenWerke OR BMW OR #BMW OR $BMW',
  #`start_time`=starttm,
  expansions="referenced_tweets.id",
  `end_time`=starttm,
  `max_results` = '500',
  `tweet.fields` = 'author_id,text,created_at,referenced_tweets,lang'
)

#Extraxt the data
response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', httr::add_headers(.headers=headers), query = params)
fas_body <-
  content(
    response,
    as = 'parsed',
    type = 'application/json',
    simplifyDataFrame = TRUE
  )

#Control codes
lastindex<-i
if(is.character(fas_body$title)==TRUE) break

#I divide the "referenced_tweets" column of the matrix of the extracted tweets because it is a list with possible values for each tweet 
v<- vector()
v<-fas_body$data[,"referenced_tweets"]
v[sapply(fas_body$data[,"referenced_tweets"], is.null)]<-NA

v_type_retweeted <- vector()
v_type_quoted <- vector()
v_type_replied_to <- vector()

v_id_retweeted <- vector()
v_id_quoted <- vector()
v_id_replied_to <- vector()

for( l in 1 :length(v)){
  if (any(is.na(as.data.frame(v[l])[,1]))==TRUE ) {
    v_type_retweeted[l] <- NA 
    v_type_quoted[l] <- NA
    v_type_replied_to[l] <- NA
    
    v_id_retweeted[l] <- NA
    v_id_quoted[l] <- NA
    v_id_replied_to[l] <- NA
    
  }else {
    if(length(as.data.frame(v[l])[,1])>3) break 
    
    if(ifelse(is.na(as.data.frame(v[l])[1,"type"])==TRUE,"NA",as.data.frame(v[l])[1,"type"])=="retweeted" ||  
       ifelse(is.na(as.data.frame(v[l])[2,"type"])==TRUE,"NA",as.data.frame(v[l])[2,"type"])=="retweeted" ||  
       ifelse(is.na(as.data.frame(v[l])[3,"type"])==TRUE,"NA",as.data.frame(v[l])[3,"type"])=="retweeted" ) {
      v_type_retweeted[l]="retweeted"} else v_type_retweeted[l]=NA
      
      if(ifelse(is.na(as.data.frame(v[l])[1,"type"])==TRUE,"NA",as.data.frame(v[l])[1,"type"])=="quoted" ||  
         ifelse(is.na(as.data.frame(v[l])[2,"type"])==TRUE,"NA",as.data.frame(v[l])[2,"type"])=="quoted" ||  
         ifelse(is.na(as.data.frame(v[l])[3,"type"])==TRUE,"NA",as.data.frame(v[l])[3,"type"])=="quoted" ){
        v_type_quoted[l]="quoted"} else v_type_quoted[l]=NA
        
        if(ifelse(is.na(as.data.frame(v[l])[1,"type"])==TRUE,"NA",as.data.frame(v[l])[1,"type"])=="replied_to" ||  
           ifelse(is.na(as.data.frame(v[l])[2,"type"])==TRUE,"NA",as.data.frame(v[l])[2,"type"])=="replied_to" ||  
           ifelse(is.na(as.data.frame(v[l])[3,"type"])==TRUE,"NA",as.data.frame(v[l])[3,"type"])=="replied_to" ){
          v_type_replied_to[l]="replied_to"} else v_type_replied_to[l]=NA
          
          
          if(ifelse(is.na(as.data.frame(v[l])[1,"type"])==TRUE,"NA",as.data.frame(v[l])[1,"type"])=="retweeted"){
            v_id_retweeted[l]=as.data.frame(v[l])[1,"id"] } else{
              
              if(ifelse(is.na(as.data.frame(v[l])[2,"type"])==TRUE,"NA",as.data.frame(v[l])[2,"type"])=="retweeted") {
                v_id_retweeted[l]=as.data.frame(v[l])[2,"id"]} else {
                  
                  if(ifelse(is.na(as.data.frame(v[l])[3,"type"])==TRUE,"NA",as.data.frame(v[l])[3,"type"])=="retweeted"){
                    v_id_retweeted[l]=as.data.frame(v[l])[3,"id"]} else v_id_retweeted[l]=NA  
                }}
          
          
          if(ifelse(is.na(as.data.frame(v[l])[1,"type"])==TRUE,"NA",as.data.frame(v[l])[1,"type"])=="quoted"){
            v_id_quoted[l]=as.data.frame(v[l])[1,"id"] } else{
              
              if(ifelse(is.na(as.data.frame(v[l])[2,"type"])==TRUE,"NA",as.data.frame(v[l])[2,"type"])=="quoted") {
                v_id_quoted[l]=as.data.frame(v[l])[2,"id"]} else {
                  
                  if(ifelse(is.na(as.data.frame(v[l])[3,"type"])==TRUE,"NA",as.data.frame(v[l])[3,"type"])=="quoted"){
                    v_id_quoted[l]=as.data.frame(v[l])[3,"id"]} else v_id_quoted[l]=NA  
                }}
          
          
          if(ifelse(is.na(as.data.frame(v[l])[1,"type"])==TRUE,"NA",as.data.frame(v[l])[1,"type"])=="replied_to"){
            v_id_replied_to[l]=as.data.frame(v[l])[1,"id"] } else{
              
              if(ifelse(is.na(as.data.frame(v[l])[2,"type"])==TRUE,"NA",as.data.frame(v[l])[2,"type"])=="replied_to") {
                v_id_replied_to[l]=as.data.frame(v[l])[2,"id"]} else {
                  
                  if(ifelse(is.na(as.data.frame(v[l])[3,"type"])==TRUE,"NA",as.data.frame(v[l])[3,"type"])=="replied_to"){
                    v_id_replied_to[l]=as.data.frame(v[l])[3,"id"]} else v_id_replied_to[l]=NA  
                }}
          
  }
}

#Saved the list 
v_reftweet <- c(v_reftweet,
                fas_body$data[1:(nrow(fas_body$data)),"referenced_tweets"])

#Load the Tweets matrix
Tweet <- rbind(Tweet,
               cbind(fas_body$data[1:(nrow(fas_body$data)),"id"],
                     fas_body$data[1:(nrow(fas_body$data)),"text"],
                     fas_body$data[1:(nrow(fas_body$data)),"author_id"],
                     fas_body$data[1:(nrow(fas_body$data)),"created_at"],
                     fas_body$data[1:(nrow(fas_body$data)),"lang"],
                     v_type_retweeted[1:(length(v_type_retweeted))],
                     v_type_quoted[1:(length(v_type_quoted))],
                     v_type_replied_to[1:(length(v_type_replied_to))],
                     v_id_retweeted[1:(length(v_id_retweeted))],
                     v_id_quoted[1:(length(v_id_quoted))],
                     v_id_replied_to[1:(length(v_id_replied_to))]))


#Control codes in the case that "referenced_tweets" of the referenced tweets matrix is empy
if ( length(table(colnames(fas_body$includes$tweets)=="referenced_tweets"))>1){
d<- vector()
d<-fas_body$includes$tweets[,"referenced_tweets"]
d[sapply(fas_body$includes$tweets[,"referenced_tweets"], is.null)]<-NA
} else {
d<- rep(list(NA),nrow(fas_body$includes$tweets))

}
#I divide the "referenced_tweets" column of the matrix of the referenced tweets in the vector d
d_type_retweeted <- vector()
d_type_quoted <- vector()
d_type_replied_to <- vector()

d_id_retweeted <- vector()
d_id_quoted <- vector()
d_id_replied_to <- vector()

for( j in 1 :length(d)){
  if (any(is.na(as.data.frame(d[j])[,1]))==TRUE ) {
    d_type_retweeted[j] <- NA 
    d_type_quoted[j] <- NA
    d_type_replied_to[j] <- NA
    
    d_id_retweeted[j] <- NA
    d_id_quoted[j] <- NA
    d_id_replied_to[j] <- NA
    
  }else {
    if(length(as.data.frame(d[j])[,1])>3) break 
    
    if(ifelse(is.na(as.data.frame(d[j])[1,"type"])==TRUE,"NA",as.data.frame(d[j])[1,"type"])=="retweeted" ||  
       ifelse(is.na(as.data.frame(d[j])[2,"type"])==TRUE,"NA",as.data.frame(d[j])[2,"type"])=="retweeted" ||  
       ifelse(is.na(as.data.frame(d[j])[3,"type"])==TRUE,"NA",as.data.frame(d[j])[3,"type"])=="retweeted" ) {
      d_type_retweeted[j]="retweeted"} else d_type_retweeted[j]=NA
      
      if(ifelse(is.na(as.data.frame(d[j])[1,"type"])==TRUE,"NA",as.data.frame(d[j])[1,"type"])=="quoted" ||  
         ifelse(is.na(as.data.frame(d[j])[2,"type"])==TRUE,"NA",as.data.frame(d[j])[2,"type"])=="quoted" ||  
         ifelse(is.na(as.data.frame(d[j])[3,"type"])==TRUE,"NA",as.data.frame(d[j])[3,"type"])=="quoted" ){
        d_type_quoted[j]="quoted"} else d_type_quoted[j]=NA
        
        if(ifelse(is.na(as.data.frame(d[j])[1,"type"])==TRUE,"NA",as.data.frame(d[j])[1,"type"])=="replied_to" ||  
           ifelse(is.na(as.data.frame(d[j])[2,"type"])==TRUE,"NA",as.data.frame(d[j])[2,"type"])=="replied_to" ||  
           ifelse(is.na(as.data.frame(d[j])[3,"type"])==TRUE,"NA",as.data.frame(d[j])[3,"type"])=="replied_to" ){
          d_type_replied_to[j]="replied_to"} else d_type_replied_to[j]=NA
          
          
          if(ifelse(is.na(as.data.frame(d[j])[1,"type"])==TRUE,"NA",as.data.frame(d[j])[1,"type"])=="retweeted"){
            d_id_retweeted[j]=as.data.frame(d[j])[1,"id"] } else{
              
              if(ifelse(is.na(as.data.frame(d[j])[2,"type"])==TRUE,"NA",as.data.frame(d[j])[2,"type"])=="retweeted") {
                d_id_retweeted[j]=as.data.frame(d[j])[2,"id"]} else {
                  
                  if(ifelse(is.na(as.data.frame(d[j])[3,"type"])==TRUE,"NA",as.data.frame(d[j])[3,"type"])=="retweeted"){
                    d_id_retweeted[j]=as.data.frame(d[j])[3,"id"]} else d_id_retweeted[j]=NA  
                }}
          
          
          if(ifelse(is.na(as.data.frame(d[j])[1,"type"])==TRUE,"NA",as.data.frame(d[j])[1,"type"])=="quoted"){
            d_id_quoted[j]=as.data.frame(d[j])[1,"id"] } else{
              
              if(ifelse(is.na(as.data.frame(d[j])[2,"type"])==TRUE,"NA",as.data.frame(d[j])[2,"type"])=="quoted") {
                d_id_quoted[j]=as.data.frame(d[j])[2,"id"]} else {
                  
                  if(ifelse(is.na(as.data.frame(d[j])[3,"type"])==TRUE,"NA",as.data.frame(d[j])[3,"type"])=="quoted"){
                    d_id_quoted[j]=as.data.frame(d[j])[3,"id"]} else d_id_quoted[j]=NA  
                }}
          
          
          if(ifelse(is.na(as.data.frame(d[j])[1,"type"])==TRUE,"NA",as.data.frame(d[j])[1,"type"])=="replied_to"){
            d_id_replied_to[j]=as.data.frame(d[j])[1,"id"] } else{
              
              if(ifelse(is.na(as.data.frame(d[j])[2,"type"])==TRUE,"NA",as.data.frame(d[j])[2,"type"])=="replied_to") {
                d_id_replied_to[j]=as.data.frame(d[j])[2,"id"]} else {
                  
                  if(ifelse(is.na(as.data.frame(d[j])[3,"type"])==TRUE,"NA",as.data.frame(d[j])[3,"type"])=="replied_to"){
                    d_id_replied_to[j]=as.data.frame(d[j])[3,"id"]} else d_id_replied_to[j]=NA  
                }}
          
  }
}
#saved the list 
if ( length(table(colnames(fas_body$includes$tweets)=="referenced_tweets"))>1){
d_reftweet <- c(d_reftweet,
                    fas_body$includes$tweets[1:(nrow(fas_body$includes$tweets)),"referenced_tweets"])
} else {
d_reftweet <- c(d_reftweet,rep(list(NULL),nrow(fas_body$includes$tweets)))

}
#load the referenced tweet matrix
ReferencedTweet <- rbind(ReferencedTweet,
                    cbind(fas_body$includes$tweets[,"id"],
                          fas_body$includes$tweets[,"text"],
                          fas_body$includes$tweets[,"author_id"],
                          fas_body$includes$tweets[,"created_at"],
                          fas_body$includes$tweets[,"lang"],
                          d_type_retweeted,
                          d_type_quoted,
                          d_type_replied_to,
                          d_id_retweeted,
                          d_id_quoted,
                          d_id_replied_to))


print(i)
#saved the date of the last tweet extract
starttm<- fas_body$data[nrow(fas_body$data),"created_at"]

}

#Saved the object
save(Tweet, file="###")
save(ReferencedTweet, file="###")
save(v_reftweet , file="###")
save(d_reftweet , file="###")
