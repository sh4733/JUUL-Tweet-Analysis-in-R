install.packages("ROAuth")
install.packages("rtweet")
install.packages("httpuv")
install.packages("tidytext")
install.packages("RColorBrewer")
install.packages("saotd")
library(rtweet)
library(ROAuth)
library(dplyr)
library(ggplot2)
library(tidytext)
library(RColorBrewer)
library(wordcloud)
library(saotd)
library(tidyverse)

setwd("C:/Users/Soohyun Hwangbo/Desktop/Intro to Study of Society/Juul_tweet")

#store api keys
apiKey<-"MQwCAVDcBDGLbw01kVHIXPX2J"
apiSecret<-"hO9kYR8hI2xKzfvamXwBiraN3JJwtcM72SucgzUDWv1bJMDDZD"

#authenticate via web browser
token<-create_token(app="JuulData",consumer_key = apiKey, consumer_secret = apiSecret)

#search n=1000 sample tweets with juultags (year:2019)
juulTag<-search_tweets("#juultricks OR #juulmemes OR #juulnation OR #juulvapor OR #juuling OR #juul",n=500, 
                            fromDate="201901010000", toDate="201911220000",env_name = "fullJuulData",parse=TRUE)
write_as_csv(juulTag, file_name= "juulData")
juulData<-read.csv("juulData.csv")

juulUsers<-users_data(juulData)
write_as_csv(juulUsers, file_name = "juulUsers")

juulData$text<-gsub("http.*","",juulTag$text) #remove links in text
juulData$text<-gsub("https.*","",juulTag$text)
juulData$text<-gsub("$amp;","&",juulTag$text)

juulData_clean<-juulTag %>% #remove punctuation, convert to lowercase
  dplyr::select(text) %>%
  unnest_tokens(word, text)

cleaned_juulData <- juulData_clean %>% anti_join(stop_words) #remove stop words (part of NLP data cleaning)
nrow(cleaned_juulData)

##plot top 15 frequent words in tweet texts
cleaned_juulData %>%
  count(word,sort=TRUE) %>%
  top_n(15) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(x=word,y=n)) + geom_col(fill="#56B4E9") + xlab(NULL) + coord_flip() + labs(y="Frequency Counts", x="Unique words", title="Top 15 frequent words in sample tweets") + scale_fill_brewer()+theme_bw()

sum(cleaned_juulData$word=="kids")#count no. of the word 'kids'
sum(cleaned_juulData$word=="youth")
sum(cleaned_juulData$word=="school") #done for 'teen'

##extract top relevant hashtags used with JUUL tags
juulData$hashtags<-as.character(juulTag$hashtags)
juulData$hashtags<-gsub("c\\(","",juulTag$hashtags)

juulData_hashtag <- juulData %>%
  dplyr::select(hashtags) %>%
  unnest_tokens(juulData, hashtags)

#wordcloud
set.seed(1234)
wordcloud(juulData_hashtag, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

juulData_hashtag <- juulData_hashtag[!(juulData_hashtag$juulData=="juul"),]

juulData_hashtag %>%
  count(juulData,sort=TRUE) %>%
  top_n(10) %>%
  mutate(hashtags=reorder(juulData,n)) %>%
  ggplot(aes(x=juulData,y=n)) + geom_col() + xlab(NULL) + coord_flip() + labs(y="Frequency Counts", x="Unique Hashtags", title="Top 10 frequent hashtags in sample tweets") + scale_fill_brewer()

