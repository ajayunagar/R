library(twitteR)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)

conkey = "OYti8F6EWXYWXkGxzXhhvg8XJ"
consecret = "47S7J2668RygKJwUMxKdoTBZ0CNInAQQ5JyWGBtopGDqTwxwF7"
acctoken = "1492337995-bUAgkG6WSIyqbNzDqibwJQj4QswENobNOYoLr6o"
accsecret = "NSM4Pbnm3QOMchrHYsrOnOplFeDKffG9JKLuhOc8k5as9"
setup_twitter_oauth(conkey,consecret,acctoken,accsecret)

tweets20 = searchTwitter("#JNU",1000,since = "2016-02-20",until = "2016-02-21",lang = "en")
tweets21 = searchTwitter("#JNU",1000,since = "2016-02-21",until = "2016-02-22")
tweets22 = searchTwitter("#JNU",1000,since = "2016-02-22",until = "2016-02-23")
tweets23 = searchTwitter("#JNU",1000,since = "2016-02-23",until = "2016-02-24")
tweets24 = searchTwitter("#JNU",1000,since = "2016-02-24",until = "2016-02-25")
tweets25 = searchTwitter("#JNU",1000,since = "2016-02-25",until = "2016-02-26")
tweets26 = searchTwitter("#JNU",1000,since = "2016-02-26",until = "2016-02-27")
tweets27 = searchTwitter("#JNU",1000,since = "2016-02-27",until = "2016-02-28")
tweets28 = searchTwitter("#JNU",1000,since = "2016-02-28",until = "2016-02-29")

tweets20.df <- twListToDF(tweets20)
dim(tweets20.df)
tweet_corpus_20 <- Corpus(VectorSource(tweets20.df$text))
tweet_corpus_20 <- tm_map(tweet_corpus_20,removePunctuation)
tweet_corpus_20 <- tm_map(tweet_corpus_20,removeNumbers)
tweet_corpus_20 <- tm_map(tweet_corpus_20,content_transformer(tolower))
removeURL <- function(x) gsub("http[[:alnum:]]*","",x)
tweet_corpus_20 <- tm_map(tweet_corpus_20,removeURL)
tweet20StopWords <- c(stopwords("english"),"for","was","when","am","are","will","what","why",
                    "who","how","is","while")
tweet_corpus_20 <- tm_map(tweet_corpus_20,removeWords,tweet20StopWords)
tweet_corpus_20Copy<- tweet_corpus_20
tweet_corpus_20 <- tm_map(tweet_corpus_20,stemDocument)
tweet_corpus_20 <- tm_map(tweet_corpus_20,stripWhitespace)
tweet_corpus_20 <- tm_map(tweet_corpus_20,PlainTextDocument)
tdm <- TermDocumentMatrix(tweet_corpus_20)
dtm <- DocumentTermMatrix(tweet_corpus_20)
(freq.terms <- findFreqTerms(tdm,lowfreq = 25))
term.freq <- sort(rowSums(as.matrix(tdm)),decreasing = TRUE)
term.freq <- subset(term.freq, term.freq >= 30)
df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)
ggplot(df,aes(x = term, y = freq))+geom_bar(stat = "identity")+xlab("Terms")+ylab("Count")+
    coord_flip()
findAssocs(tdm,"jnu",0.2)
findAssocs(tdm,"speech",0.4)
m <- as.matrix(tdm)
word.freq <- sort(rowSums(m),decreasing = TRUE)
wordcloud(words = names(word.freq),freq = word.freq,min.freq = 20, rot.per = 0.2,
          colors = brewer.pal(6,"Dark2"),max.words = 100,random.order = F)
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix,method = "ward.D")
plot(fit)
rect.hclust(fit,k=6)

m3 <- t(m2)
set.seed(122)
k <-5
kmeansResult <- kmeans(m3,k)
round(kmeansResult$centers,digits = 3)
for(i in 1:k){
  cat(paste("cluster",i,": ",sep = ""))
  s<- sort(kmeansResult$centers[i,],decreasing = T)
  cat(names(s)[1:5],"\n")
}

library(fpc)
pamResults <- pamk(m3,metric="manhattan")
k <- pamResults$nc
pamResult <- pamResults$pamobject
for(i in 1:k){
  cat("cluster", i,": ", colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)],"\n")
}
layout(matrix(c(1,2),2,1))
plot(pamResult,color = F,labels = 4,lines = 0, cex = 0.8, col.clus=1,col.p = pamResult$clustering)