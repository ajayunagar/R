#twittr data analysis

library(rjson)
library(twitteR)
library(tm)

conkey = "OYti8F6EWXYWXkGxzXhhvg8XJ"
consecret = "47S7J2668RygKJwUMxKdoTBZ0CNInAQQ5JyWGBtopGDqTwxwF7"
acctoken = "1492337995-bUAgkG6WSIyqbNzDqibwJQj4QswENobNOYoLr6o"
accsecret = "NSM4Pbnm3QOMchrHYsrOnOplFeDKffG9JKLuhOc8k5as9"
setup_twitter_oauth(conkey,consecret,acctoken,accsecret)
tweets = searchTwitter("#JNU",10000,since = "2016-02-10", until = "2016-02-28")

#JNU_text = sapply(tweets, function(x) x$getText())
#JNU_corpus = Corpus(VectorSource(JNU_clean))
#tdm = TermDocumentMatrix(JNU_corpus, control = list(
 # removePunctuation = TRUE,
  #stopwords=TRUE,
  #removeNumbers =TRUE))
tweets.df<-