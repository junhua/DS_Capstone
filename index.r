library(gdata)  
library(stringi)
library(stringr)
library(NLP)
library(tm)
library(ggplot2)
library(quanteda)
library(slam)
# library(data.table)

##################### Read data #####################

setwd("~/Playground/coursera_capstone/")

usblogs = readLines("data/final/en_US/en_US.blogs.txt")
# ustwitter = readLines("data/final/en_US/en_US.twitter.txt")
# usnews = readLines("data/final/en_US/en_US.news.txt")
# corpus.blogs = Corpus(VectorSource(usblogs))

# Overcome warning: lines contain an embedded nul
#ustwitter = gsub("\032", "", ustwitter, ignore.case = F, perl = T)
processData = function(data) {
  vc=VCorpus(VectorSource(data))
  vc=tm_map(vc,removePunctuation)
  vc=tm_map(vc,removeNumbers)
  vc=tm_map(vc,content_transformer(tolower))
  vc=tm_map(vc,removeWords, stopwords("english"))
  vc=tm_map(vc,stemDocument)
  vc=tm_map(vc,stripWhitespace)
  tdm = TermDocumentMatrix(vc)
  freq = sort(row_sums(tdm, na.rm=TRUE), decreasing = TRUE)
  word=names(freq)
  data.frame(word=word,freq=freq)  
}

usblogs.sample = sample(usblogs, length(usblogs), size=1000)
df = processData(usblogs.sample)
# vc=VCorpus(VectorSource(usblogs.sample))
# vc=tm_map(vc,removePunctuation)
# vc=tm_map(vc,content_transformer(tolower))
# vc=tm_map(vc,removeWords, stopwords("english"))
# vc=tm_map(vc,stemDocument)
# vc=tm_map(vc,stripWhitespace)
# tdm = TermDocumentMatrix(vc)
# freq = sort(row_sums(tdm, na.rm=TRUE), decreasing = TRUE)
# word=names(freq)
# df.blog=data.frame(word=word,freq=freq)
# df.blog.top20 = df.blog[1:20,]
# df.blog.top20
barplot(df$freq,names.arg = df$word)



corpus.blogs.sample = Corpus(VectorSource(usblogs.sample))



##################### Summarize data #####################

# Word counts
sum(stri_length(corpus.blogs.sample))
# Line counts
length(corpus.blogs.sample)
# Avg length of word
mean(stri_length(usblogs.sample))
max(stri_length(usblogs.sample))
min(stri_length(usblogs.sample))
# Document-term Matrix
# myDtm = TermDocumentMatrix(corpus.blogs.sample, control = list(minWordLength = 1))
# inspect(myDtm[0:20,1:5])

##################### Clean up data #####################
# transform to lower, remove punctuation, numbers, stopwords and whitespace
corpus.blogs.sample = tm_map(corpus.blogs.sample, tolower)
corpus.blogs.sample = tm_map(corpus.blogs.sample, removePunctuation)
corpus.blogs.sample = tm_map(corpus.blogs.sample, removeNumbers)

myStopwords = c(stopwords('english'), stopwords('SMART'))
idx = which(myStopwords == "r")
myStopwords = myStopwords[-idx]
corpus.blogs.sample = tm_map(corpus.blogs.sample, removeWords, myStopwords)
corpus.blogs.sample = tm_map(corpus.blogs.sample,stripWhitespace) 

       
df<-data.frame(text=unlist(sapply(corpus.blogs.sample$content, '[')), stringsAsFactors=F)



##################### Stemming Words #####################
inspect(corpus.blogs.sample)
blogs.dfm = dfm(corpus.blogs.sample, verbose=FALSE)




##################### Archive #####################
# max(stri_length(c(stri_length(ustwitter), mydata, usnews)))
# sum(str_count(ustwitter, "love")>0)/sum(str_count(ustwitter, "hate")>0)
# ustwitter[grep("biostats", ustwitter)]
# sum(str_count(ustwitter, "A computer once beat me at chess, but it was no match for me at kickboxing"))
# sample.blogs = usblogs[rbinom(length(usblogs),1,.1)]
# sample.twitter = ustwitter[rbinom(length(ustwitter),1,.1)]
# sample.news = usnews[rbinom(length(usnews),1,.1)]
# str = concat(sample.blogs, sample.twitter, sample.news)


# Ngram model for 1,2 or 3 words

# path = file.path("data","final", "en_US")
# docs = Corpus(DirSource(path))