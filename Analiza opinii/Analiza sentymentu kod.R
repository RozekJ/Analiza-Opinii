setwd("C:/Users/kubir/Desktop/OpinionMining")
Data<-read.csv("CommentsOM.csv",
               header = TRUE,
               sep = ",",  # or ";"
               strip.white = TRUE,
               fill = TRUE,
               comment.char = "#",
               stringsAsFactors = FALSE)
head(Data)
# Transformation into Matrix
MyData = as.data.frame.matrix(Data) 
MyData1 <- MyData[MyData$Brand=="Under Armour",]
MyData_1 <- MyData1[,1]
n<-length(MyData_1) 
n 
head(MyData_1) 
library(tm)
docs <-VCorpus(x = VectorSource(MyData_1),
               readerControl = list(reader=readPlain,
                                    language="en"))
docs
docs<-tm_map(docs,removePunctuation)
docs <-tm_map(docs, removeNumbers)
#docs <-tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

for (j in seq(docs)) {
  docs[[j]] <-gsub("/", "", docs[[j]])
  docs[[j]] <-gsub("@", "", docs[[j]])
  docs[[j]] <-gsub("–", "", docs[[j]])
  docs[[j]] <-gsub("’", "", docs[[j]])
  docs[[j]] <-gsub("“", "", docs[[j]])
  docs[[j]] <-gsub("‘", "", docs[[j]])
  docs[[j]] <-gsub(")", "", docs[[j]])
  docs[[j]] <-gsub("”", "", docs[[j]])
}
#docs <-tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))
docs <-tm_map(docs, tolower)   
#docs <-tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))
length(stopwords("english"))   
stopwords("english")
docs <-tm_map(docs, removeWords, stopwords("English"))
#docs <-tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))
docs <-tm_map(docs, stripWhitespace)
#docs <-tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))
library(SnowballC)
for (j in seq(docs)) {
  docs[[j]]<-stemDocument(docs[[j]], language = "english")  
} 
writeLines(as.character(docs[[1]]))
docs <-tm_map(docs, PlainTextDocument)
dtm <-DocumentTermMatrix(docs)
dtm 
write.csv(as.matrix(dtm),file="DTM.csv")
rownames(dtm)<-seq(1,n)
rownames(dtm)
freqr <-colSums(as.matrix(dtm))
length(freqr)
freq <-sort(freqr, decreasing=TRUE)
mk<-min(head(freq, 30))
mk
wf=data.frame(word=names(freq),freq=freq)
library(ggplot2)
# Full Zipf'slaw
#dev.new(width = 100, height = 100, unit = "px") #could be useful
p <-ggplot(subset(wf, freq>1), aes(x = reorder(word, -freq), y = freq))
p <-p + geom_bar(stat="identity")
p <-p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
# Zipf'slaw with minimal frequency = MK#dev.new(width = 100, height = 100, unit = "px")#could be useful
p <-ggplot(subset(wf, freq>mk), aes(x = reorder(word, -freq), y = freq))
p <-p + geom_bar(stat="identity")
p <-p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
library(wordcloud)
set.seed(42)
wordcloud(names(freq),freq, min.freq=70)
set.seed(142)
wordcloud(names(freq), freq, max.words=100)
wordcloud(names(freq), freq, min.freq=70,colors=brewer.pal(6,"Dark2"))
set.seed(142)
dark2<-brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)

raw.sum=apply(dtm,1,FUN=sum)
raw.sum
mmm<-nrow(dtm[raw.sum==0,])
mmm
if (mmm==0) {
  dtm2<-dtm
  NN<-nrow(dtm)
  NN
} else {
  dtm2<-dtm[raw.sum!=0,]
  NN<-nrow(dtm2)
}
n
NN
dtm2
library(topicmodels) 
burnin <-4000
iter <-2000
thin <-500
seed <-list(2003,5,63,100001,765)
nstart <-5
best <-TRUE
k <-4
ldaOut <-LDA(dtm2, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
str(ldaOut)
ldaOut.terms <-as.matrix(terms(ldaOut,10))
ldaOut.terms
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms_1.csv"))
topicProbabilities <-as.data.frame(ldaOut@gamma)
topicProbabilities
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
col.sum=apply(topicProbabilities,2,FUN=sum)
col.sum<-as.matrix(col.sum)
dim(col.sum)
sum.TP=col.sum/sum(col.sum)
sum.TP
ldaOut.topics <-as.matrix(topics(ldaOut))
ldaOut.topics
write.csv(ldaOut.topics,file=paste("LDAGibbs",k, "DocsToTopics.csv"))

nrow(ldaOut.topics)
Comment<-seq(1, NN, by=1)
Comment
wf=data.frame(Comment=Comment, Topics=ldaOut.topics)
wf
topic1<-wf[wf[2] == 4,] 
topic1$Comment
kk1<-nrow(topic1)
kk1
kk<-nrow(dtm2)
kk
list1<-c()
i=1
while(i<=kk) {
  if (wf[i,2]==4) { 
    list1<-c(list1,i)}
  i=i+1
}
list1
wf1=NULL
for (i in 1:kk) {
  for (j in 1:kk1) {
    if (i==list1[j]){
      c <-data.frame(file=as.character(wf[list1[j],1]),document=as.character(docs[[i]]))
      wf1=rbind(wf1,c)
    }
  }
}
wf1
wf1$document[1]
Topic_4_docs <-Corpus(VectorSource(as.character(wf1$document))) 
writeLines(as.character(Topic_4_docs [[1]])) 
Topic_4_docs
mycorpus_dataframe <-data.frame(text=wf1$document, stringsAsFactors=F)
mycorpus_dataframe
write.csv(mycorpus_dataframe, "Topic_4_docs.csv", row.names=FALSE) 
dtm_topic_4 <-DocumentTermMatrix(Topic_4_docs)
dtm_topic_4
freqr <-colSums(as.matrix(dtm_topic_4))
length(freqr)
freq <-sort(freqr, decreasing=TRUE)
mk<-min(head(freq, 30))
mk
wf1=data.frame(word=names(freq),freq=freq)
library(ggplot2)
dev.off()
dev.new(width = 100, height = 100, unit = "px")
p <-ggplot(subset(wf1, freq>mk), aes(x = reorder(word, -freq), y = freq))
p <-p + geom_bar(stat="identity")
p <-p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
library(wordcloud)
dev.off()
dev.new(width = 100, height = 100, unit = "px") 
set.seed(142)
dark2 <-brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)


#install.packages("plyr")
#install.packages("syuzhet", dependencies=TRUE, repos='http://cran.rstudio.com/')
library("plyr")
library("stringr")
library("syuzhet")
neg=scan("C:/Users/kubir/Desktop/OpinionMining/negative-words.txt", what="character", comment.char=";" )
pos=scan("C:/Users/kubir/Desktop/OpinionMining/positive-words.txt", what="character", comment.char=";" )
score.sentiment = function(docs, pos.words, neg.words, .progress='none')
{
  scores = laply(docs_s, function(docs, pos.words, neg.words) {
    word.list = str_split(docs, '\\s+')
    words = unlist(word.list) 
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) -sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=docs)
  return(scores.df)
}
result=c()
docs<-Topic_4_docs 
m1=c()
for (j in seq(docs)) {
  docs_s=as.character(docs[[j]])
  print(docs_s)
  result = score.sentiment(docs_s, pos, neg)
  newRow1 <-data.frame(Doc=j,Score = result$score, Documents = result$text)
  #print(newRow1) 
  m1<-rbind(m1,newRow1)
  #print(m1) 
}
m1[1:3,]
summary(m1$Score)
minn<-min(m1$Score)
minn
maxx<-max(m1$Score)
maxx
mmm<-maxx-minn
mmm
h<-hist(m1$Score, 
        main="Histogram for the Sentiment by Stan wizualny i fizyczny   ", 
        xlab="Scores",
        ylab="Number of of Opinions",
        right=FALSE,
        border="blue",
        col="green",
        freq=TRUE,
        las=1,
        xlim=c(minn,maxx),
        breaks=mmm
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5),cex = 0.8, pos = 1)
m1$Score
h$count
hist(m1$Score,
     main="Histogram for the Sentiment by Stan wizualny i fizyczny    ", 
     xlab="Scores", 
     ylab="Probability",
     border="blue", 
     col="green",
     prob = TRUE,
     right=FALSE,
     xlim=c(minn,maxx),
     breaks=mmm
)
lines(density(m1$Score))
m11<-as.matrix(m1)
m11
write.csv(m11, file="Sent_1.csv")

pos1<-m1[m1$Score>=1,]
pos1$Documents
pos1$Score
length(pos1$Score)
neu1<-m1[(m1$Score<1)&(m1$Score>=0),]
neu1$Documents
neu1$Score
length(neu1$Score)
neg1<-m1[m1$Score<0,]
neg1$Score
length(neg1$Score)
neg1$Documents
pos_docs_4<-Corpus(VectorSource(pos1$Documents))
pos_docs_4
neu_docs_4<-Corpus(VectorSource(neu1$Documents))
neu_docs_4
neg_docs_4<-Corpus(VectorSource(neg1$Documents))
neg_docs_4
pos_docs_4_dataframe <-data.frame(text=pos1$Documents, stringsAsFactors=F)
pos_docs_4_dataframe
write.csv(pos_docs_4_dataframe,'Pos_Topic_4_Comments.csv', row.names=FALSE)
neg_docs_4_dataframe <-data.frame(text=neg1$Documents, stringsAsFactors=F)
neg_docs_4_dataframe
write.csv(neg_docs_4_dataframe,'Neg_Topic_4_Comments.csv', row.names=FALSE)
neu_docs_4_dataframe <-data.frame(text=neu1$Documents, stringsAsFactors=F)
neu_docs_4_dataframe
write.csv(neu_docs_4_dataframe,'Neu_Topic_4_Comments.csv', row.names=FALSE)
dtm_topic_4_pos <-DocumentTermMatrix(pos_docs_4) 
dtm_topic_4_pos 
freqr <-colSums(as.matrix(dtm_topic_4_pos)) 
length(freqr)
freq <-sort(freqr, decreasing=TRUE)
mk<-min(head(freq, 30))
mk
wf=data.frame(word=names(freq),freq=freq)
library(ggplot2)
dev.off()
dev.new(width = 100, height = 100, unit = "px") 
p <-ggplot(subset(wf, freq>mk), aes(x = reorder(word, -freq), y = freq))
p <-p + geom_bar(stat="identity")
p <-p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
library(wordcloud)
dev.off()
dev.new(width = 100, height = 100, unit = "px") 
set.seed(142)   
dark2 <-brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)
Topic_1<-read.csv("Topic_4_docs.csv",
                  header = TRUE,
                  sep = ",",  # or ";"
                  strip.white = TRUE, 
                  fill = TRUE, 
                  comment.char = "#",
                  stringsAsFactors = FALSE 
)
head(Topic_1)
Topic_1 = as.data.frame.matrix(Topic_1) 
mycorpus_dataframe1<-data.frame(text=Topic_1, stringsAsFactors=F)
mycorpus_dataframe1
usableText=str_replace_all(mycorpus_dataframe1$text,"[^[:graph:]]", " ")
d<-get_nrc_sentiment(usableText)
head(d)
d$anger
t(d)
td<-data.frame(t(d))
td[,5]
td_new <-data.frame(rowSums(td))
td_new
names(td_new)[1] <-"count"
td_new <-cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <-NULL
td_new
td_new2<-td_new[1:10,]
td_new2
library("ggplot2")
qplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment)+ggtitle("Opinion sentiments for Stan wizualny i fizyczny  by Under Armour")

