rm(list=ls())

#load text mining library
library(tm)
library(dplyr)
#set working directory (modify path as needed)
getwd()
# Read in data
DAT<- read.csv("staff.csv", stringsAsFactors = FALSE)
docs<-Corpus(VectorSource(DAT$text_value.What.do.you.like.least.about.our.school.))



#start preprocessing
#Transform to lower case
docs <-tm_map(docs,content_transformer(tolower))
#remove potentially problematic symbols
#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Good practice to check every now and then
writeLines(as.character(docs[[25]]))
#Stem documentb and remove stop words. 
myStopwords <- c("school", "teacher",
                 "kids" , "etc",   "also",  "na", "can",  "one", "need", "suggest", "even" ,"like" , "make", "think")
docs <- tm_map(docs, removeWords, myStopwords)

docs <- tm_map(docs,stemDocument)
writeLines(as.character(docs[[25]]))

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- DAT[,41]
m<-as.matrix(dtm)
#m
#dtm

## remove blank documents here - turns out you the function won't run with those. oops. 
raw.sum.r=apply(dtm,1,FUN=sum) #sum by raw each row of the table
dtm=dtm[raw.sum.r!=0,]
raw.sum.c=apply(dtm,2,FUN=sum) #sum by raw each column of the table)
    # add a function that removes common words here automatically - topic modeling relies on a sparcity assumption. 

#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))

#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk

freq[ord]
write.csv(freq[ord],"word_freq.csv")

############How Many Topics################
#install.packages("Rmpfr")
library(Rmpfr)

harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

##Set up to test 2 through 25 topics. First, you have to have 2...second, no way is hanover analyzing 25+ topics. 
seqk <- seq(2, 25, 1)
burnin <- 1000
iter <- 100
keep <- 50
system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(dtm, k = k,
                                                                     method = "Gibbs",control = list(burnin = burnin,
                                                                                                     iter = iter, keep = keep) )))

logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])


# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
#save the number of topics as k so you can use it in the actual function. Be sure to add a line so the researcher can overwrite it if necessary. 
k<-seqk[which.max(hm_many)]
k

####Visualize the results with ggplot################
## Unlikely that we will put it in a document, but nice to see regradless. 
##########################################################################
library(ggplot2)
ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
  theme(text = element_text(family= NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +theme_classic()+
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  annotate("text", x =k, y = -25000, label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
  ggtitle(expression(atop("Latent Topic Model Analysis of Staff Suggestions", atop(italic("How many distinct topics in staff suggestions?"), ""))))

ldaplot



#load topic models library
#install.packages("topicmodels")
library(topicmodels)
?LDA()

#Set parameters for Gibbs sampling
#We set the burn-in parameter to  4000. Following the burn-in period, we perform 2000 iterations, taking every 500th  iteration for further use. The reason we do this is to avoid correlations between samples. 
burnin <- 4000
iter <- 2000
thin <- 500
#We use 5 different starting points (nstart=5) - that is, five independent runs. Each starting point requires a seed integer (this also ensures reproducibility),  so I have provided 5 random integers in my seed list.
seed <-list(2003,5,63,100001,765)
nstart <- 5
#Finally I've set best to TRUE (actually a default setting), which instructs the algorithm to return results of the run with the highest posterior probability.
best <- TRUE
k<-k
k


#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", 
             control=list(nstart=nstart, seed = seed, best=best, 
                          burnin = burnin, iter = iter, thin=thin))

###############LDAViz################
#http://davidmeza1.github.io/2015/07/20/topic-modeling-in-R.html

#install.packages("LDAvis")
library(LDAvis)
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

serVis(topicmodels2LDAvis(ldaOut))

#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
v <- rownames(ldaOut.topics)
v1<-as.data.frame(v)
docs_post<-Corpus(VectorSource(v1$v))


write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))
getwd()
#top 30 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,30))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))


#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)

topicProbabilities$text <- v

#full<-dplyr::bind_cols(ldaOut.topics$V1, topicProbabilities)

write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))


summary(ldaOut)
#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])


#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])


#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))


