install.packages("devtools")
install.packages("pacman")
install.packages("ggeasy")
# Load----
library(tm)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library("qdap")
library("dplyr")
library("plotrix")
library("dendextend")
library("ggthemes")
library("RWeka")
library("reshape2")
library("quanteda")
library("textcat")
library(caTools)
library("rpart")
library(prp)
library(rpart.plot)
library(date)
library(devtools)
library(pacman)
library(ggeasy)
library(sentimentr)
library(dplyr)
library(MASS)


pacman::p_load(tidyverse,tidytext,tm,topicmodels,magrittr,
               jtools,gridExtra,knitr,widyr,ggraph,igraph,kableExtra)
devtools::install_github("56north/happyorsad")

# import data----
setwd("/Users/samo/Documents/CSS program/cultural markets/project/re-exam/datasets/google play app reviews")
reviews <- read.csv("reviews.csv")
apps <- read.csv("apps.csv")
reviews <- na.omit(reviews)

glimpse(reviews)
nrow(reviews) # 16388

#creating Y variables:----
## recommend variable where it is 1 when rating as 5 and 0 other wise
## like variable where it is 1 if thumbs up and 0 otherwise.
## recommend variable (where customers recommend app to buy or install)
reviews <- reviews %>%
  mutate(score = as.integer(score),
         recommended = as.integer(score >= 4))
## like variable (where customers give their opinion and liked the app)
reviews <- reviews %>%
  mutate(thumbsUpCount = as.integer(thumbsUpCount),
         liked = as.integer(thumbsUpCount == 0))

table(reviews$recommended)
table(reviews$liked)

# detect language of reviews----
reviews$language <-  textcat(reviews$content)

# plot languages----
ggplot(reviews, aes(x=language))+
  geom_bar(stat="count", fill="pink") +
  ggtitle("Figure 1: Language Count") +
  xlab("Language") + ylab("Count") +
  theme_minimal() + coord_flip()

# select only english:----
reviews <-  subset(reviews, language == "english")

# select the top two rated apps
unique(reviews$appId) # "com.anydo"
reviews <- reviews[reviews$appId %in% c("com.anydo", "cc.forestapp"), ]

# plot ratings (scores)----
ggplot(reviews, aes(x=score))+
  geom_bar(stat="bin", bins= 9, fill="dark green") +
  geom_text(stat='count', aes(label=..count..), vjust=1.6, color="white") +
  ggtitle("Score Counts") +
  xlab("Score") + ylab("Count") +
  theme_minimal()

# clean content----
## pre-processing text:
clean.text = function(x)
{
  # convert to lower case
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # some other cleaning text
  x = gsub('https://','',x)
  x = gsub('http://','',x)
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:punct:]]', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = str_replace_all(x,"[^[:graph:]]", " ")
  return(x)
}
library(tidyverse)
library(stringr)

reviews$cleanText <- clean.text(reviews$content)
# remove empty results (if any)
idx <- which(cleanText == " ")
cleanText <- clean.text[cleanText != " "]


# clean up sentences with R's regex-driven global substitute, gsub() function:----
reviews$cleanText = gsub('https://','',reviews$cleanText)
reviews$cleanText = gsub('http://','',reviews$cleanText)
reviews$cleanText = gsub('[^[:graph:]]', ' ',reviews$cleanText)
reviews$cleanText = gsub('[[:punct:]]', '', reviews$cleanText)
reviews$cleanText = gsub('[[:cntrl:]]', '', reviews$cleanText)
reviews$cleanText = gsub('\\d+', '', reviews$cleanText)
reviews$cleanText = str_replace_all(reviews$cleanText,"[^[:graph:]]", " ")
# and convert to lower case:
reviews$cleanText = tolower(reviews$cleanText)

# split into words. str_split is in the stringr package
word.list = str_split(reviews$cleanText, '\\s+')
# sometimes a list() is one level of hierarchy too much
words = unlist(word.list)

# Calculating the sentiment score----
table(reviews$score)
# 1   2   3   4   5
# 167 178 346 166 152

# Histogram of sentiment scores----
library(ggplot2)
library(dplyr)
library(hrbrthemes)

reviews %>%
  ggplot(aes(x=score, fill = appId)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")+
ylab("Frequency") +
  xlab("sentiment score") +
  ggtitle("Distribution of Sentiment scores of the tweets") +
  ggeasy::easy_center_title()+    facet_wrap(~appId)

#Analysis: From the Histogram of Sentiment scores, we can see that around half of the tweets have sentiment score as zero i.e. Neutral and overall as expected, the distribution depicts negative sentiment in the tweets related to global warming, since it is a major issue of concern.

# Barplot of sentiment type (positive, negative)----
neutral <- length(which(reviews$score == 0))
positive <- length(which(reviews$score >=3))
negative <- length(which(reviews$score < 3))
Sentiment <- c("Positive","Neutral","Negative")
Count <- c(positive,neutral,negative)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)
ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(stat = "identity", aes(fill = Sentiment))+
  ggtitle("Barplot of Sentiment type of reviews")

## create a corpus----
TextDoc <- Corpus(VectorSource(reviews$cleanText))

# cleaning corpus----
#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove context specific stop words
TextDoc <- tm_map(TextDoc, removeWords,c("use", "get","like", "made", "cant", "im", "had", "just", "I", "s", "app", "as", "and", "dont", "one", "wont", "can", "make", "tasks","really","please","even","every", "also", "now", "the", "and", "that", "for", "doesnt"))

# Stemming ( reduce derived words to root)----
TextDoc <- tm_map(TextDoc , stemDocument)


# WordCloud plotting---- cancelled ##
set.seed(123)
wordcloud(TextDoc, min.freq = 1, max.words = 100, scale = c(2.2,1),
          colors=brewer.pal(8, "Dark2"), random.color = T, random.order = F)
#Analysis: Wordcloud helps us to visually understand the important terms frequently used in the tweets related to global warming, here for example, “climate change”, “environmental”, “temperature”, “emissions”, etc.

# plot most frequent words---- cancelled ##
# # Find the 20 most frequent terms: term_count
term_count <- freq_terms(TextDoc, 20)

# Plot 20 most frequent terms
plot(term_count, main ="most frequent terms", fill = color)
print(term_count + ggtitle("most frequent terms"))

#Analysis: we can infer that the most frequently used terms in the tweets related to global warming are, “climate”, “climatechange”, “since”, “biggest”, “hoax”, etc.



# Create Document Term Matrix----
DTM <- DocumentTermMatrix(TextDoc)

# Build a term-document matrix----
tdm <- TermDocumentMatrix(TextDoc)
tdm_m <- as.matrix(tdm)


# Sort by descearing value of frequency
dtm_v <- sort(rowSums(tdm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)
#After cleaning the text data, the next step is to count the occurrence of each word, to identify popular or trending topics. Using the function TermDocumentMatrix() from the text mining package, you can build a Document Matrix – a table containing the frequency of words.

# Plot the most 5 frequent words
barplot(dtm_d[1:10,]$freq, las = 2, names.arg = dtm_d[1:10,]$word,
        col ="light blue", main ="Figure 2: Top 10 most frequent words",
        ylab = "Word frequencies")

#generate word cloud of dtm----
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40,
          colors=brewer.pal(8, "Dark2"))
## Below is a brief description of the arguments used in the word cloud function;
#words – words to be plotted
#freq – frequencies of words
#min.freq – words whose frequency is at or above this threshold value is plotted (in this case, I have set it to 5)
#max.words – the maximum number of words to display on the plot (in the code above, I have set it 100)
#random.order – I have set it to FALSE, so the words are plotted in order of decreasing frequency
#rot.per – the percentage of words that are displayed as vertical text (with 90-degree rotation). I have set it 0.40 (40 %), please feel free to adjust this setting to suit your preferences
#colors – changes word colors going from lowest to highest frequencies
#The word cloud shows additional words that occur frequently and could be of interest for further analysis. Words like “need”, “support”, “issu” (root for “issue(s)”, etc. could provide more context around the most frequently occurring words and help to gain a better understanding of the main themes.

# Find associations ----
findAssocs(dtm, terms = c("task","calendar","list"), corlimit = 0.25)
# Find associations for words that occur at least 50 times----
findAssocs(dtm, terms = findFreqTerms(dtm, lowfreq = 30), corlimit = 0.25)

# Emotion Classification:----
syuzhet_vector <- get_sentiment(reviews$cleanText, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

bing_vector <- get_sentiment(reviews$cleanText, method="bing")
head(bing_vector)
summary(bing_vector)
#affin
afinn_vector <- get_sentiment(reviews$cleanText, method="afinn")
head(afinn_vector)
summary(afinn_vector)

#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score:
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust
# It also counts the number of positive and negative emotions found in each row
d<-get_nrc_sentiment(reviews$cleanText) #emotion classification
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)
library(kableExtra)

  kbl(head(d) , caption = "Table 3: Data frame returned by get_nrc_sentiment function") %>%
  kable_paper("hover", full_width = F)
# plotting charts of emotions----
#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:253]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Figure 5: emotions classification in sentiments")
## Bar Plot showing the count of words in the text, associated with each emotion

#Plot two - count of words associated with each sentiment, expressed as a percentage----
barplot(
  sort(colSums(prop.table(d[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Figure 6: Emotions in percent", xlab="Percentage"
)
## This bar plot allows for a quick and easy comparison of the proportion of words associated with each emotion in the text. The emotion “trust” has the longest bar and shows that words associated with this positive emotion constitute just over 20% of all the meaningful words in this text. On the other hand, the emotion of “disgust” has the shortest bar and shows that words associated with this negative emotion constitute less than 7% of all the meaningful words in this text. Overall, words associated with the positive emotions of “trust” and “anticipation” account for almost 45% of the meaningful words in the text, which can be interpreted as a good sign of positive

# Comparison of Corpus¶----
pos_comments <- subset(reviews$cleanText , reviews$recommend==1)
neg_comments <- subset(reviews$cleanText , reviews$recommend==0)
#paste and collapse positive and negative comments

pos_terms <- paste(pos_comments , collapse =" ")
neg_terms <- paste(neg_comments , collapse =" ")

#Combine both positive and negative terms
all_terms <- c(pos_terms, neg_terms)

#Creating corpus for all the terms
all_corpus <- VCorpus(VectorSource(all_terms))
all_corpus <-  tm_map(all_corpus,removeWords,c("use", "get","like", "made", "cant", "im", "had", "just", "I", "s", "app", "as", "and", "dont", "one", "wont", "can", "make", "tasks","really","please","even","every", "also", "now", "the", "and", "that", "for", "doesnt"))
all_tdm <- TermDocumentMatrix(
  # Use all_corpus
  all_corpus,
  control = list(
    # Use TFIDF weighting
    #weighting = weightTfIdf,
    # Remove the punctuation
    removePunctuation = TRUE,
    #Remove numbers
    removeNumbers =TRUE,
    #Stemming of Documents
    stemDocument = TRUE,
    #Convert to lowercase
    tolower = TRUE ,
    # Use English stopwords
    stopwords = stopwords("english"),
    removeWords,c("get","like", "made", "cant", "im", "had", "just", "I", "s", "app", "as", "and", "dont", "one", "wont", "can", "make", "tasks","really","please","even","every", "also", "now")

  )
)

all_tdm_m <- as.matrix(all_tdm)
colnames(all_tdm_m) <- c("recommended","not recommended")
all_term_freq <- rowSums(all_tdm_m)
all_term_freq <- sort(all_term_freq,TRUE)
all_term_freq[1:20]

# Cloud plot
set.seed(1234)
comparison.cloud(
  all_tdm_m,
  max.words = 100,
  colors = c("darkgreen", "darkred")
)

#Polarized tag plot¶----
common_words  <- subset(all_tdm_m,all_tdm_m[,1] > 0 & all_tdm_m[,2] > 0)
difference <- abs(common_words[,1]-common_words[,2])
common_words <- cbind(common_words , difference)
common_words <- common_words[order(common_words[,3], decreasing =T),]
head(common_words)

prop.table(table(reviews$recommended))


