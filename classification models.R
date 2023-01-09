# import data----
setwd("/Users/samo/Documents/CSS program/cultural markets/project/re-exam/datasets/google play app reviews")
reviews <- read.csv("reviews.csv")
apps <- read.csv("apps.csv")

# create a binary variable:----
reviews <- reviews %>%
  mutate(score = as.integer(score),
         recommended = as.factor(score >= 4))
## like variable (where customers give their opinion and liked the app)
reviews <- reviews %>%
  mutate(thumbsUpCount = as.integer(thumbsUpCount),
         liked = as.integer(thumbsUpCount == 0))

# Creating input variables - Bag of Words approach----
corpus = VCorpus(VectorSource(reviews$cleanText))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus,removeWords,c("use", "get","like", "made", "cant", "im", "had", "just", "I", "s", "app", "as", "and", "dont", "one", "wont", "can", "make", "tasks","really","please","even","every", "also", "now"),stopwords("english"))
corpus = tm_map(corpus, stemDocument)

# Bag of words technique
dtm = DocumentTermMatrix(TextDoc)
sparse = removeSparseTerms(dtm, 0.999)
reviewsSparse = as.data.frame(as.matrix(sparse))
colnames(reviewsSparse) = make.names(colnames(reviewsSparse))

# frequency
freq<- sort(colSums(as.matrix(sparse)), decreasing=TRUE)
findFreqTerms(sparse, lowfreq=60) #identifying terms that appears more than 60times
library(ggplot2)
wf<- data.frame(word=names(freq), freq=freq)
head(wf)
#add our dependent variable back
reviewsSparse$recommended <-  reviews$recommended

####################################
# Naive Bayes
freq.terms <- findFreqTerms(dtm, 3)
clean.corpus.dtm.freq <- DocumentTermMatrix(corpus, list(dictionary = freq.terms))

## function to convert numerical data to categorical data
## The naive Bayes classifier is typically trained on data with categorical features. This poses a problem since the cells in the sparse matrix indicate a count of the times a word appears in a message. We should change this to a factor variable that simply indicates yes or no depending on whether the word appears at all.


convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
datasetNB <- apply(clean.corpus.dtm.freq, 2, convert_count)

dataset = as.data.frame(as.matrix(datasetNB))
dataset$recommended = reviewsSparse$recommended
str(dataset$recommended)
head(dataset)
dim(dataset)

#Data Splitting

set.seed(222)
split = sample(2,nrow(dataset),prob = c(0.75,0.25),replace = TRUE)
train_set = dataset[split == 1,]
test_set = dataset[split == 2,]

prop.table(table(train_set$recommended))
prop.table(table(test_set$recommended))

text.classifer <- naiveBayes(train_set, train_set$recommended)
text.pred <- predict(text.classifer, test_set)
install.packages("gmodels")
library(gmodels)  # Crosstable

CrossTable(text.pred, test_set$recommended,
           prop.chisq = FALSE,
           prop.t = FALSE,
           dnn = c('predicted', 'actual'))

Model accuracy will be: (1 + .97) / 2 = 0.985

####################################
#Logistic
dataset_lg = as.data.frame(as.matrix(reviewsSparse))
dataset_lg$recommended = reviewsSparse$recommended
glm_model <- glm(recommended ~.,family=binomial(link='logit'),data=dataset_lg)

# split data
set.seed(123)

training.samples <- dataset_lg$recommended %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- dataset_lg[training.samples, ]
test.data <- dataset_lg[-training.samples, ]
x <- model.matrix(recommended~.,train.data)
y <- as.numeric(train.data$recommended)

library(glmnet)
library(caret)

        set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial",type.measure = "mse" )
summary(cv.lasso)
plot(cv.lasso)
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
# Display regression coefficients
coef(model)
odds_ratio <- exp(coef(model))
odds_ratio <- as.matrix(exp(coef(model)))
library(dplyr)
subset(odds_ratio, odds_ratio[,1] > 1)
library(psych)
# convert matrix to dataframe
odds_ratio_df <- as.data.frame(odds_ratio)
# convert rownames to column
odds_ratio_df<- odds_ratio_df %>% rownames_to_column("variables")
colnames(odds_ratio_df) <- c("variable", "value")
# order values by descending
odds_ratio_df <- odds_ratio_df[order(odds_ratio_df$value),]
# desc orders from largest to smallest
odds_ratio_df <- odds_ratio_df %>%
  arrange(desc(value))

write.csv(odds_ratio, "odds_ratio.csv")
#The odds ratio is interpreted as follows: A product which has “compliment” in its review has 5.61 times more odds to be recommended compared to a product review which does not have the word. The odds ratio of the other words can also be similarly interpreted.

###################################
# Splitting data
set.seed(174)

split = sample.split(reviewsSparse$recommended, SplitRatio = 0.7)
reviewsSparse$split = split
train = subset(reviewsSparse, split==TRUE)
test = subset(reviewsSparse, split==FALSE)
#nrow(train)
#nrow(test)

table(train$recommended)
3390 /nrow(train) # 35 %
#Therefore the baseline accuracy is around 36%. This means that more than 3 quarters of all reviews are negative Hence the dataset is biased towards positive reviews, and the machine learning algorithm will also be more likely to predict positive reviews. There are different ways to handle this, for instance I took this into account when splitting the data into training and testing by preserving the ratio of positive to negative reviews. I could have also chosen to remove some of the positive reviews to get a more balanced dataset, however since this reflects the natural behavior of reviews, I am happy to preserve the positive bias.

# Classification tree
cartModel = rpart(recommended ~ ., data=train, method="class")
prp(cartModel)

# Cross validation (Model Enhancement)
numFolds=trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp=seq(0.001, 0.01, 0.001))
train(recommended ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
## The cross validation gave me the optimal parameter cp = 0.001 so I will now re-build the tree with this parameter.

cartModelImproved = rpart(recommended ~ ., data=train, method="class" ,control = rpart.control(minsplit = 200,  minbucket = 30, cp = 0.001))
prp(cartModelImproved)
## In this case the optimal tree has a lot of splits, which makes it more difficult to interpret, but gives us a good overview of which words are used to make the split decisions, and hence which words contribute most to the recommend/not recommend sentiment.

#Let's now obtain predictions using the new tree.
predictCARTImproved = predict(cartModelImproved, newdata=test, type="class")
table(test$recommended, predictCARTImproved)
(2300+731)/nrow(test) # 73%

#The improved model has an 74% test-set accuracy.

#This is more than 40% improvement over the baseline, and also for an NLP model this is considered very high accuracy. The main reason I am able to obtain such high accuracy from text data is because all of the text is sentiment-related.. This is specific to reviews data, if I was looking at something else, like sentiment from Twitter, a lot of the text would be factual, and would not express opinions. Therefore, reviews data is ideal for sentiment analysis.

#Secondly, as I am using rating as a predictor, this is very acurate. Some other approach like using a sentiment lexicon suffer from the intricacies of human language (e.g. humor and sarcasm can lead to positive words being used as negative and viceversa).




