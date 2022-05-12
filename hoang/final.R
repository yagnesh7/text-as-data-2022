rm(list = ls())
set.seed(100)
library(quanteda)
library(quanteda.corpora)
library(quanteda.textstats)
library(dplyr)
library(ggplot2)
library(quanteda.corpora)
library(quanteda.textstats)
library(quanteda.textplots)
library(stylest)


setwd("/home/hoang/Desktop/NYU/TAD/final/")

# 1.3 Devtools and the quanteda corpus -----------------------

# Install the package "devtools" which is used to install packages directly from Github
# install.packages("devtools")
#library("devtools")

# Use devtools to install some sample data
#devtools::install_github("quanteda/quanteda.corpora")
#devtools::install_github("quanteda/quanteda.textplots")

# Load it into our environment


# Read about the data available: https://github.com/quanteda/quanteda.corpora

# 1.4 Versions of quanteda -----------------------

# to check version
packageVersion("quanteda")

# How would you get an older version of quanteda? 
#(For example, if you accidentally installed the 
#dev version from GitHub but you want to go back to 
#the last stable release, or you want a legacy version to 
#support old code.)

# - Check the CRAN archive
# use the install_version function, e.g.:
# devtools::install_version("quanteda", version = "0.99.12", repos = "http://cran.us.r-project.org")
# If you want the latest dev version of quanteda, it's on GitHub, devtools::install_github("quanteda/quanteda") 


#-----------------------------
# 1 THE CORPUS OBJECT
#-----------------------------

# 1.1 load the State of the Union (tweet_corpus) corpus ---------------------
tweet_data <- read.csv("final_processed_data.csv", stringsAsFactors = FALSE)
tweet_corpus <- corpus(tweet_data, text_field = "full_text")

head(docvars(tweet_corpus))
docvars(tweet_corpus)
meta(tweet_corpus)  # corpus-level variables

summary(tweet_corpus) # summary of corpus

ndocs <- ndoc(tweet_corpus)

# summary of the corpus 
corpusinfo <- summary(tweet_corpus, n = ndocs)  
head(corpusinfo)
summary(corpusinfo$Tokens)
summary(corpusinfo$Types)
summary(corpusinfo$Sentences)


token_plot <- ggplot(data = corpusinfo, aes(x = depression, y = Tokens))  + geom_point() + theme_bw()
token_plot






dep_corpusinfo <- corpusinfo[corpusinfo$depression == 1, ]  

summary(dep_corpusinfo$Tokens)
summary(dep_corpusinfo$Types)
summary(dep_corpusinfo$Sentences)


nodep_corpusinfo <-  corpusinfo[corpusinfo$depression == 0, ]   

summary(nodep_corpusinfo$Tokens)
summary(nodep_corpusinfo$Types)
summary(nodep_corpusinfo$Sentences)

dep_corpus <- corpus_subset(tweet_corpus, depression == 1)
no_corpus <- corpus_subset(tweet_corpus, depression == 0)

# key words in context (KWIC) #it is case sensitive
head(kwic(dep_corpus, pattern = "like", valuetype = "regex", window = 6), 10)

head(kwic(no_corpus, pattern = "like", valuetype = "regex", window = 6), 10)

dep_dfm <- dfm(dep_corpus, stem=TRUE, tolower=TRUE, remove_punct = TRUE, remove = stopwords("english"))
no_dfm  <- dfm(no_corpus, stem=TRUE, tolower=TRUE, remove_punct = TRUE, remove = stopwords("english"))
# wordclouds
textplot_wordcloud(dep_dfm, max_words = 100)
textplot_wordcloud(no_dfm, max_words = 100)



vocab_custom <- stylest_select_vocab(tweet_data$preprocess, tweet_data$depression,  # fits n-fold cross-validation
                                     filter = NULL, smooth = 1, nfold = 5,
                                     cutoff_pcts = c(25, 50, 75, 99))
vocab_custom

vocab_custom$cutoff_pct_best  # percentile with best prediction rate
vocab_custom$miss_pct  # rate of incorrectly predicted speakers of held-out texts
# (2) subset features
vocab_subset <- stylest_terms(tweet_data$preprocess, tweet_data$depression, vocab_custom$cutoff_pct_best , filter = NULL) # USE SAME FILTER
# (3) fit model with "optimal" percentile threshold (i.e. feature subset)
style_model <- stylest_fit(tweet_data$preprocess, tweet_data$depression, terms = vocab_subset, filter = NULL)

summary(style_model)

# explore output
head(stylest_term_influence(style_model, tweet_data$preprocess, tweet_data$depression))  # influential terms

authors <- unique(tweet_data$depression)
term_usage <- style_model$rate
lapply(authors, function(x) head(term_usage[x,][order(-term_usage[x,])])) %>% setNames(authors)

# odds for known texts
odds <- stylest_odds(style_model, novels_excerpts$text, novels_excerpts$author)
odds

# (4) predict speaker of a new text
new_text <- emma$text[30:75] %>% paste(., collapse = "") 
pred <- stylest_predict(style_model, new_text)
pred$predicted
pred$log_probs



