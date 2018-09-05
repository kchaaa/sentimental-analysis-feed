# Sentimental Analysis on Tweets related to Chronological Order
# By Kevin Cha

# In Case ---
# Removes Memory 
# rm(list=ls())

setwd()

# Required Libraries ----
# General Purpose (dplyr, tidyr, ggplot2, stringr, readr, purr, tibble, forcats)
library(tidyverse)

# Sentimental Analysis
library(tidytext)

# Access Twitter
library(twitteR)

# Twitter Authentication ----
# Consumer and Access keys 
consumer_key <- "consumer key"
consumer_secret <- "secret consumer key"
access_token <- "access token"
access_secret <- "secret access token"
# Authenticate
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Retrieve Twitter Data ----
# Search Twitter for tweets related to 'chronological feed' for the last year
tweets <- twitteR::searchTwitter('chronological feed', n = 1800, lang ='en', since = '2018-08-24', until = '2018-09-01')
# Strip Retweets
tweets <- strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE)
# Convert to Data Frame
df <- twListToDF(tweets) 
# Search Twitter for tweets related to 'chronological feed' for the last year
tweets_2 <- twitteR::searchTwitter('chronological feed', n = 1800, lang ='en', since = '2018-08-08', until = '2018-09-16')
# Strip Retweets
tweets_2 <- strip_retweets(tweets_2, strip_manual = TRUE, strip_mt = TRUE)
# Convert to Data Frame
df_2 <- twListToDF(tweets_2) 

# Combine the data frames
df_3 <- full_join(df, df_2)

# Extract the data frame save it locally
saveRDS(df, file='tweets.rds')
saveRDS(df_2, file='tweets_2.rds')
saveRDS(df_3, file='tweets_3.rds') # combination of df + df_2
data <- readRDS('tweets_3.rds')


# Clean Tweets ----
# Glimpse at first few rows
head(data$text, 6)

# Clean text for analysis 
data1 <- data %>% select(text)

# Get rid of links
data1$clean_text <- gsub("http.*", "", data1$text) 
data1$clean_text <- gsub("https.*", "", data1$clean_text)
# Get rid of '@' and '#'
data1$clean_text <- gsub("@", "", data1$clean_text)
data1$clean_text <- gsub("#", "", data1$clean_text)
# Get rid of non-english characters
data1$clean_text <- gsub("[^\x01-\x7F]", "", data1$clean_text)

# 1) Convert text to lowercase, Remove Punctuation, Add Unique ID for each tweet
data1_clean <- data1 %>% 
  dplyr::select(clean_text) %>% 
  unnest_tokens(word, clean_text)

# Get Rid of Stop Words ----
data("stop_words")

cleaned_tweet_words <- data1_clean %>% 
                          anti_join(stop_words)

# Plot Top 15 Words ----
cleaned_tweet_words %>% 
  count(word, sort=TRUE) %>% 
  top_n(15) %>% 
  mutate(word=reorder(word, n)) %>% 
  ggplot(aes(x=word, y=n)) +
  geom_text(aes(label=n), position=position_dodge(width=0.9), hjust=-0.3) +
  geom_col() + 
  xlab(NULL) + 
  coord_flip() +
    labs(y="Count",
         x="Unique Words",
         title="Count of Unique Words",
         subtitle="Stop Words Removed From List")

# Sort by count
cleaned_tweet_words <- cleaned_tweet_words %>% count(word, sort=TRUE)

# Sentimental Analysis ----
# Turn Unique ID into a Column
data1_clean$tweet_index <- rownames(data1_clean)
# Combine tweet_index into 1 ex 1, 1.1, 1.2, etc => 1
data1_clean$tweet_index <- as.integer(data1_clean$tweet_index)

# Afinn 
afinn <- data1_clean %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(index=tweet_index) %>% 
  summarise(sentiment=sum(score)) %>% 
  mutate(method="AFINN")
  
# Bing and NRC
bing_and_nrc <- bind_rows(data1_clean %>% 
                            inner_join(get_sentiments("bing")) %>% 
                            mutate(method="Bing et al."),
                          data1_clean %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", "negative"))) %>% 
                            mutate(method="NRC")) %>% 
  count(method, index=tweet_index, sentiment) %>% 
  spread(sentiment, n, fill=0) %>% 
  mutate(sentiment=positive - negative)
# bing
bing <- bing_and_nrc %>% 
          filter(method=="Bing et al.")
# NRC
nrc <- bing_and_nrc %>% 
          filter(method=="NRC")
  
# Plot Sentiment
bind_rows(afinn,
          bing_and_nrc) %>% 
  ggplot(aes(index, sentiment, fill=method)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~method, ncol=1, scales="free_y")

# Sentiment Count ----
# AFINN Count
afinn_positive <- sum(afinn$sentiment > 1)
afinn_negative <- sum(afinn$sentiment < -1)
afinn_neutral <- sum(afinn$sentiment == c(0,1,-1))
# DataFrame
method <- 'afinn'
type <- c('positive', 'negative', 'neutral')
count <- as.integer(c(afinn_positive, afinn_negative, afinn_neutral))
afinn_count <- data.frame(cbind(method, type, count), stringsAsFactors=FALSE)
afinn_count$count <- as.integer(afinn_count$count)

# NRC Count 
nrc_positive <- sum(nrc$sentiment > 1)
nrc_negative <- sum(nrc$sentiment < -1)
nrc_neutral <- sum(nrc$sentiment == c(0,1,-1))
# DataFrame
method <- 'nrc'
type <- c('positive', 'negative', 'neutral')
count <- as.integer(c(nrc_positive, nrc_negative, nrc_neutral))
nrc_count <- data.frame(cbind(method, type, count), stringsAsFactors=FALSE)
nrc_count$count <- as.integer(nrc_count$count)

# Bing Count
bing_positive <- sum(bing$sentiment > 1)
bing_negative <- sum(bing$sentiment < -1)
bing_neutral <- sum(bing$sentiment == c(0,1,-1))
# DataFrame
method <- 'bing'
type <- c('positive', 'negative', 'neutral')
count <- c(bing_positive, bing_negative, bing_neutral)
bing_count <- data.frame(cbind(method, type, count), stringsAsFactors=FALSE)
bing_count$count <- as.integer(bing_count$count)


# Plot: By Type
bind_rows(afinn_count, nrc_count, bing_count) %>% 
ggplot(aes(x=type, y=count)) +
  geom_bar(aes(fill=method), position="dodge", stat="identity") +
  labs(y="Count",
       x="Type",
       title="Count of Each Sentiment Type") +
 ylim(0,80)

# Plot: By Method
bind_rows(afinn_count, nrc_count, bing_count) %>% 
  ggplot(aes(x=method, y=count)) +
  geom_bar(aes(fill=type), position="dodge", stat="identity") +
  labs(y="Count",
       x="Method",
       title="Count of Each Method") +
  ylim(0,80)
