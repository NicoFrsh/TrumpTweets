# Project Analysis of Trumps Tweets:
# personal, mean tweets from Android, iPhone tweets from his staff 
# based on http://varianceexplained.org/r/trump-tweets/

library(dplyr)
library(purrr)
library(twitteR)


#### NOT WORKING ########################################

# You'd need to set global options with an authenticated app
setup_twitter_oauth(getOption("twitter_consumer_key"),
                    getOption("twitter_consumer_secret"),
                    getOption("twitter_access_token"),
                    getOption("twitter_access_token_secret"))

# We can request only 3200 tweets at a time; it will return fewer
# depending on the API
trump_tweets <- userTimeline("realDonaldTrump", n = 3200)
trump_tweets_df <- tbl_df(map_df(trump_tweets, as.data.frame))

####################################################

# if you want to follow along without setting up Twitter authentication,
# just use my dataset:
load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))

# cleaning data, only iphone or android tweets
library(tidyr)

tweets <- trump_tweets_df %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") %>%
  filter(source %in% c("iPhone", "Android"))

# check time of tweet, which seems to be significant
library(lubridate)
library(scales)
library(ggplot2)

tweets %>%
  count(source, hour = hour(with_tz(created, "EST"))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

# Android (Trumps) tweets in the morning, iPhone(staff) tweets in the afternoon/evening

# Now check the "manually retweets" (quoting others by surrounding the tweet with quotation marks)
library(stringr)

tweet_quotation_count <- tweets %>%
  count(source, quote = ifelse(str_detect(text, '^"'), "Quoted", "Not Quoted"))

ggplot(tweet_quotation_count, aes(source, n, fill = quote)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of Tweets", fill = "")

# We see that almost all the manual retweets come from Android

# Now we check which tweets include an image
tweet_picture_counts <- tweets %>%
  filter(!str_detect(text, '^"')) %>%
  count(source,
        picture = ifelse(str_detect(text, "t.co"),
                         "Picture/link", "No picture/link"))

ggplot(tweet_picture_counts, aes(source, n, fill = picture)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "")

# Most picture including tweets come from iPhone, which makes sense to our hypothesis,
# because staff / campaign managers tend to write announcement tweets about events 

######### 
# Next we'll focus on the choice of words

library(tidytext)

# seperate all words
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

# check most common words
tweet_word_count <- tweet_words %>%
  count(word) %>%
  arrange(desc(n)) %>%
  slice(1:20)

ggplot(tweet_word_count, aes(x = reorder(word,n), y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip()

# Now, log odds ratio ??
android_iphone_ratios <- tweet_words %>%
  count(word, source) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0)  %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(logratio = log2(Android / iPhone)) %>%
  arrange(desc(logratio))

ggplot(slice(android_iphone_ratios,c(1:15,(n()-14):n())), aes(x = reorder(word,logratio),
                                                              y = logratio,
                                                              fill = ifelse(logratio > 0, "Android", "iPhone"))) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Words", y = "Logratio", fill = "")

# Observations:
# - most Hashtags from iPhone -> makes sense
# - emotionally charged words from Android -> Trump

# Use NRC Word-Emotion-Association Lexicon to associate words with sentiments

nrc <- sentiments %>%
  dplyr::select(word, sentiment)

#nrc

sources <- tweet_words %>%
  group_by(source) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id, source, total_words)

by_source_sentiment <- tweet_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, id) %>%
  ungroup() %>%
  complete(sentiment, id, fill = list(n = 0)) %>%
  inner_join(sources) %>%
  group_by(source, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  ungroup()

head(by_source_sentiment)
