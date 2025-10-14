library(dplyr)
library(ggplot2)
library(tidytext)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(topicmodels)
library(readr)
library(tidyr)       
library(stringr)    
library(scales)      
library(forcats)     
library(SnowballC)   
library(textstem)  

bbc_data <- read_csv("https://drive.google.com/uc?id=1EFUYGKfnW1uR-E4ldn_mVHgjHGezTsp-&export=download")

# Stopwords
data("stop_words")

custom_stopwords <- tibble(
  word = c("said", "say", "says", "also", "news", "bbc", "reporter", 
           "report", "reports", "player", "time", "play"),
  lexicon = "custom"
)

all_stopwords <- bind_rows(stop_words, custom_stopwords)

tidy_articles <- bbc_data %>%
  unnest_tokens(word, Text) %>%
  anti_join(all_stopwords, by = "word") %>%  
  mutate(word = lemmatize_words(word))   


top_words <- tidy_articles %>%
  count(word, sort = TRUE) %>%
  slice_max(order_by = n, n = 15)


tf_idf_words <- tidy_articles %>%
  count(Category, word, sort = TRUE) %>%
  bind_tf_idf(word, Category, n) %>%
  arrange(desc(tf_idf))

top_tf_idf <- tf_idf_words %>%
  group_by(Category) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup()



ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Most Frequent Words",
       x = "Word", y = "Frequency")


word_freq <- tidy_articles %>% count(word, sort = TRUE)
set.seed(123)
wordcloud(words = word_freq$word,
          freq = word_freq$n,
          max.words = 400,
          random.order = FALSE,  
          rot.per = 0.2,        
          scale = c(6, 0.5),     
          colors = brewer.pal(8, "Dark2"))



ggplot(top_tf_idf, aes(x = reorder_within(word, tf_idf, Category), y = tf_idf, fill = Category)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Category, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top TF-IDF Words by Category",
       x = "Word", y = "TF-IDF")

bing <- get_sentiments("bing")
sentiment_counts <- tidy_articles %>%
  inner_join(bing, by = "word") %>%
  count(Category, sentiment)

ggplot(sentiment_counts, aes(x = Category, y = n, fill = sentiment)) +
  geom_col(position = "dodge") +
  labs(title = "Sentiment by Category",
       x = "Category", y = "Count")

bing <- get_sentiments("bing")


article_sentiment <- tidy_articles %>%
  inner_join(bing, by = "word") %>%
  count(URL, Category, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)   # net score


category_sentiment <- article_sentiment %>%
  group_by(Category) %>%
  summarise(
    total_positive = sum(positive),
    total_negative = sum(negative),
    avg_net_sentiment = mean(net_sentiment),
    .groups = "drop"
  )




ggplot(article_sentiment, aes(x = Category, y = net_sentiment, fill = Category)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.6) +
  coord_flip() +
  labs(title = "Distribution of Net Sentiment per Article",
       x = "Category", y = "Net Sentiment Score")


top_words_category <- tidy_articles %>%
  count(Category, word, sort = TRUE) %>%
  group_by(Category) %>%
  slice_max(n, n = 10) %>%
  ungroup()


ggplot(top_words_category, aes(x = reorder_within(word, n, Category), y = n, fill = Category)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Category, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top 10 Words by Category",
    x = "Words",
    y = "Count"
  )


dtm <- tidy_articles %>%
  count(URL, word) %>%
  cast_dtm(document = URL, term = word, value = n)

lda_model <- LDA(dtm, k = 6, control = list(seed = 123))
lda_topics <- tidy(lda_model, matrix = "beta")

top_terms <- lda_topics %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 10) %>%
  ungroup()

ggplot(top_terms, aes(x = fct_reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title = "Top Terms per Topic",
       x = "Terms", y = "Beta")
