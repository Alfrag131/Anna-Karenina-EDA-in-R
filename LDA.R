# 📦 Библиотеки
library(dplyr)
library(stringr)
library(tidytext)
library(tm)
library(topicmodels)
library(slam)
library(ggplot2)

# --- 1. Загрузка текста и очистка
url <- "https://raw.githubusercontent.com/WillKoehrsen/deep-learning-v2-pytorch/master/recurrent-neural-networks/char-rnn/data/anna.txt"
text <- tolower(paste(readLines(url, encoding = "UTF-8"), collapse = " ")) %>%
  str_replace_all("[^a-zа-яё\\s]", " ") %>%
  str_replace_all("\\s+", " ") %>%
  str_trim()

# --- 2. Разбиение на "документы" по 100 слов
words <- unlist(strsplit(text, "\\s+"))
chunk_size <- 100
chunks <- split(words, ceiling(seq_along(words) / chunk_size))
docs <- sapply(chunks, paste, collapse = " ")

# --- 3. Создание корпуса
corpus <- VCorpus(VectorSource(docs))

# --- 4. Очистка
stop_words_custom <- tolower(readLines("C:/Users/Фира/Desktop/datasets/Anna Karenina/stop_words.txt", encoding = "UTF-8"))
stop_words_total <- c(stopwords("en"), stop_words_custom)

corpus <- corpus %>%
  tm_map(removeWords, stop_words_total) %>%
  tm_map(stripWhitespace)

# --- 5. Матрица документ-термин
dtm <- DocumentTermMatrix(corpus)

# Удаляем пустые документы (где сумма по строке = 0)
dtm <- dtm[slam::row_sums(dtm) > 0, ]

# Удаляем редкие слова
dtm <- removeSparseTerms(dtm, 0.98)

# Снова удаляем пустые строки (они могли появиться после удаления редких слов!)
dtm <- dtm[slam::row_sums(dtm) > 0, ]

# --- 6. LDA
k_topics <- 5
lda_model <- LDA(dtm, k = k_topics, control = list(seed = 1234))

# --- 7. Вывод топ-слов
library(tidytext)
topics_terms <- tidy(lda_model, matrix = "beta")

top_terms <- topics_terms %>%
  group_by(topic) %>%
  slice_max(beta, n = 7) %>%
  ungroup() %>%
  arrange(topic, -beta)

# --- 8. Визуализация
ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms per Topic in Anna Karenina (LDA)",
       x = "Term", y = "Beta (Importance)") +
  theme_minimal()

