library(tidytext)
library(dplyr)
library(ggplot2)
library(stringr)
library(zoo)
library(tidyr)

# Чтение и очистка текста
url  <- "https://raw.githubusercontent.com/WillKoehrsen/deep-learning-v2-pytorch/master/recurrent-neural-networks/char-rnn/data/anna.txt"
text <- readLines(url, encoding = "UTF-8")
text_clean <- text |>
  tolower() |>
  paste(collapse = " ") |>
  str_replace_all("[[:digit:]]", " ") |>
  str_replace_all("[^a-zа-яё\\s]", " ") |>
  str_replace_all("\\s+", " ") |>
  str_trim()

# Разбивка на слова
words <- tibble(word = unlist(str_split(text_clean, "\\s+")))

# Сентимент слов (Bing)
bing_lexicon <- get_sentiments("bing")
words_sentiment <- words %>%
  inner_join(bing_lexicon, by = "word") %>%
  mutate(sentiment_value = ifelse(sentiment == "positive", 1, -1),
         index           = row_number())

# Скользящие средние
window_size1 <- 200
window_size2 <- 2000

words_sentiment <- words_sentiment %>%
  arrange(index) %>%
  mutate(
    roll_200  = zoo::rollmean(sentiment_value, k = window_size1,  fill = NA),
    roll_2000 = zoo::rollmean(sentiment_value, k = window_size2, fill = NA)
  )

# Длинный формат
df_plot <- words_sentiment %>%
  select(index, roll_200, roll_2000) %>%
  pivot_longer(cols = starts_with("roll_"),
               names_to  = "window",
               values_to = "sentiment") %>%
  filter(!is.na(sentiment)) %>%
  mutate(window = recode(window,
                         "roll_200"  = "Window = 200 words",
                         "roll_2000" = "Window = 2000 words"))

# График
ggplot(df_plot, aes(x = index, y = sentiment, color = window)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Window = 200 words"  = "orange",
                                "Window = 2000 words" = "navy")) +
  labs(title = "Rolling Sentiment with Different Window Sizes",
       x = "Word index",
       y = "Rolling sentiment score",
       color = "Window size") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5, face = "bold"))
