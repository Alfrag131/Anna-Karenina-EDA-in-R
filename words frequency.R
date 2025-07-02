text <- readLines("https://raw.githubusercontent.com/WillKoehrsen/deep-learning-v2-pytorch/master/recurrent-neural-networks/char-rnn/data/anna.txt", encoding = "UTF-8")

library(stringr)
library(dplyr)
library(ggplot2)

text <- tolower(text)

text <- str_replace_all(text, "[[:punct:]]", "")
text <- str_replace_all(text, "[[:digit:]]", "")
text <- str_replace_all(text, "\\s+", " ")
text <- str_trim(text)

words <- unlist(strsplit(text, "\\s+"))

words <- words[words != ""]

character_map <- list(
  "Anna Arkadyevna Karenina" = c("anna arkadyevna", "anna", "anna karenina", "karenina"),
  "Alexey Alexandrovitch Karenin" = c("alexey alexandrovitch", "alexandrovitch", "karenin"),
  "Sergei Alexeyitch Karenin" = c("sergei alexeyitch", "karenin"),
  "Alexey Kirillovitch Vronsky" = c("alexey kirillovitch", "alexey", "vronsky"),
  "Konstantin Dmitrich Levin" = c("konstantin dmitrich", "levin", "kostya"),
  "Katerina Alexandrovna Shtcherbatsky" = c("katerina alexandrovna", "shtcherbatsky", "kitty", "katerina alexandrovna levin", "kitty shtcherbatsky"),
  "Stepan Arkadyevitch Oblonsky" = c("stepan arkadyevitch", "arkadyevitch", "stepan", "oblonsky", "stiva"),
  "Darya Alexandrovna Oblonsky" = c("darya alexandrovna", "alexandrovna", "oblonsky", "darya", "dolly"),
  "Nicolai Dmitrich Levin" = c("nicolai", "nikolay levin"),
  "Sergei Ivanitch Koznyshev" = c("sergei ivanitch", "sergey", "koznyshev", "ivanovitch")
)



stop_words <- readLines("C:/Users/ิ่๐เ/Desktop/datasets/Anna Karenina/stop_words.txt", encoding = "UTF-8")
words <- words[!words %in% stop_words]

all_hero_names <- unlist(character_map)
words <- words[!words %in% all_hero_names]



word_freq <- data.frame(word = words) %>%
  group_by(word) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq))

top_words <- word_freq %>% slice_max(freq, n = 20)

ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(
    title = "Top-20 Most Frequently Used Words",
    x = "",
    y = ""
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
