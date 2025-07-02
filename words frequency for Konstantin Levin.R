library(dplyr)
library(stringr)
library(tibble)
library(ggplot2)
library(tidyr)


url <- "https://raw.githubusercontent.com/WillKoehrsen/deep-learning-v2-pytorch/master/recurrent-neural-networks/char-rnn/data/anna.txt"
text <- tolower(paste(readLines(url, encoding = "UTF-8"), collapse = " ")) %>%
  str_replace_all("[^a-zР°-СЏС‘\\s]", " ") %>%
  str_replace_all("\\s+", " ") %>%
  str_trim()

replacements <- c(
  # Имена
  "sergey" = "sergey",
  "sergei" = "sergey",
  "alexei" = "alexey",
  "aleksei" = "alexey",
  "aleksey" = "alexey",
  "nikolay" = "nicolay",
  "nikolai" = "nicolay",
  "kostya" = "konstantin",
  "stiva" = "stepan",
  "dolly" = "darya",
  "kitty" = "katerina",
  
  # Отчества
  "arkadyevna" = "arkadyevna",
  "alexandrovitch" = "alexandrovitch",
  "alexyevitch" = "alexeyevitch",
  "alexeitch" = "alexeyevitch",
  "kirillovitch" = "kirillovitch",
  "dmitrich" = "dmitrievitch",
  "ivanitch" = "ivanovitch",
  
  "koznishev" = "koznyshev"
)
for (pat in names(replacements)) {
  text <- str_replace_all(text, paste0("\\b", pat, "\\b"), replacements[[pat]])
}

words <- unlist(str_split(text, "\\s+"))
words <- words[words != ""]

stop_words <- readLines("C:/Users/Фира/Desktop/datasets/Anna Karenina/stop_words_for_character_glossary_KLevin.txt", encoding = "UTF-8")
words <- words[!words %in% stop_words]

character_map <- list(
  Anna = c("anna arkadyevna", "anna karenina", "anna", "karenina"),
  Karenin = c("alexey alexandrovitch", "alexey karenin", "karenin"),
  Vronsky = c("alexey kirillovitch", "alexey vronsky", "vronsky", "aliosha"),
  Levin = c("konstantin dmitrievitch", "konstantin levin", "levin", "kostya"),
  Kitty = c("katerina alexandrovna", "katerina shtcherbatsky", "katerina", "shtcherbatsky", "katerina levina"),
  Stepan = c("stepan arkadyevitch", "stepan", "stepan oblonsky", "oblonsky"),
  Darya = c("darya alexandrovna", "darya oblonsky", "oblonsky", "darya"),
  Nicolay = c("nicolay dmitrievitch", "nicolay levin"),
  Sergey = c("sergey ivanovitch", "sergey koznyshev", "koznyshev")
)

word_df <- tibble(word = words)

extract_context_words <- function(hero_variants, window = 5) {
  idxs <- which(word_df$word %in% hero_variants)
  context <- c()
  for (i in idxs) {
    start <- max(1, i - window)
    end <- min(nrow(word_df), i + window)
    context <- c(context, word_df$word[start:end])
  }
  return(context)
}

anna_context <- extract_context_words(unlist(character_map$Levin), window = 5)

anna_freq <- tibble(word = anna_context) %>%
  filter(!word %in% stop_words) %>%
  filter(!word %in% unlist(character_map)) %>%  
  count(word, sort = TRUE) %>%
  filter(n >= 5)

ggplot(anna_freq[1:20,], aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    title = "Most Frequent Words near Konstantin Levin (В±5 words)",
    x = "Word", y = "Frequency"
  ) +
  theme_minimal()
