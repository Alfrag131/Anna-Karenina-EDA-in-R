library(stringr)
library(dplyr)
library(ggplot2)

# --- Загружаем текст ---
text <- readLines("https://raw.githubusercontent.com/WillKoehrsen/deep-learning-v2-pytorch/master/recurrent-neural-networks/char-rnn/data/anna.txt", encoding = "UTF-8")

# --- Обрабатываем текст ---
text_joined <- tolower(paste(text, collapse = " "))
text_joined <- str_replace_all(text_joined, "[[:punct:]]", "")
text_joined <- str_replace_all(text_joined, "[[:digit:]]", "")
text_joined <- str_replace_all(text_joined, "\\s+", " ")
text_joined <- str_trim(text_joined)

# --- Словарь нормализации имён и отчеств ---
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

normalize_text <- function(text, replacements) {
  for (variant in names(replacements)) {
    standard <- replacements[[variant]]
    text <- str_replace_all(text, paste0("\\b", variant, "\\b"), standard)
  }
  return(text)
}

# --- Применяем нормализацию ---
text_joined <- normalize_text(text_joined, replacements)

# --- Список героев с нормализованными вариантами ---
character_map <- list(
  "Anna Arkadyevna Karenina" = c("anna arkadyevna", "anna karenina", "anna", "karenina"),
  "Alexey Alexandrovitch Karenin" = c("alexey alexandrovitch", "alexey karenin", "karenin"),
  "Sergey Alexeyevitch Karenin" = c("sergey alexeyevitch", "sergey karenin", "seryozha"),
  "Alexey Kirillovitch Vronsky" = c("alexey kirillovitch", "alexey vronsky", "vronsky", "aliosha"),
  "Konstantin Dmitrievitch Levin" = c("konstantin dmitrievitch", "konstantin levin", "levin", "kostya"),
  "Katerina Alexandrovna Shtcherbatsky" = c("katerina alexandrovna", "katerina shtcherbatsky", "katerina", "shtcherbatsky", "katerina levina"),
  "Stepan Arkadyevitch Oblonsky" = c("stepan arkadyevitch", "stepan", "stepan oblonsky", "oblonsky"),
  "Darya Alexandrovna Oblonsky" = c("darya alexandrovna", "darya oblonsky", "oblonsky", "darya"),
  "Nicolay Dmitrievitch Levin" = c("nicolay dmitrievitch", "nicolay levin"),
  "Sergey Ivanovitch Koznyshev" = c("sergey ivanovitch", "sergey koznyshev", "koznyshev")
)

# --- Подсчитываем частоту упоминаний героев ---
hero_freq <- data.frame(hero = names(character_map), freq = 0)

for (hero in names(character_map)) {
  variants <- character_map[[hero]]
  total_freq <- sum(sapply(variants, function(name) {
    str_count(text_joined, paste0("\\b", name, "\\b"))
  }))
  hero_freq$freq[hero_freq$hero == hero] <- total_freq
}

hero_freq <- hero_freq %>% arrange(desc(freq))

# --- Строим график ---
ggplot(hero_freq, aes(x = reorder(hero, -freq), y = freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequency of Character Mentions in Anna Karenina",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
