# рџ“¦ Р‘РёР±Р»РёРѕС‚РµРєРё
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

# --- 2. Р—Р°РјРµРЅС‹ РёРјС‘РЅ
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

# --- 3. РџСЂРµРѕР±СЂР°Р·СѓРµРј С‚РµРєСЃС‚ РІ РІРµРєС‚РѕСЂ СЃР»РѕРІ
words <- unlist(str_split(text, "\\s+"))
words <- words[words != ""]

# --- 4. Р—Р°РіСЂСѓР¶Р°РµРј СЃС‚РѕРї-СЃР»РѕРІР°
stop_words <- readLines("C:/Users/Фира/Desktop/datasets/Anna Karenina/stop_words_for_character_glossary_Kitty.txt", encoding = "UTF-8")
words <- words[!words %in% stop_words]

# --- 5. РџРµСЂСЃРѕРЅР°Р¶Рё
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

# --- 6. Р’ tibble
word_df <- tibble(word = words)

# --- 7. Р¤СѓРЅРєС†РёСЏ: СЃР»РѕРІР° В±5 РІРѕРєСЂСѓРі РїРµСЂСЃРѕРЅР°Р¶Р°
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

# --- 8. РџСЂРёРјРµСЂ: Р°РЅР°Р»РёР· РђРЅРЅС‹
anna_context <- extract_context_words(unlist(character_map$Kitty), window = 5)

# --- 9. Р§Р°СЃС‚РѕС‚РЅС‹Р№ Р°РЅР°Р»РёР·
anna_freq <- tibble(word = anna_context) %>%
  filter(!word %in% stop_words) %>%
  filter(!word %in% unlist(character_map)) %>%  
  count(word, sort = TRUE) %>%
  filter(n >= 5)

# --- 10. Р“СЂР°С„РёРє
ggplot(anna_freq[1:20,], aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    title = "Most Frequent Words near Kitty (В±5 words)",
    x = "Word", y = "Frequency"
  ) +
  theme_minimal()

