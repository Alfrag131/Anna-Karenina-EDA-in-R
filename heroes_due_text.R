
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(ggplot2)
library(patchwork)

url_txt <- "https://raw.githubusercontent.com/WillKoehrsen/deep-learning-v2-pytorch/master/recurrent-neural-networks/char-rnn/data/anna.txt"

text_clean <- readLines(url_txt, encoding = "UTF-8") |>
  tolower() |>
  paste(collapse = " ") |>
  str_replace_all("[[:punct:]]", " ") |>
  str_replace_all("[[:digit:]]", " ") |>
  str_replace_all("\\s+", " ") |>
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

normalize_text <- function(txt, dict) {
  imap(dict, \(repl, pat) {
    txt <<- str_replace_all(txt,
                            regex(paste0("\\b", pat, "\\b"), ignore_case = FALSE),
                            repl)
  })
  txt
}

text_norm <- normalize_text(text_clean, replacements)

split_parts <- function(txt, n = 10) {
  words <- str_split(txt, "\\s+")[[1]]
  size  <- ceiling(length(words) / n)
  indices <- ceiling(seq_along(words) / size)
  tapply(words, indices, paste, collapse = " ") |> unname()
}

text_parts   <- split_parts(text_norm, 10)
part_lengths <- sapply(text_parts, \(x) str_count(x, "\\S+"))  # число слов

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


get_freq <- function(txt, variants) {
  pattern <- paste0("\\b(", str_c(variants, collapse = "|"), ")\\b")
  str_count(txt, regex(pattern))
}

hero_freq <- map_dfr(seq_along(text_parts), \(p) {
  part_txt <- text_parts[[p]]
  map_dfr(names(character_map), \(h) {
    tibble(
      part = p,
      hero = h,
      freq = get_freq(part_txt, character_map[[h]])
    )
  })
}) |>
  mutate(norm = freq / part_lengths[part] * 1000)  # на 1 000 слов


heatmap_plot <- ggplot(hero_freq,
                       aes(part, fct_rev(hero), fill = norm)) +
  geom_tile(colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Heatmap • mentions per 1 000 words",
    x = "Part (10 % bins)",
    y = NULL,
    fill = "Mentions\n/1 000 words"
  ) +
  theme_minimal()

print(heatmap_plot)
