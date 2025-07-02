
library(dplyr); library(stringr); library(purrr)
library(igraph); library(ggraph); library(tidyr)

url <- "https://raw.githubusercontent.com/WillKoehrsen/deep-learning-v2-pytorch/master/recurrent-neural-networks/char-rnn/data/anna.txt"
text_clean <- readLines(url, encoding = "UTF-8") |>
  tolower()                                  |>
  paste(collapse = " ")                      |>
  str_replace_all("[^a-zа-яё\\s]", " ")      |>
  str_replace_all("\\s+", " ")               |>
  str_trim()

replacements <- c(
  "sergei"="sergey", "aleksei"="alexey", "aleksey"="alexey",
  "nikolai"="nicolay", "kostya"="konstantin", "stiva"="stepan",
  "dolly"="darya", "kitty"="katerina", "koznishev"="koznyshev"
)
normalize <- function(txt, dict){
  for(p in names(dict)){
    txt <- str_replace_all(txt, paste0("\\b", p, "\\b"), dict[[p]])
  }
  txt
}
text_clean <- normalize(text_clean, replacements)

character_map <- list(
  Anna      = c("anna arkadyevna","anna karenina","anna","karenina"),
  Vronsky   = c("alexey kirillovitch","alexey vronsky","vronsky","aliosha"),
  Karenin   = c("alexey alexandrovitch","karenin"),
  Levin     = c("konstantin dmitrievitch","konstantin levin","levin"),
  Kitty     = c("katerina alexandrovna","katerina shtcherbatsky","katerina"),
  Stepan    = c("stepan arkadyevitch","oblonsky","stepan"),
  Darya     = c("darya alexandrovna","darya","dolly"),
  Nicolay   = c("nicolay dmitrievitch","nicolay levin"),
  Sergey    = c("sergey ivanovitch","koznyshev")
)

# Разбивка на окна по 300 слов
words     <- str_split(text_clean, "\\s+")[[1]]
win_size  <- 300
chunks    <- split(words, ceiling(seq_along(words)/win_size)) |>
  map_chr(paste, collapse = " ")

# Какие герои упомянуты в каждом окне
mentions <- map(chunks, \(chunk){
  keep(names(character_map), \(hero){
    any(str_detect(chunk,
                   paste0("\\b(", str_c(character_map[[hero]], collapse="|"), ")\\b")))
  })
})

# Матрица ко-упоминаний
heroes  <- names(character_map)
co_mat  <- matrix(0, length(heroes), length(heroes), dimnames=list(heroes,heroes))
walk(mentions, \(m){
  if(length(m)>1){
    combn(m,2, \(p){ co_mat[p[1],p[2]] <<- co_mat[p[1],p[2]] + 1 })
  }
})
co_mat <- co_mat + t(co_mat)   # симметрия

# Граф: фильтр рёбер (<5) и узлов (degree<2)
g <- graph_from_adjacency_matrix(co_mat, "undirected", weighted=TRUE, diag=FALSE)
g <- delete_edges(g, E(g)[weight < 5])
g <- delete_vertices(g, V(g)[degree(g) < 2])     # убираем редко связанных

# Группы (сюжетные линии) и палитры
V(g)$group <- case_when(
  V(g)$name %in% c("Anna","Vronsky","Karenin")           ~ "Anna arc",
  V(g)$name %in% c("Levin","Kitty","Nicolay","Sergey")   ~ "Levin arc",
  V(g)$name %in% c("Stepan","Darya")                     ~ "Oblonskys",
  TRUE                                                   ~ "Other"
)
node_pal <- c("Anna arc"="#d72638",
              "Levin arc"="#1b998b",
              "Oblonskys"="#f4a259",
              "Other"="gray70")

edge_grp <- apply(ends(g, E(g)), 1, function(e) {
  g1 <- V(g)$group[e[1]]
  g2 <- V(g)$group[e[2]]
  if (!is.na(g1) && !is.na(g2) && g1 == g2) g1 else "Inter"
})
E(g)$e_grp <- edge_grp
edge_pal <- c(node_pal, Inter="gray80")


set.seed(42)
ggraph(g, layout="fr") +
  geom_edge_link(aes(width=weight, colour=e_grp), alpha=.8) +
  geom_node_point(aes(color=group), size=7) +
  geom_node_text(aes(label=name), repel=TRUE, size=4, fontface="bold") +
  scale_edge_width(range=c(0.4,3)) +
  scale_color_manual(values=node_pal) +
  scale_edge_colour_manual(values=edge_pal, guide="none") +
  theme_void() +
  labs(title = "Anna Karenina — co-mention network (window = 300 words)",
       subtitle = "Edge width - co-mentions ≥ 5 | node & edge colour = storyline")
