load("../data/albums_scrape.Rdata")


library(tidyverse)


theme_standard <- ggplot2::theme(
  panel.background = element_blank(),
  panel.border = element_rect(color = "black", fill = NA),
  panel.grid = element_blank(),
  panel.grid.major.x = element_line(color = "gray93"),
  legend.background = element_rect(fill = "gray93"),
  plot.title = element_text(
    size = 15,
    family = "sans",
    face = "bold",
    vjust = 1.3
  ),
  plot.title.position = "plot",
  plot.subtitle = element_text(size = 10, family = "sans"),
  legend.title = element_text(
    size = 10,
    family = "sans",
    face = "bold"
  ),
  axis.title = element_text(
    size = 9,
    family = "sans",
    face = "bold"
  ),
  axis.text = element_text(size = 8,
                           family = "sans"),
  strip.background = element_rect(color = "black",
                                  fill = "black"),
  strip.text.x = element_text(color = "white"),
  strip.text.y = element_text(color = "white")
)
ggplot2::theme_set(theme_standard)
albums <- text_raw %>%
  map(bind_cols) %>%
  bind_rows() %>%
  separate(artist_album,
           sep = ",\\s(?=\\')|\\s,|'\\s'",
           #special cases
           into = c("artist", "album")) %>%
  separate(
    label_year,
    sep = ",\\s",
    into = c("label", "year"),
    convert = TRUE
  ) %>%
  mutate(rank = nrow(.):1,
         across(where(is.character), ~ str_remove_all(.x, "\\\"|^\\'|\\'$")))


#' I recode some faulty years.
recodes <- exprs(
  album == "Tumbleweed Connection" ~ 1970,
  album == "CrazySexyCool" ~ 1994,
  album == "The Blueprint" ~ 2001,
  TRUE ~ year
)
albums <- albums %>% mutate(year = case_when(!!!recodes))


#' Luckily, rank is a unique key, saving me a potentially nasty fuzzy join.
genres <- genres %>% select(number, genre, subgenre) %>%
  mutate(genre = str_replace_all(
    genre,
    c(
      "\\s(/|\\\\)\\s" = ", ",
      ",J" = ", ",
      "Folk, World, & Country" = "Folk"
    )
  ))

albums <-
  left_join(albums,
            genres,
            by = c(rank = "number"))


library(tidytext)


by_word <- albums %>%
  separate_rows(genre, sep = ",\\s")
custom_stops = c(
  "song",
  "songs",
  "album",
  "albums",
  "music",
  "band",
  "record",
  "track",
  "hip",
  "hop",
  "",
  unique(by_word$genre),
  str_to_lower(genres$genre)
)

by_word <- by_word %>%
  unnest_tokens(word, description) %>%
  filter(!word %in% c(stop_words$word, custom_stops))


#' I elect to break some compound genres like "Funk /Soul" into individual pieces.

#' Counting words, they overwhelmingly come from the rock genre.
count(by_word, genre, sort = TRUE)

count(by_word, word, genre, sort = TRUE)

#'The most common words in each genre look as we'd expect, but some overlap.
by_word %>% group_by(genre, word) %>%
  summarize(count = n()) %>%
  slice_max(order_by = count, n = 3) %>%
  filter(!genre %in% c("Classical", "Stage & Screen")) %>%
  arrange(genre, -count)

drop_genres <-
  c("Claasical", "Latin", "Stage & Screen", "Reggae", "Jazz")
word_genre <- by_word %>%
  filter(!genre %in% drop_genres) %>%
  add_count(genre, word) %>%
  filter(n > 4)


word_genre %>% count(genre, word) %>%
  bind_tf_idf(term = word, document = genre, n = n) %>%
  ggplot(aes(
    x = log(tf),
    y = tf - idf,
    label = word
  )) +
  geom_point(aes(color = genre)) +
  geom_text()


library(topicmodels)


lda <-
  word_genre %>% cast_dtm(document = album,
                          term = word,
                          value = n) %>%
  LDA(k = 8)

pal <-
  RColorBrewer::brewer.pal(n_distinct(word_genre$genre), "Set3")
tidy(lda, matrix = "gamma") %>%
  left_join(distinct(word_genre, album, .keep_all = TRUE),
            by = c(document = "album")) %>%
  group_by(document) %>%
  slice_max(n = 1, order_by = gamma)  %>%
  ungroup() %>%
  mutate(genre = factor(genre)) %>%
  group_by(topic) %>%
  group_map(
    function(.x, .y){
      counts <- unname(table(.x$genre))
    ggplot(.x, aes(
      x = fct_reorder(document, gamma),
      y = gamma,
      color = genre
    )) +
      geom_point() +
      scale_color_manual(
        drop = FALSE,
        breaks = waiver(),
        ,
        values = pal,
        labels = function(x)
          paste0(x, " (n = ", counts, ")")
      )  +
      coord_flip() +
      labs(title = "Albums by Topic",  x = NULL, color = "Genre")
    }, .keep = TRUE)

#' I try to fit a lasso model, useful here to filter irrelevant
#' predictors. Inspired by [this](https://www.youtube.com/watch?v=3PecUbnuYC4&t=6s)
#' Tidy Tuesday screencast.
library(glmnet)

set.seed(1111)
sparse <- word_genre %>% cast_sparse(album, word, n)

y <- word_genre$genre[match(rownames(sparse), word_genre$album)]

#train <- sample(nrow(sparse), nrow(sparse)/5, replace = FALSE)

mod <- cv.glmnet(sparse,
                 y,
                 family = "multinomial",
                 type.measure = "class",
                 nfolds = 15)

plot(mod)

#' The optimal value of lambda turn out to be:
mod$lambda.min

top_terms <- tidy(mod$glmnet.fit) %>% group_by(term) %>%
  slice_max(order_by = abs(estimate), n = 1) %>%
  ungroup() %>%
  arrange(-abs(estimate))

head(top_terms, 15)
