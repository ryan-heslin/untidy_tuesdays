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

#' A major problem appears. 351 albums have rock listed as a genre, so that
#' class is overwhelmingly predominant.
sum(str_detect(albums$genre, "Rock"))

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
  str_to_lower(unique(by_word$genre))
)

by_word <- by_word %>%
  unnest_tokens(word, description) %>%
  filter(!word %in% c(stop_words$word, custom_stops))


#' I elect to break some compound genres like "Funk /Soul" into individual pieces.

#' Counting words, they overwhelmingly come from the rock genre.
count(by_word, genre, sort = TRUE)

count(by_word, word, genre, sort = TRUE) %>% 
  slice_head(n = 10)

#'The most common words in each genre look as we'd expect, but some overlap.
by_word %>% group_by(genre, word) %>%
  summarize(count = n()) %>%
  slice_max(order_by = count, n = 3) %>%
  filter(!genre %in% c("Classical", "Stage & Screen")) %>%
  arrange(genre, -count) %>% 
  head(10)

#' I drop uncommon genres and code each album as the first genre listed - arbitrary
#' but consistent.
drop_genres <-
  c("Classical", "Latin", "Stage & Screen", "Reggae", "Jazz")
word_genre <- by_word %>%
  filter(!genre %in% drop_genres) %>%
  distinct(artist, album, word, .keep_all = TRUE) %>%
  add_count(genre, word) %>%
  filter(n > 4)

#' The funk and hip-hop clusters are most apparent.
library(ggrepel)
word_genre %>% count(genre, word) %>%
  bind_tf_idf(term = word, document = genre, n = n) %>%
  ggplot(aes(
    x = log(tf),
    y = tf - idf,
    label = word
  )) +
  geom_point(aes(color = genre)) +
  geom_text_repel()


library(topicmodels)

#' To get a closer look at the structure of the data, I fit an LDA topic model with
#' 8 topics, one for each genre of interest. The documents are the album descriptions.
lda <-
  word_genre %>% cast_dtm(document = album,
                          term = word,
                          value = n) %>%
  LDA(k = 8)

#' Plots show some topics seem to have captured most of one genre, but others are
#' more dispersed (remember a document can contain multiple topics)
pal <-
  RColorBrewer::brewer.pal(n_distinct(word_genre$genre), "Set3")
plots <- tidy(lda, matrix = "gamma") %>%
  left_join(distinct(word_genre, album, .keep_all = TRUE),
            by = c(document = "album")) %>%
  group_by(document) %>%
  slice_max(n = 1, order_by = gamma)  %>%
  ungroup() %>%
  mutate(genre = factor(genre)) %>%
  group_by(topic) %>%
  group_map(function(.x, .y) {
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
        values = pal,
        labels = function(x)
          paste0(x, " (n = ", counts, ")")
      )  +
      coord_flip() +
      labs(title = paste("Albums in Topic", .y), x = NULL, color = "Genre") +
      theme(axis.text.y = element_text(size = 7, face = "italic", hjust = 1), axis.ticks.y = element_blank(),
            plot.margin = unit(rep(.5, 4), "cm"),)
  }, .keep = TRUE)

walk(plots, print)

#' One topic per genre is probably too many, given the predominance of rock
#' albums.
enframe(get_topics(lda), name = "album", value = "topic") %>%
  mutate(top_word = get_terms(lda)[match(topic, as.character(seq_along(get_terms(lda))))]) %>%
  right_join(albums, by = "album") %>%
  select(album, artist, genre, topic, top_word) %>% 
  head(10)

#' I next try to fit a lasso model, useful here to filter irrelevant
#' predictors. Inspired by [this](https://www.youtube.com/watch?v=3PecUbnuYC4&t=6s)
#' Tidy Tuesday screencast.
#' 
#' I'm not concerned with prediction and the data are sparse, so I don't create
#' test and training sets.
library(glmnet)

set.seed(1111)
sparse <- word_genre %>% cast_sparse(album, word, n)

y <- word_genre$genre[match(rownames(sparse), word_genre$album)]

mod <- cv.glmnet(sparse,
                 y,
                 family = "multinomial",
                 type.measure = "class",
                 nfolds = 15)

plot(mod)

#' The optimal value of lambda turn out to be:
mod$lambda.min

#' The deviance ratio (proportion of deviance) for the best model is pretty
#' high, indicating we don't improve much on the null model. The highest
#' coefficients, though,make sense, such as "motown" for funk.
top_terms <- tidy(mod$glmnet.fit) %>% group_by(term) %>%
  filter(lambda == mod$lambda.min) %>%
  slice_max(order_by = abs(estimate), n = 1) %>%
  ungroup() %>%
  arrange(-abs(estimate))

head(top_terms, 15)

#' The most striking associations are for hip-hop, with
#' words like "beats", "jam", and "sample" that make sense.
#' The words for rock, far and away the dominant genre, are
#' less focused, and include some names.
coef.glmnet(mod, s = mod$lambda.min) %>%
  map_dfr(tidy, .id = "genre") %>%
  arrange(-abs(value)) %>%
  select(-column) %>% 
  head(10)

pred <- predict(mod, sparse, s = mod$lambda.min, type = "class")

#' The model, which naturally classified most albums as rock, the dominant class,
#' mostly misclassifies albums for which rock is a secodnary genre.
tibble(pred = pred,
       actual = y,
       album = rownames(sparse)) %>%
  right_join(albums, by = "album") %>%
  filter(pred != actual) %>%
  select(pred, actual, album, artist, subgenre) 
