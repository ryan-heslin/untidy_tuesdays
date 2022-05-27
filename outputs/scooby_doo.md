Scooby Doo Analysis
================
Ryan Heslin
May 27, 2022





I was scanning the Tidy Tuesday repo for interesting topics, and I
couldn’t resist Scooby-Doo.

``` r
scoobydoo <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv", na = "NULL")
glimpse(scoobydoo)
```

    Rows: 603
    Columns: 75
    $ index                    [3m[38;5;246m<dbl>[39m[23m 1, 2, 3, 4, 5, 6, 7, 8, 9…
    $ series_name              [3m[38;5;246m<chr>[39m[23m "Scooby Doo, Where Are Yo…
    $ network                  [3m[38;5;246m<chr>[39m[23m "CBS", "CBS", "CBS", "CBS…
    $ season                   [3m[38;5;246m<chr>[39m[23m "1", "1", "1", "1", "1", …
    $ title                    [3m[38;5;246m<chr>[39m[23m "What a Night for a Knigh…
    $ imdb                     [3m[38;5;246m<dbl>[39m[23m 8.1, 8.1, 8.0, 7.8, 7.5, …
    $ engagement               [3m[38;5;246m<dbl>[39m[23m 556, 479, 455, 426, 391, …
    $ date_aired               [3m[38;5;246m<date>[39m[23m 1969-09-13, 1969-09-20, …
    $ run_time                 [3m[38;5;246m<dbl>[39m[23m 21, 22, 21, 21, 21, 21, 2…
    $ format                   [3m[38;5;246m<chr>[39m[23m "TV Series", "TV Series",…
    $ monster_name             [3m[38;5;246m<chr>[39m[23m "Black Knight", "Ghost of…
    $ monster_gender           [3m[38;5;246m<chr>[39m[23m "Male", "Male", "Male", "…
    $ monster_type             [3m[38;5;246m<chr>[39m[23m "Possessed Object", "Ghos…
    $ monster_subtype          [3m[38;5;246m<chr>[39m[23m "Suit", "Suit", "Phantom"…
    $ monster_species          [3m[38;5;246m<chr>[39m[23m "Object", "Human", "Human…
    $ monster_real             [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ monster_amount           [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ caught_fred              [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, TRUE…
    $ caught_daphnie           [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ caught_velma             [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ caught_shaggy            [3m[38;5;246m<lgl>[39m[23m TRUE, TRUE, FALSE, FALSE,…
    $ caught_scooby            [3m[38;5;246m<lgl>[39m[23m TRUE, FALSE, TRUE, FALSE,…
    $ captured_fred            [3m[38;5;246m<lgl>[39m[23m FALSE, TRUE, FALSE, FALSE…
    $ captured_daphnie         [3m[38;5;246m<lgl>[39m[23m FALSE, TRUE, FALSE, FALSE…
    $ captured_velma           [3m[38;5;246m<lgl>[39m[23m FALSE, TRUE, FALSE, FALSE…
    $ captured_shaggy          [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ captured_scooby          [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ unmask_fred              [3m[38;5;246m<lgl>[39m[23m FALSE, TRUE, TRUE, TRUE, …
    $ unmask_daphnie           [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ unmask_velma             [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ unmask_shaggy            [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ unmask_scooby            [3m[38;5;246m<lgl>[39m[23m TRUE, FALSE, FALSE, FALSE…
    $ snack_fred               [3m[38;5;246m<lgl>[39m[23m TRUE, FALSE, TRUE, FALSE,…
    $ snack_daphnie            [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, TRUE…
    $ snack_velma              [3m[38;5;246m<lgl>[39m[23m FALSE, TRUE, FALSE, FALSE…
    $ snack_shaggy             [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ snack_scooby             [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ unmask_other             [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ caught_other             [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ caught_not               [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ trap_work_first          [3m[38;5;246m<chr>[39m[23m NA, "FALSE", "FALSE", "TR…
    $ setting_terrain          [3m[38;5;246m<chr>[39m[23m "Urban", "Coast", "Island…
    $ setting_country_state    [3m[38;5;246m<chr>[39m[23m "United States", "United …
    $ suspects_amount          [3m[38;5;246m<dbl>[39m[23m 2, 2, 0, 2, 1, 2, 1, 2, 1…
    $ non_suspect              [3m[38;5;246m<lgl>[39m[23m FALSE, TRUE, TRUE, FALSE,…
    $ arrested                 [3m[38;5;246m<lgl>[39m[23m TRUE, TRUE, TRUE, TRUE, T…
    $ culprit_name             [3m[38;5;246m<chr>[39m[23m "Mr. Wickles", "Cptn. Cut…
    $ culprit_gender           [3m[38;5;246m<chr>[39m[23m "Male", "Male", "Male", "…
    $ culprit_amount           [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ motive                   [3m[38;5;246m<chr>[39m[23m "Theft", "Theft", "Treasu…
    $ if_it_wasnt_for          [3m[38;5;246m<chr>[39m[23m NA, NA, NA, NA, NA, NA, N…
    $ and_that                 [3m[38;5;246m<chr>[39m[23m NA, NA, NA, NA, NA, NA, N…
    $ door_gag                 [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ number_of_snacks         [3m[38;5;246m<chr>[39m[23m "2", "1", "3", "2", "2", …
    $ split_up                 [3m[38;5;246m<dbl>[39m[23m 1, 0, 0, 1, 0, 0, 1, 0, 0…
    $ another_mystery          [3m[38;5;246m<dbl>[39m[23m 1, 0, 0, 0, 1, 0, 0, 0, 0…
    $ set_a_trap               [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 1, 1, 0…
    $ jeepers                  [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 1, 0, 0, 0…
    $ jinkies                  [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0…
    $ my_glasses               [3m[38;5;246m<dbl>[39m[23m 1, 0, 0, 0, 1, 0, 0, 1, 0…
    $ just_about_wrapped_up    [3m[38;5;246m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0…
    $ zoinks                   [3m[38;5;246m<dbl>[39m[23m 1, 3, 1, 2, 0, 2, 1, 0, 0…
    $ groovy                   [3m[38;5;246m<dbl>[39m[23m 0, 0, 2, 1, 0, 0, 1, 0, 0…
    $ scooby_doo_where_are_you [3m[38;5;246m<dbl>[39m[23m 0, 1, 0, 0, 1, 0, 0, 1, 0…
    $ rooby_rooby_roo          [3m[38;5;246m<dbl>[39m[23m 1, 0, 0, 0, 0, 1, 1, 1, 1…
    $ batman                   [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ scooby_dum               [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ scrappy_doo              [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ hex_girls                [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ blue_falcon              [3m[38;5;246m<lgl>[39m[23m FALSE, FALSE, FALSE, FALS…
    $ fred_va                  [3m[38;5;246m<chr>[39m[23m "Frank Welker", "Frank We…
    $ daphnie_va               [3m[38;5;246m<chr>[39m[23m "Stefanianna Christophers…
    $ velma_va                 [3m[38;5;246m<chr>[39m[23m "Nicole Jaffe", "Nicole J…
    $ shaggy_va                [3m[38;5;246m<chr>[39m[23m "Casey Kasem", "Casey Kas…
    $ scooby_va                [3m[38;5;246m<chr>[39m[23m "Don Messick", "Don Messi…

``` r
theme_standard <- ggplot2::theme(
  panel.background = element_blank(),
  panel.border = element_rect(color = "black", fill = NA),
  panel.grid = element_blank(), panel.grid.major.x = element_line(
    color =
      "gray93"
  ),
  legend.background = element_rect(fill = "gray93"), plot.title = element_text(
    size = 15,
    family = "sans", face = "bold", vjust = 1.3
  ), plot.title.position =
    "plot",
  plot.subtitle = element_text(size = 10, family = "sans"),
  legend.title = element_text(
    size = 10, family = "sans",
    face = "bold"
  ), axis.title = element_text(
    size = 9,
    family = "sans", face = "bold"
  ), axis.text = element_text(
    size = 8,
    family = "sans"
  ), strip.background = element_rect(
    color = "black",
    fill = "black"
  ), strip.text.x = element_text(color = "white"),
  strip.text.y = element_text(color = "white")
)
ggplot2::theme_set(theme_standard)
```

After skimming the variable list, my attention inevitably lands on the
`scrappy_doo` indicator. While I sadly am not versed in Scooby-Doo lore,
I know Scrappy is widely disliked.

``` r
logicals <- map_lgl(scoobydoo, ~ any(.x %in% c("TRUE", "FALSE")))
scoobydoo <- mutate(scoobydoo, across(which(logicals), as.logical))
```

Nonetheless, mean IMDB ratings for episodes with Scrappy are not
significantly different from those without.

``` r
t.test(imdb ~ scrappy_doo, data = scoobydoo)
```

``` 

    Welch Two Sample t-test

data:  imdb by scrappy_doo
t = 2.1906, df = 585.7, p-value = 0.02888
alternative hypothesis: true difference in means between group FALSE and group TRUE is not equal to 0
95 percent confidence interval:
 0.01093575 0.20055791
sample estimates:
mean in group FALSE  mean in group TRUE 
           7.307565            7.201818 
```

He appears in 27% of episodes.

To my surprise, there are 112 episodes with a real monster.

The highest-rated episode is a crossover with *Supernatural*, which
makes sense.

``` r
arrange(scoobydoo, -imdb) |>
  pull(title) |>
  head(10)
```

``` 
 [1] "Scoobynatural"             "Come Undone"              
 [3] "All Fear the Freak"        "Night Terrors"            
 [5] "Through the Curtain"       "Wrath of the Krampus"     
 [7] "The Man in the Mirror"     "Nightmare in Red"         
 [9] "Escape from Mystery Manor" "Pawn of Shadows"          
```

Ratings seem constant over time, with the exception of a trough in the
early 2000s.

``` r
filter(scoobydoo, !season %in% c("Crossover", "Movie", "Special")) |>
  mutate(series_name = str_wrap(series_name, 15)) |>
  ggplot(aes(x = date_aired, y = imdb)) +
  geom_line(aes(color = season)) +
  facet_wrap(. ~ series_name, scales = "free_x") +
  scale_x_date(date_labels = "%b %Y") +
  labs(x = "Airdate", color = "Season", title = "IMDB Rating by Season") +
  ylim(c(0, 10)) +
  theme(axis.text.x = element_text(size = 6, angle = 45))
```

<img src="/home/rheslin/R/Projects/untidy_tuesdays/outputs/scooby_doo_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

The main story of this plot is that Shaggy really likes to say
“Zoinks\!” They also seem to have toned down the catchphrases in
later seasons.

``` r
phrase_vars <- quote(split_up:rooby_rooby_roo)
scoobydoo |>
  filter(!season %in% c("Crossover", "Movie", "Special")) |>
  group_by(season) |>
  summarize(across(!!phrase_vars, sum, na.rm = TRUE)) |>
  pivot_longer(!!phrase_vars, names_to = "Phrase", values_to = "Count") |>
  mutate(
    Phrase = paste0('"', str_replace_all(str_to_title(Phrase), "_", " "), '!"') |> str_wrap(width = 10),
    Count = ifelse(is.na(Count), 0, Count)
  ) |>
  ggplot(aes(x = Phrase, y = season, fill = Count)) +
  geom_tile(color = "white") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient(low = "purple", high = "green", trans = "log10", oob = scales::squish_infinite) +
  labs(title = "Phrases Uttered by Season", y = "Season") +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = .5
  )) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = .5),
    legend.background = element_rect(fill = "gray")
  )
```

<img src="/home/rheslin/R/Projects/untidy_tuesdays/outputs/scooby_doo_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

It seems that “competition” as a villain motive waxed and waned the most
over time, peaking in the 1990s. An interesting direction for further
analysis.

``` r
library(gganimate)

year_to_decade <- function(dates) {
  lubridate::year(dates) |>
    as.numeric() |>
    (function(x) x - x %% 10)() |>
    paste0("s")
}
# https://stackoverflow.com/questions/52403952/polar-plot-ggplot2-outer-ring-removed-grid-on-top
scoobydoo <- scoobydoo |>
  mutate(
    decade = year_to_decade(date_aired),
    decade_count = ave(decade, decade, FUN = length) |> as.integer(),
    motive = fct_lump_prop(motive, prop = .02)
  )

p <- scoobydoo |>
  filter(!is.na(motive), !is.na(date_aired)) |>
  group_by(decade) |>
  count(motive, name = "Proportion") |>
  mutate(
    Proportion = Proportion / sum(Proportion),
    ymax = cumsum(Proportion),
    ymin = dplyr::lag(ymax, default = 0),
  ) |>
  ungroup() |>
  ggplot(aes(xmin = 1, xmax = 3.5, ymin = ymin, ymax = ymax, fill = motive)) +
  geom_rect(color = "black") +
  coord_polar(theta = "y") +
  xlim(c(0, 4)) +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = .5
  )) +
  labs(fill = "Motive") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.key = element_rect(fill = "gray"),
    legend.background = element_rect(fill = "gray"),
    line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = .5),
    plot.subtitle = element_text(hjust = .5)
  )
p + transition_states(decade,
  transition_length = 1, state_length = 3, wrap = TRUE
) +
  ease_aes("linear") +
  enter_drift() +
  exit_drift() +
  ggtitle(label = "Villain Motivations for {closest_state} Episodes", subtitle = paste("n = {scoobydoo |> filter(decade == closest_state)  |> pull(decade_count)  |> unique()}"))
```

<img src="/home/rheslin/R/Projects/untidy_tuesdays/outputs/scooby_doo_files/figure-gfm/unnamed-chunk-7-1.gif" style="display: block; margin: auto;" />
