Taking the DuBois Challenge
================
Ryan Heslin
May 27, 2022





This is an attempt to replicate data visualizations created by the great
writer and sociologist W.E.B. DuBois.

``` r
library(tidyverse)
library(tidytuesdayR)
library(ggplot2)

dubois <- tt_load(2021, week = 8)
```

``` 

    Downloading file 1 of 8: `freed_slaves.csv`
    Downloading file 2 of 8: `census.csv`
    Downloading file 3 of 8: `furniture.csv`
    Downloading file 4 of 8: `city_rural.csv`
    Downloading file 5 of 8: `income.csv`
    Downloading file 6 of 8: `occupation.csv`
    Downloading file 7 of 8: `conjugal.csv`
    Downloading file 8 of 8: `georgia_pop.csv`
```

``` r
freed_slaves <- dubois$freed_slaves
head(freed_slaves)
```

<div class="kable-table">

| Year | Slave | Free |
| ---: | ----: | ---: |
| 1790 |  92.0 |  8.0 |
| 1800 |  88.0 | 11.0 |
| 1810 |  86.5 | 13.5 |
| 1820 |  87.0 | 13.0 |
| 1830 |  86.0 | 14.0 |
| 1840 |  87.0 | 13.0 |

</div>

``` r
labs <- sprintf("%.0f%%", rep(dubois$freed_slaves$Free, each = 2))
labs_coords <-
  c(
    rep(dubois$freed_slaves$Slave[-nrow(dubois$freed_slaves)], each = 2),
    rep(dubois$freed_slaves$Slave[nrow(dubois$freed_slaves) - 1], 2)
  )
p1 <-
  freed_slaves %>%
  pivot_longer(c(Slave, Free), values_to = "Number", names_to = "Status") %>%
  ggplot(aes(
    x = Year,
    y = Number,
    fill = Status,
    alpha = Status
  )) +
  stat_summary(
    geom = "area",
    position = "fill",
    fun = "identity"
  ) +
  geom_vline(
    aes(xintercept = Year),
    color = "black",
    size = .5,
    alpha = .5
  ) +
  scale_fill_manual(values = c(Free = "green4", Slave = "Black")) +
  scale_alpha_manual(values = c(Free = .9, Slave = 1)) +
  scale_x_continuous(
    expand = c(.025, 0),
    breaks = seq(1790, 1870, by = 10),
    position = "top"
  ) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent_format()) +
  annotate(
    "text",
    x = 1830,
    y = .95,
    label  = "FREE - LIBRE",
    fontface = "bold",
    size = 5
  ) +
  annotate(
    "text",
    x = 1830,
    y = .6,
    "label" = "SLAVES\nESCLAVES",
    color = "white",
    size = 7,
    fontface = "bold"
  ) +
  geom_text(aes(y = (labs_coords / 100) + .025), label = labs) +
  labs(y = NULL, x = NULL) +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill = "white")
  )
ggsave("dubois1.png", plot = p1, path = "../outputs/plots")
```

``` r
factor_levels <- c(
  "Agriculture, Fisheries and Mining",
  "Domestic and Personal Service",
  "Manufacturing and Mechanical Industries",
  "Trade and Transportation",
  "Professions"
)
gap <- 20
occupation <- dubois$occupation %>%
  group_by(Group) %>%
  mutate(
    Occupation = factor(Occupation,
      levels = factor_levels,
      ordered = TRUE
    ),
    ord = order(Occupation, factor_levels)
  ) %>%
  arrange(ord) %>%
  mutate(
    xmax = gap + cumsum(Percentage * ((100 - (
      gap * 2
    )) / 100)) + (100 * (cur_group_id() - 1)),
    xmin = lag(xmax, default = gap + (100 * (cur_group_id() - 1))),
    ymin = 0,
    ymax = 1,
    label_x = xmin + (xmax - xmin) / 2
  ) %>%
  ungroup()

key <-
  setNames(
    c("red", "yellow", "blue", "#DFBF9F", "gray"),
    levels(occupation$Occupation)
  )
circle_data <- tibble(
  color = key,
  x = c(5, 90, 195, 110, 100),
  y = .9,
  size = 7,
)
text_data <- tibble(
  label = c(
    "AGRICULTURE, FISHERIES\n AND MINING",
    "DOMESTIC AND\n PERSONAL SERVICE",
    "MANUFACTURING AND\n MECHANICAL INDUSTRIES",
    "TRADE AND\n TRANSPORTATION",
    "PROFESSIONS"
  ),
  x = c(5, 90, 195, 110, 100),
  y = .8,
  size = 4.35,
  hjust = c("left", "right", "left", "right", "right")
)
group_labs <-
  tibble(
    x = c(50, 150),
    y = 1.1,
    label = c("NEGROES.", "WHITES.")
  )
p2 <- occupation %>%
  ggplot(
    aes(
      x = Percentage,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      group = Group,
      fill = Occupation
    )
  ) +
  geom_rect(position = "dodge", show.legend = FALSE) +
  geom_text(aes(
    label = paste0(Percentage, "%"),
    x = label_x,
    y = .9,
  ), size = 2.5) +
  coord_polar(
    clip = "off",
    start = -pi / 2
  ) +
  xlim(c(0, 200)) +
  scale_fill_manual(values = key) +
  geom_point(
    data = circle_data,
    aes_auto(names(circle_data)),
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_text(
    data = text_data,
    aes_auto(names(text_data)),
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_text(data = group_labs, aes_auto(names(group_labs)), inherit.aes = FALSE) +
  scale_color_identity() +
  labs(title = "OCCUPATIONS OF NEGROES AND WHITES IN GEORGIA.") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.title.position = "plot",
    plot.title = element_text(hjust = .5, face = "bold")
  )

ggsave("dubois2.png", plot = p2, path = "../outputs/plots/")
```
