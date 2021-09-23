Code-along, Week 01: Emmy nominees
================
Alex Homer
23 September 2021

``` r
library(tidyverse)
```

## Read data

The data are drawn from the “Tidy Tuesday” project: [2021 Week 39: Emmy
Awards and
Nominees](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-09-21/readme.md)
(credit: [Emmys
website](https://www.emmys.com/awards/nominations/award-search)/[Susie
Lu](https://www.susielu.com/data-viz/emmy-2017)). They have been lightly
cleaned.

``` r
nominees <- readRDS("data/nominees_clean.rds")
```

## Who won the most Emmys?

``` r
winners <- nominees %>%
  filter(type == "Winner") %>%
  select(-type)

(
  winners_count <- winners %>%
    count(title, sort = TRUE)
)
```

    ## # A tibble: 719 × 2
    ##    title                                  n
    ##    <chr>                              <int>
    ##  1 Game Of Thrones                      245
    ##  2 Saturday Night Live                  229
    ##  3 Last Week Tonight With John Oliver   148
    ##  4 The Daily Show With Jon Stewart      133
    ##  5 RuPaul's Drag Race                    98
    ##  6 The Amazing Race                      88
    ##  7 The Voice                             87
    ##  8 Dancing With The Stars                81
    ##  9 American Idol                         73
    ## 10 Modern Family                         71
    ## # … with 709 more rows

Game of Thrones won the most Emmys. Let’s see a graph!

``` r
#First make a new data frame with an "other" category.
cutoff <- 60

winners_count %>% filter(n >= cutoff) %>%
  mutate(title = fct_reorder(title, n)) %>%
  ggplot(aes(x = n, y = title)) +
  geom_col() +
  labs(
    x = "Number of wins",
    y = "Show",
    title = "Shows with most Emmy wins",
    subtitle = paste("Shows with more than", cutoff, "wins shown"),
    caption = "Source: Emmys website/Susie Lu"
  ) +
  theme_minimal()
```

![](emmys_files/figure-gfm/winner-plot-1.png)<!-- -->
