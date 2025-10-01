tidy_data
================
2025-09-23

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(haven)
```

This document will show how to tidy data.

## Pivot longer

aka consolidating columns into one column, with new variables, this
esp. important for columns you see repeated in diff. variations note:
after the transformation it is a good sign that data has more
observations note: names_prefix is used to delete leading prefixes note:
whenever you want to change something, use `mutate` function

``` r
pulse_df = 
  read_sas("p8105/lecture_code/data_import_examples/public_pulse_data.sas7bdat") %>% 
  janitor::clean_names() %>% 
    pivot_longer(
    cols = bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    values_to = "bdi_score",
    names_prefix = "bdi_score_"
 ) %>% 
mutate(visit = replace(visit, visit == "bl", "00m")) %>% 
relocate(id, visit)
```

note: `case_match` replaces specified old value with new value you want,
you can also use `case_when` for multiple variables

``` r
litters_df = 
  read_csv("p8105/lecture_code/data_import_examples/FAS_litters.csv", na = c("NA", ".", "")) %>% 
  janitor::clean_names() %>% 
  pivot_longer (
  cols = gd0_weight:gd18_weight,
  names_to ="gd_time",
  values_to = "weight"
  ) %>% 
  mutate(gd_time = case_match(
    gd_time, 
    "gd0_weight" ~ 0,
    "gd18weight" ~ 18
  ))
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Pivot wider

let’s make up an analysis result table

``` r
analysis_df = 
  tibble(group = c("treatment", "treatment", "control", "control"),
         time = c("pre", "post", "pre", "post"),
         mean = c(4, 10, 4.2, 5))
```

pivot wider for human readability `knitr::kable()` calling the kable
function from kntir package to help refine output look

``` r
analysis_df %>% 
  pivot_wider(
    names_from = time,
    values_from = mean
  ) %>% 
  knitr::kable()
```

| group     | pre | post |
|:----------|----:|-----:|
| treatment | 4.0 |   10 |
| control   | 4.2 |    5 |

## Bind tables

note: if you want to stack similar tables together, use `bind_rows`
function note: `mutate(movie = "fellowship_ring")` = I created a new
column and variables

``` r
fellowship_ring = 
  read_excel("p8105/lecture_code/data_import_examples/LotR_Words.xlsx", range = "B3:D6") %>% 
  mutate(movie = "fellowship_ring")

two_towers = 
  read_excel("p8105/lecture_code/data_import_examples/LotR_Words.xlsx", range = "F3:H6") %>% 
  mutate(movie = "two_towers")

return_king = 
  read_excel("p8105/lecture_code/data_import_examples/LotR_Words.xlsx", range = "J3:L6") %>% 
  mutate(movie = "return_king")

lotr_df = 
  bind_rows(fellowship_ring, two_towers, return_king) %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    cols = female:male,
    names_to = "sex",
    values_to = "words" 
    ) %>% 
    relocate(movie) %>% 
    mutate(race = str_to_lower(race))
```

## Join FAS datasets

note: joining two tables by unique id

``` r
litters_df = 
  read_csv("p8105/lecture_code/data_import_examples/FAS_litters.csv", na = c("NA", ".", "")) %>%
  janitor::clean_names() %>% 
  mutate(wt_gain = gd18_weight - gd0_weight) %>% 
  separate(
    group, into = c("dose", "day_of_treatment"), sep = 3
  )
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pups_df = 
   read_csv("p8105/lecture_code/data_import_examples/FAS_pups.csv", skip = 3, na = c("NA", ".", "")) %>%
  janitor::clean_names() %>% 
  mutate(
    sex = case_match(
    sex,
    1 ~ "male",
    2 ~ "female"
  ))
```

    ## Rows: 313 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

join the datasets! note: if there is a litter that has no value in
`pup_df` than we lose row

``` r
fas_df = 
  left_join(pups_df, litters_df, by = "litter_number") %>% 
  relocate(litter_number, dose, day_of_treatment)
```
