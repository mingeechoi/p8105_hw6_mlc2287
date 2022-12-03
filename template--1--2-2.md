Homework 6
================
Mingee Choi
12/1/2022

# Problem 2

Create city_state variable and binary variable indicating whether
homicide is solved. Omit certain cities and Tulsa, AL. Limit analysis
for whom victim_race is white or black. Make sure victim_age is numeric.

``` r
homicide_data=
  read_csv("./data/homicide-data.csv")%>%
  janitor::clean_names()%>%
  mutate(reported_date = lubridate::ymd(reported_date))%>%
  drop_na(reported_date)
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
homicide_df=
homicide_data%>%
  mutate(city_state= str_c(city, state, sep=",")
  )%>%
  select(uid,city_state,everything())%>%
  group_by(city_state)%>%
  mutate(solved = case_when(disposition == "Closed by arrest" ~ 1,
                            disposition ==  "Closed without arrest" ~ 0,
                            disposition == "Open/No arrest" ~ 0),
         victim_age = as.numeric(victim_age),
         victim_race = as.factor(victim_race)) %>% 
  filter(city_state != "Dallas,TX", 
         city_state != "Phoenix,AZ", 
         city_state != "Kansas City,MO",
         city_state != "Tulsa,AL",
         victim_race %in% c("Black", "White")) %>% 
  select(city_state, solved, victim_age, victim_race, victim_sex)

homicide_df
```

    ## # A tibble: 39,692 × 5
    ## # Groups:   city_state [47]
    ##    city_state     solved victim_age victim_race victim_sex
    ##    <chr>           <dbl>      <dbl> <fct>       <chr>     
    ##  1 Albuquerque,NM      0         15 White       Female    
    ##  2 Albuquerque,NM      0         72 White       Female    
    ##  3 Albuquerque,NM      0         91 White       Female    
    ##  4 Albuquerque,NM      0         56 White       Male      
    ##  5 Albuquerque,NM      0         NA White       Male      
    ##  6 Albuquerque,NM      1         43 White       Female    
    ##  7 Albuquerque,NM      1         52 White       Male      
    ##  8 Albuquerque,NM      1         22 White       Female    
    ##  9 Albuquerque,NM      1         15 Black       Male      
    ## 10 Albuquerque,NM      1         25 Black       Male      
    ## # … with 39,682 more rows

Fit logistic regression with resolved vs unresolved as outcome and
victim and age, sex, and race, as predictors.Save glm as R object.

``` r
baltimore_df =
  homicide_df %>% 
  filter(city_state == "Baltimore,MD")

baltimore_glm=
  baltimore_df%>%
  glm(solved ~ victim_age + victim_race + victim_sex, data = ., family = binomial()) 

save(baltimore_glm, file="baltimore_glm.RData")
```

Apply broom: tidy to object and obtain estimate and confidence interval
of adjusted OR for solving homicides comparing male to female victims,
keeping all other variable fixed.

``` r
baltimore_glm%>%
 broom::tidy() %>% 
  mutate(OR = exp(estimate),
         lower_ci = exp(estimate - 1.96*std.error),
         upper_ci = exp(estimate + 1.96*std.error)) %>% 
  select(term, log_OR = estimate, OR, p.value, lower_ci, upper_ci) %>% 
  knitr::kable(digits = 3)
```

| term             | log_OR |    OR | p.value | lower_ci | upper_ci |
|:-----------------|-------:|------:|--------:|---------:|---------:|
| (Intercept)      |  0.310 | 1.363 |   0.070 |    0.975 |    1.907 |
| victim_age       | -0.007 | 0.993 |   0.043 |    0.987 |    1.000 |
| victim_raceWhite |  0.842 | 2.320 |   0.000 |    1.648 |    3.268 |
| victim_sexMale   | -0.854 | 0.426 |   0.000 |    0.325 |    0.558 |

Run glm for each cities in dataset and extract adjusted OR and CI for
solving homicides comparing male to female victims. Unnest.

``` r
cities_glm=
  homicide_df%>%
  nest(data = -city_state) %>%
  mutate( 
    models = map(.x = data, ~glm(solved ~ victim_age + victim_sex + victim_race, data = .x, family = binomial())), 
    results = map(models, broom::tidy)) %>%  
  select(city_state, results) %>% 
  unnest(results) %>%
  mutate(OR = exp(estimate),
         lower_ci = exp(estimate - 1.96*std.error),
         upper_ci = exp(estimate + 1.96*std.error)) %>%
  select(city_state, term, OR, lower_ci, upper_ci)

cities_glm
```

    ## # A tibble: 193 × 5
    ## # Groups:   city_state [47]
    ##    city_state     term                       OR lower_ci upper_ci
    ##    <chr>          <chr>                   <dbl>    <dbl>    <dbl>
    ##  1 Albuquerque,NM (Intercept)       1.84           0.534    6.37 
    ##  2 Albuquerque,NM victim_age        0.981          0.963    0.998
    ##  3 Albuquerque,NM victim_sexMale    1.77           0.831    3.76 
    ##  4 Albuquerque,NM victim_sexUnknown 0.000000471    0      Inf    
    ##  5 Albuquerque,NM victim_raceWhite  1.51           0.668    3.41 
    ##  6 Atlanta,GA     (Intercept)       2.39           1.49     3.83 
    ##  7 Atlanta,GA     victim_age        0.988          0.979    0.997
    ##  8 Atlanta,GA     victim_sexMale    1.00           0.684    1.46 
    ##  9 Atlanta,GA     victim_raceWhite  1.31           0.749    2.28 
    ## 10 Baltimore,MD   (Intercept)       1.36           0.975    1.91 
    ## # … with 183 more rows

Plot that shows estimated ORs and CIs for each city.

``` r
cities_glm%>%
  filter(term == "victim_sexMale")%>%
  mutate(city_state = fct_reorder(city_state, OR)) %>% 
  ggplot(aes(x = city_state, y = OR)) +
  geom_point() + 
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) +
  labs(y = "OR", x = "City, State",title = "Homicides")+
   theme(axis.text.x = element_text(angle = 80, hjust = 1))
```

<img src="template--1--2-2_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

In most states, the odds that the homicide will remain solved for cases
where the victim is male is lower than the odds that the homicide will
remain solved for cases where the victim is female (ex: Chicago,
Cincinnati, Detroit)

# Problem 3

Load and clean data for regression analysis

``` r
birthweight_data =
  read_csv("./data/birthweight.csv") %>% 
  drop_na() %>% 
  janitor::clean_names()
```

    ## Rows: 4342 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (20): babysex, bhead, blength, bwt, delwt, fincome, frace, gaweeks, malf...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#checking datatype
  str(birthweight_data)
```

    ## tibble [4,342 × 20] (S3: tbl_df/tbl/data.frame)
    ##  $ babysex : num [1:4342] 2 1 2 1 2 1 2 2 1 1 ...
    ##  $ bhead   : num [1:4342] 34 34 36 34 34 33 33 33 36 33 ...
    ##  $ blength : num [1:4342] 51 48 50 52 52 52 46 49 52 50 ...
    ##  $ bwt     : num [1:4342] 3629 3062 3345 3062 3374 ...
    ##  $ delwt   : num [1:4342] 177 156 148 157 156 129 126 140 146 169 ...
    ##  $ fincome : num [1:4342] 35 65 85 55 5 55 96 5 85 75 ...
    ##  $ frace   : num [1:4342] 1 2 1 1 1 1 2 1 1 2 ...
    ##  $ gaweeks : num [1:4342] 39.9 25.9 39.9 40 41.6 ...
    ##  $ malform : num [1:4342] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ menarche: num [1:4342] 13 14 12 14 13 12 14 12 11 12 ...
    ##  $ mheight : num [1:4342] 63 65 64 64 66 66 72 62 61 64 ...
    ##  $ momage  : num [1:4342] 36 25 29 18 20 23 29 19 13 19 ...
    ##  $ mrace   : num [1:4342] 1 2 1 1 1 1 2 1 1 2 ...
    ##  $ parity  : num [1:4342] 3 0 0 0 0 0 0 0 0 0 ...
    ##  $ pnumlbw : num [1:4342] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ pnumsga : num [1:4342] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ppbmi   : num [1:4342] 26.3 21.3 23.6 21.8 21 ...
    ##  $ ppwt    : num [1:4342] 148 128 137 127 130 115 105 119 105 145 ...
    ##  $ smoken  : num [1:4342] 0 0 1 10 1 0 0 0 0 4 ...
    ##  $ wtgain  : num [1:4342] 29 28 11 30 26 14 21 21 41 24 ...

``` r
birthweight_df =
  birthweight_data%>%
  mutate(
    babysex = as.factor(babysex),
    malform = as.factor(malform),
    frace = as.factor(frace),
    frace = recode_factor(frace,
                      "1" = "White",
                      "2" = "Black",
                      "3" = "Asian",
                      "4" = "Puerto Rican",
                      "8" = "Other"),
    mrace = as.factor(mrace),
    mrace = recode_factor(mrace,
                      "1" = "White",
                      "2" = "Black",
                      "3" = "Asian",
                      "4" = "Puerto Rican",
                      "8" = "Other")
  )%>%
  select(bwt,everything())
```

Propose regression model for birthweight

``` r
#checking assumption-normal distribution and residuals
birthweight_df %>% 
  ggplot(aes(x = bwt)) + 
  geom_histogram(binwidth = 25) + 
  labs(
    title = "Checking Assumption", x = "Birthweight (g)",y = "Count"
  )
```

<img src="template--1--2-2_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

``` r
#I excluded pnumlbw and pnumgsa because they have 0 observations, malform and parity because they have low cell count, and ppbmi because I can use ppwt instead.
full_model = 
  lm(
    bwt ~ babysex + bhead + blength + delwt+ fincome+frace + gaweeks + menarche+ mheight + momage + mrace + smoken + ppwt + wtgain,
    data = birthweight_df
  )

#Summary table to see which covariates have p<0.05
summary(full_model) %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value)%>% 
   mutate(
    p_value = format.pval(p.value, digits = 3, eps = 0.05)
  ) %>% 
  select(-p.value) %>%
  arrange(p_value) %>% 
  knitr::kable()
```

| term              |      estimate | p_value |
|:------------------|--------------:|:--------|
| (Intercept)       | -6068.4938166 | \<0.05  |
| babysex2          |    29.2645106 | \<0.05  |
| bhead             |   130.9601419 | \<0.05  |
| blength           |    74.8904878 | \<0.05  |
| delwt             |     4.1290227 | \<0.05  |
| gaweeks           |    11.2616326 | \<0.05  |
| mheight           |     6.7694335 | \<0.05  |
| mraceBlack        |  -151.4204451 | \<0.05  |
| smoken            |    -4.8595918 | \<0.05  |
| ppwt              |    -2.7526850 | \<0.05  |
| fincome           |     0.2752995 | 0.125   |
| mraceAsian        |   -92.7619838 | 0.197   |
| mracePuerto Rican |   -56.8242317 | 0.208   |
| menarche          |    -3.5277645 | 0.223   |
| fracePuerto Rican |   -47.4657670 | 0.288   |
| momage            |     0.9797015 | 0.422   |
| fraceBlack        |    14.4417524 | 0.754   |
| fraceAsian        |    20.4717867 | 0.768   |
| fraceOther        |     4.3848564 | 0.953   |

Variables that I found significant were babysex, bhead, blength, delwt,
gaweeks, mheight, mraceBlack, smoken, and ppwt.

``` r
reduced_model = 
  lm(
    bwt ~ babysex + bhead + blength + delwt+ gaweeks +  mheight + mrace + smoken + ppwt,
    data = birthweight_df
  )

birthweight_df %>% 
  add_predictions(reduced_model) %>% 
  add_residuals(reduced_model) %>% 
  ggplot(aes(x = pred, y = resid)) + 
  geom_point()+
  labs(
    title = "Residuals against Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  )
```

<img src="template--1--2-2_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

Based on this plot, there seems to be a violation of the assumption of
constance variance. There are some outliers that needs to be taken care
of, and the distribution is heavily left-skewed. Possible solutions
might be creating formal rule to exclude outliers, transforming variable
using log transformation, or selecting best models of subsets by
maximizing adjusted R-squared and minimizing prediction error.

Alternative Models

``` r
alt_model1 =
  lm(bwt ~ blength + gaweeks, data = birthweight_df)

alt_model2 = 
  lm(bwt ~ bhead + blength + babysex + bhead*blength + blength*babysex + bhead*babysex + bhead*blength*babysex, data = birthweight_df)
```

Compare models using cross-validation

``` r
cv_df = 
  crossv_mc(birthweight_df, 100) %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  )

cv_df =
  cv_df %>% 
  mutate(
    reduced_model = map(.x = train, ~lm(bwt ~ babysex + bhead + blength + delwt+ gaweeks +  mheight + mrace + smoken + ppwt, data = .x)),
    alt_model1 = map(.x = train, ~lm(bwt ~ blength + gaweeks, data = .x)),
    alt_model2 = map(.x = train, ~lm(bwt ~ bhead + blength + babysex + bhead*blength + blength*babysex + bhead*babysex + bhead*blength*babysex, data = .x))
  ) %>% 
  mutate(
    rmse_reduced_model = map2_dbl(.x = reduced_model, .y = test, ~rmse(model = .x, data = .y)),
    rmse_alt1 = map2_dbl(.x = alt_model1, .y = test, ~rmse(model = .x, data = .y)),
    rmse_alt2 = map2_dbl(.x = alt_model2, .y = test, ~rmse(model = .x, data = .y))
  )

  cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>% 
  ggplot(aes(x = model, y = rmse)) +
  geom_boxplot()
```

<img src="template--1--2-2_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />
From the graph, we can clearly see that my reduced model has the lowest
mean rmse value and therfore, might be the best fit among these three
models.
