library(DasGuptR)
library(readxl)
library(here)
library(tidyverse)
library(binom)
library(rsample)

scot_reconv <- readRDS(here::here("01_data", "combined_reconvictions.rds"))

# set up 


uncount_split <- function(splits){
  
  analysis(splits) |> 
    count(year, age, gender, reconvicted) |> 
    pivot_wider(names_from = "reconvicted",
                values_from = "n") |> 
    mutate(number_of_offenders = n_not_reconvicted + number_reconvicted,
           prevalence = number_reconvicted / number_of_offenders)
  
  
}


calc_dg <- function(d){
  
  d |> 
    dgnpop(pop="year", 
           factors=c("prevalence"),id_vars=c("gender","age"),
           crossclassified="number_of_offenders",
           agg = TRUE) |> 
    dg_table() |> 
    rownames_to_column()
  
}


n_draws <- 500



# just the first and last year
dat <- 
scot_reconv |> 
  filter(year == "2019-20" | year == "2020-21")

dat_long <- 
dat |> 
  mutate(n_not_reconvicted = number_of_offenders - number_reconvicted) |> 
  select(year, age, gender, n_not_reconvicted, number_reconvicted) |> 
  pivot_longer(n_not_reconvicted:number_reconvicted, names_to = "reconvicted", values_to = "n") |> 
  mutate(n = as.integer(n)) |>
  uncount(n)



dat_long |> 
  count(year, age, gender, reconvicted) |> 
  pivot_wider(names_from = "reconvicted",
              values_from = "n")



tmp <- 
dat_long |> 
  group_by(year) |> 
  rsample::bootstraps(times = n_draws) |> 
  mutate(dat = map(splits, uncount_split),
         results = map(dat, calc_dg),
         draw = 1:n_draws) |> 
  select(draw, results)

full_data_2019 <- 
scot_reconv |> 
  filter(year == "2019-20" | year == "2020-21") |> 
  dgnpop(pop="year", 
       factors=c("prevalence"),id_vars=c("gender","age"),
       crossclassified="number_of_offenders",
       agg = TRUE) |> 
  dg_table() |> 
  rownames_to_column()

tmp |> 
  unnest(results) |> 
  pivot_longer(`2019-20`:`2020-21`,
               names_to = "year",
               values_to = "rate") |> 
  filter(year == "2019-20") |> 
  ggplot(aes(x = decomp, y = rowname)) +
  ggdist::stat_halfeye() +
  geom_point(
    data = full_data_2019,
    aes(x = decomp, y = rowname),
    size = 4,
    colour = "red"
  ) +
  ggtitle(
    label = "Red point is real data"
  ) +
  facet_wrap(~ rowname, scales = "free_x")




tmp |> 
  unnest(results) |> 
  pivot_longer(`2019-20`:`2020-21`,
               names_to = "year",
               values_to = "rate") |> 
  group_by(rowname) |> 
  summarise(confs = quantile(decomp, c(0.025, 0.975)))

# looks fine

dat2 <- 
  scot_reconv |> 
  filter(year == "2004-05" | year == "2005-06")

dat2_long <- 
  dat2 |> 
  mutate(n_not_reconvicted = number_of_offenders - number_reconvicted) |> 
  select(year, age, gender, n_not_reconvicted, number_reconvicted) |> 
  pivot_longer(n_not_reconvicted:number_reconvicted, names_to = "reconvicted", values_to = "n") |> 
  mutate(n = as.integer(n)) |>
  uncount(n)


tmp2 <- 
  dat2_long |> 
  group_by(year) |> 
  rsample::bootstraps(times = n_draws) |> 
  mutate(res = map(splits, uncount_split),
         results = map(res, calc_dg),
         draw = 1:n_draws) |> 
  select(draw, results)


full_dat_res <- 
 dat2 |> 
  dgnpop(pop="year", 
         factors=c("prevalence"),id_vars=c("gender","age"),
         crossclassified="number_of_offenders",
         agg = TRUE) |> 
  dg_table() |> 
  rownames_to_column()



tmp2 |> 
  unnest(results) |> 
  pivot_longer(`2004-05`:`2005-06`,
               names_to = "year",
               values_to = "rate") |> 
  filter(year == "2004-05") |> 
  ggplot(aes(x = decomp, y = rowname)) +
  ggdist::stat_halfeye() +
  scale_x_continuous(limits = c(-250, 250)) +
  geom_point(
    data = full_dat_res,
    aes(x = decomp, y = rowname),
    size = 4,
    colour = "red"
  ) +
  ggtitle(
    label = "Red point is real data"
  )



tmp2 |> 
  unnest(results) |> 
  pivot_longer(`2004-05`:`2005-06`,
               names_to = "year",
               values_to = "rate") |> 
  filter(rowname == "age_struct") |> 
  group_by(rowname, year) |> 
  summarise(confs = quantile(decomp, c(0.025, 0.5, 0.975)))

# diff not decomp


tmp2 |> 
  unnest(results) |> 
  pivot_longer(`2004-05`:`2005-06`,
               names_to = "year",
               values_to = "rate") |> 
  filter(year == "2004-05") |> 
  ggplot(aes(x = diff, y = rowname)) +
  ggdist::stat_halfeye() +
#  scale_x_continuous(limits = c(-250, 250)) +
  geom_point(
    data = full_dat_res,
    aes(x = diff, y = rowname),
    size = 4,
    colour = "red"
  ) +
  ggtitle(
    label = "Red point is real data"
  )


# ruh roh


scot_reconv |> 
  filter(year == "2004-05" | year == "2005-06") |> 
  dgnpop(pop="year", 
         factors=c("prevalence"),id_vars=c("gender","age"),
         crossclassified="number_of_offenders",
         agg = TRUE) |> 
  dg_table() |> 
  rownames_to_column()





# stratified --------------------------------------------------------------




tmp2_strat <- 
  dat2_long |> 
  group_by(year, age, gender) |> 
  rsample::bootstraps(times = n_draws) |> 
  ungroup() |> 
  mutate(dat = map(splits, uncount_split),
         results = map(dat, calc_dg),
         draw = 1:n_draws) |> 
  select(draw, results)

tmp2_strat |> 
  unnest(results) |> 
  pivot_longer(`2004-05`:`2005-06`,
               names_to = "year",
               values_to = "rate") |> 
  filter(rowname == "age_struct") |> 
  group_by(rowname, year) |> 
  summarise(confs = quantile(decomp, c(0.025, 0.5, 0.975)))

dat2  |> 
  dgnpop(pop="year", 
             factors=c("prevalence"),id_vars=c("gender","age"),
             crossclassified="number_of_offenders",
             agg = TRUE) |> 
  dg_table() |> 
  rownames_to_column()



tmp2_strat |> 
  unnest(results) |> 
  pivot_longer(`2004-05`:`2005-06`,
               names_to = "year",
               values_to = "rate") |> 
  filter(year == "2004-05") |> 
  ggplot(aes(x = decomp, y = rowname)) +
  ggdist::stat_halfeye() +
  scale_x_continuous(limits = c(-250, 250)) +
  geom_point(
    data = full_dat_res,
    aes(x = decomp, y = rowname),
    size = 4,
    colour = "red"
  ) +
  ggtitle(
    label = "Red point is real data"
  )




# some other year ---------------------------------------------------------


dat3 <- 
  scot_reconv |> 
  filter(year == "2010-11" | year == "2011-12")

dat3_long <- 
  dat3 |> 
  mutate(n_not_reconvicted = number_of_offenders - number_reconvicted) |> 
  select(year, age, gender, n_not_reconvicted, number_reconvicted) |> 
  pivot_longer(n_not_reconvicted:number_reconvicted, names_to = "reconvicted", values_to = "n") |> 
  mutate(n = as.integer(n)) |>
  uncount(n)


tmp3 <- 
  dat3_long |> 
  group_by(year) |> 
  rsample::bootstraps(times = n_draws) |> 
  mutate(res = map(splits, uncount_split),
         results = map(res, calc_dg),
         draw = 1:n_draws) |> 
  select(draw, results)


full_dat3_res <- 
  dat3 |> 
  dgnpop(pop="year", 
         factors=c("prevalence"),id_vars=c("gender","age"),
         crossclassified="number_of_offenders",
         agg = TRUE) |> 
  dg_table() |> 
  rownames_to_column()



tmp3 |> 
  unnest(results) |> 
  pivot_longer(`2010-11`:`2011-12`,
               names_to = "year",
               values_to = "rate") |> 
  filter(year == "2010-11") |> 
  ggplot(aes(x = decomp, y = rowname)) +
  ggdist::stat_halfeye() +
  scale_x_continuous(limits = c(-250, 250)) +
  geom_point(
    data = full_dat3_res,
    aes(x = decomp, y = rowname),
    size = 4,
    colour = "red"
  ) +
  ggtitle(
    label = "Red point is real data"
  )



tmp3 |> 
  unnest(results) |> 
  pivot_longer(`2010-11`:`2011-12`,
               names_to = "year",
               values_to = "rate") |> 
  filter(year == "2010-11") |> 
  group_by(rowname, year) |> 
  summarise(confs = quantile(diff, c(0.025, 0.5, 0.975)))




# checking ----------------------------------------------------------------


tmp2 <- 
  dat2_long |> 
  group_by(year) |> 
  rsample::bootstraps(times = n_draws) |> 
  mutate(res = map(splits, uncount_split),
         results = map(res, calc_dg),
         draw = 1:n_draws)

dat2



tmp2$results[[1]]

dat2  |> 
  dgnpop(pop="year", 
         factors=c("prevalence"),id_vars=c("gender","age"),
         crossclassified="number_of_offenders",
         agg = TRUE) |> 
  dg_table() |> 
  rownames_to_column()


tmp2 |> 
  select(draw, results) |> 
  unnest(results) |> 
  ggplot(aes(x = diff, y = rowname))+
  ggdist::stat_halfeye()



tmp3 |> 
  select(draw, results) |> 
  unnest(results) |> 
  ggplot(aes(x = diff, y = rowname))+
  ggdist::stat_halfeye()


tmp |> 
  select(draw, results) |> 
  unnest(results) |> 
  ggplot(aes(x = diff, y = rowname))+
  ggdist::stat_halfeye()

years <- 
tibble(
  y1 = distinct(scot_reconv, year) |> pull()
) |> 
  mutate(y2 = lead(y1)) |> 
  filter(!is.na(y2))



calc_dg_filter <- function(y1, y2, dat){
  
  dat |>
    filter(year == y1 | year == y2) |> 
    dgnpop(pop="year", 
           factors=c("prevalence"),id_vars=c("gender","age"),
           crossclassified="number_of_offenders",
           agg = TRUE) |> 
    dg_table() |> 
    rownames_to_column()
  
  
}



years <- 
years |> 
  mutate(dg = map2(y1, y2, ~
                   scot_reconv |>
                     filter(year == .x | year == .y) |> 
                     dgnpop(pop="year", 
                            factors=c("prevalence"),id_vars=c("gender","age"),
                            crossclassified="number_of_offenders",
                            agg = TRUE) |> 
                     dg_table() |> 
                     rownames_to_column() |> 
                     select(rowname, diff)
                     ))


years |> 
  unnest(dg) |> 
  filter(rowname == "crude") |> 
  arrange(abs(diff))


# try 97/98






dat4 <- 
  scot_reconv |> 
  filter(year == "1997-98" | year == "1998-99")

dat4_long <- 
  dat4 |> 
  mutate(n_not_reconvicted = number_of_offenders - number_reconvicted) |> 
  select(year, age, gender, n_not_reconvicted, number_reconvicted) |> 
  pivot_longer(n_not_reconvicted:number_reconvicted, names_to = "reconvicted", values_to = "n") |> 
  mutate(n = as.integer(n)) |>
  uncount(n)


tmp4 <- 
  dat4_long |> 
  group_by(year) |> 
  rsample::bootstraps(times = n_draws) |> 
  mutate(res = map(splits, uncount_split),
         results = map(res, calc_dg),
         draw = 1:n_draws) |> 
  select(draw, results)


full_dat4_res <- 
  dat4 |> 
  dgnpop(pop="year", 
         factors=c("prevalence"),id_vars=c("gender","age"),
         crossclassified="number_of_offenders",
         agg = TRUE) |> 
  dg_table() |> 
  rownames_to_column()



tmp4 |> 
  unnest(results) |> 
  pivot_longer(`1997-98`:`1998-99`,
               names_to = "year",
               values_to = "rate") |> 
  filter(year == "1997-98") |> 
  ggplot(aes(x = decomp, y = rowname)) +
  ggdist::stat_halfeye() +
  scale_x_continuous(limits = c(-250, 250)) +
  geom_point(
    data = full_dat3_res,
    aes(x = decomp, y = rowname),
    size = 4,
    colour = "red"
  ) +
  ggtitle(
    label = "Red point is real data"
  )



tmp4 |> 
  unnest(results) |> 
  pivot_longer(`1997-98`:`1998-99`,
               names_to = "year",
               values_to = "rate") |> 
  filter(year == "1997-98") |> 
  group_by(rowname, year) |> 
  summarise(confs = quantile(decomp, c(0.025, 0.5, 0.975)))





# and now 05/06 -----------------------------------------------------------




dat5 <- 
  scot_reconv |> 
  filter(year == "2005-06" | year == "2006-07")

dat5_long <- 
  dat5 |> 
  mutate(n_not_reconvicted = number_of_offenders - number_reconvicted) |> 
  select(year, age, gender, n_not_reconvicted, number_reconvicted) |> 
  pivot_longer(n_not_reconvicted:number_reconvicted, names_to = "reconvicted", values_to = "n") |> 
  mutate(n = as.integer(n)) |>
  uncount(n)


tmp5 <- 
  dat5_long |> 
  group_by(year) |> 
  rsample::bootstraps(times = n_draws) |> 
  mutate(res = map(splits, uncount_split),
         results = map(res, calc_dg),
         draw = 1:n_draws) |> 
  select(draw, results)


full_dat5_res <- 
  dat5 |> 
  dgnpop(pop="year", 
         factors=c("prevalence"),id_vars=c("gender","age"),
         crossclassified="number_of_offenders",
         agg = TRUE) |> 
  dg_table() |> 
  rownames_to_column()



tmp5 |> 
  unnest(results) |> 
  pivot_longer(`2005-06`:`2006-07`,
               names_to = "year",
               values_to = "rate") |> 
  filter(year == "2005-06") |> 
  ggplot(aes(x = decomp, y = rowname)) +
  ggdist::stat_halfeye() +
  scale_x_continuous(limits = c(-250, 250)) +
  geom_point(
    data = full_dat3_res,
    aes(x = decomp, y = rowname),
    size = 4,
    colour = "red"
  ) +
  ggtitle(
    label = "Red point is real data"
  )



tmp5 |> 
  unnest(results) |> 
  pivot_longer(`2005-06`:`2006-07`,
               names_to = "year",
               values_to = "rate") |> 
  filter(year == "2005-06") |> 
  group_by(rowname, year) |> 
  summarise(confs = quantile(decomp, c(0.05, 0.5, 0.95)))

