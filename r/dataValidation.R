# Data validation - comparing with existing HT and with StatistikBanken (for
# background population)
library(data.table)
library(readxl)
library(ggplot2)
pop_reference <- fread("data/pop_age_10yr_dst.csv")
pop_ht <- fread("data/pop_summary_age.txt")
pop_sif <- fread("data/pop_age_10y_SIF.csv")

pop <- merge(
  pop_reference,
  pop_ht,
  by.x = c("sex", "age_group", "year"),
  by.y = c("sex", "grouping", "year"),
  suffixes = c(".ref", ".ht")
)

pop <- merge(
  pop,
  pop_sif,
  by.x = c("sex", "age_group", "year"),
  by.y = c("sex", "age_group", "year")
)
setnames(pop, "count", "count.sif")
# Find difference between StatistikBanken population and my population and SIF
pop[, count_diff := count.ht - count.ref]
pop[, count_diff_sif := count.sif - count.ref]
pop[, diff_proportion := count_diff / count.ref * 100]
pop[, diff_proportion_sif := count_diff_sif / count.ref * 100]
# Not huge differences in population numbers
pop[, range(count_diff)]
pop[, range(diff_proportion)]

pop[, range(count_diff_sif)]
pop[, range(diff_proportion_sif)]


pop[pop[, which.max(abs(count_diff))]]
pop[pop[, which.max(abs(diff_proportion))]]
head(pop[sex == "female"], 75)
head(pop[sex == "male"], 75)

# Plot distribution of differences
hist(pop[, count_diff])
hist(pop[, diff_proportion])
hist(pop[, count_diff_sif])
hist(pop[, diff_proportion_sif])
# Sum over age and sex for each year
pop_year <-
  pop[, .(count.ref = sum(count.ref),
          count.ht = sum(count.ht)), by = year]
pop_year[, count.diff := count.ht - count.ref]
pop_year[, diff.proportion := (count.diff / count.ref) * 100]
diff(pop_year$diff.proportion)
pop_year[, range(diff.proportion)]




# COMPARE OUTCOME DATA WITH OLD HJERTETAL ---------------------------------
data_files <-
  list.files("C:/Users/mphelps/Hjerteforeningen/Forskning - Old hjertetal data",
             full.names = TRUE)
dat_ht1 <- lapply(data_files, function(i) {
  read_xlsx(i) %>% setDT()
}) %>% rbindlist()
setnames(dat_ht1, c("outcome", "variable", "sex", "year", "count_ht1"))
dat_ht1[sex == "Kvinder", sex := "female"]
dat_ht1[sex == "Mænd", sex := "male"]
shiny_dat <- readRDS(file = "data/shiny_dat_dk.rds")

d1_prev1 <-
  dat_ht1[variable == "prevalence" &
            sex != "Total", .(sex, year, count_ht1)]
d1_prev2 <-
  shiny_dat$d1$national[, .(sex, year, count_n_prevalence)]
validate_prev <- merge(d1_prev1, d1_prev2, by = c("sex", "year"))
validate_prev[, count_diff := count_n_prevalence - count_ht1]

ggplot(validate_prev) + geom_line(aes(
  x = year,
  y = count_diff,
  group = sex,
  color = sex
))
