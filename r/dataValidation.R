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
pop[, diff_count := count.ht - count.ref]
pop[, diff_count_sif := count.sif - count.ref]
pop[, diff_proportion := diff_count / count.ref * 100]
pop[, diff_proportion_sif := diff_count_sif / count.ref * 100]
# Not huge differences in population numbers
pop[, range(diff_count)]
pop[, range(diff_proportion)]

pop[, range(diff_count_sif)]
pop[, range(diff_proportion_sif)]


pop[pop[, which.max(abs(diff_count))]]
pop[pop[, which.max(abs(diff_proportion))]]
head(pop[sex == "female"], 75)
head(pop[sex == "male"], 75)

# Plot distribution of differences
hist(pop[, diff_count])
hist(pop[, diff_proportion])
hist(pop[, diff_count_sif])
hist(pop[, diff_proportion_sif])
# Sum over age and sex for each year
pop_year <-
  pop[, .(count.ref = sum(count.ref),
          count.ht = sum(count.ht)), by = year]
pop_year[, count.diff := count.ht - count.ref]
pop_year[, diff.proportion := (count.diff / count.ref) * 100]
diff(pop_year$diff.proportion)
pop_year[, range(diff.proportion)]





# COMPARE OUTCOME DATA WITH OLD HJERTETAL NATIONALK LEVEL -----------------

shiny_dat <- readRDS(file = "data/shiny_dat_dk.rds")
outcome_desc <-
  fread(file = "data/outcome_descriptions.csv", encoding = "UTF-8")
outcome_desc[, name_dk := tolower(name_dk)]

data_files <-
  list.files(
    "C:/Users/mphelps/Hjerteforeningen/Forskning - Old hjertetal data/udvikling/",
    full.names = TRUE
  )
dat_ht1 <- lapply(data_files, function(i) {
  read_xlsx(i) %>% setDT()
}) %>% rbindlist(use.names = FALSE)
setnames(dat_ht1, c("outcome", "variable", "sex", "year", "count_ht1"))

# Re-code variables in ht1 dataset
dat_ht1[sex == "Kvinder", sex := "female"]
dat_ht1[sex == "Mænd", sex := "male"]
dat_ht1[is.na(sex), sex := "Total"]
dat_ht1[, variable := paste0("count_n_", variable)]
dat_ht1[, outcome := tolower(outcome)]
dat_ht1[, year := as.integer(year)]
dat_ht1 <-
  merge(dat_ht1, outcome_desc[, .(hjertetal_code, name_dk)], by.x = "outcome", by.y = "name_dk")


data_diff_national <- function(old, new, variable_str, ht_code) {
  d1_prev1 <-
    dat_ht1[variable == variable_str &
              hjertetal_code == ht_code,
            sex != "Total", .(sex, year, count_ht1)]
  
  keep_vars <- c("sex", "year", variable_str)
  d1_prev2 <-
    shiny_dat[[ht_code]]$national[, ..keep_vars]
  x <- merge(d1_prev1, d1_prev2, by = c("sex", "year"))
  x[, diff_count := get(variable_str) - count_ht1]
  x[]
}

d1_prev <-
  data_diff(dat_ht1,
            shiny_dat,
            variable_str =  "count_n_prevalence",
            ht_code = "d1")
d1_incidence <-
  data_diff(dat_ht1,
            shiny_dat,
            variable_str =  "count_n_incidence",
            ht_code = "d1")

ggplot(d1_prev) + geom_line(aes(
  x = year,
  y = diff_count,
  group = sex,
  color = sex
)) + theme_classic()

ggplot(d1_incidence) + geom_line(aes(
  x = year,
  y = diff_count,
  group = sex,
  color = sex
)) + theme_classic()



# COMPARE AGE LEVELS ------------------------------------------------------

data_files <-
  list.files(
    "C:/Users/mphelps/Hjerteforeningen/Forskning - Old hjertetal data/age/",
    full.names = TRUE
  )
dat_age <- lapply(data_files, function(i) {
  read_xlsx(i, skip = 2) %>% setDT()
}) %>% rbindlist(use.names = FALSE)

setnames(dat_age,
         c("sex", "grouping", "count_ht1", "year", "outcome", "variable"))

# Re-code variables
dat_age[is.na(sex), sex := "Total"]
dat_age[is.na(grouping), grouping := "Total"]
dat_age[sex == "Kvinder", sex := "female"]
dat_age[sex == "Mænd", sex := "male"]
dat_age[, outcome := tolower(outcome)]
dat_age[, variable := paste0("count_n_", variable)]
dat_age[, grouping := gsub("-", " - ", grouping)]
dat_age[, grouping := gsub(" år", "", grouping)]


dat_age <-
  merge(dat_age, outcome_desc[, .(hjertetal_code, name_dk)], by.x = "outcome", by.y = "name_dk")



data_diff_age <- function(old, new, variable_str, ht_code) {
  x_old <-
    old[variable == variable_str &
          hjertetal_code == ht_code &
          sex != "Total", .(sex, year, grouping, count_ht1, outcome)]
  
  keep_vars <- c("sex", "year", "grouping", variable_str)
  x_new <-
    new[[ht_code]]$age[, ..keep_vars]
  x <- merge(x_old, x_new, by = c("sex", "year", "grouping"))
  x[, diff_count := get(variable_str) - count_ht1]
  x[, diff_prop := diff_count / get(variable_str)]
  x[]
}
d1_incidence_age <-
  data_diff_age(dat_age,
                shiny_dat,
                variable_str = "count_n_incidence",
                ht_code = "d1")

d1_prev_age <-
  data_diff_age(dat_age,
                shiny_dat,
                variable_str = "count_n_prevalence",
                ht_code = "d1")

ggplot(d1_incidence_age) + geom_line(aes(
  x = grouping,
  y = diff_count,
  group = sex,
  color = sex
)) + facet_wrap("year") + theme_classic()

ggplot(d1_prev_age) + geom_line(aes(
  x = grouping,
  y = diff_count,
  group = sex,
  color = sex
)) + facet_wrap("year") + theme_classic()
