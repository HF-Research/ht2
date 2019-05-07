# Data validation - comparing with existing HT and with StatistikBanken (for
# background population)
library(data.table)
pop_reference <- fread("data/pop_age_10yr_dst.csv")
pop_ht <- fread("data/pop_summary_age.txt")


pop <- merge(
  pop_reference,
  pop_ht,
  by.x = c("sex", "age_group", "year"),
  by.y = c("sex", "grouping", "year"),suffixes = c(".ref", ".ht")
)

# Find difference between StatistikBanken population and my population.
pop[, count_diff := count.ht - count.ref]
pop[, diff_proportion := count_diff / count.ref * 100]
# Not huge differences in population numbers
pop[, range(count_diff)]
pop[, range(diff_proportion)]

pop[pop[, which.max(abs(count_diff))]]
head(pop[sex == "female"], 75)
head(pop[sex == "male"], 75)
hist(pop[, count_diff])
hist(pop[, diff_proportion])
