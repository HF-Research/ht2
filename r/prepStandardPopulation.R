
# Prep european standard population
esp <- fread("data/esp2013.csv")

labs <-
  c("35 - 44", "45 - 54", "55 - 64", "65 - 74", "75 - 84", "85+")
labs_age_5yr <-
  c(
    "35 - 39",
    "40 - 44",
    "45 - 49",
    "50 - 54",
    "55 - 59",
    "60 - 64",
    "65 - 69",
    "70 - 74",
    "75 - 79",
    "80 - 84",
    "85+"
  )

# Group into poplulation groups needed for our analysis
std_pop <- esp[,
               sum(esp2013),
               by = .(sex,
                      cut(
                        esp$age_group,
                        breaks = c(34, 44, 54, 64, 74, 84, 105),
                        include.lowest = TRUE,
                        labels = labs
                      ))]
colnames(std_pop) <- c("sex", "age_group", "std_pop")


# For 5yr pop, aggregate those above 85 into one age group
std_pop5yr <- esp[age_group >= 35 ,] %>%
  .[age_group >= 85, esp2013 := sum(esp2013), by = sex] %>%
  .[age_group <= 85,] %>%
  .[, age := labs_age_5yr] %>%
  .[, age_group := NULL] %>%
  set_colnames(c("sex","std_pop","age_group"))
setcolorder(std_pop5yr, c("sex","age_group","std_pop"))
  

# Find weights
std_pop <- std_pop[!is.na(age_group)]
std_pop[, tot_pop_sex := sum(std_pop), by = .(sex)]
std_pop[, weight := std_pop / tot_pop_sex]
std_pop[, tot_pop_sex := NULL]

std_pop5yr <- std_pop5yr[!is.na(age_group)]
std_pop5yr[, tot_pop_sex := sum(std_pop), by = .(sex)]
std_pop5yr[, weight := std_pop / tot_pop_sex]
std_pop5yr[, tot_pop_sex := NULL]


rm(esp, labs)

