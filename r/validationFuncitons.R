# Function definitions
data_diff_national <- function(old, new, variable_str, ht_code) {

  old <-
    old[variable == variable_str &
          hjertetal_code == ht_code &
        sex != "Total", .(sex, year, count_ht1)]
  
  keep_vars <- c("sex", "year", variable_str)
  new <-
    new[[ht_code]]$national[, ..keep_vars]
  x <- merge(old, new, by = c("sex", "year"))
  x[, diff_count := get(variable_str) - count_ht1]
  x[,  diff_prop := diff_count / get(variable_str) * 100]
  x[]
}

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


plot_national <- function(dat, diff_var) {
  diff_var <- ensym(diff_var)
  
  out <- ggplot(dat) + geom_line(aes(
    x = year,
    y = !!diff_var,
    group = sex,
    color = sex
  ), size = 1.5) + 
    ylim(min(c(0, dat[[diff_var]])), max(dat[[diff_var]])) +
    theme_classic() +
    geom_hline(aes(yintercept = 0), linetype = 2 )
  out
}


plot_age_pop <- function(dat, diff_var) {
  diff_var <- ensym(diff_var)
  ggplot(dat) + geom_line(aes(
    x = age_group,
    y = !!diff_var,
    group = sex,
    color = sex
  ), size = 1.5) +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    facet_wrap("year") + theme_classic()
}


plot_age <- function(dat, diff_var) {
  diff_var <- ensym(diff_var)
  ggplot(dat) + geom_line(aes(
    x = grouping,
    y = !!diff_var,
    group = sex,
    color = sex
  ), size = 1.5) +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    facet_wrap("year") + theme_classic()
}
