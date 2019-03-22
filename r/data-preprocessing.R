library(data.table)
library(magrittr)
export_opr <- fread("data/export_opr.txt", encoding = "UTF-8")
export_med <- fread("data/export_med.txt", encoding = "UTF-8")
export_diag <- fread("data/export_diag1.txt", encoding = "UTF-8")
export_diag2 <- fread("data/export_diag2.txt", encoding = "UTF-8")
export_diag <- rbind(export_diag, export_diag2)

preProccess <- function(export_dat) {
  dat <- split(export_dat, by = "outcome") %>%
    lapply(., split, by = "aggr_level")
  
  lapply(dat, function(outcome) {
    lapply(outcome, function(aggr_level) {
      aggr_level[, `:=` (outcome = NULL, aggr_level = NULL)]
    })
  })
}
dat_opr <- preProccess(export_opr)
dat_med <-  preProccess(export_med)
dat_diag <-  preProccess(export_diag)


## Add lists together (med, diag, opr) and save output as one massive list
shiny_list <-
  list(opr_dat = dat_opr,
       med_dat = dat_med,
       diag_dat = dat_diag)
shiny_dat <- c(dat_opr, dat_med, dat_diag)
saveRDS(shiny_list, file = "data/shiny_list.rds")
saveRDS(shiny_dat, file = "data/shiny_dat.rds")
