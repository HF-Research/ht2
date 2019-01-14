load("data/export_opr.Rdata")
load("data/export_med.Rdata")
opr_dat <- lapply(export_opr, function(outer) {
  lapply(outer, function(mid) {
    mid[order(year, -sex, grouping)]
    cols_to_round <- grep("rate", colnames(mid), value = TRUE)
    mid[, (cols_to_round) := lapply(.SD, round, digits = 1),
        .SDcols = cols_to_round]
  })
})

med_dat <- lapply(export_med, function(outer) {
  lapply(outer, function(mid) {
    mid[order(year, -sex, grouping)]
    cols_to_round <- grep("rate", colnames(mid), value = TRUE)
    mid[, (cols_to_round) := lapply(.SD, round, digits = 1),
        .SDcols = cols_to_round]
  })
})

## Add lists together (med, diag, opr) and save output as one massive list
shiny_list <- list(opr_dat = opr_dat, med_dat = med_dat)
shiny_dat <- c(opr_dat, med_dat)
save(shiny_list, file = "data/shiny_list.rda")
save(shiny_dat, file = "data/shiny_dat.rda")
