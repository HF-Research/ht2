library(data.table)
load("data/export_opr.Rdata")
load("data/export_med.Rdata")
load("data/export_diag.Rdata")


preProccess <- function(dat) {
  x <- copy(dat)
  lapply(x, function(outer) {
    lapply(outer, function(mid) {
      mid[order(year, -sex, grouping)]
      cols_to_round <- grep("rate", colnames(mid), value = TRUE)
      mid[, (cols_to_round) := lapply(.SD, round, digits = 1),
          .SDcols = cols_to_round]
    })
  })
}
opr_dat <- preProccess(export_opr)
med_dat <-  preProccess(export_med)
diag_dat <-  preProccess(export_diag)


## Add lists together (med, diag, opr) and save output as one massive list
shiny_list <-
  list(opr_dat = opr_dat,
       med_dat = med_dat,
       diag_dat = diag_dat)
shiny_dat <- c(opr_dat, med_dat, diag_dat)
save(shiny_list, file = "data/shiny_list.rda")
save(shiny_dat, file = "data/shiny_dat.rda")