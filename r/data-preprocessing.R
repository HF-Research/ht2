load("data/export_opr.Rdata")

opr_dat <- lapply(export_opr, function(outer){
  lapply(outer, function(mid){
    mid[order(year, -sex, grouping)]
  })
})
  
med_dat <- list(a = "a")

## Add lists together (med, diag, opr) and save output as one massive list
shiny_list <- list(opr_dat = opr_dat, med_dat = med_dat)
shiny_dat <- c(opr_dat, med_dat)
save(shiny_list, file = "data/shiny_list.rda")
save(shiny_dat, file = "data/shiny_dat.rda")