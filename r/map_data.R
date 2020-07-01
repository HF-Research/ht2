##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dat
##' @param data_var
##' @param pretty_aggr_level
##' @param map_obj
##' @param ui_sex
map_data <- function(dat = dat,
                     data_var = prettyVariable()[2],
                     pretty_aggr_level = prettyAggr_level(),
                     map_obj = mapObj(),
                     ui_sex = ui_sex) {
  
  keep_vars <- c("id", pretty_aggr_level, data_var)
  # Set Zero values to NA - 0s mean <4 observations, so we don't know the actual
  # value
  dat[get(data_var) == 0, (data_var) := NA]
  
  # MALES
  out_m <- map_obj
  tmp_m <-
    dat[get(ui_sex) == "male" &
          get(pretty_aggr_level) != "Unknown"]
  
  out_m@data <-
    merge(tmp_m,
          out_m@data,
          by.x = pretty_aggr_level,
          by.y = "name_kom",
          all.y = TRUE)
  
  # Remove unneed vars and re-order data
  out_m@data <- out_m@data[, ..keep_vars]
  setorder(out_m@data, id)
  
  # Female
  out_f <- map_obj
  tmp_f <-
    dat[get(ui_sex) == "female" &
          get(pretty_aggr_level) != "Unknown"]
  
  out_f@data <-
    merge(tmp_f,
          out_f@data,
          by.x = pretty_aggr_level,
          by.y = "name_kom",
          all.y = TRUE)
  
  # Remove un-needed vars and re-order data
  out_f@data <- out_f@data[, ..keep_vars]
  setorder(out_f@data, id)
  
  # Combine for output
  list(male = out_m,
       female = out_f)
  
}
