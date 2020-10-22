##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param is_kom
##' @param is_region
##' @param dk_sp
map_obj <-
  function(is_kom = isKom(),
           is_region = isRegion(),
           dk_sp = dk_sp) {
    if (is_kom) {
      return(dk_sp$l2)
    }
    return(dk_sp$l1)
  }
