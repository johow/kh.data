#' Recode locality labels from character strings to numeric (and vice versa)
#'
#' Depending on chosen input format, this function return a vector containing the lavbels,
#' either numbers ord characters, each consistent
#' @param loc Locality label
#' @param evloc_list A Locality label-number list
#' @keywords kh localitiy
#' @export
#' @examples
#' recode_evloc(1:10)
#' recode_evloc("WY")
recode_evloc <- function(loc=NULL, evloc_list=list(
  "name" = c("CA", "CH", "CN", "CI", "EI", "FR", "GS", "GI", "GH", "GM", "HW", "HI", "JE", "LA", "LV", "LP", "LO", "MA", "MW", "NE", "PE", "PI", "RY", "SU", "TW", "UP", "UT", "VI", "WE", "WI", "WZ", "WO", "WY", "x.out", "x.unknown"),
  "label" = 1:35)){
  if (all(is.numeric(loc))){
  return(unlist(lapply(loc, recode_evloc_int, evloc_list)))
  } else  return(unlist(lapply(paste(loc), recode_evloc_int, evloc_list)))
}


recode_evloc_int <- function(x=NULL, evloc_list=NULL){
  if (is.numeric(x)){
    return(paste(evloc_list[["name"]][ifelse(x<34,x,"x.unknown")]))
  } else
    return(ifelse(paste(x) %in% paste(evloc_list[["name"]]),
      which(paste(evloc_list[["name"]]) == paste(x)), 34))
}
