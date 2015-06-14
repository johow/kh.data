#' Array from event data
#'
#' This function returns an array for  event data in the form date, id, location, special, status. Here, gemini births count as one single event (see get_evmat_twins() for including the birth of twins as multiple events).
#' @param df_ev A dataframe in long format containing individual events
#' @param vars A list of variables for chronological date, ID related to event, location, special, status, and type of event)
#' @param events A list of labels used in \code{vars[["evtyp"]]} to indicate births, born children, and deaths
#' @param censored A logical indicator, whether censored dates should be included (Default is FALSE)
#' @keywords kh kin events
#' @export
#' @examples
#' \dontrun{
#' df_ind <- get_exmpl_df()
#' df_fam <- data.frame(idf = c(0,unique(df_ind$momid[df_ind$momid>0])), fall = "C")
#' evmat <- get_evmat(df_ind, df_fam)
#' }
get_evmat <- function(df_ev=NULL,
                      vars = list(evdat = "evdat",
                                  evid = "evid",
                                  evloc = "evloc",
                                  evspc = "evspc",
                                  status = "status",
                                  evtyp = "evtyp",
                                  id = "id"),
                      events = list("birth" = "*",
                                    "child" = "#",
                                    "death" = "+"),
                      events_exclude = list("marriage" = "oo"),
                      censored = TRUE){
  df_ev <- as.data.frame(df_ev)
  df_ev <- df_ev[, paste(unlist(vars))]
  stopifnot(!any(is.na(df_ev$evtyp)) & all(df_ev$evtyp %in% unlist(list(events, events_exclude))))
if (censored==FALSE){df_ev$status <- 1}
  df_ev <- df_ev[order(df_ev[,vars[[7]]], df_ev[,vars[[1]]]),]
  tmp_id <- unique(df_ev[,vars[[7]]])
  df_ev <- df_ev[df_ev[,vars[[6]]] %in% paste(unlist(events)),]
  df_ev[,vars[[6]]] <- factor(df_ev[,vars[[6]]])
  tmp_max <- max(table(df_ev[,vars[[7]]]))
  dim_n <- length(tmp_id)
  evmat <- array(
    as.numeric(rep(NA, (tmp_max*5*dim_n))),
    dim = c(dim_n, tmp_max, 5),
    dimnames=
      list(id = tmp_id,
           tt = c("*", paste(ifelse(c(1:(tmp_max-2))>9, paste0("#",c(1:(tmp_max-2))), paste0("#0",c(1:(tmp_max-2))))), "+"),
           vv = c("evdat", "evid", "evloc", "evspc", "status")))

  tmp_birthdates <- tapply(df_ev[df_ev[,vars[[6]]] %in% events[[2]],vars[[1]]],
                         df_ev[df_ev[,vars[[6]]] %in% events[[2]],vars[[7]]], list)



# df_ev$evloc <- unlist(paste(df_ev$evloc), kinlab::get_kirchspiele)
  par_list <- vector("list", length(names(df_ev)[which(!unlist(lapply(df_ev, is.numeric)))] ))
 names(par_list) <- names(df_ev)[which(!unlist(lapply(df_ev, is.numeric)))]
 par_list[[vars[["evdat"]]]] <- list("origin"="1970-01-01")
 par_list[[vars[["evloc"]]]] <- list("label" = c("CA", "CH", "CN", "CI", "EI", "FR", "GS", "GI", "GH", "GM", "HW", "HI", "JE",
                                                 "LA", "LV", "LP", "LO", "MA", "MW", "NE", "PE", "PI", "RY", "SU", "TW", "UP", "UT", "VI", "WE", "WI", "WZ", "WO", "WY", "x.out",
                                                 "x.unknown"),
                                     "value" = 1:35)

 par_list[[vars[["evtyp"]]]] <- list("label" = c("*", "#", "+"),
                                     "value" = 1:3)

 par_list[[vars[["evspc"]]]] <- list("label" = c("OHNE", "ERRECHNET", "TOTGEBURT", "ZEREMONIE", "X", "ZWILLING"),
                                     "value" = 1:6)

df_ev[,vars[["evdat"]]] <- as.numeric(as.Date(paste(df_ev[,vars[["evdat"]]])))
# df_ev[,vars[["evloc"]]] <- as.numeric(factor(df_ev[,vars[["evloc"]]], levels=c("CA", "CH", "CN", "CI", "EI", "FR", "GS", "GI", "GH", "GM", "HW", "HI", "JE",
#                                               "LA", "LV", "LP", "LO", "MA", "MW", "NE", "PE", "PI", "RY", "SU", "TW", "UP", "UT", "VI", "WE", "WI", "WZ", "WO", "WY", "x.out",
#                                               "x.unknown"), ordered=TRUE))

# df_ev[,vars[["evtyp"]]] <- as.numeric(factor(df_ev[,vars[["evtyp"]]], levels=c("*", "#", "+"), ordered=TRUE))


df_ev[,vars[["evspc"]]] <- as.numeric(factor(df_ev[,vars[["evspc"]]], levels=c("OHNE", "ERRECHNET", "TOTGEBURT", "ZEREMONIE", "X", "ZWILLING"), ordered=TRUE))




###
#
#
#
#
#
for (i in 1:5){
    tmp_children <- tapply(df_ev[df_ev[,vars[["evtyp"]]] %in% events[["child"]],i],
                           df_ev[df_ev[,vars[["evtyp"]]] %in% events[["child"]],vars[["id"]]], list)

    tmp_childbirths <- mapply("[", tmp_children,
                              lapply(lapply(lapply(tmp_birthdates, duplicated), "!"), which))

    tmp_children_n <- unlist(lapply(tmp_children, length))

    tmp_chilbirth_n <- unlist(lapply(tmp_childbirths, length))


  evmat[paste(df_ev[df_ev[,vars[[6]]] %in% events[["birth"]],vars[[7]]]),
        1,i] <- as.numeric(df_ev[df_ev[,vars[[6]]] %in% events[["birth"]],vars[[i]]])

  for (k in 1:(tmp_max-2)){
    evmat[names(which(tmp_chilbirth_n>=k)),1+k,i] <-
    as.numeric(unlist(lapply(tmp_childbirths[which(tmp_chilbirth_n>=k)], "[[", k)))
  }
  evmat[paste(df_ev[df_ev[,vars[[6]]] %in% events[["death"]],vars[[7]]]),
        tmp_max,i] <- as.numeric(df_ev[df_ev[,vars[[6]]] %in% events[["death"]],vars[[i]]])
    }

  tmp_twinbirths <-mapply("[", tmp_birthdates, lapply(lapply(tmp_birthdates, duplicated), which))
    tmp_twinbirths <- tmp_twinbirths[unlist(lapply(lapply(tmp_birthdates, duplicated), any))]


twin_ids<-  mapply("[", tapply(df_ev[df_ev[,vars[[6]]] %in% events[[2]],vars[[2]]],
                     df_ev[df_ev[,vars[[6]]] %in% events[[2]],vars[[7]]], list)[unlist(lapply(lapply(tmp_birthdates, duplicated), any))],
         mapply("%in%", tmp_birthdates[unlist(lapply(lapply(tmp_birthdates, duplicated), any))], tmp_twinbirths))

evmat[,,5] <- ifelse(is.na(evmat[,,1]), 0, 1)
evmat <- evmat[as.numeric(which(apply(evmat[,,5], 1, sum, na.rm=TRUE)>0)),,]
evmat[,1:(tmp_max-1),4] <- ifelse(evmat[,1:(tmp_max-1),2] %in% unlist(twin_ids)
, 6, evmat[,1:(tmp_max-1),4])

evmat[,tmp_max,1] <-  ifelse(is.na(evmat[,tmp_max,1]), as.numeric(apply(evmat[,,1], 1, max, na.rm=TRUE)), evmat[,tmp_max,1])
evmat[,tmp_max,1] <-  ifelse(as.numeric(apply(evmat[,,1], 1, max, na.rm=TRUE))>=evmat[,tmp_max,1], as.numeric(apply(evmat[,,1], 1, max, na.rm=TRUE))+0.5, evmat[,tmp_max,1])

  for (i in 1:tmp_max){
  evmat[,i,1] <-  ifelse(is.na(evmat[,i,1]), apply(evmat[,,1], 1, max, na.rm=TRUE), evmat[,i,1])
  }
  evmat[is.na(evmat)] <- 0
  for (i in (dim(evmat)[[2]]-1):1){
    evmat[,i,3] <- ifelse(evmat[,i,3]==0,   evmat[,i+1,3],   evmat[,i,3])
  }
  for (i in 2:dim(evmat)[[2]]){
    evmat[,i,3] <- ifelse(evmat[,i,3]==0,   evmat[,i-1,3],   evmat[,i,3])
  }
  return(list("evmat" = evmat, "param" = par_list))
}
