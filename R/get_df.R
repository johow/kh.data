#' Prepare data for pedigrees
#'
#' This function returns a dataframe being required to contruct pedigrees via the _kinship2::pedigree()_ function
#' @param df_ind A dataframe with individual cases
#' @param df_fam A dataframe with family cases
#' @param silent Should messages be suppressed (Default is FALSE)
#' @param get_fam If TRUE, the dataframe 'df_fam with additional columns 'idm' and 'idf' is returned instead (default is FALSE)
#' @param replace_sex Should entries be corrected for individuals appearing as mothers or fathers? (default is TRUE)
#' @keywords kh import
#' @export
#' @examples
#' \dontrun{
#' kh_df <- get_df(df_ind, df_fam, get_fam=FALSE)
#' }

get_df <- function(df_ind=NULL, df_fam=NULL, silent = TRUE, get_fam=FALSE, replace_sex = TRUE){
  if(!"id" %in% names(df_ind)){df_ind$id <- 1:nrow(df_ind)}
  df_ind$bdate <- kinlab::mean_date(df_ind, "gebk4", "gebk8")
  df_ind$ddate <- kinlab::mean_date(df_ind, "todk4", "todk8")
  df_ind$bplace <- df_ind$gebortk
  df_ind$dplace <- df_ind$todortk
  df_ind$bstatus <- paste(df_ind$gebkk)
  df_ind$dstatus <- paste(df_ind$todkk)
  df_ind$sex2 <- ifelse(df_ind$sex=="U",  "W", ifelse(df_ind$sex=="W", "W", "M"))

  df_fam$bdatef <- kinlab::mean_date(df_fam, "gebf4", "gebf8")
  df_fam$bdatem <- kinlab::mean_date(df_fam, "gebm4", "gebf8")
  df_fam$ddatef <- kinlab::mean_date(df_fam, "todf4", "todf8")
  df_fam$ddatem <- kinlab::mean_date(df_fam, "todm4", "todm8")
  df_fam$bplacem <- df_fam$gebortm
  df_fam$dplacem <- df_fam$todortm
  df_fam$bplacef <- df_fam$gebortf
  df_fam$dplacef <- df_fam$todortf
  df_fam$bstatusm <- paste(df_fam$gebmk)
  df_fam$dstatusm <- paste(df_fam$todmk)
  df_fam$bstatusf <- paste(df_fam$gebfk)
  df_fam$dstatusf <- paste(df_fam$todfk)
  df_fam$bstatusm <- paste(df_fam$gebmk)
  df_fam$dstatusm <- paste(df_fam$todmk)
  tmp_fam <- df_fam[,c("doc", "eltnrf", "eltnrm", "ehef", "ehem", names(df_fam)[grepl("ehenrf", names(df_fam)) & !grepl("p", names(df_fam))],
                       names(df_fam)[grepl("ehenrm", names(df_fam)) & !grepl("p", names(df_fam))],
                       "bdatef", "bplacef", "bstatusf", "bdatem", "bplacem", "bstatusm",
                       "ddatef", "dplacef", "dstatusf", "ddatem", "dplacem", "dstatusm")]

  for (sex2 in c("M", "W")){
    for (i in 1:5){
      tmp_ind <- df_ind[df_ind$sex2==paste(sex2),c("id", names(df_ind)[grepl("famnrk", names(df_ind)) & !grepl("p", names(df_ind))])]
      names(tmp_ind)[names(tmp_ind) %in% paste0("famnrk", i)] <- "doc"
      names(tmp_ind)[names(tmp_ind) %in% "id"] <- paste0("id",  tolower(sex2), i)
      tmp_fam <- merge(tmp_fam, tmp_ind[,c("doc", paste0("id", tolower(sex2), i))], by = "doc", all.x=TRUE)
    }
  }

  # check for conflicts:
  if (any(apply(apply(tmp_fam[,grepl("idm", names(tmp_fam))], 2, is.na), 1, sum)<3)){
    if (silent==FALSE){ message("found conflict in family ", tmp_fam$doc[which(apply(apply(tmp_fam[,grepl("idm", names(tmp_fam))], 2, is.na), 1, sum)<3)])}
  }
  if (any(apply(apply(tmp_fam[,grepl("idw", names(tmp_fam))], 2, is.na), 1, sum)<3)){
    if (silent==FALSE){  message("found conflict in family ", tmp_fam$doc[which(apply(apply(tmp_fam[,grepl("idw", names(tmp_fam))], 2, is.na), 1, sum)<3)])}
  }

  tmp_fam$idm <- NA
  for (i in paste0("idm", 1:5)){
    if(silent==FALSE & any(!is.na(tmp_fam$idm) & !is.na(tmp_fam[,paste(i)]))){message("Warning! Check possible conflicts:")
                                                                              print(tmp_fam[(!is.na(tmp_fam$idm) & !is.na(tmp_fam[,paste(i)])),])} else
                                                                                tmp_fam$idm <- ifelse(is.na(tmp_fam$idm), tmp_fam[,paste(i)], tmp_fam$idm)
  }

  tmp_fam$idw <- NA
  for (i in paste0("idw", 1:5)){
    if(silent==FALSE & any(!is.na(tmp_fam$idw) & !is.na(tmp_fam[,paste(i)]))){message("Warning! Check possible conflicts:")
                                                                              print(tmp_fam[(!is.na(tmp_fam$idw) & !is.na(tmp_fam[,paste(i)])),])} else
                                                                                tmp_fam$idw <- ifelse(is.na(tmp_fam$idw), tmp_fam[,paste(i)], tmp_fam$idw)
  }

  tmp_fam$erstehe_m <- ifelse(tmp_fam$ehem %in%  c("", "1"), 1, 0)
  tmp_fam$erstehe_f <- ifelse(tmp_fam$ehef %in%  c("", "1"), 1, 0)
  tmp_fam$idm[is.na(tmp_fam$idm) & tmp_fam$erstehe_m==1] <- max(df_ind$id) + 1:length(tmp_fam$idm[is.na(tmp_fam$idm) & tmp_fam$erstehe_m==1])
  tmp_fam$idw[is.na(tmp_fam$idw) & tmp_fam$erstehe_f==1] <- max(tmp_fam$idm, na.rm=TRUE) + 1:length(tmp_fam$idw[is.na(tmp_fam$idw) & tmp_fam$erstehe_f==1])


  for (i in which(is.na(tmp_fam$idw))){
    tmp_fam$idw[i] <- na.omit(unique(tmp_fam$idw[
      tmp_fam$ehenrf1 %in% tmp_fam$doc[i] |
        tmp_fam$ehenrf2 %in%   tmp_fam$doc[i] |
        tmp_fam$ehenrf3 %in% tmp_fam$doc[i] |
        tmp_fam$ehenrf4 %in% tmp_fam$doc[i]]))
  }



  for (i in which(is.na(tmp_fam$idm))){
    tmp_fam$idm[i] <- na.omit(unique(tmp_fam$idm[
      tmp_fam$ehenrm1 %in% tmp_fam$doc[i] |
        tmp_fam$ehenrm2 %in%   tmp_fam$doc[i] |
        tmp_fam$ehenrm3 %in% tmp_fam$doc[i] |
        tmp_fam$ehenrm4 %in% tmp_fam$doc[i]]))
  }

  df_ind$sex <- ifelse(df_ind$sex=="U", 3, ifelse(df_ind$sex=="W", 2, 1))

  df_ind$idm <- ifelse(df_ind$sex==1, df_ind$id, 0)
  df_ind$idw <- ifelse(df_ind$sex==2, df_ind$id, 0)
  df_ind <- merge(df_ind[,c("id","doc", "sex", "idw", "idm", "bdate", "bplace", "bstatus", "ddate", "dplace", "dstatus")],
                  tmp_fam[!duplicated(tmp_fam$idm),c("idm", "bdatem", "bplacem", "bstatusm", "ddatem", "dplacem", "dstatusm")], by = "idm", all.x=TRUE)
  df_ind <- merge(df_ind,
                  tmp_fam[!duplicated(tmp_fam$idw),c("idw", "bdatef", "bplacef", "bstatusf", "ddatef", "dplacef", "dstatusf")], by = "idw", all.x=TRUE)

  df_ind$bdate[is.na(df_ind$bdate)] <- as.Date(ifelse(
    df_ind$sex[is.na(df_ind$bdate)]==2,
    df_ind$bdatef[is.na(df_ind$bdate)],
    df_ind$bdatem[is.na(df_ind$bdate)]), origin="1970-01-01")

  df_ind$bplace[is.na(df_ind$bplace)] <- ifelse(
    df_ind$sex[is.na(df_ind$bplace)]==2,
    df_ind$bplacef[is.na(df_ind$bplace)],
    df_ind$bplacem[is.na(df_ind$bplace)])


  df_ind$bstatus[is.na(df_ind$bstatus)] <- ifelse(
    df_ind$sex[is.na(df_ind$bstatus)]==2,
    df_ind$bstatusf[is.na(df_ind$bstatus)],
    df_ind$bstatusm[is.na(df_ind$bstatus)])

  df_ind$ddate[is.na(df_ind$ddate)] <- as.Date(ifelse(
    df_ind$sex[is.na(df_ind$ddate)]==2,
    df_ind$ddatef[is.na(df_ind$ddate)],
    df_ind$ddatem[is.na(df_ind$ddate)]), origin="1970-01-01")


  df_ind$dplace[is.na(df_ind$dplace)] <- ifelse(
    df_ind$sex[is.na(df_ind$dplace)]==2,
    df_ind$dplacef[is.na(df_ind$dplace)],
    df_ind$dplacem[is.na(df_ind$dplace)])


  df_ind$dstatus[is.na(df_ind$dstatus)] <- ifelse(
    df_ind$sex[is.na(df_ind$dstatus)]==2,
    df_ind$dstatusf[is.na(df_ind$dstatus)],
    df_ind$dstatusm[is.na(df_ind$dstatus)])

  df_ind$bplace <- unlist(lapply(df_ind$bplace, kinlab::get_kirchspiele))
  df_ind$dplace <- unlist(lapply(df_ind$dplace, kinlab::get_kirchspiele))

  tmp_fam$bplacef <- unlist(lapply(tmp_fam$bplacef, kinlab::get_kirchspiele))
  tmp_fam$dplacef <- unlist(lapply(tmp_fam$dplacef, kinlab::get_kirchspiele))
  tmp_fam$bplacem <- unlist(lapply(tmp_fam$bplacem, kinlab::get_kirchspiele))
  tmp_fam$dplacem <- unlist(lapply(tmp_fam$dplacem, kinlab::get_kirchspiele))

  tmp_fam$momid <- tmp_fam$idw
  tmp_fam$dadid <- tmp_fam$idm
  tmp_fam$idf <- kinlab::zero2na(tmp_fam$momid)
  tmp_fam$idm <- kinlab::zero2na(tmp_fam$dadid)

if (get_fam==TRUE){
     return(merge(df_fam,  tmp_fam[,c("doc", "idf", "idm")], by ="doc", all.x=TRUE))
   }
if (get_fam==FALSE){

     df_out <-  merge(df_ind[,c("id", "bdate", "bplace", "bstatus", "ddate", "dplace", "dstatus",  "sex", "doc")], tmp_fam[,c("doc", "dadid", "momid")], by ="doc")[,-1]

     df_out <-  rbind(df_out, data.frame(id =
                                           unique(tmp_fam$idm[!tmp_fam$idm %in% df_ind$id]),
                                         bdate = as.Date(tmp_fam$bdatem[!tmp_fam$idm %in% df_ind$id & !duplicated(tmp_fam$idm)],origin="1970-01-01"),
                                         bplace = tmp_fam$bplacem[!tmp_fam$idm %in% df_ind$id & !duplicated(tmp_fam$idm)] ,
                                         bstatus =  tmp_fam$bstatusm[!tmp_fam$idm %in% df_ind$id & !duplicated(tmp_fam$idm)],
                                         ddate =  as.Date(tmp_fam$ddatem[!tmp_fam$idm %in% df_ind$id & !duplicated(tmp_fam$idm)],origin="1970-01-01"),
                                         dplace = tmp_fam$dplacem[!tmp_fam$idm %in% df_ind$id & !duplicated(tmp_fam$idm)] ,
                                         dstatus =  tmp_fam$dstatusm[!tmp_fam$idm %in% df_ind$id & !duplicated(tmp_fam$idm)],
                                         sex =1,
                                         dadid =0,
                                         momid =0))

     df_out <-  rbind(df_out, data.frame(id =
                                           unique(tmp_fam$idw[!tmp_fam$idw %in% df_ind$id]),
                                         bdate = as.Date(tmp_fam$bdatef[!tmp_fam$idw %in% df_ind$id & !duplicated(tmp_fam$idw)],origin="1970-01-01"),
                                         bplace = tmp_fam$bplacef[!tmp_fam$idw %in% df_ind$id & !duplicated(tmp_fam$idw)] ,
                                         bstatus =  tmp_fam$bstatusf[!tmp_fam$idw %in% df_ind$id & !duplicated(tmp_fam$idw)],
                                         ddate = as.Date(tmp_fam$ddatef[!tmp_fam$idw %in% df_ind$id & !duplicated(tmp_fam$idw)],origin="1970-01-01"),
                                         dplace = tmp_fam$dplacef[!tmp_fam$idw %in% df_ind$id & !duplicated(tmp_fam$idw)] ,
                                         dstatus =  tmp_fam$dstatusf[!tmp_fam$idw %in% df_ind$id & !duplicated(tmp_fam$idw)],
                                         sex =2,
                                         dadid =0,
                                         momid =0))
     if (replace_sex == TRUE){
     df_out$sex <- ifelse(df_out$id %in% df_out$momid, 2,  ifelse(df_out$id %in% df_out$dadid, 1, df_out$sex))
     }

     df_out$status <- 1
     df_out$affected <- 1
     df_out$bspecial <- paste0(df_out$bstatus)
     df_out$dspecial <- paste0(df_out$dstatus)

     return(df_out[order(df_out$id),c("id", "dadid", "momid", "sex",  "bdate", "bplace","bspecial", "bstatus",  "ddate", "dplace", "dspecial", "dstatus", "status", "affected")])
   }
}
