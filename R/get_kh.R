#' Wrapper function to import and format Krummh\enc{ö}{oe}rn data for children and families from SPSS files
#'
#' This function returns a data.frame containing genealogical information of individuala
#' @param source_path File path to  the SPSS file.
#' @param source_files File names (ususally 'kindermc.sav' and 'familienmc.sav', as default)
#' @param Should family data be returned? Default is FALSE
#' @keywords kh import
#' @export
#' @examples
#' twinmat <- get_twinmat()
#' str(twinmat)
get_kh <- function(source_path=NULL,
                   source_files=NULL,
                   get_fam = FALSE){
  ###############################
  #
  #  DATA IMPORT AND PREPARATION
  #
  ###############################

  #
  # IMPORT DATA
  #

  kh <- list("")
  for (i in 1:length(source_files)){
    kh[[i]] <- haven::read_sav(file.path(source_path, source_files[[i]]))
    names(kh)[[i]] <- source_files[[i]]
  }

  for (j in 1:length(kh)){
    for (i in 1:ncol(kh[[j]])){
      if (is.character(kh[[j]][[i]])){
        kh[[j]][[i]] <-  gsub("\u00c4", "AE", paste(kh[[j]][[i]]))
        kh[[j]][[i]] <-  gsub("\u00d6", "OE", paste(kh[[j]][[i]]))
        kh[[j]][[i]] <-  gsub("\u00dc", "UE", paste(kh[[j]][[i]]))
        kh[[j]][[i]] <-  gsub("\u00e4", "AE", paste(kh[[j]][[i]]))
        kh[[j]][[i]] <-  gsub("\u00f6", "OE", paste(kh[[j]][[i]]))
        kh[[j]][[i]] <-  gsub("\u00fc", "UE", paste(kh[[j]][[i]]))
        kh[[j]][[i]] <-  gsub("\u00df", "SS", paste(kh[[j]][[i]]))
        #  levels(kh[[j]][[i]]) <- trim(levels(kh[[j]][[i]])) ..haven::read_sav
        kh[[j]][[i]] <- toupper(paste(kh[[j]][[i]]))
      }
    }
  }

  kh[[1]]$id <- 1:nrow(kh[[1]])
  df_x <- kh[[2]][,c("doc", names(kh[[2]])[which(grepl("ehe", names(kh[[2]])))])]
  for (i in c("M", "W")){
    for (j in 1:5){
      df_tmp    <- kh[[1]][kh[[1]]$sex == paste(i) &
                                 kh[[1]][,paste0("famnrk",j)] != "",
                                c("id", paste0("famnrk",j))]
      names(df_tmp) <- c(paste0("id", ifelse(i == "M", "m", "f"), j), "doc")
      df_x <- merge(df_x, df_tmp, by = "doc", all.x = TRUE)
      df_tmp <- NULL
    }
  }


  stopifnot(max(rowSums(apply(df_x[,which(grepl("idf", names(df_x)))],2, is.na)==FALSE))<=1)
  stopifnot(max(rowSums(apply(df_x[,which(grepl("idm", names(df_x)))],2, is.na)==FALSE))<=1)

  df_x$idm <- df_x$idm1
  df_x$idf <- df_x$idf1

  for (i in c("m", "f")){
    for (j in 2:5){
      df_x[,paste0("id", i)] <- ifelse(is.na(df_x[,paste0("id", i)]),
                                       df_x[,paste0("id", i, j)],
                                       ifelse(is.na(df_x[,paste0("id", i, j)]),
                                              df_x[,paste0("id", i)], -999))

    }
    if (any(  df_x[!is.na(df_x[,paste0("id", i)]),paste0("id", i)]<0)){
      message("ID conflict!")
      print(df_x[df_x[!is.na(df_x[,paste0("id", i)]),paste0("id", i)]<0,])
    }
    stopifnot(!any(  df_x[!is.na(df_x[,paste0("id", i)]),paste0("id", i)]<0))
  }

  stopifnot(all(kh[[1]]$famnrk1[kh[[1]]$sex=="M"] %in% df_x$doc[!is.na(df_x[,"idm"])] | kh[[1]]$famnrk1[kh[[1]]$sex=="M"] == ""))
  stopifnot(all(kh[[1]]$famnrk2[kh[[1]]$sex=="M"] %in% df_x$doc[!is.na(df_x[,"idm"])] | kh[[1]]$famnrk2[kh[[1]]$sex=="M"] == ""))
  stopifnot(all(kh[[1]]$famnrk3[kh[[1]]$sex=="M"] %in% df_x$doc[!is.na(df_x[,"idm"])] | kh[[1]]$famnrk3[kh[[1]]$sex=="M"] == ""))
  stopifnot(all(kh[[1]]$famnrk4[kh[[1]]$sex=="M"] %in% df_x$doc[!is.na(df_x[,"idm"])] | kh[[1]]$famnrk4[kh[[1]]$sex=="M"] == ""))
  stopifnot(all(kh[[1]]$famnrk5[kh[[1]]$sex=="M"] %in% df_x$doc[!is.na(df_x[,"idm"])] | kh[[1]]$famnrk5[kh[[1]]$sex=="M"] == ""))

  # ADD IDs OF 1ST-(EVER-)MARRIED HUSBANDS NOT LISTED AS CHILD
  df_x$idm[is.na(df_x$idm) & df_x$ehem %in% c("", "1")] <- c(1:length(df_x$idm[is.na(df_x$idm) & df_x$ehem %in% c("", "1")]))+nrow(kh[[1]])



  # summary(df_x$idm)
  # MATCH MULTIPLE MARRIAGES
  for (i in paste0("ehenrm", 1:4)){
    df_x<- merge(df_x, data.frame(doc = df_x[df_x[,paste(i)] != "" & !duplicated(df_x[,paste(i)]),paste(i)],
                                  idm_tmp = df_x[df_x[,paste(i)] != "" & !duplicated(df_x[,paste(i)]),"idm"]), by = "doc", all.x=TRUE)
    df_x$idm <- ifelse(is.na(df_x$idm), df_x$idm_tmp, df_x$idm)
    df_x$idm_tmp <- NULL
  }
  # summary(df_x$idm)

  if (any(which(df_x[is.na(df_x$idm), paste0("ehenrm", 1)] %in% df_x[!is.na(df_x$idm),"doc"]))){
    df_x[is.na(df_x$idm), "idm"] <-  df_x$idm[df_x$doc == paste(df_x[is.na(df_x$idm), paste0("ehenrm", 1)])[which(df_x[is.na(df_x$idm), paste0("ehenrm", 1)] %in% df_x[!is.na(df_x$idm),"doc"])]]
  }
  # summary(df_x$idm)

  if (any(which(df_x[is.na(df_x$idm), paste0("ehenrm", 2)] %in% df_x[!is.na(df_x$idm),"doc"]))){
    df_x[is.na(df_x$idm), "idm"] <-  df_x$idm[df_x$doc == paste(df_x[is.na(df_x$idm), paste0("ehenrm", 2)])[which(df_x[is.na(df_x$idm), paste0("ehenrm", 2)] %in% df_x[!is.na(df_x$idm),"doc"])]]
  }
  # summary(df_x$idm)

  if (any(which(df_x[is.na(df_x$idm), paste0("ehenrm", 3)] %in% df_x[!is.na(df_x$idm),"doc"]))){
    df_x[is.na(df_x$idm), "idm"] <-  df_x$idm[df_x$doc == paste(df_x[is.na(df_x$idm), paste0("ehenrm", 3)])[which(df_x[is.na(df_x$idm), paste0("ehenrm", 3)] %in% df_x[!is.na(df_x$idm),"doc"])]]
  }
  # summary(df_x$idm)

  if (any(which(df_x[is.na(df_x$idm), paste0("ehenrm", 4)] %in% df_x[!is.na(df_x$idm),"doc"]))){
    df_x[is.na(df_x$idm), "idm"] <-  df_x$idm[df_x$doc == paste(df_x[is.na(df_x$idm), paste0("ehenrm", 4)])[which(df_x[is.na(df_x$idm), paste0("ehenrm", 4)] %in% df_x[!is.na(df_x$idm),"doc"])]]
  }
  # summary(df_x$idm)
  stopifnot(!any(is.na(df_x$idm)))
  # summary(df_x$idf)
  # ADD IDs OF 1ST-(EVER-)MARRIED WIFES NOT LISTED AS CHILD
  df_x$idf[is.na(df_x$idf) & df_x$ehef %in% c("", "1")] <- c(1:length(df_x$idf[is.na(df_x$idf) & df_x$ehef %in% c("", "1")]))+max(df_x$idm)
  # summary(df_x$idf)
  # MATCH MULTIPLE MARRIAGES
  for (i in names(df_x)[which(grepl("ehenrf", names(df_x)))]){
    df_x<- merge(df_x, data.frame(doc = df_x[df_x[,paste(i)] != "" & !duplicated(df_x[,paste(i)]),paste(i)],
                                  idf_tmp = df_x[df_x[,paste(i)] != "" & !duplicated(df_x[,paste(i)]),"idf"]), by = "doc", all.x=TRUE)
    df_x$idf <- ifelse(is.na(df_x$idf), df_x$idf_tmp, df_x$idf)
    df_x$idf_tmp <- NULL
  }
  stopifnot(!any(is.na(df_x$idf)))
  # summary(df_x$idf)


  df_out <- merge(df_x[,c("doc", "idm", "idf")], kh[[2]], by = "doc")
  df_x2 <- merge(kh[[1]][,c("id", "doc")], df_x[,c("doc", "idm", "idf")], by ="doc", all.x = TRUE)
  df_x2 <-df_x2[,c("id", "idm", "idf")]
  # str(df_x2)
  stopifnot(!any(df_x2$idm %in% df_x2$idf))

  ord_x <- pedigree::orderPed(df_x2)
  df_x2 <- df_x2[order(ord_x),]
  # str(df_x2)


  df_y <- pedigree::add.Inds(df_x2)
  # str(df_y)
  df_y$id <- as.numeric(paste(df_y$id))


  # prepare data from "Kindermc.sav"
   df_k <- kh[[1]]
   df_k$bplace <- unlist(lapply(df_k$gebortk, kinlab::get_kirchspiele))
   df_k$bdate <- kinlab::mean_date(df_k, "gebk4", "gebk8")
   df_k$bstatus <- 0
      df_k$bstatus[!is.na(df_k$gebk4) & !is.na(df_k$gebk8)] <- ifelse(
        mapply("difftime",
               df_k[!is.na(df_k$gebk4) & !is.na(df_k$gebk8),"gebk4"],
               df_k[!is.na(df_k$gebk4) & !is.na(df_k$gebk8),"gebk8"], units="days")/365.25 <= 3, 1, 0)

      df_k$bspecial <- paste(ifelse(paste(df_k$gebkk) == "E", "ERRECHNET",
                              ifelse(paste(df_k$gebkk) == "G", "TOTGEBURT",
                                     ifelse(paste(df_k$gebkk) == "T", "ZEREMONIE", "OHNE"))))

      df_k$dplace <- unlist(lapply(df_k$gebortk, kinlab::get_kirchspiele))
      df_k$ddate <- kinlab::mean_date(df_k, "todk4", "todk8")
   #   df_k$dstatus <- 0
      df_k$dstatus[!is.na(df_k$todk4) & !is.na(df_k$todk8)] <- ifelse(
        mapply("difftime",
               df_k[!is.na(df_k$todk4) & !is.na(df_k$todk8),"todk4"],
               df_k[!is.na(df_k$todk4) & !is.na(df_k$todk8),"todk8"], units="days")/365.25 <= 1/12, 1, 0)

      df_k$dspecial <- paste(ifelse(paste(df_k$todkk) == "B", "ZEREMONIE",
                                    ifelse(paste(df_k$todkk) == "E", "ERRECHNET","OHNE")))


  df_ped <- merge(df_y, df_k[,c("id", "bdate", "bplace", "bspecial", "bstatus", "ddate", "dplace", "dspecial", "dstatus", "sex")], by ="id", all.x=TRUE)

  # prepare family data

  df_fam <- kh[[2]]

  # FATHER BIRTHS
   df_fam$bplace.dad <- unlist(lapply(df_fam$gebortm, kinlab::get_kirchspiele))
  df_fam$bdate.dad <- kinlab::mean_date(df_fam, "gebm4", "gebm8")
  df_fam$bstatus.dad <- 0
  df_fam$bstatus.dad[!is.na(df_fam$gebm4) & !is.na(df_fam$gebm8)] <- ifelse(
    mapply("difftime",
           df_fam[!is.na(df_fam$gebm4) & !is.na(df_fam$gebm8),"gebm4"],
           df_fam[!is.na(df_fam$gebm4) & !is.na(df_fam$gebm8),"gebm8"], units="days")/365.25 <= 3, 1, 0)

  df_fam$bspecial.dad <- paste(ifelse(paste(df_fam$gebmk) == "T", "ZEREMONIE",
                                ifelse(paste(df_fam$gebmk) == "E", "ERRECHNET","OHNE")))
  # FATHER DEATHS

  df_fam$dplace.dad <- unlist(lapply(df_fam$todortm, kinlab::get_kirchspiele))
  df_fam$ddate.dad <- kinlab::mean_date(df_fam, "todm4", "todm8")
  df_fam$dstatus.dad <- 0
  df_fam$dstatus.dad[!is.na(df_fam$todm4) & !is.na(df_fam$todm8)] <- ifelse(
    mapply("difftime",
           df_fam[!is.na(df_fam$todm4) & !is.na(df_fam$todm8),"todm4"],
           df_fam[!is.na(df_fam$todm4) & !is.na(df_fam$todm8),"todm8"], units="days")/365.25 <= 1/12, 1, 0)

  df_fam$dspecial.dad <- paste(ifelse(paste(df_fam$todmk) == "B", "ZEREMONIE",
                                   ifelse(paste(df_fam$todmk) == "E", "ERRECHNET","OHNE")))

  # MOTHER BIRTHS
  df_fam$bplace.mom <- unlist(lapply(df_fam$gebortm, kinlab::get_kirchspiele))
  df_fam$bdate.mom <- kinlab::mean_date(df_fam, "gebf4", "gebf8")
  df_fam$bstatus.mom <- 0
  df_fam$bstatus.mom[!is.na(df_fam$gebf4) & !is.na(df_fam$gebf8)] <- ifelse(
    mapply("difftime",
           df_fam[!is.na(df_fam$gebf4) & !is.na(df_fam$gebf8),"gebf4"],
           df_fam[!is.na(df_fam$gebf4) & !is.na(df_fam$gebf8),"gebf8"], units="days")/365.25 <= 3, 1, 0)

  df_fam$bspecial.mom <- paste(ifelse(paste(df_fam$gebfk) == "T", "ZEREMONIE",
                                   ifelse(paste(df_fam$gebfk) == "E", "ERRECHNET","OHNE")))
  # MOTHER DEATHS

  df_fam$dplace.mom <- unlist(lapply(df_fam$todortf, kinlab::get_kirchspiele))
  df_fam$ddate.mom <- kinlab::mean_date(df_fam, "todf4", "todf8")
  df_fam$dstatus.mom <- 0
  df_fam$dstatus.mom[!is.na(df_fam$todf4) & !is.na(df_fam$todf8)] <- ifelse(
    mapply("difftime",
           df_fam[!is.na(df_fam$todf4) & !is.na(df_fam$todf8),"todf4"],
           df_fam[!is.na(df_fam$todf4) & !is.na(df_fam$todf8),"todf8"], units="days")/365.25 <= 1/12, 1, 0)

  df_fam$dspecial.mom <- paste(ifelse(paste(df_fam$todfk) == "B", "ZEREMONIE",
                                   ifelse(paste(df_fam$todfk) == "E", "ERRECHNET","OHNE")))


  df_fam <- merge(df_fam, df_x[,c("doc", "idm", "idf")], by = "doc", all.x=TRUE)


  df_fam$id.dad <- kinlab::na2zero(df_fam$idm)
 df_fam$id.mom <- kinlab::na2zero(df_fam$idf)
   for (i in c("b", "d")){
     for (j in c("dad", "mom")){
      df_ped <- merge(df_ped, cbind(id = df_fam[,paste("id", j, sep=".")][!duplicated(df_fam[,paste("id", j, sep=".")])],
                                     df_fam[!duplicated(df_fam[,paste("id", j, sep=".")]), paste0(i, c("date", "place", "special", "status"), ".", j)]), by = "id", all.x=TRUE)
     }
   }


 df_ped$sex <- ifelse(df_ped$id %in% df_fam$id.mom, "W", ifelse(df_ped$id %in% df_fam$id.dad, "M", df_ped$sex))

summary(df_ped$bdate)
df_ped$bdate  <- ifelse(is.na(df_ped$bdate),
                        ifelse(df_ped$sex=="W",
                               df_ped$bdate.mom,
                               df_ped$bdate.dad), df_ped$bdate)

df_ped$bplace  <- ifelse(is.na(df_ped$bplace),
                        ifelse(df_ped$sex=="W",
                               df_ped$bplace.mom,
                               df_ped$bplace.dad), df_ped$bplace)


df_ped$bstatus  <- ifelse(is.na(df_ped$bstatus),
                        ifelse(df_ped$sex=="W",
                               df_ped$bstatus.mom,
                               df_ped$bstatus.dad), df_ped$bstatus)


df_ped$bspecial  <- ifelse(is.na(df_ped$bspecial),
                        ifelse(df_ped$sex=="W",
                               df_ped$bspecial.mom,
                               df_ped$bspecial.dad), df_ped$bspecial)

df_ped$ddate  <- ifelse(is.na(df_ped$ddate),
                        ifelse(df_ped$sex=="W",
                               df_ped$ddate.mom,
                               df_ped$ddate.dad), df_ped$ddate)

df_ped$dplace  <- ifelse(is.na(df_ped$dplace),
                         ifelse(df_ped$sex=="W",
                                df_ped$dplace.mom,
                                df_ped$dplace.dad), df_ped$dplace)


df_ped$dstatus  <- ifelse(is.na(df_ped$dstatus),
                          ifelse(df_ped$sex=="W",
                                 df_ped$dstatus.mom,
                                 df_ped$dstatus.dad), df_ped$dstatus)


df_ped$dspecial  <- ifelse(is.na(df_ped$dspecial),
                           ifelse(df_ped$sex=="W",
                                  df_ped$dspecial.mom,
                                  df_ped$dspecial.dad), df_ped$dspecial)

df_ped$momid <- kinlab::na2zero(df_ped$idf)
df_ped$dadid <- kinlab::na2zero(df_ped$idm)
df_ped$sex <- ifelse(df_ped$sex=="W", 2,ifelse(df_ped$sex=="M", 1,3))
df_ped$bdate <- as.Date(df_ped$bdate, origin="1970-01-01")
df_ped$ddate <- as.Date(df_ped$ddate, origin="1970-01-01")

if (get_fam==FALSE){
df_out <- df_ped[,c("id", "dadid", "momid", "sex", "bdate", "bplace", "bspecial", "bstatus", "ddate", "dplace", "dspecial", "dstatus")]
}
return(df_out)
}
