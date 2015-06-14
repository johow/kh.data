#' Built Krummhörn data files
#'
#' This function prepares SPSS data provided by the 'Krummhörn Family Reconstitution'
#' carried out by Voland et al. (e.g. Voland 2015) for being used with 'kinlab'.
#' @param source_path Character string for the file path from which SPSS source(s) (e.g. `file1`) will be read.
#' @param local_path Character string for a file path to which 'kinlab' writes produced `.RData` files.
#' @param file1 Character string for the mame of the first source file in SPSS format  (i.e. `.sav`).
#' @param file2 An optional character string for the mame of the second source file in SPSS format  (i.e. `.sav`).
#' @param silent If TRUE, a final message giving the runtime of this function is omitted. (Default is FALSE)
#' @param n_sample Number of random draws, if n_sample < number of observations. (Default is 100)
#' @param force_new Should existing files be replaced? Default is FALSE
#' @param keep_files List specific files being kept in 'local_path' in case of `force_new=TRUE`.
#' @param set_seed Optional random seed being to allow reproducibility? Default is null, no seed is set.
#' @keywords pedigree
#' @export
#' @examples
#' \dontrun{
#' grap_ped(1067, df_ped)
#' }

built_kh <- function(source_path = NULL, local_path = getwd(),
                     file1 = "kindermc.sav",
                     file2 = "familienmc.sav",
                     silent = FALSE,
                     n_sample = 10,
                     force_new=FALSE,
                     keep_files = NULL,
                     set_seed = NULL){
  if (!is.null(set_seed)){set.seed(set_seed)}
t_start <- Sys.time()
if(force_new==FALSE){my_files <- list.files(local_path)} else {my_files <- paste(keep_files)}
message("## t_start: ", format(t_start, "%X"))
message("## source_path: \"", source_path, "\"")
message("## local_path: \"", local_path, "\"")
if(length(my_files)>0){
  for (i in my_files){
    message("## left unchanged: ", i)
  }
} else  message("## local_path empty.")
if (!all(c("kh_evn.RData", "kh_geo.RData" ,
       "kh_ind.RData", "kh_kin.RData" ,
       "kh_mat.RData", "kh_par.RData", "kh_ped.RData",
       "kh_src.RData", "kh_twn.RData") %in% my_files)){
###########################################################
#
#     'kh_src' IS A LIST OF SOURCE DATA:
#           [[1]] "kh_src1" 'KINDERMC.SAV', SEE DATA MANUAL
#           [[2]] "kh_src2"  'FAMILIENMC.SAV', SEE DATA MANUAL
#
#     'id' IS A UNIQUE IDENTIFIER FOR INDIVIDUALS IN [[1]]
#
###########################################################

if (!"kh_src.RData" %in% my_files){
    kh_src1 <- haven::read_sav(
      paste(source_path, file1, sep="/")
      )
#    knitr::kable(head(kh_src1[,1:9], 4))
    kh_src2 <- haven::read_sav(
      paste(source_path, file2, sep="/")
      )
#    knitr::kable(head(kh_src2[,1:9], 4))
    kh_src1$id <- 1:nrow(kh_src1)
    kh_src2 <- get_df(kh_src1, kh_src2, get_fam=TRUE)
    kh_src <- list(kh_src1, kh_src2)
#     message("     ", format(Sys.time(), "%X"), " | Now writing \"", local_path, "/kh_src.RData\"")
#     save(kh_src,
#          file = paste(local_path, "kh_src.RData", sep = "/")
#          )
} else load(paste(local_path, "kh_src.RData", sep = "/"))

kh_src1 <- kh_src[[1]]
kh_src2 <- kh_src[[2]]
###########################################################
#
#     'kh_ind' IS A DATAFRAME FOR INDIVIDUAL PEDIGREE DATA
#           [,1] "id" -- UNIQUE IDENTIFIER FOR INDIVIDUALS
#           [,2] "dadid" -- ID OF FATHER (0, IF FOUNDER)
#           [,2] "momid" -- ID OF MOTHER (0, IF FOUNDER)
#
###########################################################

if (!"kh_ind.RData" %in% my_files){
  kh_ind <- get_df(kh_src1, kh_src2)
  # ASSIGN INDIVIDUAL PEDIGREE MEMBERSHIP:
  kh_ind$famid <- with(kh_ind,
                       kinship2::makefamid(id, dadid, momid)
                       )
  table(kh_ind$famid)[1:10]
  table(kh_ind$famid==1)
  round(table(kh_ind$famid==1)/sum(table(kh_ind$famid==1)),2)
  # EXCLUDE INDIVIDUALS OUTSIDE THE LARGEST PEDIGREE
  kh_ind <- kh_ind[kh_ind$famid==1,]
#   message("     ", format(Sys.time(), "%X"), " | Now writing \"", local_path, "/kh_ind.RData\"")
#   save(kh_ind,
#        file = paste(local_path, "kh_ind.RData", sep = "/")
#   )
  } else load(paste(local_path, "kh_ind.RData", sep = "/"))

###########################################################
#
#     'kh_twn' IS A MATRIX WITH THREE COLUMNS:
#         [,1] 'id1' ID OF INDIVIDUAL 1
#         [,2] 'id2' ID OF INDIVIDUAL 2
#         [,3] 'code' RELATION CODE:
#               1=MONOZYGOTIC, 2=DIZYGOTIC, 3=UNKNOWN
#
###########################################################

if (!"kh_twn.RData" %in% my_files){
  kh_twn <- get_twinmat(kh_ind,
                              bstatus_ok = c("", "G")
                              )
  knitr::kable(kh_twn[1:10,])
#   message("     ", format(Sys.time(), "%X"), " | Now writing \"", local_path, "/kh_twn.RData\"")
#   save(kh_twn,
#        file = paste(local_path, "kh_twn.RData", sep = "/")
#   )
  } else load(paste(local_path, "kh_twn.RData", sep = "/"))

###########################################################
#
#     'kh_evn' IS A DATAFRAME FOR INDIVIDUAL EVENTS IN LONG
#              FORMAT
#
###########################################################

if (!"kh_evn.RData" %in% my_files){
  kh_evn <- get_dt_ev(kh_ind, kh_src2)
#  knitr::kable(kh_evn[1:20,])
  kh_evn$evloc <- recode_evloc(kh_evn$evloc)
#   message("     ", format(Sys.time(), "%X"), " | Now writing \"", local_path, "/kh_evn.RData\"")
#   save(kh_evn,
#        file = paste(local_path, "kh_evn.RData", sep = "/")
#   )
  } else load(paste(local_path, "kh_evn.RData", sep = "/"))

###########################################################
#
#     'kh_mat' IS AN ARRAY WITH THREE DIMENSIONS:
#              'id' INDIVIDUAL ID
#              'tt' TRANSITIONS 1:X
#                       [,1,] "*"       BIRTH
#                       [,2,] "#"       PARITY
#                       [,X,] "+"       DEATH
#              'vv' VARIABLES
#                       [,,1] "evdat"   NUMERIC DATE
#                       [,,2] "evid"    INDIVIDUAL
#                       [,,3] "evloc"   LOCATION
#                       [,,4] "evspc"   SPECIAL
#                       [,,5] "status"  STATUS
#
###########################################################

if (!"kh_mat.RData" %in% my_files){
  kh_mat <- get_evmat(kh_evn)
#   message("     ", format(Sys.time(), "%X"), " | Now writing \"", local_path, "/kh_mat.RData\"")
#   save(kh_mat,
#        file = paste(local_path, "kh_mat.RData", sep = "/")
#        )
} else load(paste(local_path, "kh_mat.RData", sep = "/"))


###########################################################
#
#       kh_mom IS A SAMPLE OF n MOTHERS
#
###########################################################

###########################################################
#
#     'kh_ped' IS A LIST CONTAINING SCALED PEDIGREES AS
#              RETURNED FROM 'kinship2::pedigree()'
#
#     *** COMPUTATION CAN BE VERY TIME-CONSUMING !! ***
#
###########################################################

if (!"kh_ped.RData" %in% my_files){

  set.seed(13)
  kh_mom <- kinlab::sample_kh(kh_ind, kh_src2, n = n_sample)

  message("     Start scaling of ", n_sample, " pedigrees at ", format(Sys.time(), "%X..."))
  if(silent==FALSE){ kh_ped <- plyr::llply(
    kh_mom, kinlab::grap_ped, kh_ind, kh_twn,
    as.numeric(dimnames(kh_mat[[1]])$id), .progress="text"
    )} else kh_ped <- plyr::llply(
      kh_mom, kinlab::grap_ped, kh_ind, kh_twn,
      as.numeric(dimnames(kh_mat[[1]])$id)
    )
  message("     ...finished at ", format(Sys.time(), "%X."))

  names(kh_ped) <- kh_mom
  message("     ", format(Sys.time(), "%X"), " | Now writing \"", local_path, "/kh_ped.RData\"")
  save(kh_ped,
       file=paste(local_path, "kh_ped.RData", sep="/")
       )
} else load(paste(local_path, "kh_ped.RData", sep = "/"))

message("     kh_ped, a list of ", length(kh_ped), " pedigrees")

###########################################################
#
#
#       kh_geo -- A LIST WITH THREE COMPONENTS:
#                     - DATAFRAME HOLDING GEOPOSITIONS
#                     - MAP-OBJECTS
#                     - A CORRESPONDING DISTANCE MATRIX
#
###########################################################

if (!"kh_geo.RData" %in% my_files){
  kh_geo <- get_geo()
  message("     ", format(Sys.time(), "%X"), " | Now writing \"", local_path, "/kh_geo.RData\"")
  save(kh_geo,
       file=paste(local_path, "kh_geo.RData", sep="/")
       )
} else load(paste(local_path, "kh_geo.RData", sep = "/"))

###########################################################
#
#       'kh_kin' -- A NESTED LIST OF LENGTH N INDIVIDUALS,
#                   EACH COMPONENT IS A LIST OF DATAFRAMES
#                   HOLDING KIN DATA AT A SPECIFIC DATE
#
###########################################################

if (!"kh_kin.RData" %in% my_files){
  kh_kin <- vector("list", length(kh_ped))
  names(kh_kin) <- names(kh_ped)
  t_1 <- Sys.time()
  for (my_id in names(kh_ped)){
    if(silent==FALSE){cat("ID",
      paste0(my_id),"|", format(Sys.time(),"%H:%M:%S"), "\n"
      )}
    kh_kin[[paste(my_id)]] <- vector("list",
                               sum(kh_mat[[1]][paste(my_id),,5])
                               )
    names(kh_kin[[paste(my_id)]]) <-
      paste(as.Date(kh_mat[[1]][paste(my_id),,1][
          which(kh_mat[[1]][paste(my_id),,5]==1)],
          origin="1970-01-01")
          )
    for (my_date in paste(as.Date(kh_mat[[1]][paste(my_id),,1][
         which(kh_mat[[1]][paste(my_id),,5]==1)],
         origin="1970-01-01"))){
    if(silent==FALSE){cat("         |", format(Sys.time(), "%X"), "|",
          paste0(my_date), "\n")}
      kh_kin[[paste(my_id)]][[paste(my_date)]] <-
        try(kinlab::get_kinset(my_id, my_date,
                               kh_ped[[paste(my_id)]],
                               kh_ind, kh_src2, kh_mat[[1]],
                               kh_geo[["dist"]]
                               )
        )
      }
    if(silent==FALSE){ cat(round(which(names(kh_ped)==my_id)/length(kh_ped)*100, 2),
        "% (runtime:", round(difftime(Sys.time(), t_1), 2), attr(difftime(Sys.time(), t_1), "units"),
      ")\n------------------------------------------\n"
    )}
  }
  message("     ", format(Sys.time(), "%X"), " | Now writing \"", local_path, "/kh_kin.RData\"")
  save(kh_kin,
       file=paste(local_path, "kh_kin.RData", sep="/")
  )
} else load(paste(local_path, "kh_kin.RData", sep = "/"))
n_sample <- length(kh_kin)
message("## number of cases: ", n_sample)

out <- restrict2sample(df_evn=kh_evn,
                       df_ind =kh_ind,
                       list_kin =kh_kin,
                       array_mat =kh_mat[[1]],
                       list_src =kh_src,
                       df_twn =kh_twn,
                       param = kh_mat[[2]])


if(file.exists(paste(local_path, "kh_mat.RData", sep = "/"))){file.remove(paste(local_path, "kh_mat.RData", sep = "/"))}


if (!"kh_evn.RData" %in% my_files){
kh_evn <- out[["evn"]]
save(kh_evn, file = paste(local_path, "kh_evn.RData", sep="/"))
message(paste0(format(Sys.time(), "%X"), " ...wrote \"", local_path, "/kh_evn.RData\""))
}

if (!"kh_mat.RData" %in% my_files){
kh_mat <- out[["mat"]]
save(kh_mat, file = paste(local_path, "kh_mat.RData", sep="/"))
message(paste0(format(Sys.time(), "%X"), " ...wrote \"", local_path, "/kh_mat.RData\""))
}

if (!"kh_ind.RData" %in% my_files){
kh_ind <- out[["ind"]]
save(kh_ind, file = paste(local_path, "kh_ind.RData", sep="/"))
message(paste0(format(Sys.time(), "%X"), " ...wrote \"", local_path, "/kh_ind.RData\""))
}

if (!"kh_src.RData" %in% my_files){
kh_src <- out[["src"]]
save(kh_src, file = paste(local_path, "kh_src.RData", sep="/"))
message(paste0(format(Sys.time(), "%X"), " ...wrote \"", local_path, "/kh_src.RData\""))
}

if (!"kh_twn.RData" %in% my_files){
kh_twn <- out[["twn"]]
save(kh_twn, file = paste(local_path, "kh_twn.RData", sep="/"))
message(paste0(format(Sys.time(), "%X"), " ...wrote \"", local_path, "/kh_twn.RData\""))
}

if (!"kh_par.RData" %in% my_files){
kh_par <- out[["par"]]
save(kh_par, file = paste(local_path, "kh_par.RData", sep="/"))
message(paste0(format(Sys.time(), "%X"), " ...wrote \"", local_path, "/kh_par.RData\""))
}
} else message("no action required!")
kinlab::run_time(t_start)
}
