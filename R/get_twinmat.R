#' Wrapper function to return a matrix for twin relations
#'
#' This function returns a matrix describing twin relations in the form 'id1-id2-code', where code stands for 1=monozygotous, 2=dizygotous, 3=unknown. Here, zygosity is soley based on sex and therefore only twins of different sex are given code=2 (i.e. dizygotous), while same-sex twins are classified as unknown (code=3). In the case of triplets (and twins of higher order), each individual will have relations to each of his co-twins.
#' @param df_ped A dataframe containing the children's birth dates and and the ID of their parents
#' @param bspecial_exclude Possible values of 'bspecial' to exclude as twin births
#' @param bstatus_ok A value of the variabe 'bstatus', which indicates that a birth date is adequately documented. )
#' @keywords pedigree
#' @export
#' @examples
#' \dontrun{
#' twinmat <- get_twinmat(df_ped, df_kinder)
#' }
get_twinmat <- function(df_ped = NULL, bspecial_exclude = "ALL",   bstatus_ok = NULL){
  ################################
  #
  # STEP 3: PROCESS TWIN RELATIONS
  #
  ################################
  if (!is.null(bstatus_ok)){
  if (all(is.numeric(bstatus_ok))){
df_ped <- df_ped[!is.na(df_ped$bdate) & df_ped$momid>0 & df_ped$bstatus %in% bstatus_ok,]
} else df_ped <- df_ped[!is.na(df_ped$bdate) & df_ped$momid>0 & df_ped$bstatus %in% paste(bstatus_ok),]
}

df_ped <- df_ped[order(df_ped$momid, df_ped$bdate),]
df_ped$ibi <- ifelse(df_ped$momid==c(df_ped$momid[-1], NA),
                     as.numeric(difftime(df_ped$bdate, df_ped$bdate, units="days"))/365.25, NA)
df_ped$pre.ibi <- ifelse(df_ped$momid==c(NA, df_ped$momid[-nrow(df_ped)]),
                     as.numeric(difftime(df_ped$bdate, as.Date(c(NA, df_ped$bdate[-nrow(df_ped)]),origin="1970-01-01"), units="days"))/365.25, NA)

  df_twins <- subset(df_ped, (!is.na(pre.ibi) & pre.ibi == 0) | (!is.na(ibi) & ibi == 0))
  df_twins <- df_twins[order(df_twins$momid, df_twins$bdate),]
  df_twins$id_twin <- as.numeric(factor(paste(df_twins$momid,  df_twins$bdate, sep="-")))
  df_twins <- df_twins[order(df_twins$id_twin),]
#
#   table(as.numeric(lapply(lapply(tapply(df_twins$momid, df_twins$id_twin, list), unique), length)))
#   table(as.numeric(lapply(lapply(tapply(df_twins$dadid, df_twins$id_twin, list), unique), length)))

  twinlist <- tapply(df_twins$id, df_twins$id_twin, list)
  table(as.numeric(lapply(twinlist, length)))

  twintypelist  <-  ifelse(unlist(lapply(tapply(df_twins$sex, df_twins$id_twin, unique), length))==1, 3, 2)


  twinlist2 <- twinlist[which( as.numeric(lapply(twinlist, length)) == 2)]
  twinlist3 <- twinlist[which( as.numeric(lapply(twinlist, length)) == 3)]
  twinlist4 <- twinlist[which( as.numeric(lapply(twinlist, length)) == 4)]
  twinlist5 <- twinlist[which( as.numeric(lapply(twinlist, length)) == 5)]

  twinmat2 <- matrix(unlist(twinlist2), nrow= length(twinlist2),  byrow=TRUE,  dimnames =
                       list(1:length(twinlist2), c("id1", "id2")))

  twinmat2 <- cbind(twinmat2, twintypelist[which( as.numeric(lapply(twinlist, length)) == 2)])
  dimnames(twinmat2)[[2]][3] <- "code"


  twinmat3 <- matrix(c(unlist(twinlist3)[rep(c(TRUE, TRUE, FALSE))], unlist(twinlist3)[rep(c(TRUE, FALSE, TRUE))],
                       unlist(twinlist3)[rep(c(FALSE, TRUE, TRUE))]),
                     nrow= length(twinlist3)*3,  byrow=TRUE,  dimnames =
                       list(1:(length(twinlist3)*3)+length(twinlist2), c("id1", "id2")))


  twinmat3 <- cbind(twinmat3, "code" = NA)
  twinmat3[rep(c(TRUE, FALSE, FALSE)), 3] <- twintypelist[which( as.numeric(lapply(twinlist, length)) == 3)]
  twinmat3[rep(c(FALSE, TRUE, FALSE)), 3] <- twintypelist[which( as.numeric(lapply(twinlist, length)) == 3)]
  twinmat3[rep(c(FALSE, FALSE, TRUE)), 3] <- twintypelist[which( as.numeric(lapply(twinlist, length)) == 3)]

  twinmat <- rbind(twinmat2, twinmat3)


  if (length(twinlist4)>0){
    twinmat4 <- matrix(c(unlist(twinlist4)[rep(c(TRUE, TRUE, FALSE, FALSE))],
                         unlist(twinlist4)[rep(c(TRUE, FALSE, TRUE, FALSE))],
                         unlist(twinlist4)[rep(c(TRUE, FALSE, FALSE, TRUE))],
                         unlist(twinlist4)[rep(c(FALSE, TRUE, TRUE, FALSE))],
                         unlist(twinlist4)[rep(c(FALSE, TRUE, FALSE, TRUE))],
                         unlist(twinlist4)[rep(c(FALSE, FALSE, TRUE, TRUE))]),
                       nrow= length(twinlist4)*6,  byrow=TRUE,  dimnames =
                         list(1:(length(twinlist4)*6)+(length(twinlist2)+length(twinlist3)), c("id1", "id2")))


    twinmat4 <- cbind(twinmat4, "code" = NA)
    twinmat4[rep(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)), 3]  <-  twintypelist[which( as.numeric(lapply(twinlist, length)) == 4)]
    twinmat4[rep(c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)), 3]  <-  twintypelist[which( as.numeric(lapply(twinlist, length)) == 4)]
    twinmat4[rep(c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)), 3]  <-  twintypelist[which( as.numeric(lapply(twinlist, length)) == 4)]
    twinmat4[rep(c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)), 3]  <-  twintypelist[which( as.numeric(lapply(twinlist, length)) == 4)]
    twinmat4[rep(c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)), 3]  <-  twintypelist[which( as.numeric(lapply(twinlist, length)) == 4)]
    twinmat4[rep(c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)), 3]  <-  twintypelist[which( as.numeric(lapply(twinlist, length)) == 4)]

    twinmat <- rbind(twinmat, twinmat4)
  }
if (length(twinlist5)>0){
  twinmat5 <- matrix(c(unlist(twinlist5)[rep(c(TRUE, TRUE, FALSE, FALSE, FALSE))],
                       unlist(twinlist5)[rep(c(TRUE, FALSE, TRUE, FALSE, FALSE))],
                       unlist(twinlist5)[rep(c(TRUE, FALSE, FALSE, TRUE, FALSE))],
                       unlist(twinlist5)[rep(c(TRUE, FALSE, FALSE, FALSE, TRUE))],
                       unlist(twinlist5)[rep(c(FALSE, TRUE, TRUE, FALSE, FALSE))],
                       unlist(twinlist5)[rep(c(FALSE, TRUE, FALSE, TRUE, FALSE))],
                       unlist(twinlist5)[rep(c(FALSE, TRUE, FALSE, FALSE, TRUE))],
                       unlist(twinlist5)[rep(c(FALSE, FALSE, TRUE, TRUE, FALSE))],
                       unlist(twinlist5)[rep(c(FALSE, FALSE, TRUE, FALSE, TRUE))],
                       unlist(twinlist5)[rep(c(FALSE, FALSE, FALSE, TRUE, TRUE))]),
                     nrow= length(twinlist5)*10,  byrow=TRUE,  dimnames =
                       list(1:(length(twinlist5)*10)+(length(twinlist2)+length(twinlist3)+length(twinlist4)), c("id1", "id2")))


  twinmat5 <- cbind(twinmat5, "code" = NA)
  twinmat5[, 3]  <-  twintypelist[which( as.numeric(lapply(twinlist, length)) == 5)]
  twinmat <- rbind(twinmat, twinmat5)
}

  #       k <- 1
  #       for (i in relate[,1]){
  #       relate[k,4] <- as.numeric(paste(df_twins$famid[df_twins$id == i]))
  #       attributes(relate)$dimnames[[1]][k] <- paste(df_twins$famid[df_twins$id == i])
  #       k <- k+1
  #       }

  return(twinmat[twinmat[,1] %in% df_ped$id & twinmat[,2] %in% df_ped$id,])
  ############################

}
