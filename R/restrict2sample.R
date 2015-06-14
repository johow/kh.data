#' Restrict data to a given sample size
#'
#' This function returns a dataframe being required to contruct pedigrees via the _kinship2::pedigree()_ function
#' @param df_evn A dataframe in long format holding event data
#' @param df_ind TBD
#' @param list_kin TBD
#' @param array_mat TBD
#' @param list_src TBD
#' @param df_twn TBD
#' @param
#' @keywords kh sample size
#' @export
#' @examples
#' \dontrun{
#' kh_df <- get_df(df_ind, df_fam, get_fam=FALSE)
#' }

restrict2sample <- function(df_evn=NULL,
                              df_ind =NULL,
                              list_kin =NULL,
                              array_mat =NULL,
                            list_src =NULL,
                             df_twn =NULL,
                            param = NULL){
 included_id <- as.numeric(unlist(lapply(list_kin, lapply, "[[", 1)))
 df_evn <- subset(df_evn, id %in% included_id)
 df_ind <- subset(df_ind, id %in% included_id)
 array_mat <- array_mat[paste(included_id),,]
 list_src[[1]] <- subset(list_src[[1]],  id %in% included_id)
 list_src[[2]] <- subset(list_src[[2]], doc %in% list_src[[1]]$doc |
                           doc %in% list_src[[1]]$famnrk1 |
                           doc %in% list_src[[1]]$famnrk2 |
                           doc %in% list_src[[1]]$famnrk3 |
                           doc %in% list_src[[1]]$famnrk4 |
                           doc %in% list_src[[1]]$famnrk5)

 df_twn <- df_twn[df_twn[,1] %in% included_id  & df_twn[,2] %in% included_id,]

 return(list("evn" = df_evn, "ind" = df_ind,
             "mat" = array_mat, "src"  = list_src,
             "twn" = df_twn, "par" = param))
}
