#' Krummhörn Parameter Labels
#'
#' A list containing a descritpion for labels of numerical factors and a 3-dim array (as returned by `get_evmat()`) consisting of the dimensions:
#' 1. "id" 73133 individual IDs
#' 2. "tt" 20 rows for the following events:
#' + "*" birth
#' + "#01" 1st child
#' + "#02" 2nd child
#' + ...
#' + "#18" 18th child.
#' + "+" death
#' 3. "vv" 5 columns named as follows:
#' "vv" holds five columns for the variables:
#' + "evdat": difference in days since "1970-01-01"
#' + "evid": own ID at birth and death, otherwise ID one child being born
#' + "evloc": Code of specific "Kirchspiel" (see `?decode_evloc()`)
#' + "evspc": Code of event-specific attributes, e.g. for a still-birth (see `?decode_evspc()`)
#' + "status": 1 if event observed , otherwise 0.
#'
#' @format An 3-dim array
#' @source see `citation("kh.data")`
"kh_par"
