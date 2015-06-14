#' Geopositional Data List
#'
#' This function returns a list of 3 elements:
#' \enumerate{
#'   \item{df_geo}{A dataframe with 33 rows for the Krummhörn Kirchspiele's Longitude and Lattitude (as returned from Google Maps)}
#'   \item{maps}{A list of 2 Maps returned from 'ggmap::ggmap()'}
#'  \item{mat}{A distance Matrix for all 33 Kirchspiele, as returned from Google Maps}
#' }
#' @param data_source A character string for the name of a given population (Currently "kh" for 'Krummhörn' is the only valid value and default)
#' @keywords kh import
#' @export
#' @examples
#' twinmat <- get_twinmat()
#' str(twinmat)

get_geo <- function(data_source = "kh"){
if(data_source == "kh"){
df_geo <- data.frame(
  kirchspiel = c("Campen", "Canhusen", "Canum", "Cirkwehrum", "Eilsum", "Freepsum", "Greetsiel", "Grimersum", "Groothusen", "Groß Midlum", "Hamswehrum", "Hinte", "Jennelt", "Larrelt", "Logumer Vorwerk", "Loppersum", "Loquard", "Manslagt", "Marienwehr", "Nesserland", "Pewsum", "Pilsum", "Rysum", "Suurhusen", "Twixlum", "Upleward", "Uttum", "Visquard", "Westerhusen", "Wirdum", "Woltzeten", "Woquard", "Wybelsum"),
  label = c("CA", "CH", "CN", "CI", "EI", "FR", "GS", "GI", "GH", "GM", "HW", "HI", "JE", "LA", "LV", "LP", "LO", "MA", "MW", "NE", "PE", "PI", "RY", "SU", "TW", "UP", "UT", "VI", "WE", "WI", "WZ", "WO", "WY"))


# Geopostional data can be retrieved from Google Maps via `ggmap::geocode()` given a list of recent locations (here: `df_geo`).

geo_list <- list("")
for (i in 1:nrow(df_geo)){
  geo_list[[i]] <- ggmap::geocode(paste0(df_geo$kirchspiel[i], ", Krummhörn"))
}
df_geo$lon <- NA
df_geo$lat <- NA
for (i in 1:nrow(df_geo)){
  df_geo$lon[i] <- geo_list[[i]]$lon
  df_geo$lat[i] <- geo_list[[i]]$lat
}



# A distance matrix


from <- paste0(df_geo$kirchspiel,", Niedersachsen")
to <- from

geo_dist <- matrix(rep(NA,33*33), nrow=33, byrow=TRUE, dimnames=list(from=paste0(df_geo$label), to=paste0(df_geo$label)))

for (i in 1:33){
  geo_dist[i,-i] <- ggmap::mapdist(from[i], to[-i], mode = 'walking')$km
  geo_dist[i,i] <- 0
}

outlist <- list("df" = df_geo,
                "maps" = list(ggmap::get_map("Krummhörn",
                                             source = "stamen",
                                             maptype = "toner-lite"),
                              ggmap::get_map("Krummhörn",
                                             source = "stamen",
                                             maptype = "watercolor")),
                "mat" = geo_dist)
}
return(outlist)
}
