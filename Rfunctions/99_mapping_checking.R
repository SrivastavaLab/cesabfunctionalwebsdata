## mapping and plotting functions for checking data:
get_map_SA <- function() {
  SAm <- c(left = -120, bottom = -56, right = -34, top = 30)
  map <- get_stamenmap(SAm, zoom = 3, maptype = "watercolor")
}
