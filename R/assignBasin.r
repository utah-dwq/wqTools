#' Assign basin (management unit) to records
#'
#' This function assigns a basin field (default `Mgmt_Unit`) using
#' `wqTools::wmu_poly`. It uses latitude/longitude when available and then
#' fills any remaining missing basin values using `ASSESS_ID`.
#'
#' @param data Input dataset.
#' @param lat Name of latitude column. Default matches WQP objects.
#' @param long Name of longitude column. Default matches WQP objects.
#' @param assess_id Name of assessment unit id column used for fallback.
#'   Defaults to `ASSESS_ID`.
#' @param basin_col Name of basin column in `wmu_poly` to return.
#'   Defaults to `Mgmt_Unit`.
#' @importFrom sf st_centroid
#' @importFrom sf st_geometry
#' @importFrom sf st_join
#' @examples
#' # Read a couple of sites and assign basin
#' sites=readWQP(type="sites", siteid=c("UTAHDWQ_WQX-4900440","UTAHDWQ_WQX-4900470"))
#' sites_basin=assignBasin(sites)
#' @return Returns the input data frame with basin information appended.
#' @export
assignBasin <- function(
  data,
  lat = "LatitudeMeasure",
  long = "LongitudeMeasure",
  assess_id = "ASSESS_ID",
  basin_col = "Mgmt_Unit"
) {
  
  out <- data
  basin_vals <- rep(NA_character_, nrow(out))

  # Prefer spatial assignment for rows with coordinates.
  if (lat %in% names(out) && long %in% names(out)) {
    lat_vals <- suppressWarnings(as.numeric(out[[lat]]))
    long_vals <- suppressWarnings(as.numeric(out[[long]]))
    has_coords <- !is.na(lat_vals) & !is.na(long_vals)

    if (any(has_coords)) {
      by_coords <- out[has_coords, , drop = FALSE]
      by_coords[[lat]] <- lat_vals[has_coords]
      by_coords[[long]] <- long_vals[has_coords]
      by_coords <- assignPolys(
        data = by_coords,
        polygon = wqTools::wmu_poly,
        lat = lat,
        long = long,
        columns = basin_col
      )
      basin_vals[has_coords] <- as.character(by_coords[[basin_col]])
    }
  }

  out[[basin_col]] <- basin_vals

  # Fill missing basin values using ASSESS_ID -> basin lookup.
  if (assess_id %in% names(out)) {
    au_wmu <- sf::st_centroid(wqTools::au_poly)
    au_wmu <- suppressMessages({
      suppressWarnings({
        sf::st_join(au_wmu, wqTools::wmu_poly[, basin_col], left = TRUE)
      })
    })
    sf::st_geometry(au_wmu) <- NULL
    au_wmu <- unique(au_wmu[, c("ASSESS_ID", basin_col)])

    lookup_idx <- match(out[[assess_id]], au_wmu$ASSESS_ID)
    fill_idx <- is.na(out[[basin_col]]) & !is.na(lookup_idx)
    out[[basin_col]][fill_idx] <- as.character(au_wmu[[basin_col]][lookup_idx[fill_idx]])
  }

  out
}
