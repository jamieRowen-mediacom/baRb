#' @include utils.R
meta_class_name_ = function(x) {
  x = ensure_no_slash(x)
  glue::glue("barb_meta_{x}")
}

#' @include rexport-tibble.R
#' @include utils.R
meta_gen_ = function(path, ..., class = meta_class_name_(path)) {
  force(class)
  path = ensure_slashes(path)
  return(function(..., as_tibble = TRUE, token = NA_character_) {
    res = barb(path, ..., token = token) %>%
      with_class(class)
    if (as_tibble) {
      res = as_tibble(res)
    }
    res
  })
}

#' Get the list of panels
#'
#' Returns a complete list of panel codes and panel names.
#' This is useful for finding the correct panel code when using panel
#' code as a query parameter for other endpoints.
#'
#' @param as_tibble if TRUE cast result as a tibble object, otherwise list
#'   representation of the raw json response
#' @family meta data endpoints
#' @inheritParams barb
#' @export
#' @examples \dontrun{
#'   meta_panels()
#' }
#' @return if as_tibble, a tibble, otherwise list representation of raw json
meta_panels = meta_gen_("panels")

#' Get the list of stations
#'
#' Informations about the stations codes is used as a
#' query parameter in the advertising spots, audiences_by_time, 
#' programme_ratings, spot_schedule, programme_schedule endpoints.
#'
#' @inheritParams meta_panels
#' @family meta data endpoints
#' @export
#' @return if as_tibble, a tibble, otherwise list representation of raw json
meta_stations = meta_gen_("stations")

#' Get the list of advertisers
#' 
#' Returns a complete list of advertiser names
#' 
#' @inheritParams meta_panels
#' @family meta data endpoints
#' @export
#' @return if as_tibble, a tibble, otherwise list representation of raw json
meta_advertisers = meta_gen_("advertisers")

#' Get the list of buyers
#' 
#' Returns a complete list of buyer names.
#' 
#' @inheritParams meta_panels
#' @family meta data endpoints
#' @export
#' @return if as_tibble, a tibble, otherwise list representation of raw json
meta_buyers = meta_gen_("buyers")

#' Get the list of panel households
#' 
#' Returns a list of households, where each household is acombination of
#' household data and a list of the devices present in the household.
#' 
#' @inheritParams meta_panels
#' @family meta data endpoints
#' @export
#' @return if as_tibble, a tibble, otherwise list representation of raw json
meta_households = meta_gen_("households")

#' Get the list of panel members
#' 
#' Returns details for a set of panel members.
#' 
#' @inheritParams meta_panels
#' @family meta data endpoints
#' @export
#' @return if as_tibble, a tibble, otherwise list representation of raw json
meta_panel_members = meta_gen_("panel_members")

#' Get the list of target audience categories
#' 
#' Returns the list of target audience categories by selected period and panel.
#' 
#' @inheritParams meta_panels
#' @family meta data endpoints
#' @export
#' @return if as_tibble, a tibble, otherwise list representation of raw json
meta_target_audience_members = meta_gen_("target_audience_members")

#' Get the list of viewing stations
#' 
#' Information about the viewing stations codes is used as a query parameter
#' in the viewing, programme_audience endpoints.
#' @inheritParams meta_panels
#' @family meta data endpoints
#' @export
#' @return if as_tibble, a tibble, otherwise list representation of raw json
meta_viewing_stations = meta_gen_("viewing_stations")

#' Get the spots scheduled
#' 
#' Get all the spots scheduled by each station.
#' @inheritParams meta_panels
#' @family meta data endpoints
#' @export
#' @return if as_tibble, a tibble, otherwise list representation of raw json
meta_spot_schedule = meta_gen_("spot_schedule")

#' Get the programmes scheduled
#' 
#' Get all the programmes scheduled by each station.
#' @inheritParams meta_panels
#' @family meta data endpoints
#' @export
#' @return if as_tibble, a tibble, otherwise list representation of raw json
meta_programme_schedule = meta_gen_("programme_schedule")

#' Get the list of programme content details
#' @inheritParams meta_panels
#' @family meta data endpoints
#' @export
#' @return if as_tibble, a tibble, otherwise list representation of raw json
meta_programme_content_details = meta_gen_("programme_content_details")

#' Get the list of transmission log programme details
#' @inheritParams meta_panels
#' @family meta data endpoints
#' @export
#' @return if as_tibble, a tibble, otherwise list representation of raw json
meta_transmission_log_programme_details = meta_gen_("transmission_log_programme_details")


#' Get the list of panels
#'
#' Returns a complete list of panel codes and panel names.
#' This is useful for finding the correct panel code when using panel
#' code as a query parameter for other endpoints.
#'
#' @param as_tibble if TRUE cast result as a tibble object, otherwise list
#'   representation of the raw json response
#' @inheritParams barb
#' @export
#' @examples \dontrun{
#'   meta_panels()
#' }
#' @return if as_tibble, a tibble, otherwise list representation of raw json
barb_get_panels = function(as_tibble = TRUE, token = NA_character_) {
  meta_panels(as_tibble = as_tibble, token = token)
}

#' @exportS3Method as_tibble barb_meta_panels
#' @rdname as_tibble
#' @export
as_tibble.barb_meta_panels = function(
    x, ..., .rows = NULL,
    .name_repair = c("check_unique", "unique", "universal", "minimal"),
    rownames = pkgconfig::get_config("tibble::rownames", NULL)
) {
  x %>%
    tidyjson::as_tbl_json() %>%
    tidyjson::spread_values(panel_code = tidyjson::jstring('panel_code')) %>%
    tidyjson::spread_values(panel_region = tidyjson::jstring('panel_region')) %>%
    tidyjson::spread_values(is_macro_region = tidyjson::jstring('is_macro_region')) %>%
    dplyr::select(panel_code, panel_region, is_macro_region) %>%
    tibble::as_tibble()
}

#' Get a tibble of station names
#'
#' @return A tibble of available stations
#' @export
#'
#' @examples
#' barb_get_stations()
barb_get_stations <- function(){

  api_result <- barb_query_api(
    barb_url_meta_stations()
  )

  if(is.null(api_result$json)) return(NULL)

  result <- api_result$json %>%
    tidyjson::as_tbl_json() %>%
    tidyjson::spread_values(station_code = tidyjson::jstring('station_code')) %>%
    tidyjson::spread_values(station_name = tidyjson::jstring('station_name')) %>%
    dplyr::select(station_code, station_name) %>%
    tibble::as_tibble()

  result
}
