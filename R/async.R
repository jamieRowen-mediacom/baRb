async_collector = new.env()
async_collector$job_id = c()

async_results_url = function(job_id) {
  glue::glue("{.async_root}/results/{job_id}")
}

async_barb = function(path, ..., output_format = c("csv", "parquet"), token = NA_character_) {
  output_format = match.arg(output_format)
  path = ensure_slashes(path)
  token = get_token(token)
  query = list(...)
  query[["output_format"]] = output_format
  response = httr::POST(
    url = glue::glue("{.async_root}{path}"),
    barb_auth_header(token),
    body = query
  )
  res = httr::content(response)
  if (response$status == 200) {
    async_collector$job_id = c(async_collector$job_id, res$job_id)
  }
  res
}

async_sitrep = function() {
  purrr::map_dfr(async_collector$job_id, async_barb_result)
}

async_barb_result = function(job_id, token = NA_character_) {
  url = async_results_url(job_id)
  token = get_token(token)
  response = httr::GET(
    url = url,
    barb_auth_header(token)
  )
  res = httr::content(response)
  if (length(res$result) == 0) {
    res$result = list(NA_character_)
  }
  res
}

# barb_get_access_token() %>%
#   set_token()

# res = barb("/advertisers")

# res_aud = async_barb(
#   "programme_audience",
#   min_session_datetime = "2020-01-01 00:00:00",
#   max_session_datetime = "2023-01-02 00:00:00",
#   transmission_log_programme_name = "sherlock"
# )

# async_sitrep()
