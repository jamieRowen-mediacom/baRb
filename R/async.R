async_collector = new.env()
async_collector$job_id = c()

async_results_url = function(job_id) {
  glue::glue("{.async_root}/results/{job_id}")
}

#' Make a request to the barb async API
#' 
#' Send a request to the barb API. This is a lightweight general purpose
#' request generating function for requests to the barb API async endpoints and
#' can be used as a workhorse to query any of the async API endpoints.
#' 
#' Async requests which are successfully initiated are automatically added to be tracked
#' in the current R session. See async_sitrep 
#'
#' @inheritParams barb
#' @param output_format character(1), one of "csv", "parquet", determines the type of data files
#'  generated for download.
#' @return list response from the endpoint
#' @export
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

#' Check the status of async requests
#' 
#' Will query the current status of all async endpoint requests generated in
#' the current R session.
#' 
#' @return a tibble
#' \describe{
#'  \item{uuid}{Unique identifier for the resource}
#'  \item{created_at}{character(n) time stamp of the request initiation}
#'  \item{status}{Current execution status of the report, one of "started", "successful", "failed"}
#'  \item{result}{list(character(n)) results, if available, of files for download}
#' }
#' @export
async_sitrep = function() {
  purrr::map_dfr(async_collector$job_id, async_barb_result)
}

#' Get/check status of result
#' 
#' While an async job is being processed, the response will contain the progress information
#' allowing users to track the completion status.
#' 
#' Possible statuses:
#' 
#'  * started
#'  * successful
#'  * failed
#' 
#' Once the job is completed, the response will provide the final result, which consists
#' of an array of files that can be downloaded by the uers.
#' 
#' The result includes two types of files
#' 
#' * CSV Files: We unload data into CSV files and apply gzip compression to reduce the data size.
#' * Parquet Files: We also provide the option to unload data into Parquet files, which offer columnar storage and efficient compression. We use the Snappy compression algorithm for Parquet files, ensuring a balance between compression ratio and processing speed.
#' @param job_id character(1) the job id returned by initiating a request with async_barb
#' @param token character(1) barb API token. if NA try to find the session default token.
#' @return list from raw response json
#' @export
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
