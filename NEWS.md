# baRb (development version)

* ci: R CMD check standard
* feat: Added some utilities for making requests against the async endnpoints
    * `async_barb()` - initiate a post request for async endpoint
    * `async_barb_result()` - get result of async request, or status if not finished
    * `async_sitrep()` - check on the status of all async requests initiated in the current R session

# baRb 0.3.0

* docs: Section out the reference section of the pkgdown site
* feat: Added a collection of functions for all meta data endpoints
    * `meta_panels()`
    * `meta_stations()`
    * `meta_advertisers()`
    * `meta_buyers()`
    * `meta_households()`
    * `meta_panel_members()`
    * `meta_target_audience_members()`
    * `meta_viewing_stations()`
    * `meta_spot_schedule()`
    * `meta_programme_schedule()`
    * `meta_programme_content_details()`
    * `meta_transmission_log_programme_details()`
* Added a `NEWS.md` file to track changes to the package.
