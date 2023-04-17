#' Authenticate bgScorer to GCS
#' 
#' @param file_name Name of file containing Service Account key
#'
#' @return Function to be called when starting the app
#' @export
bg_gcs_auth <- function(file_name = "bgScorer-testing.json") {
  if (get_golem_config("use_gcs")) {
    json <- gargle:::secret_read("bgScorer", file_name)
    googleCloudStorageR::gcs_auth(rawToChar(json))
    default_bucket <- Sys.getenv("GCS_DEFAULT_BUCKET")
    googleCloudStorageR::gcs_global_bucket(default_bucket)
  }
}
