url <- "https://github.com/amirshnll/custom-device-emulation-chrome"

device_info <- url |>
  rvest::read_html() |>
  rvest::html_nodes("table") |>
  _[[1]] |>
  rvest::html_table() |>
  janitor::clean_names()

write.csv(device_info, "dev/device_info.csv", row.names = FALSE)

