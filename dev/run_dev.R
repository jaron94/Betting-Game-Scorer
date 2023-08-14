# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
#options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list = ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

port <- getOption("shiny.port", 3838)
host <- getOption("shiny.host", "127.0.0.1")

device <- "iPhone 12"

# Use app.R or app_mobile.R depending on the device
appPath <- if (grepl("ipad", device)) {
  "app.R"
} else {
  "app_mobile.R"
}

app_manager <<- shinybg:::app_manager # nolint: undesirable_operator_linter

if (port %in% app_manager$list_ports()) {
  app_manager$kill_app(port)
}

app <- callr::r_bg(
  function(appPath, port) {
    options(shiny.trace=TRUE)
    shiny::runApp(appPath, port = port, launch.browser = FALSE)
  },
  args = list(appPath = appPath, port = port),
  stdout = "shinybg.log",
  stderr = "shinybgerr.log",
  supervise = TRUE
)

app_manager$register_app(port, app)

Sys.sleep(5)

device_info_tab <- read.csv("dev/device_info.csv")
landscape <- FALSE

device_info <- device_info_tab |>
  subset(device_name == device) |>
  as.list()

width <- device_info$width_viewport
height <- device_info$height_viewport
scale_factor <- device_info$dpr_js_css_pixel_ratio

chrom <- chromote::Chromote$new()

b <- chromote::ChromoteSession$new()

# Start chrome in mobile emulation mode
b$Emulation$setDeviceMetricsOverride(
  width = if (landscape) height else width,
  height = if (landscape) width else height,
  deviceScaleFactor = device_info$dpr_js_css_pixel_ratio,
  mobile = TRUE
)

b$Emulation$setUserAgentOverride(userAgent = device_info$user_agent)

# b$Emulation$setTouchEmulationEnabled(enabled = TRUE)

b$Page$navigate(paste0("http://", host, ":", port))

b$view()

# withr::with_options(
#   list(
#     browser = chromote::find_chrome()
#   ),
#   shinyMobile::preview_mobile(
#     url = paste0("http://", host, ":", port),
#     device = device,
#     landscape = TRUE
#   )
# )
