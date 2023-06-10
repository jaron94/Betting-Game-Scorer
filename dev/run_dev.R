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

appPath <- "app_mobile.R"

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

withr::with_options(
  list(
    browser = "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe" # nolint: absolute_path_linter
  ),
  shinyMobile::preview_mobile(
    url = paste0("http://", host, ":", port),
    device = "iphoneX"
  )
)
