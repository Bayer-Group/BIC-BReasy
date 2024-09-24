#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom grDevices dev.size
#' @importFrom graphics arrows axis grconvertX grconvertY par points text
#' @importFrom stats confint
#' @importFrom utils read.csv write.csv

run_app <- function(
  ...
) {

  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(...)
  )
}
