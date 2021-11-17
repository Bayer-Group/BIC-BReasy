#' about_breasy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_about_breasy_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns('about_breasy1'))
  )
}
    
#' about_breasy Server Function
#'
#' @noRd 
mod_about_breasy_server <- function(input, output, session){
  ns <- session$ns
  output$about_breasy1 <- shiny::renderUI({
    list(
      HTML(
        "<h1> The concept of the BReasy R Shiny app for structured benefit-risk assessment </h1>
        
        <p> <img src='www/AppIcon_BReasy_210x210mm_RGB.png' alt='Graphic cannot be displayed' width = '350' height='350' align = 'right'>
        
      The BReasy R Shiny app provides a handy platform for structured benefit-risk assessment using clinical study or pooled data.
      With the provided forest plot estimates for all relevant efficacy and safety outcomes can be shown in one graph. In the graphical display the presented outcomes can be separated into efficacy and safety. To enable a structured and transparent approach to assessing the benefit-risk profile, the outcomes shown in the forest plot are based on the same analysis sets and data scopes. 
      Furthermore, a presentation by study (in case of pooled data), stratum and subgroups is possible to assess the benefit-risk profile in subpopulations of interest.

        The following estimates for comparison can be presented with the BReasy R Shiny app:</p>
        
        <li> Hazard Ratios; </li>
        <li> Excess number of subjects, for example based on Kaplan-Meier differences or risk differences;</li>
        <li> Odds Ratios; </li>
        <li> Risk Differences; </li>
        <li> Relative risks. </li>
        
        <p> An optional display of the following data is possible to be added to the forest plot: </p>
        
        <li> The actual estimates displayed in the forest plot; </li>
        <li> Event and patient counts; </li>
        <li> Number needed to treat (NNT) / Number needed to harm (NNH).</li>
        
         <p> <img src='www/Forestplot_example.png' alt='Graphic cannot be displayed' width = '1225' height='575'> </p>
        <h4> Value Tree </h4>
        <p> Additionally, the BReasy app includes the possibility of creating a value tree showing the relevant benefit and risk criteria.
        Benefit criteria will be colour-coded as green, and the risk criteria will be colour-coded as red.
        The interactively created value tree can be saved in html format, please see below for an example.
        </p>
        <p> <img src='www/Valuetree_example.png' alt='Graphic cannot be displayed'> </p>
        "
      )
    )
  })
}
    
## To be copied in the UI
# mod_about_breasy_ui("about_breasy_ui_1")
    
## To be copied in the server
# callModule(mod_about_breasy_server, "about_breasy_ui_1")
 
