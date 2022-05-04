#' data_manual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_manual_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns('data_manual1'))
  )
}
    
#' data_manual Server Function
#'
#' @noRd 
mod_data_manual_server <- function(input, output, session){
  ns <- session$ns
  #### Data Manual-tab ####
  output$data_manual1 <- shiny::renderUI({
    list(
      HTML(
        paste0(
          "<h1> Data Manual Tab </h1>
           <p> <img src='www/AppIcon_BReasy_210x210mm_RGB.png' alt='Graphic cannot be displayed' width='350' height='350' align = 'right'>
           <h2> File Format and Structure </h2>
           <h3> File Format </h3>
           <p> BReasy is designed to upload a CSV file containing effect estimates for outcomes of interest.</p>
           
           <h3> </h3>
           <p>
            When uploading a CSV file the following formats are allowed and can be chosen in the app: </p>
              <p>  Separator between variables </p>
              <li>  Comma, Semicolon or Tab </li>
              <p>  Quote highlighting variables </p>
              <li>  None, Double Quote (",'"',") or Single Quote (') </li>
              <p> Decimal character </p>
              <li>  None, Point (.) or Comma (,) </li>
            In case an incorrect setting is chosen the app might run into an error. 
           </p>
           
           <h3> File Structure </h3>
           <p> In order to use the BReasy, the CSV file has to include the following variables: </p>
           
           <style>
            table, th, td {
              border: 1px solid black;
            }
           </style>
           <table style='width:50%'>
             <tr>
              <th style='width:10%;'>          </th>
              <th style='width:20%;'> Variable: </th>
              <th style='width:40%;'> Description: </th> 
              <th style='width:10%;'> Type: </th>
            </tr>
            <tr>
              <td rowspan='6'> Required Variables </td>
              <td> ESTIMATE </td>
              <td> Type of estimate as basis for comparison, e.g. crude incidence or Kaplan-Meier cumulative incidence </td>
              <td> Character </td>
            </tr>
            <tr>
              <td>ANALYSIS_SET</th>
              <td>Analyis set</td>
              <td>Character</td>
            </tr>
            <tr>
              <td> OUTCOME </td>
              <td> Outcome of interest </td>
              <td> Character </td>
            </tr>
            <tr>
              <td> EFFECT_xx </td>
              <td> Variable containing the value of the estimate used for comparison
                  xx indicates the estimate chosen:
                  ARD = Risk difference
                  EXCESS = Excess number of subjects
                  HR = Hazard Ratio
                  OR = Odds Ratio
                  RR = Relative risk </td>
              <td> Numeric </td>
            </tr>
            <tr>
              <td> LOWERxx </td>
              <td> Corresponding lower Confidence interval bound with xx indicating 1-alpha  </td>
              <td> Numeric </td>
            </tr>
            <tr>
              <td> UPPERxx </td>
              <td> Corresponding upper Confidence interval bound with xx indicating 1-alpha </td>
              <td> Numeric  </td>
            </tr>
            
            <tr>
              <td rowspan='10'> Optional variables </td>
              <td> NUMBER_EVENTS_VERUM </td>
              <td> Number of events in verum group for the combination of required variables estimate,
              outcome and analysis set and optional variables study, data scope, stratum, subgroup category </td>
              <td> Integer </td>
            </tr>
            
            <tr>
              <td>NUMBER_PATIENTS_VERUM </td>
              <td>Number of patients in the verum group for the combination of required variables analysis set and optional variables study, stratum and subgroup category </td>
              <td>Integer </td>
            </tr>
            
            <tr>
              <td>NUMBER_EVENTS_COMP </td>
              <td>Number of events in comparator group for the combination of required variables estimate, outcome and analysis set and optional variables study, data scope, stratum, subgroup category </td>
              <td> Integer</td>
            </tr>
            
            <tr>
              <td>NUMBER_PATIENTS_COMP </td>
              <td> Number of patients in the comparator group for the combination of required variables analysis set and optional variables study, stratum and subgroup category</td>
              <td> Integer</td>
            </tr>
            
            <tr>
              <td>STUDY/TRIALNO </td>
              <td> Study identifier </td>
              <td>Character </td>
            </tr>
            
            <tr>
              <td> DATA_SCOPE/AVISIT</td>
              <td> Data scope, e.g. treatment-emergent</td>
              <td> Character</td>
            </tr>
            
            <tr>
              <td>SUBGROUP </td>
              <td>Subgroup of Interest - Values of 'Any', 'All' or 'Overall' or 'None' can be used for overall estimates </td>
              <td>Character </td>
            </tr>
            
            <tr>
              <td>SUBLEVEL </td>
              <td> Subgroup category corresponding to SUBGROUP</td>
              <td>Character </td>
            </tr>
            
            <tr>
              <td>STRATUM </td>
              <td> Stratum</td>
              <td> Character</td>
            </tr>
            
            <tr>
              <td> NNT</td>
              <td> Number needed to treat (NNT) / Number needed to harm (NNH)</td>
              <td> Integer</td>
            </tr>
          </table>
           
           <h3> Important points to consider </h3>
           <p> If the data is stored in more than one file of the same format, it is possible to upload several CSV files into BReasy. </p>
          <p> The maximal file size is 100 MB.</p> 
         "
        )
      )
    )
  })
}
    
## To be copied in the UI
# mod_data_manual_ui("data_manual_ui_1")
    
## To be copied in the server
# callModule(mod_data_manual_server, "data_manual_ui_1")
 
