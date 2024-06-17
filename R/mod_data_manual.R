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
            
            <ul>
              <li>  Separator between variables
              <ul>
              <li>  Comma, Semicolon or Tab </li>
              </ul>
              </li>
              <li>  Quote highlighting variables
              <ul>
              <li>  None, Double Quote (",'"',") or Single Quote (') </li>
              </ul>
              </li>
              <li> Decimal character
              <ul>
              <li>  None, Point (.) or Comma (,) </li>
              </ul>
              </li>
           </ul>
              
            <p> In case an incorrect setting is chosen the app might run into an error.</p>
           
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
              <td>ANALYSIS_SET</td>
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
              <td> Variable containing the value of the estimate used for comparison of 
                  treatment groups group for the combination of required variables estimate,
                  outcome and analysis set and optional variables study, data scope, stratum, subgroup category.
                  xx indicates the estimate chosen:
                  ARD = Absolute Risk Difference,
                  IRD = Incidence Rate Difference,
                  CID = Cumulative Incidence Difference,
                  HR = Hazard Ratio,
                  EXCESS = Excess number of subjects with events </td>
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
              <td>DAY</td>
              <td> The study day on which the cumulative incidence difference is supposed to be calculated. </td>
              <td>Integer </td>
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
           <p> The maximal file size is 1200 MB.</p> 
           <p>The flag for the analysis set variable should be coded as “Y” for character variable or as “1” for numeric variable.</p>
           
           
           
           
           
           
           
           
           
           
           
           
           
           <h3> File creation tab </h3>
           <p> To generate the CSV file ADAM analysis data sets can be uploaded with the following requirements: </p>
           
           <style>
            table, th, td {
              border: 1px solid black;
            }
           </style>
           <table style='width:50%'>
             <tr>
              <th style='width:10%;'> Dataset: </th>
              <th style='width:40%;'> Variables: </th>
              <th style='width:20%;'> Type: </th> 
              <th style='width:20%;'> Requirment: </th>
            </tr>
            <tr>
              <td rowspan='6'> ADTTE </td>
              <td> Study identifier, e.g. STUDYID </td>
              <td> Character </td>
              <td> Required </td>
            </tr>
            <tr>
              <td> Unique subject identifier, e.g. USUBJID </td>
              <td>Character</td>
              <td>Required</td>
            </tr>
            <tr>
              <td> Outcome </td>
              <td> Character </td>
              <td> Required </td>
            </tr>
            <tr>
              <td> Analysis value (Relative day of event) </td>
              <td> Numeric </td>
              <td> Required </td>
            </tr>
            <tr>
              <td> Event identifier (censor variable) </td>
              <td> Numeric </td>
              <td> Required </td>
            </tr>
            <tr>
              <td> Data scope (Analysis visit variable) </td>
              <td> Numeric/Character </td>
              <td> Optional </td>
            </tr>
            
            <tr>
              <td rowspan='10'> ADSL </td>
              <td> Study identifier, e.g. STUDYID </td>
              <td> Character </td>
              <td> Required </td>
            </tr>
            <tr>
              <td> Unique subject identifier, e.g. USUBJID  </td>
              <td> Character </td>
              <td> Required </td>
            </tr>
            <tr>
              <td> Treatment group </td>
              <td> Numeric/Character </td>
              <td> Required </td>
            </tr>
            <tr>
              <td> Analysis set </td>
              <td> Numeric/Character </td>
              <td> Required </td>
            </tr>
            <tr>
              <td> Subgroup </td>
              <td> Numeric/Character </td>
              <td> Optional </td>
            </tr>
            <tr>
              <td> Stratum </td>
              <td> Numeric/Character </td>
              <td> Optional </td>
            </tr>
        </table>
        <p> In case only ADTTE is uploaded, Analysis Set & Treatment group variables are additionally required in ADTTE. </p>
           
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
 
