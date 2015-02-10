library(shiny)
#library(leaflet)
library(plotGoogleMaps)
source('global.R')
library(rMaps)
library(leafletR)
library(rCharts)
library(shinyIncubator)



#...............................................................................
# Load databases
  CropSeasons     <-  readRDS('data/CropSeasons.Rds')
  Product_type.db <<- get_Product_type_db(CropSeasons)

  Districts       <-  readRDS('data/Districts.Rds')
  States          <-  readRDS('data/States.Rds')
  adminID.db      <<- get_adminID_db(Districts,States)

  rm(CropSeasons, Districts, States)
#.........................................................................



  Unique_States_i      = unique(adminID.db$State_Name)
  Unique_States        = lapply(Unique_States_i, as.character)

  Unique_Districts_i   = unique(adminID.db$District_Name)
  Unique_Districts     = lapply(Unique_Districts_i, as.character)


  Unique_ProductType_i = unique(Product_type.db$ProductType)
  Unique_ProductType   = lapply(Unique_ProductType_i, as.character)

  Unique_SeasonType_i  = unique(Product_type.db$SeasonType)
  Unique_SeasonType    = lapply(Unique_SeasonType_i, as.character)

  shinyUI(navbarPage("IARP BETA V 1.3", id="nav",


                   # allow for overflow on the drop menus
                   # Include our custom CSS
                  tabPanel("Data Input", tags$head(includeCSS("styles.css")),




                           sidebarLayout(
                             sidebarPanel(
                               
                               # Action Button
                               actionButton("ClearDisplay", "Clear Previous Analysis"),
                               
                               # Contract Name  
                               textInput(inputId = 'Unique_Contract_Name', label = 'Contract Name', value = ""),
                               
                               # Select user input
                                fileInput('UserInput', 'Choose / Drop file to upload', accept = c( 'text/csv', 'text/comma-separated-values', '.csv')),

                              # enter state name
                                selectInput(inputId  = 'Unique_States_input',   label = 'States',    choices   = Unique_States,      selected  = NULL,  multiple  = FALSE,  selectize = TRUE, width = NULL),

                              # enter District name
                                selectInput(inputId  = 'Unique_District_input', label = 'Districts', choices   = Unique_Districts,   selected  = 'All',    multiple  = FALSE,  selectize = TRUE, width = NULL),

                              # enter Crop name
                                selectInput(inputId  = 'Unique_Crop_input',     label = 'Crop',      choices   = Unique_ProductType, selected  = NULL,  multiple  = FALSE,  selectize = TRUE, width = NULL),

                              # enter Season name
                                selectInput(inputId  = 'Unique_Season_input',   label = 'Season',    choices   = Unique_SeasonType,   selected  = NULL, multiple  = FALSE,  selectize = TRUE, width  = NULL),

                              # enter TSI
                                numericInput(inputId = 'TSI', label = 'Sum Insured', 0 , min = 0,  max = NA, step = NA),

                              # enter EPI
                                numericInput(inputId = 'EPI', label = 'Premium',  0   , min = 0,  max = NA, step = NA),

                              # enter Premium Rate
                                numericInput(inputId = 'PR', label = 'Premium Rate (%)', 0 , min = 0,  max = 100, step = NA),

                              # Action Button
                                actionButton("goButton", "Add Entry")#,

#                               # Enter row to delete
#                                 numericInput(inputId = 'row_number_to_delete', label = 'Delete Row Number', 0 , min = 0,  max = NA, step = NA),
#
#                               # Action Button
#                                 actionButton("Delete_ui_row", "Delete Row")


                       ), #sidebarPanel

                       mainPanel(
                          tabsetPanel(
                             tabPanel('Exposure Input', h4("  "), dataTableOutput('UserInput'), downloadButton('Download_DisplayArray', 'Download User Input')), # tabPanel
                             tabPanel('MNAIS Data Audit'    ,
                                      # Download Data Audit

                                        h1(' '),
                                        h4("Summary"),
                                        h1(' '),

                                       dataTableOutput('MNAISDataAudit'),

                                        h1(' '),
                                      # Download data audit
                                        downloadButton('Download_MNAISDataAuditSummary', 'Download MNAIS Data Audit Summary'),
                                         h1(' '),
                                         h1(' '),
                                         h1(' ')

                                      ),
                             
                             tabPanel('WBCIS Data Audit'    ,
                                      # Download Data Audit
                                      
                                      h1(' '),
                                      h4("Summary"),
                                      h1(' '),
                                      
                                      dataTableOutput('WBCISDataAudit'),
                                      
                                      h1(' '),
                                      # Download data audit
                                      downloadButton('Download_WBCISDataAuditSummary', 'Download WBCIS Data Audit Summary'),
                                      h1(' '),
                                      h1(' '),
                                      h1(' ')
                                      
                             ),

                             tabPanel('LOB and State Aggregations'    ,
                                       h4("LOB Aggregations"),
                                       plotOutput("Data_Audit_LOB_Pie",   height = "700px"),
                                       h4("State Aggregations"),
                                       plotOutput("Data_Audit_State_TSI", height = "700px")
                                     ) # tabPanel




                          ) # tabsetPanel
                       )    # mainPanel
                       )    # SidebarLayout

                  ),        # Tab panel

#                  tabPanel("Exposure Dissaggregation", tags$head(includeCSS("styles.css")),
#                           fluidRow(actionButton("Dissaggregate", "Dissaggregate")),
#                           fluidRow(h4("Dissaggregated Exposure"), dataTableOutput('DisplayDissaggregated')),
#                           fluidRow(downloadButton('Download_ExposureDissaggregation', 'Download Dissaggregated Exposure'))
#                           ),        # Tab panel



                 tabPanel("Exposure Dissaggregation", tags$head(includeCSS("styles.css")),
                          sidebarLayout(sidebarPanel(actionButton("Dissaggregate",  "Dissaggregate")), #sidebarPanel

                          mainPanel(
                              tabsetPanel(
                                  tabPanel('MNAIS Disaggregated Exposure', dataTableOutput('MNAISDisplayDissaggregated'),h1(' '),
                                           downloadButton('Download_MNAIS_Disaggregated_Exposure' , 'Download MNAIS Disaggregated Exposure'),h1(' ')),

                                  tabPanel('WBCIS Disaggregated Exposure', dataTableOutput('WBCISDisplayDissaggregated'),h1(' '),
                                           downloadButton('Download_WBCIS_Disaggregated_Exposure' , 'Download WBCIS Disaggregated Exposure'),h1(' '))

                                          )# tabPanel
                                  )# tabsetPanel
                                )# mainPanel
                         ),# SidebarLayout


                 tabPanel("MNAIS Loss Calculations", tags$head(includeCSS("styles.css")),
                          sidebarLayout(
                            sidebarPanel(actionButton  ('MNAIS_Simulation'      , 'Calculate Summarized Loss'       ) ,h1(' '),h1(' '),h1(' '),h1(' '),
                                         downloadButton('Download_historic_l1'  , 'Download level 1 Historic Losses') ,h1(' '),
                                         downloadButton('Download_historic_l2'  , 'Download level 2 Historic Losses') ,h1(' '),
                                         downloadButton('Download_historic_l3'  , 'Download level 3 Historic Losses') ,h1(' '),
                                         downloadButton('Download_historic_l4'  , 'Download level 4 Historic Losses') ,h1(' '),
                                         downloadButton('Download_historic_l5'  , 'Download level 5 Historic Losses') ,h1(' '),h1(' '),h1(' '),h1(' '),
                                         downloadButton('Download_synthetic_l1' , 'Download level 1 Synthetic Losses'),h1(' '),
                                         downloadButton('Download_synthetic_l2' , 'Download level 2 Synthetic Losses'),h1(' '),
                                         downloadButton('Download_synthetic_l3' , 'Download level 3 Synthetic Losses'),h1(' '),
                                         downloadButton('Download_synthetic_l4' , 'Download level 4 Synthetic Losses'),h1(' '),
                                         downloadButton('Download_synthetic_l5' , 'Download level 5 Synthetic Losses'),h1(' '),h1(' '),h1(' '),h1(' ')
                                         ),#sidebarPanel

                            mainPanel(
                              tabsetPanel(
                                tabPanel('Historic Losses', h4("Total Indemnity Loss / Total TSI") ,dataTableOutput('HistoricLosses')), # tabPanel

                                tabPanel('Synthetic Losses', h4("Total Indemnity Loss / Total TSI"), dataTableOutput('ModelledLosses')) # tabPanel
                                ) # tabPanel
                              ) # tabsetPanel
                            ) # mainPanel
                          ),# SidebarLayout

                tabPanel("WBCIS Loss Calculations", tags$head(includeCSS("styles.css")),
                        sidebarLayout(
                          sidebarPanel(actionButton  ('WBCIS_Simulation'   , 'Calculate WBCIS Loss') ,h1(' '),h1(' '),h1(' '),h1(' '),
                                       downloadButton('Download_WBCIS_l1'  , 'Download level 1 WBCIS Losses') ,h1(' '),
                                       downloadButton('Download_WBCIS_l2'  , 'Download level 2 WBCIS Losses') ,h1(' '),
                                       downloadButton('Download_WBCIS_l3'  , 'Download level 3 WBCIS Losses') ,h1(' '),
                                       downloadButton('Download_WBCIS_l4'  , 'Download level 4 WBCIS Losses') ,h1(' '),
                                       downloadButton('Download_WBCIS_l5'  , 'Download level 5 WBCIS Losses') ,h1(' '),h1(' '),h1(' '),h1(' ')
                                      ),#sidebarPanel

                         mainPanel(
                            tabsetPanel(
                              tabPanel('WBCIS Losses', h4("Detailed Aggregated Losses") ,dataTableOutput('WBCISLosses')) # tabPanel

                               )# tabPanel
                             )# tabsetPanel
                           )# mainPanel
                        ),# SidebarLayout



                  tabPanel("Interactive Map", chartOutput("myChart", 'leaflet'))        # Tab panel
)) #Nav bar page

