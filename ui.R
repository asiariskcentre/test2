library(shiny)
#library(leaflet)
library(plotGoogleMaps)
source('global.R')
library(rMaps)
library(leafletR)
library(rCharts)


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

  shinyUI(navbarPage("IARP BETA", id="nav",
                   
                   
                   # allow for overflow on the drop menus
                   # Include our custom CSS
                  tabPanel("Data Input", tags$head(includeCSS("styles.css")),
                            
                            
                            
                            
                           sidebarLayout(
                             sidebarPanel(
                              # Select user input
                                fileInput('UserInput', 'Choose / Drop file to upload', accept = c( 'text/csv', 'text/comma-separated-values', '.csv')),
                                
#                                tags$hr(),
#                                checkboxInput('header', 'Header', TRUE), radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'),','),
                              
                              # Contract Name  
                                textInput(inputId = 'Unique_Contract_Name', label = 'Contract Name', value = ""),
                                
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
                             tabPanel('Exposure Input', actionButton("Delete_ui_row", "Delete Row"), h4("  "), dataTableOutput('UserInput')), # tabPanel
                             tabPanel('Data Audit'    ,
                                      # Download Data Audit
                                        
                                        h1(' '),
                                        h4("Summary"),
                                        h1(' '),

                                       dataTableOutput('DataAudit'),
                                        
                                        h1(' '),
                                      # Download data audit
                                        downloadButton('Download_DataAuditSummary', 'Download Data Audit Summary'),
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

                 tabPanel("Exposure Dissaggregation", tags$head(includeCSS("styles.css")),
                          fluidRow(actionButton("Dissaggregate", "Dissaggregate")),
                          fluidRow(h4("Dissaggregated Exposure"), dataTableOutput('DisplayDissaggregated')),
                          fluidRow(downloadButton('Download_ExposureDissaggregation', 'Download Dissaggregated Exposure'))
                          ),        # Tab panel

                  
                 tabPanel("Loss Calculations", tags$head(includeCSS("styles.css")),
                          sidebarLayout(
                            sidebarPanel(actionButton("Simulation", "Calculate Loss")), #sidebarPanel

                            mainPanel(
                              tabsetPanel(
                                tabPanel('Historic Losses', dataTableOutput('HistoricLosses'), downloadButton('Download_historic_losses' , 'Download Historic Losses')), # tabPanel
                                tabPanel('Synthetic Losses', dataTableOutput('ModelledLosses'), downloadButton('Download_synthetic_losses', 'Download Synthetic Losses')) # tabPanel
                                ) # tabPanel
                              ) # tabsetPanel
                            )    # mainPanel
                          ),    # SidebarLayout
                            

                  tabPanel("Interactive Map", chartOutput("myChart", 'leaflet')         

                          
                 )        # Tab panel
                          
                          

  
)) #Nav bar page

