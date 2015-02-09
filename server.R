#---------------------------------------------------------------
# IARP Version 1.3
# Added user identified indemnity
# 9th February 2015
# Nirav Khimashia
#---------------------------------------------------------------

library(shiny)
#library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(plotGoogleMaps)
source('global.R')
source('global_check_user_input.R')
library(leafletR)
library(rMaps)
library(shinyIncubator)

Sys.setlocale(locale="English")


#...............................................................................
# Load databases
  DB_Message      =   paste('Libraries loaded - ', Sys.time()); print(DB_Message)
  CropSeasons     <-  readRDS('data/CropSeasons.Rds')
  Product_type.db <<- get_Product_type_db(CropSeasons)

  Districts       <-  readRDS('data/Districts.Rds')
  States          <-  readRDS('data/States.Rds')
  adminID.db      <<- get_adminID_db(Districts,States)

  Risk_Items      <-  readRDS('data/Risk_Items.Rds')
  Crops           <-  readRDS('data/Crops.Rds')
  Exposure.db     <<- get_exposure_db(Risk_Items, Crops, adminID.db)

  raw_WBCIS       <-  readRDS('data/Risk_Items_YearWise_LossCosts.Rds')
  raw_historic    <-  readRDS('data/Risk_Items_YearWise_Historical_Yields.Rds')
  raw_synthetic   <-  readRDS('data/Risk_Items_YearWise_Synthetic_Yields.Rds')
  WBCIS_gy.db     <<- get_gy_db(raw_WBCIS,  Risk_Items, adminID.db)
  Historic_gy.db  <<- get_gy_db(raw_historic,  Risk_Items, adminID.db)
  Synthetic_gy.db <<- get_gy_db(raw_synthetic, Risk_Items, adminID.db)
  DB_Message      =   paste('All Databases loaded and prepared - ', Sys.time()); print(DB_Message)
  rm(CropSeasons, Districts, States, Risk_Items, Crops, raw_historic, raw_synthetic)
#...............................................................................


display.flag <<- 0

# Define server logic required to draw a histogram
  shinyServer(function(input, output, session) {

    
         #------------------------------------------------------------------------
         # Clean all varibales and screen
           observe({
                   input$ClearDisplay
                   if (input$ClearDisplay == 0)
                   return()
                   isolate({
                     display.flag <<- 0; display_array <<- NULL
                     MNAISdata_audit_array <<- NULL; MNAISdata_audit_display_array <<- NULL
                     WBCISdata_audit_array <<- NULL; WBCISdata_audit_display_array <<- NULL
                     MNAIS_Dissaggregated_exposure.db <<- NULL; MNAIS_Display_Dissaggregated_exposure.db <<- NULL
                     WBCIS_Dissaggregated_exposure.db <<- NULL; WBCIS_Display_Dissaggregated_exposure.db <<- NULL
                     IND_LOSS_Historic_gy.db <<- NULL
                     IND_LOSS_Synthetic_gy.db <<- NULL
                     Display_IND_LOSS_Historic_gy.db <<- NULL
                     Display_IND_LOSS_Synthetic_gy.db <<- NULL
                     L1_loss_Historic_gy.final <<- NULL
                     L2_loss_Historic_gy.final <<- NULL
                     L3_loss_Historic_gy.final <<- NULL
                     L4_loss_Historic_gy.final <<- NULL
                     L1_loss_Synthetic_gy.final <<- NULL
                     L2_loss_Synthetic_gy.final <<- NULL
                     L3_loss_Synthetic_gy.final <<- NULL
                     L4_loss_Synthetic_gy.final <<- NULL
                     Historic_summary_display_final <<- NULL
                     Synthetic_summary_display_final <<- NULL
                     WBCIS.final <<- NULL
                     L1_WBCIS_loss.final <<- NULL
                     L2_WBCIS_loss.final <<- NULL
                     L3_WBCIS_loss.final <<- NULL
                     L4_WBCIS_loss.final <<- NULL
                     
                     
                   output$UserInput <- renderDataTable({return(NULL)}, options = list(orderClasses = TRUE))
                   output$MNAISDataAudit <- renderDataTable({return(NULL)}, options = list(orderClasses = TRUE))
                   output$WBCISDataAudit <- renderDataTable({return(NULL)}, options = list(orderClasses = TRUE))
                   output$MNAISDisplayDissaggregated   <-  renderDataTable({return(NULL)}, options = list(orderClasses = TRUE))
                   output$WBCISDisplayDissaggregated   <-  renderDataTable({return(NULL)}, options = list(orderClasses = TRUE))
                   output$HistoricLosses  <- renderDataTable({return(NULL)},  options = list(orderClasses = TRUE))
                   output$ModelledLosses  <- renderDataTable({return(NULL)}, options = list(orderClasses = TRUE))
                   output$WBCISLosses  <- renderDataTable({return(NULL)},  options = list(orderClasses = TRUE))
                   #output$Data_Audit_LOB_Pie   <- renderPlot({NULL)})
                   source('global.R')
                   })
                   })
  
      
      
          #------------------------------------------------------------------------
          # read in a file and store in display array
            observe({
                       input$UserInput  # Do take a dependency on file input

                       inFile <- input$UserInput
                       if (is.null(inFile))
                       return(NULL)
                       
                       #-------------------------------------------------------------------------------------------------
                       # Busy Animation
                       # Create a Progress object
                       progress <- shiny::Progress$new()
                       progress$set(message = "Computing ....", value = 0)
                       on.exit(progress$close())
                       
                       updateProgress <- function(value = NULL, detail = NULL) 
                       {if (is.null(value)) {value <- progress$getValue(); value <- value + (progress$getMax() - value) / 7; Sys.sleep(1)}
                       progress$set(value = value, detail = detail)}
                       
                       
                      
                           
                     #-------------------------------------------------------------------------------------------------
                     # Get and check user input file
                       raw_input  <<- read.csv(inFile$datapath, header = T, sep = ',', quote = input$quote)  # No dependency on input$dataset
                       if (is.function(updateProgress)) {updateProgress(detail = 'Validating User Input ...........')}
                       Checked_raw_input <- Check_UserInput(raw_input, adminID.db, Exposure.db, Product_type.db, Check_UserInput_Name_Mismatch,  Check_UserInput_Prepare_Exposure_db, Check_UserInput_modelled_adminlevel, Check_UserInput_TSI_check)
                       Message=paste('Validated User Input ...........', Sys.time()); print(Message)
                       if (is.function(updateProgress)) {updateProgress(detail = 'Validation Successful ...........')}
                       
                     #-------------------------------------------------------------------------------------------------
                     # prepare data for UI Display
                       file_input <- data.frame(lapply(Checked_raw_input, as.character), stringsAsFactors=FALSE)
                       if(display.flag == 1) {display_array <<- rbind(file_input, display_array)}
                       if(display.flag == 0) {display_array <<- file_input; display.flag <<- 1}
                       if (is.function(updateProgress)) {updateProgress(detail = 'Prepare data for UI Display ...........')}
                       

                      #-------------------------------------------------------------------------------------------------
                      # Output Data in to the UI 
                        output$UserInput <- renderDataTable({return(display_array)}, options = list(orderClasses = TRUE))

                      #---------------------------------------------------------------------------------------------
                      # perform MNAIS data audit and display in to the UI
                        MNAIS_display_array <- display_array[,-10]
                        MNAISdata_audit_array <<- as.data.frame(Perform_Data_Audit(MNAIS_display_array))
                        x_flag = 0

                        if(!is.null(MNAISdata_audit_array))
                           {
                          if (is.function(updateProgress)) {updateProgress(detail = 'MNAIS Data Audit computing ...........')}
                            State = rownames(MNAISdata_audit_array)
                            MNAISdata_audit_display_array <<- cbind(State, format(MNAISdata_audit_array, scientific=FALSE))
                            output$MNAISDataAudit <- renderDataTable({return(MNAISdata_audit_display_array)}, options = list(orderClasses = TRUE))
                            x_flag = 1
                            MNAISdata_audit_array <<- as.data.frame(MNAISdata_audit_array)
                            Message=paste('MNAIS Data Audit computed ....', Sys.time()); print(Message)
                            if (is.function(updateProgress)) {updateProgress(detail = 'MNAIS Data Audit computed ...........')}
                           }
                      
                       # perform WBCIS data audit and display in to the UI
                         WBCIS_display_array   <- display_array[,-9]
                         WBCISdata_audit_array <<- Perform_Data_Audit(WBCIS_display_array)
                         y_flag = 0                       

                         if(!is.null(WBCISdata_audit_array))
                           {
                           if (is.function(updateProgress)) {updateProgress(detail = 'MNAIS Data Audit computing ...........')}
                            State = rownames(WBCISdata_audit_array)
                            WBCISdata_audit_display_array <<- cbind(State, format(WBCISdata_audit_array, scientific=FALSE))
                            output$WBCISDataAudit <- renderDataTable({return(WBCISdata_audit_display_array)}, options = list(orderClasses = TRUE))
                            y_flag = 1
                            WBCISdata_audit_array <<- as.data.frame(WBCISdata_audit_array)
                            Message=paste('WBCIS Data Audit computed ....', Sys.time()); print(Message)
                            if (is.function(updateProgress)) {updateProgress(detail = 'WBCIS Data Audit computed ...........')}
                           }
                     
                         #if((x_flag == 1) && (y_flag == 0)){output$Data_Audit_LOB_Pie   <- renderPlot({LOB_Pie_Plot_1(MNAISdata_audit_array, "MNAIS Line of Business")})}
                         #if((x_flag == 0) && (y_flag == 1)){output$Data_Audit_LOB_Pie   <- renderPlot({LOB_Pie_Plot_1(WBCISdata_audit_array, "WBCIS Line of Business")})}
                         #if((x_flag == 1) && (y_flag == 1)){output$Data_Audit_LOB_Pie   <- renderPlot({LOB_Pie_Plot(MNAISdata_audit_array,WBCISdata_audit_array)})}
                         #if (is.function(updateProgress)) {updateProgress(detail = 'LOB Graphics computed ...........')}
                     #---------------------------------------------------------------------------------------------
                     
                      # Pie Chart with Percentages

                        #output$Data_Audit_State_TSI <- renderPlot({State_TSI_Plot(MNAISdata_audit_array, WBCISdata_audit_array)})
                    })
          #------------------------------------------------------------------------




          #------------------------------------------------------------------------
          # Download User Input
                      output$Download_DisplayArray <- downloadHandler(
                      filename = function()     { paste('Validated_User_input.csv', sep='') },
                      content  = function(file) {write.csv(display_array, file)})
          
          # Download Data Audit Summary
                      output$Download_MNAISDataAuditSummary <- downloadHandler(
                      filename = function() { paste('MNAIS_Data_Audit_Summary', '.csv', sep='') },
                      content  = function(file) {write.csv(MNAISdata_audit_display_array, file)})
          
          
                       output$Download_WBCISDataAuditSummary <- downloadHandler(
                       filename = function() { paste('WBCIS_Data_Audit_Summary', '.csv', sep='') },
                       content  = function(file) {write.csv(WBCISdata_audit_display_array, file)})
          #------------------------------------------------------------------------




          #------------------------------------------------------------------------
          # read in all single entry inputs and store in display array
            observe({
                         input$goButton

                         if (input$goButton == 0)
                         return()
                         isolate({

                         ContractID_input <- input$Unique_Contract_Name
                         State_input      <- input$Unique_States_input
                         District_input   <- input$Unique_District_input
                         Crop_input       <- input$Unique_Crop_input
                         Season_input     <- input$Unique_Season_input
                         TSI              <- input$TSI
                         EPI              <- input$EPI
                         PR               <- input$PR
                         #if(District_input == 'All'){District_input = NA}

                         single_entry_input                   <<- isolate(cbind(State_input, District_input, Crop_input, Season_input, TSI, EPI, PR))
                         Checked_single_entry_input.tmp       <-  as.data.frame(Check_UserInput(single_entry_input,adminID.db,Exposure.db,Product_type.db))
                         Checked_single_entry_input           <-  data.frame(lapply(Checked_single_entry_input.tmp, as.character), stringsAsFactors=FALSE)

                         if(display.flag > 0) {display_array  <<- rbind(Checked_single_entry_input, display_array)}
                         if(display.flag == 0) {display_array <<- Checked_single_entry_input; display.flag <<- 1}

                       #---------------------------------------------------------------------------------------------
                       # perform MNAIS data audit and display in to the UI
                         MNAIS_display_array <- display_array[,-9]
                         MNAISdata_audit_array <<- Perform_Data_Audit(MNAIS_display_array)
                         x_flag = 0

                         if(!is.null(MNAISdata_audit_array))
                           {
                             State = rownames(MNAISdata_audit_array)
                             MNAISdata_audit_display_array <<- cbind(State, format(MNAISdata_audit_array, scientific=FALSE))
                             output$MNAISDataAudit <- renderDataTable({return(MNAISdata_audit_display_array)}, options = list(orderClasses = TRUE))
                             x_flag = 1
                           }

                       # perform WBCIS data audit and display in to the UI
                         WBCIS_display_array   <- display_array[,-8]
                         WBCISdata_audit_array <<- Perform_Data_Audit(WBCIS_display_array)
                         y_flag = 0                       

                         if(!is.null(WBCISdata_audit_array))
                           {
                            State = rownames(WBCISdata_audit_array)
                            WBCISdata_audit_display_array <<- cbind(State, format(WBCISdata_audit_array, scientific=FALSE))
                            output$WBCISDataAudit <- renderDataTable({return(WBCISdata_audit_display_array)}, options = list(orderClasses = TRUE))
                            y_flag = 1
                           }
                       
                         if((x_flag == 1) && (y_flag == 0)){output$Data_Audit_LOB_Pie   <- renderPlot({LOB_Pie_Plot_1(MNAISdata_audit_array, "MNAIS Line of Business")})}
                         if((x_flag == 0) && (y_flag == 1)){output$Data_Audit_LOB_Pie   <- renderPlot({LOB_Pie_Plot_1(WBCISdata_audit_array, "WBCIS Line of Business")})}
                         if((x_flag == 1) && (y_flag == 1)){output$Data_Audit_LOB_Pie   <- renderPlot({LOB_Pie_Plot(MNAISdata_audit_array,WBCISdata_audit_array)})}
                       
                       #---------------------------------------------------------------------------------------------
                     })
                   })
          #------------------------------------------------------------------------


          #------------------------------------------------------------------------
          # Display "display array" when go button is presses
            observe({
                       input$goButton
                       if (input$goButton == 0)
                       return()

                        # display user input  
                         isolate({output$UserInput <- renderDataTable({return(display_array)}, options = list(orderClasses = TRUE))}) #isolate
                       


                       
                       #---------------------------------------------------------------------------------------------
                       # perform MNAIS data audit and display in to the UI
                         MNAIS_display_array <- display_array[,-9]
                         MNAISdata_audit_array <<- Perform_Data_Audit(MNAIS_display_array)
                         x_flag = 0

                         if(!is.null(MNAISdata_audit_array))
                           {
                            State = rownames(MNAISdata_audit_array)
                            MNAISdata_audit_display_array <<- cbind(State, format(MNAISdata_audit_array, scientific=FALSE))
                            output$MNAISDataAudit <- renderDataTable({return(MNAISdata_audit_display_array)}, options = list(orderClasses = TRUE))
                            x_flag = 1
                           }
                       
                       # perform WBCIS data audit and display in to the UI
                         WBCIS_display_array   <- display_array[,-8]
                         WBCISdata_audit_array <<- Perform_Data_Audit(WBCIS_display_array)
                         y_flag = 0                       

                         if(!is.null(WBCISdata_audit_array))
                            {
                              State = rownames(WBCISdata_audit_array)
                              WBCISdata_audit_display_array <<- cbind(State, format(WBCISdata_audit_array, scientific=FALSE))
                              output$WBCISDataAudit <- renderDataTable({return(WBCISdata_audit_display_array)}, options = list(orderClasses = TRUE))
                              y_flag = 1
                             }

                         if((x_flag == 1) && (y_flag == 0)){output$Data_Audit_LOB_Pie   <- renderPlot({LOB_Pie_Plot_1(MNAISdata_audit_array, "MNAIS Line of Business")})}
                         if((x_flag == 0) && (y_flag == 1)){output$Data_Audit_LOB_Pie   <- renderPlot({LOB_Pie_Plot_1(WBCISdata_audit_array, "WBCIS Line of Business")})}
                         if((x_flag == 1) && (y_flag == 1)){output$Data_Audit_LOB_Pie   <- renderPlot({LOB_Pie_Plot(MNAISdata_audit_array,WBCISdata_audit_array)})}
                       
                       #---------------------------------------------------------------------------------------------
 
#                          if(!is.null(data_audit_array))
#                          {
#                            # display data audit
#                              State = rownames(data_audit_array)
#                              data_audit_display_array  <- cbind(State, format(data_audit_array, scientific=FALSE))
#                              isolate({output$DataAudit <- renderDataTable({return(data_audit_display_array)}, options = list(orderClasses = TRUE))}) #isolate
# 
#                            # Pie Chart with Percentages & barchart for state vs TSI
#                              output$Data_Audit_LOB_Pie   <- renderPlot({LOB_Pie_Plot(data_audit_array)  })
#                              output$Data_Audit_State_TSI <- renderPlot({State_TSI_Plot(data_audit_array)})
#                          }
                   })
          #------------------------------------------------------------------------



          #------------------------------------------------------------------------
          # Display "Dissaggregate" when go button is presses
            observe({
                      input$Dissaggregate
                      if (input$Dissaggregate == 0)
                      return()

                      #-------------------------------------------------------------------------------------------------
                      # Busy Animation
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "Computing ....", value = 0)
                      on.exit(progress$close())
                      
                      updateProgress <- function(value = NULL, detail = NULL) 
                      {if (is.null(value)) {value <- progress$getValue(); value <- value + (progress$getMax() - value) / 8; Sys.sleep(1)}
                       progress$set(value = value, detail = detail)}



                      # allow for district errors to pass through
                       MNAIS_display_array <- display_array[,-10]; if (is.function(updateProgress)) {updateProgress(detail = 'MNAIS array filtered ...........')}
                       WBCIS_display_array <- display_array[,-9]; if (is.function(updateProgress)) {updateProgress(detail = 'WBCIS array filtered ...........')}

                       #options(warn=-1)
                        if(!is.null(MNAIS_display_array))
                            {

                              MNAIS_display_array  = MNAIS_display_array[MNAIS_display_array[,9] == 'Good',]
                          
                             if(nrow(MNAIS_display_array) > 0)
                                 { 
                                     MNAIS_display_array = as.data.frame(Convert_Par_to_ID(MNAIS_display_array, adminID.db, Product_type.db))
                                     Message=paste('MNAIS Parameter to ID Conversion successful ....', Sys.time()); print(Message)
                                     
                                 #...............................................................................
                                 # ASSUMPTION USER INPUT DOES NOT CONTAIN ANY UNMODELLED DISTRICTS ANY MORE
                                   MNAIS_Exposure.db                            <-  get_mutually_exclusive_exposure(MNAIS_display_array, Exposure.db) # get mutually exclusive modelled states
                                   MNAIS_Dissaggregated_exposure.db             <-  disaggregate_exposure(MNAIS_Exposure.db, MNAIS_display_array, Aggregate_user_exposure, district_state_level_disaggregation, district_level_disaggregation, state_level_disaggregation)
                                   Message=paste('MNAIS Dissaggregation successful ....', Sys.time()); print(Message)
                                   if (is.function(updateProgress)) {updateProgress(detail = 'MNAIS Dissaggregation successful ...........')}
                                 
                                   MNAIS_Dissaggregated_exposure.db             <<- as.data.frame(MNAIS_Dissaggregated_exposure.db)
                                   MNAIS_Display_Dissaggregated_exposure.db     <<- Convert_ID_to_Par_Dissagregate(MNAIS_Dissaggregated_exposure.db, adminID.db, Product_type.db) 
                                   MNAIS_Display_Dissaggregated_exposure.db     =   MNAIS_Display_Dissaggregated_exposure.db[,c(-6), drop=FALSE] #remove 'is modelled' tab
                                   MNAIS_Display_Dissaggregated_exposure.db[,5] <-  format(round((as.numeric(as.character(MNAIS_Display_Dissaggregated_exposure.db[,5]))), 0), numeric = TRUE) 
                                   MNAIS_Display_Dissaggregated_exposure.db[,6] <-  format(round((as.numeric(as.character(MNAIS_Display_Dissaggregated_exposure.db[,6]))), 0), numeric = TRUE)
                                   MNAIS_Display_Dissaggregated_exposure.db[,5] =   format(MNAIS_Display_Dissaggregated_exposure.db[,5], scientific = FALSE)
                                   MNAIS_Display_Dissaggregated_exposure.db[,6] =   format(MNAIS_Display_Dissaggregated_exposure.db[,6], scientific = FALSE)
                                   MNAIS_Display_Dissaggregated_exposure.final  <<-  MNAIS_Display_Dissaggregated_exposure.db[,, drop = FALSE]
                                   isolate({output$MNAISDisplayDissaggregated   <-  renderDataTable({return(MNAIS_Display_Dissaggregated_exposure.final)}, options = list(orderClasses = TRUE))}) #isolate
                                #...............................................................................
                                }
                            }

                         if(!is.null(WBCIS_display_array))
                            {

                             WBCIS_display_array  = WBCIS_display_array[WBCIS_display_array[,9] == 'Good',]
                           
                             if(nrow(WBCIS_display_array) > 0)
                                {
                                  WBCIS_display_array = as.data.frame(Convert_Par_to_ID(WBCIS_display_array, adminID.db, Product_type.db))
                                  Message=paste('WBCIS Parameter to ID Conversion successful ....', Sys.time()); print(Message)
                                  
#                               #...............................................................................
#                               # ASSUMPTION USER INPUT DOES NOT CONTAIN ANY UNMODELLED DISTRICTS ANY MORE
                                  WBCIS_Exposure.db                            <- get_mutually_exclusive_exposure(WBCIS_display_array, Exposure.db)
                                  WBCIS_Dissaggregated_exposure.db             <-  disaggregate_exposure_WBCIS(WBCIS_Exposure.db, WBCIS_display_array,Aggregate_user_exposure, district_state_level_disaggregation, district_level_disaggregation, state_level_disaggregation)
                                  Message=paste('WBCIS Dissaggregation successful ....', Sys.time()); print(Message)
                                  if (is.function(updateProgress)) {updateProgress(detail = 'WBCIS Dissaggregation successful ...........')}

                                  WBCIS_Dissaggregated_exposure.db             <<- as.data.frame(WBCIS_Dissaggregated_exposure.db, drop = FALSE)
                                  WBCIS_Display_Dissaggregated_exposure.db     <<- Convert_ID_to_Par_Dissagregate(WBCIS_Dissaggregated_exposure.db, adminID.db, Product_type.db) 
                                  WBCIS_Display_Dissaggregated_exposure.db     =   WBCIS_Display_Dissaggregated_exposure.db[,c(-6), drop = FALSE] #remove 'is modelled' tab
                                  WBCIS_Display_Dissaggregated_exposure.db[,5] <-  format(round((as.numeric(as.character(WBCIS_Display_Dissaggregated_exposure.db[,5]))), 0), numeric = TRUE) 
                                  WBCIS_Display_Dissaggregated_exposure.db[,6] <-  format(round((as.numeric(as.character(WBCIS_Display_Dissaggregated_exposure.db[,6]))), 0), numeric = TRUE)
                                  WBCIS_Display_Dissaggregated_exposure.db[,5] =   format(WBCIS_Display_Dissaggregated_exposure.db[,5], scientific = FALSE)
                                  WBCIS_Display_Dissaggregated_exposure.db[,6] =   format(WBCIS_Display_Dissaggregated_exposure.db[,6], scientific = FALSE)
                                  WBCIS_Display_Dissaggregated_exposure.final  <<- WBCIS_Display_Dissaggregated_exposure.db[,, drop = FALSE]
                                  isolate({output$WBCISDisplayDissaggregated   <-  renderDataTable({return(WBCIS_Display_Dissaggregated_exposure.final)}, options = list(orderClasses = TRUE))}) #isolate
                              #...............................................................................
                        }     }

                     #options(warn=0)
                      
                  })
          #------------------------------------------------------------------------

          #------------------------------------------------------------------------
          # Download Dissaggregated Exposure
            output$Download_MNAIS_Disaggregated_Exposure <- downloadHandler(
            filename = function() { paste('MNAIS_Dissaggregated_exposure.csv', sep='') },
            content  = function(file) {write.csv(MNAIS_Display_Dissaggregated_exposure.final, file)})
          #------------------------------------------------------------------------

          #------------------------------------------------------------------------
          # Download Dissaggregated Exposure
            output$Download_WBCIS_Disaggregated_Exposure <- downloadHandler(
            filename = function() { paste('WBCIS_Dissaggregated_exposure.csv', sep='') },
            content  = function(file) {write.csv(WBCIS_Display_Dissaggregated_exposure.final, file)})
          #------------------------------------------------------------------------


          #------------------------------------------------------------------------
          # Compute Simulation
            observe({
                      input$MNAIS_Simulation
                      if(input$MNAIS_Simulation == 0)
                      return()
                      
                      #-------------------------------------------------------------------------------------------------
                      # Busy Animation
                      # Create a Progress object
                      progress <- shiny::Progress$new()
                      progress$set(message = "Computing ....", value = 0)
                      on.exit(progress$close())
                      
                      updateProgress <- function(value = NULL, detail = NULL) 
                      {if (is.null(value)) {value <- progress$getValue(); value <- value + (progress$getMax() - value) / 5; Sys.sleep(1)}
                       progress$set(value = value, detail = detail)}
                      
                      

                     #...............................................................................
                     # Attach Guaranteed Yield
                       UserInput.db    <- MNAIS_Dissaggregated_exposure.db
                       Historic_gy.db  =  Get_Guaranteed_gy(Historic_gy.db , UserInput.db, Exposure.db); if (is.function(updateProgress)) {updateProgress(detail = 'MNAIS - Attached Guaranteed Yield to Historic exposure ...........')}
                       Synthetic_gy.db =  Get_Guaranteed_gy(Synthetic_gy.db, UserInput.db, Exposure.db); if (is.function(updateProgress)) {updateProgress(detail = 'MNAIS - Attached Guaranteed Yield to Synthetic exposure ...........')}
                       Message=paste('MNAIS - Attach Guaranteed Yield ....', Sys.time()); print(Message)

                     #.................................................................................

                     #...............................................................................
                     # Compute Indemnity Loss
                     # gy.db = Historic_gy.db
                     if (is.function(updateProgress)) {updateProgress(detail = 'MNAIS - Indemnity loss computing ...............')}
                        IND_LOSS_Historic_gy.db          <<- Compute_Indemnity_loss(Historic_gy.db)
                        IND_LOSS_Synthetic_gy.db         <<- Compute_Indemnity_loss(Synthetic_gy.db)
                        Display_IND_LOSS_Historic_gy.db  <<- Convert_ID_to_Par_detailed_Losses(IND_LOSS_Historic_gy.db, adminID.db, Product_type.db)
                        Display_IND_LOSS_Synthetic_gy.db <<- Convert_ID_to_Par_detailed_Losses(IND_LOSS_Synthetic_gy.db, adminID.db, Product_type.db)
                        Message=paste('MNAIS - Indemnity loss computed ....', Sys.time()); print(Message)
                        if (is.function(updateProgress)) {updateProgress(detail = 'MNAIS - Indemnity loss computed ...............')}
                     
                        LOSS_Historic_gy.db        <-  Compute_aggregate(IND_LOSS_Historic_gy.db, Product_type.db, adminID.db)
                        LOSS_Synthetic_gy.db       <-  Compute_aggregate(IND_LOSS_Synthetic_gy.db, Product_type.db, adminID.db)
                        Message=paste('MNAIS - Level Aggregation computed ....', Sys.time()); print(Message)
                       if (is.function(updateProgress)) {updateProgress(detail = 'MNAIS - Level Aggregation computed ...............')}

                        L1_loss_Historic_gy.final  <<- unique(LOSS_Historic_gy.db[LOSS_Historic_gy.db[,1]   == 'level1',])
                        L2_loss_Historic_gy.final  <<- unique(LOSS_Historic_gy.db[LOSS_Historic_gy.db[,1]   == 'level2',])
                        L3_loss_Historic_gy.final  <<- unique(LOSS_Historic_gy.db[LOSS_Historic_gy.db[,1]   == 'level3',])
                        L4_loss_Historic_gy.final  <<- unique(LOSS_Historic_gy.db[LOSS_Historic_gy.db[,1]   == 'level4',])

                        L1_loss_Synthetic_gy.final <<- unique(LOSS_Synthetic_gy.db[LOSS_Synthetic_gy.db[,1] == 'level1',])
                        L2_loss_Synthetic_gy.final <<- unique(LOSS_Synthetic_gy.db[LOSS_Synthetic_gy.db[,1] == 'level2',])
                        L3_loss_Synthetic_gy.final <<- unique(LOSS_Synthetic_gy.db[LOSS_Synthetic_gy.db[,1] == 'level3',])
                        L4_loss_Synthetic_gy.final <<- unique(LOSS_Synthetic_gy.db[LOSS_Synthetic_gy.db[,1] == 'level4',])
                     #...............................................................................

                     #...............................................................................
                     # Display Losses  
                        Historic_summary_display        <-  Compute_display_aggregate(IND_LOSS_Historic_gy.db, Product_type.db, adminID.db)
                        State                           =   rownames(Historic_summary_display)
                        Historic_summary_display_final  <<- cbind(State, format(Historic_summary_display, scientific=FALSE))

                        Synthetic_summary_display       <-  Compute_display_aggregate(IND_LOSS_Synthetic_gy.db, Product_type.db, adminID.db)
                        State                           =   rownames(Synthetic_summary_display)
                        Synthetic_summary_display_final <<- cbind(State, format(Synthetic_summary_display, scientific=FALSE))

                        isolate({output$HistoricLosses  <- renderDataTable({return(Historic_summary_display_final)},  options = list(orderClasses = TRUE))}) #isolate
                        isolate({output$ModelledLosses  <- renderDataTable({return(Synthetic_summary_display_final)}, options = list(orderClasses = TRUE))}) #isolate
                     #...............................................................................
                  })
          #------------------------------------------------------------------------ 





                    #------------------------------------------------------------------------
                    # Compute Simulation
                        observe({
                           input$WBCIS_Simulation
                           if(input$WBCIS_Simulation == 0)
                           return()
                           
                           #-------------------------------------------------------------------------------------------------
                           # Busy Animation
                           # Create a Progress object
                           progress <- shiny::Progress$new()
                           progress$set(message = "Computing ....", value = 0)
                           on.exit(progress$close())
                           
                           updateProgress <- function(value = NULL, detail = NULL) 
                           {if (is.null(value)) {value <- progress$getValue(); value <- value + (progress$getMax() - value) / 2; Sys.sleep(1)}
                            progress$set(value = value, detail = detail)}
  
                    #...............................................................................
                    # Calculate WBCIS Loss
                           UserInput.db    <- as.data.frame(WBCIS_Display_Dissaggregated_exposure.db)
                           WBCIS_GY        <- Convert_ID_to_Par_WBCIS(WBCIS_gy.db, adminID.db, Product_type.db)
                           t               =  merge(UserInput.db, WBCIS_GY, by = c('State_Name','District_name','Crop_Name','Season_Name'))
                           WBCIS.tmp       =  t[,c(-6,-7,-8)]
                           WBCIS.tmp[,7]   =  WBCIS.tmp[,7] / 100
                           TSI             =  as.numeric(as.character(WBCIS.tmp[,5]))
                           LossP           =  as.numeric(as.character(WBCIS.tmp[,7]))
                           Indemnity_Loss  =  TSI * LossP
                           WBCIS.final     <<-  cbind(WBCIS.tmp, Indemnity_Loss)
                           Message=paste('WBCIS - Loss Computed ....', Sys.time()); print(Message)
                           if (is.function(updateProgress)) {updateProgress(detail = 'WBCIS - Loss Computed .................')}
                      #...............................................................................
   
                          Aggregated_WBCIS_Losses <- Compute_aggregate_WBCIS(WBCIS.final, Product_type.db, adminID.db)
                          Message=paste('WBCIS - Level Aggregation computed ....', Sys.time()); print(Message)
                          if (is.function(updateProgress)) {updateProgress(detail = 'WBCIS - Level Aggregation computed .................')}
                          L1_WBCIS_loss.final     <<- unique(Aggregated_WBCIS_Losses[Aggregated_WBCIS_Losses[,1]   == 'level1',])
                          L2_WBCIS_loss.final     <<- unique(Aggregated_WBCIS_Losses[Aggregated_WBCIS_Losses[,1]   == 'level2',])
                          L3_WBCIS_loss.final     <<- unique(Aggregated_WBCIS_Losses[Aggregated_WBCIS_Losses[,1]   == 'level3',])
                          L4_WBCIS_loss.final     <<- unique(Aggregated_WBCIS_Losses[Aggregated_WBCIS_Losses[,1]   == 'level4',])

                          isolate({output$WBCISLosses  <- renderDataTable({return(Aggregated_WBCIS_Losses)},  options = list(orderClasses = TRUE))}) #isolate
                        #------------------------------------------------------------------------
                               })

                        #------------------------------------------------------------------------
                        # Download Historic Losses
                          output$Download_WBCIS_l1 <- downloadHandler(
                          filename = function()     { paste('Level1_WBCIS_Losses.csv', sep='') },
                          content  = function(file) {write.csv(L1_WBCIS_loss.final, file)})

                          output$Download_WBCIS_l2 <- downloadHandler(
                          filename = function()     { paste('Level2_WBCIS_Losses.csv', sep='') },
                           content  = function(file) {write.csv(L2_WBCIS_loss.final, file)})

                          output$Download_WBCIS_l3 <- downloadHandler(
                          filename = function()     { paste('Level3_WBCIS_Losses.csv', sep='') },
                          content  = function(file) {write.csv(L3_WBCIS_loss.final, file)})

                          output$Download_WBCIS_l4 <- downloadHandler(
                          filename = function()     { paste('Level4_WBCIS_Losses.csv', sep='') },
                          content  = function(file) {write.csv(L4_WBCIS_loss.final, file)})

                          output$Download_WBCIS_l5 <- downloadHandler(
                          filename = function()     { paste('Level5_WBCIS_Losses.csv', sep='') },
                          content  = function(file) {write.csv(WBCIS.final, file)})
                        #------------------------------------------------------------------------
 
                        #------------------------------------------------------------------------
                        # Download Historic Losses
                          output$Download_historic_l1 <- downloadHandler(
                          filename = function()     { paste('Level1_Historic_Losses', '.csv', sep='') },
                          content  = function(file) {write.csv(L1_loss_Historic_gy.final, file)})
 
                          output$Download_historic_l2 <- downloadHandler(
                          filename = function()     { paste('Level2_Historic_Losses', '.csv', sep='') },
                          content  = function(file) {write.csv(L2_loss_Historic_gy.final, file)})
 
                          output$Download_historic_l3 <- downloadHandler(
                          filename = function()     { paste('Level3_Historic_Losses', '.csv', sep='') },
                          content  = function(file) {write.csv(L3_loss_Historic_gy.final, file)})
 
                          output$Download_historic_l4 <- downloadHandler(
                          filename = function()     { paste('Level4_Historic_Losses', '.csv', sep='') },
                          content  = function(file) {write.csv(L4_loss_Historic_gy.final, file)})
 
                          output$Download_historic_l5 <- downloadHandler(
                          filename = function()     { paste('Level5_Historic_Losses', '.csv', sep='') },
                          content  = function(file) {write.csv(Display_IND_LOSS_Historic_gy.db, file)})
                       #------------------------------------------------------------------------

                       #------------------------------------------------------------------------
                       # Download Synthetic Losses
                          output$Download_synthetic_l1 <- downloadHandler(
                          filename = function()     { paste('Level1_Synthetic_Losses', '.csv', sep='') },
                          content  = function(file) {write.csv(L1_loss_Synthetic_gy.final, file)})

                          output$Download_synthetic_l2 <- downloadHandler(
                          filename = function()     { paste('Level2_Synthetic_Losses', '.csv', sep='') },
                          content  = function(file) {write.csv(L2_loss_Synthetic_gy.final, file)})

                          output$Download_synthetic_l3 <- downloadHandler(
                          filename = function()     { paste('Level3_Synthetic_Losses', '.csv', sep='') },
                          content  = function(file) {write.csv(L3_loss_Synthetic_gy.final, file)})

                          output$Download_synthetic_l4 <- downloadHandler(
                          filename = function()     { paste('Level4_Synthetic_Losses', '.csv', sep='') },
                          content  = function(file) {write.csv(L4_loss_Synthetic_gy.final, file)})

                          output$Download_synthetic_l5 <- downloadHandler(
                          filename = function()     { paste('Level5_Synthetic_Losses', '.csv', sep='') },
                          content  = function(file) {write.csv(Display_IND_LOSS_Synthetic_gy.db, file)})
                       #------------------------------------------------------------------------
                   
                       #------------------------------------------------------------------------
                       # Display Interactive Map
                         output$myChart <- renderMap({
                         map3 <- Leaflet$new()
                         map3$tileLayer(provider = "MapQuestOpen.OSM")
                         map3$set(width = 1600, height = 800)
                         map3$setView(c(20,78), zoom = 4)
                         map3
                         })
                       #------------------------------------------------------------------------
})#on.exit(rm(list=ls(all=TRUE)))