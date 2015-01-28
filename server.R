
library(shiny)
#library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(plotGoogleMaps)
source('global.R')
library(leafletR)
library(rMaps)


#...............................................................................
# Load databases
  CropSeasons     <-  readRDS('data/CropSeasons.Rds')
  Product_type.db <<- get_Product_type_db(CropSeasons)

  Districts       <-  readRDS('data/Districts.Rds')
  States          <-  readRDS('data/States.Rds')
  adminID.db      <<- get_adminID_db(Districts,States)

  Risk_Items      <-  readRDS('data/Risk_Items.Rds')
  Crops           <-  readRDS('data/Crops.Rds')
  Exposure.db     <<- get_exposure_db(Risk_Items, Crops, adminID.db)

  raw_historic    <-  readRDS('data/Risk_Items_YearWise_Historical_Yields.Rds')
  raw_synthetic   <-  readRDS('data/Risk_Items_YearWise_Synthetic_Yields.Rds')
  Historic_gy.db  <<- get_gy_db(raw_historic,  Risk_Items, adminID.db)
  Synthetic_gy.db <<- get_gy_db(raw_synthetic, Risk_Items, adminID.db)

  rm(CropSeasons, Districts, States, Risk_Items, Crops, raw_historic, raw_synthetic)
#...............................................................................


display.flag <<- 0

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

          #------------------------------------------------------------------------
          # read in a file and store in display array
            observe({
                       input$UserInput  # Do take a dependency on file input

                       inFile <- input$UserInput
                       if (is.null(inFile))
                       return(NULL)

                     #-------------------------------------------------------------------------------------------------
                     # Get and check user input file
                       raw_input  <<- read.csv(inFile$datapath, header = T, sep = ',', quote = input$quote)  # No dependency on input$dataset
                       Checked_raw_input <- Check_UserInput(raw_input, adminID.db, Exposure.db, Product_type.db)
                       print('Validated User Input ...........')
                     #-------------------------------------------------------------------------------------------------
                     # prepare data for UI Display
                       Display_Checked_raw_input <- Get_Display_UserInput(Checked_raw_input, adminID.db, Product_type.db)
                       print('Prepared User Input For Display ...........')
                       file_input <- data.frame(lapply(Display_Checked_raw_input, as.character), stringsAsFactors=FALSE)
                       if(display.flag == 1) {display_array <<- rbind(file_input, display_array)}
                       if(display.flag == 0) {display_array <<- file_input; display.flag <<- 1}

                      #-------------------------------------------------------------------------------------------------
                      # Output Data in to the UI 
#                         output$UserInput <- renderDataTable({return(display_array)}, options = list(orderClasses = TRUE))
                     
                     #reactive row-selection 
#                        rowSelect <<- reactive({
#                           if(is.null(input[["row"]])) 1 #initialize
#                           else as.numeric(input[["row"]])
#                           print(row)})
#                      
#                      
#                      # User-selected sorting of dataset
                          output$UserInput = renderDataTable({
                                    addRadioButtons <- paste0('<input type="radio" name="row', 1:nrow(display_array), '" value="', 1:nrow(display_array), '">',"")
                                    #Display table with radio buttons
                                     cbind(Pick=addRadioButtons, display_array[, , drop=FALSE])
                                  }, options = list(bSortClasses = TRUE, aLengthMenu = c(5, 25, 50), iDisplayLength = 25))
                     
                     

                      #-------------------------------------------------------------------------------------------------
#                      # perform data aufit and display in to the UI
                         data_audit_array <<- Perform_Data_Audit(display_array)
                         State = rownames(data_audit_array)
                         data_audit_display_array <<- cbind(State, format(data_audit_array, scientific=FALSE))
                         output$DataAudit <- renderDataTable({return(data_audit_display_array)}, options = list(orderClasses = TRUE))

                         
                         

                       # Pie Chart with Percentages
                         output$Data_Audit_LOB_Pie   <- renderPlot({LOB_Pie_Plot(data_audit_array)})
                         output$Data_Audit_State_TSI <- renderPlot({State_TSI_Plot(data_audit_array)})
                    })
          #------------------------------------------------------------------------
          
          #------------------------------------------------------------------------
          # Download Data Audit Summary
                      output$Download_DataAuditSummary <- downloadHandler(
                      filename = function() { paste('Data_Audit_Summary', '.csv', sep='') },
                      content  = function(file) {write.csv(data_audit_display_array, file)})
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
                       if(District_input == 'All'){District_input = NA}


                       single_entry_input                   <- isolate(cbind(ContractID_input, State_input, District_input, Crop_input, Season_input, TSI, EPI, PR))
                       Checked_single_entry_input.tmp       <-  as.data.frame(Check_SingleEntry_UserInput(single_entry_input,adminID.db,Exposure.db,Product_type.db))
                       Checked_single_entry_input           <-  data.frame(lapply(Checked_single_entry_input.tmp, as.character), stringsAsFactors=FALSE)
                       
                       
                       if(display.flag > 0) {display_array  <<- rbind(Checked_single_entry_input, display_array)}
                       if(display.flag == 0) {display_array <<- Checked_single_entry_input; display.flag <<- 1}
                       
                       data_audit_array <<- Perform_Data_Audit(display_array)
                       print(data_audit_array)
                     })

                   })
          #------------------------------------------------------------------------


          #------------------------------------------------------------------------
          # Display "display array" when go button is presses
            observe({
                       input$goButton
                       if (input$goButton == 0)
                       return()

#                       # display user input  
                        # isolate({output$UserInput <- renderDataTable({return(display_array)}, options = list(orderClasses = TRUE))}) #isolate

                         isolate({
                         output$UserInput = renderDataTable({
                                    addRadioButtons <- paste0('<input type="radio" name="row', 1:nrow(display_array), '" value="', 1:nrow(display_array), '">',"")
                                    cbind(Pick=addRadioButtons, display_array[, , drop=FALSE]) #Display table with radio buttons
                                    }, options = list(bSortClasses = TRUE, aLengthMenu = c(5, 25, 50), iDisplayLength = 25))}) #isolate
 
                         if(!is.null(data_audit_array))
                         {
 #                         # display data audit
                             State = rownames(data_audit_array)
                             data_audit_display_array  <- cbind(State, format(data_audit_array, scientific=FALSE))
                             isolate({output$DataAudit <- renderDataTable({return(data_audit_display_array)}, options = list(orderClasses = TRUE))}) #isolate

                           # Pie Chart with Percentages & barchart for state vs TSI
                             output$Data_Audit_LOB_Pie   <- renderPlot({LOB_Pie_Plot(data_audit_array)  })
                             output$Data_Audit_State_TSI <- renderPlot({State_TSI_Plot(data_audit_array)})
                         }
                   })
          #------------------------------------------------------------------------



          #------------------------------------------------------------------------
          # Display "Dissaggregate" when go button is presses
            observe({          
                     input$Dissaggregate
                     if (input$Dissaggregate == 0)
                     return()

                   # allow for district errors to pass through
                     display_array[display_array[,9]=='Crop by District not modelled',3] = NA
                     display_array[display_array[,9]=='Crop by District not modelled',9] <- 'Good'

                     display_array[display_array[,9]=='District mismatch',3] = NA
                     display_array[display_array[,9]=='District mismatch',9] <- 'Good'
                   
                     #display_array[display_array[,3]=='All',3] <- NA
                     #display_array[display_array[,3]=='<NA>',3] = NA

                     display_array = display_array[display_array[,9] == 'Good',]
                     display_array = Convert_Par_to_ID(display_array, adminID.db, Product_type.db)

                    #...............................................................................
                    # ASSUMPTION USER INPUT DOES NOT CONTAIN ANY UNMODELLED DISTRICTS ANY MORE
                       Exposure.db  <- get_mutually_exclusive_exposure(display_array, Exposure.db) # get mutually exclusive modelled states
                       Dissaggregated_exposure.db <- disaggregate_exposure(Exposure.db, display_array)
                      # Dissaggregated_exposure.db <- Dissaggregated_exposure.db[,c(-6,-7)]
                       Dissaggregated_exposure.db <<- as.data.frame(Dissaggregated_exposure.db)
                       Display_Dissaggregated_exposure.db <<- Convert_ID_to_Par_Dissagregate(Dissaggregated_exposure.db, adminID.db, Product_type.db)

                       Display_Dissaggregated_exposure.db       = Display_Dissaggregated_exposure.db[,c(-7)] #remove 'is modelled' tab
                       Display_Dissaggregated_exposure.db[,6]  <- format(round((as.numeric(as.character(Display_Dissaggregated_exposure.db[,6]))), 0), numeric = TRUE) 
                       Display_Dissaggregated_exposure.db[,7]  <- format(round((as.numeric(as.character(Display_Dissaggregated_exposure.db[,7]))), 0), numeric = TRUE)

                       Display_Dissaggregated_exposure.db[,6]  =  format(Display_Dissaggregated_exposure.db[,6], scientific = FALSE)
                       Display_Dissaggregated_exposure.db[,7]  =  format(Display_Dissaggregated_exposure.db[,7], scientific = FALSE)
                       Display_Dissaggregated_exposure.final     <<- Display_Dissaggregated_exposure.db[,-1] #remove contract name / id

                       isolate({output$DisplayDissaggregated   <- renderDataTable({return(Display_Dissaggregated_exposure.final)}, options = list(orderClasses = TRUE))}) #isolate
                   #...............................................................................
                  })
          #------------------------------------------------------------------------


                #------------------------------------------------------------------------
                # Download Dissaggregated Exposure
                  output$Download_ExposureDissaggregation <- downloadHandler(
                  filename = function() { paste('Dissaggregated_exposure', '.csv', sep='') },
                  content  = function(file) {write.csv(Display_Dissaggregated_exposure.final, file)})
                #------------------------------------------------------------------------


          #------------------------------------------------------------------------
          # Compute Simulation
            observe({          
                      input$Simulation
                      if (input$Simulation == 0)
                      return()

                     #...............................................................................
                     # Attach Guaranteed Yield
                       UserInput.db   <- Dissaggregated_exposure.db
                       Historic_gy.db  = Get_Guaranteed_gy(Historic_gy.db , UserInput.db, Exposure.db)
                       Synthetic_gy.db = Get_Guaranteed_gy(Synthetic_gy.db, UserInput.db, Exposure.db)

                     # Replace Synthetic Guaranteed GY by Historic Guaranteed GY
                       tmp.Historic_gy.db = Historic_gy.db[,-8:-9] #remove year and actual yield
                       tmp.Historic_gy.db = unique(tmp.Historic_gy.db)
 
                       x = merge(Synthetic_gy.db, tmp.Historic_gy.db, by=c('State_ID','District_ID','CropSeasonID'))
                       Synthetic_gy.db = x[,c(-10:-14)]#[,-13:-20]
                       colnames(Synthetic_gy.db) <- c('State_ID','District_ID','CropSeasonID','TSI','Modelled','Planted_Area','Indemnity','Year','Yield','Guaranteed_GY')
                     #.................................................................................

                     #...............................................................................
                     # Compute Indemnity Loss
                     # gy.db = Historic_gy.db
                       IND_LOSS_Historic_gy.db  <<- Compute_Indemnity_loss(Historic_gy.db)
                       IND_LOSS_Synthetic_gy.db <<- Compute_Indemnity_loss(Synthetic_gy.db)
                     #...............................................................................

                     #...............................................................................
                     # Compute Crop District Aggregation
                       loss_Historic_gy.db          <<- Compute_Crop_District_aggregate(IND_LOSS_Historic_gy.db)
                       loss_Synthetic_gy.db         <<- Compute_Crop_District_aggregate(IND_LOSS_Synthetic_gy.db)
                       Display_loss_Historic_gy.db  <<- Convert_ID_to_Par_Losses(loss_Historic_gy.db, adminID.db, Product_type.db)
                       Display_loss_Synthetic_gy.db <<- Convert_ID_to_Par_Losses(loss_Synthetic_gy.db, adminID.db, Product_type.db)
                     #...............................................................................

                     #...............................................................................
                     # Prepare data for output
                        Display_loss_Historic_gy.final  <<- prepare_loss_data_for_display(Display_loss_Historic_gy.db)
                        Display_loss_Synthetic_gy.final <<- prepare_loss_data_for_display(Display_loss_Synthetic_gy.db)

                        isolate({output$HistoricLosses  <- renderDataTable({return(Display_loss_Historic_gy.final)},  options = list(orderClasses = TRUE))}) #isolate
                        isolate({output$ModelledLosses  <- renderDataTable({return(Display_loss_Synthetic_gy.final)}, options = list(orderClasses = TRUE))}) #isolate
                     #...............................................................................
            })


                #------------------------------------------------------------------------
                # Download Historic Losses
                  output$Download_historic_losses <- downloadHandler(
                  filename = function() { paste('Historic_Losses', '.csv', sep='') },
                  content  = function(file) {write.csv(Display_loss_Historic_gy.final, file)})
                #------------------------------------------------------------------------

                #------------------------------------------------------------------------
                # Download Sythetic Losses
                   output$Download_synthetic_losses <- downloadHandler(
                   filename = function() { paste('Synthetic_Losses', '.csv', sep='') },
                   content  = function(file) {write.csv(Display_loss_Synthetic_gy.final, file)})
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


})