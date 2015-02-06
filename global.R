 #...............................................................................
 # Check all CSV user input for valid and invalid entires
   Check_UserInput <- function(UserInput.db,adminID.db,Exposure.db,Product_type.db)
        {      
   #...............................................................................
   # FUNCTION DATA_AUDIT
   # Create function to check User Contract for any errors
   # INPUT:  AdminID.db, ProductID.db, SeasonID.db, MNAIS_Terms.db, UserInput.db
   # Output: Acccepted_UserInput.db
   #...............................................................................
   #
   #  a) check if all  State_ID & District_ID are valid.
   #  b) Check if all Crop_ID's and Season_ID's are valid.
   #  c) Check if District_ID's, Crop_ID's and Season_ID's are valid as per the MNAIS Term database.
   #  d) Check if all reported entries are modelled crops per district and season (Check with TOM)
   #  c) Check if [TSI] or [EPI and Premium rate] are provided
   #  d) If all 3 (TSI, EPI & Premium rate) are provided, then check if  EPI/ Premium Rate = approximate(TSI) (decide threshold for "approximate". suggested values could be with 0.5% of the difference)
   #  e) check if premium rate > 1
   #  f) If EPI & TSI are provided check if EPI >= TSI
   #  g) Identify entries where TSI is not reported, and compute TSI (FORMULA: EPI / Premium rate = TSI)
   #  h) Input data audit
   #      h-a) report all accepted and rejected entries as a summary table (csv) by STATE, LOB, SEASON, TSI and EPI
   #      h-b) Dump all unaccepted entries in a csv "data dump")
   #  * removeblank lines
   #
   #  NOTE: AdminID, Exposure and Product_type will be assumed to be clean
   #..........................................................................................................
   
   # UserInput.db = raw_input
   rowL            = nrow(UserInput.db)
   `%ni%`          <-Negate(`%in%`)
   UserInput.db    = as.data.frame(UserInput.db)
   
   #..........................................................................................................
   #  a) check if all  State_ID & District_ID are valid.
   #  b) Check if all Crop_ID's and Season_ID's are valid.
      Admin_State          =   as.character((unique(adminID.db[,2])))
      Admin_District       =   as.character((unique(adminID.db[,3])))
      Prod_Crop            =   as.character((unique(Product_type.db[,2])))
      Prod_Season          =   as.character((unique(Product_type.db[,3])))

      State_mismatch       <-  UserInput.db[as.character(UserInput.db[,1]) %ni% Admin_State,]
      UserInput.db         <-  UserInput.db[as.character(UserInput.db[,1]) %in% Admin_State,]

      District_mismatch    <-  UserInput.db[as.character(UserInput.db[,2]) %ni% Admin_District,]
      UserInput.db         <-  UserInput.db[as.character(UserInput.db[,2]) %in% Admin_District,]

      Crop_mismatch        <-  UserInput.db[as.character(UserInput.db[,3]) %ni% Prod_Crop,]
      UserInput.db         <-  UserInput.db[as.character(UserInput.db[,3]) %in% Prod_Crop,]

      Season_mismatch      <-  UserInput.db[as.character(UserInput.db[,4]) %ni% Prod_Season,]
      UserInput.db         <-  UserInput.db[as.character(UserInput.db[,4]) %in% Prod_Season,]


   # store all mismatches
      Column_names_output <- c('State_Name','District_Name','Crop','Season','TSI','EPI','Premium_rate','MNAIS_Check','WBCIS_Check') 
   
     if(nrow(State_mismatch)    > 0) {State_mismatch     <- cbind(State_mismatch,    'State mismatch'   , 'State mismatch')   ; colnames(State_mismatch)    <- Column_names_output} 
     if(nrow(District_mismatch) > 0) {District_mismatch  <- cbind(District_mismatch, 'District mismatch', 'District mismatch'); colnames(District_mismatch) <- Column_names_output}
     if(nrow(Crop_mismatch)     > 0) {Crop_mismatch      <- cbind(Crop_mismatch,     'Crop mismatch'    , 'Crop mismatch')    ; colnames(Crop_mismatch)     <- Column_names_output}
     if(nrow(Season_mismatch)   > 0) {Season_mismatch    <- cbind(Season_mismatch,   'Season mismatch'  , 'Season mismatch')  ; colnames(Season_mismatch)   <- Column_names_output}
   #..........................................................................................................

   
   #..........................................................................................................
   #  c) Check if District_ID's, Crop_ID's and Season_ID's are valid as per the MNAIS Term database.
   #  d) Check if all reported entries are modelled crops per district and season (Check with TOM)
  
   #add names to exposure db parameters
     State_adminDB          = unique(adminID.db[,c(-1,-3,-5)])
     District_adminDB       = adminID.db[,c(-1,-2,-4)]
     CropSeasonDB           = cbind(as.character(Product_type.db[,2]),  as.character(Product_type.db[,3]), as.numeric(Product_type.db[,4]))
     colnames(CropSeasonDB) <- c('Crop', 'Season', 'CropSeasonID')
     x = merge(Exposure.db, State_adminDB, by=c('State_ID'))
     x = merge(x, District_adminDB, by=c('District_ID'))
     x = merge(x, CropSeasonDB, by=c('CropSeasonID'))
     Exposure.db <- cbind(x[,11:14], x[,4:10])
     Exposure.db <- Exposure.db[,c(-5,-7,-8,-10,-11)]
     colnames(Exposure.db) <- c('State_Name','District_Name','Crop','Season','Is_MNAIS_Modeled','Is_WBCIS_Modeled')
   
   #prepare Exposure.db for entries where district has not been stated
     state_levels <- unique(as.character(Exposure.db$State_Name))
     db_flag = 0

     for(i in 1:length(state_levels))
        {
         st_l        = state_levels[i]
         state.Exposure.db.db = Exposure.db[as.character(Exposure.db[,1]) == st_l,]
         crop_levels = unique(as.character(state.Exposure.db.db$Crop))

         for(j in 1:length(crop_levels))
            {
             cr_l       = crop_levels[j]
             crop.Exposure.db.db = state.Exposure.db.db[as.character(state.Exposure.db.db[,3]) == cr_l,]
             season_levels = unique(as.character(state.Exposure.db.db$Season))

             for(k in 1:length(season_levels))
                {
                 sn_l = season_levels[k]
                 season.Exposure.db.db = crop.Exposure.db.db[crop.Exposure.db.db[,4] == sn_l,]

                 mnais <- sum(season.Exposure.db.db$Is_MNAIS_Modeled)
                 wbcis <- sum(season.Exposure.db.db$Is_WBCIS_Modeled)

                 if(mnais > 0){season.Exposure.db.db[,5] = 1}
                 if(wbcis > 0){season.Exposure.db.db[,6] = 1}

                 season.Exposure.db.db[,2] = 'All'

                 season.Exposure.db.db = unique(season.Exposure.db.db)
                 if(db_flag == 1){ final.Exposure.db <- rbind(final.Exposure.db, season.Exposure.db.db)}    
                 if(db_flag == 0){ final.Exposure.db <- season.Exposure.db.db; db_flag = 1    }
             } } }

    Exposure.db = rbind(Exposure.db,final.Exposure.db )

   #merge with user input

    colnames(UserInput.db) <- c('State_Name','District_Name','Crop','Season','TSI','EPI','PR')
    ui = merge(UserInput.db, Exposure.db, by=c('State_Name','District_Name','Crop','Season'))
    ui = cbind(ui, ui[,8:9])
    colnames(ui) <- c('State_Name','District_Name','Crop','Season','TSI','EPI','Premium_rate','Is_MNAIS_Modeled','Is_WBCIS_Modeled','State_Is_MNAIS_Modeled','State_Is_WBCIS_Modeled')

    state_levels <- unique(as.character(ui$State_Name))
    db_flag = 0
    
    for(i in 1:length(state_levels))
       {
         st_l        = state_levels[i]
         state.ui.db = ui[as.character(ui[,1]) == st_l,]
         crop_levels = unique(as.character(state.ui.db$Crop))
         
         for(j in 1:length(crop_levels))
         {
           cr_l       = crop_levels[j]
           crop.ui.db = state.ui.db[as.character(state.ui.db[,3]) == cr_l,]
           season_levels = unique(as.character(state.ui.db$Season))

           for(k in 1:length(season_levels))
           {
             sn_l = season_levels[k]
             season.ui.db = crop.ui.db[crop.ui.db[,4] == sn_l,]
             
             if(nrow(season.ui.db)> 0)
               {
                 mnais <- sum(season.ui.db$State_Is_MNAIS_Modeled)
                 wbcis <- sum(season.ui.db$State_Is_WBCIS_Modeled)
                 
                 if(mnais > 0){season.ui.db[,10] = 1}
                 if(wbcis > 0){season.ui.db[,11] = 1}
               } 

             if(db_flag == 1){ final.ui <- rbind(final.ui, season.ui.db)}    
             if(db_flag == 0){ final.ui <- season.ui.db; db_flag = 1    }
       } } }
   
    ui = cbind(final.ui, final.ui[,10:11])
   
   colnames(ui) <- c('State_Name','District_Name','Crop','Season','TSI','EPI','Premium_rate',
   'Is_MNAIS_Modeled','Is_WBCIS_Modeled','State_Is_MNAIS_Modeled','State_Is_WBCIS_Modeled',
   'MNAIS_Check', 'WBCIS_Check')
   
    #put in flags
    ui[ui[,8] == 0,  12] <- 'Crop by District not modelled'
    ui[ui[,9] == 0,  13] <- 'Crop by District not modelled'
   
    ui[ui[,10] == 0, 12] <- 'Crop by State not modelled'
    ui[ui[,11] == 0, 13] <- 'Crop by State not modelled'
   
    
    UserInput.db = as.data.frame(ui[,c(-8,-9,-10,-11)])
    UserInput.db[UserInput.db[,8] == 1,8] <- 'Good'
    UserInput.db[UserInput.db[,9] == 1,9] <- 'Good'
   #..........................................................................................................
   
   
   
   #..........................................................................................................
   #  c) Check if [TSI] or [EPI and Premium rate] are provided
   #  d) If all 3 (TSI, EPI & Premium rate) are provided, then check if  EPI/ Premium Rate = approximate(TSI) (decide threshold for "approximate". suggested values could be with 0.5% of the difference)
   #  e) check if premium rate > 1 or less than 0
   #  f) If EPI & TSI are provided check if EPI >= TSI
   #  g) Identify entries where TSI is not reported, and compute TSI (FORMULA: EPI / Premium rate = TSI)
     
     if (nrow(UserInput.db) > 0)
        { 
         for(i in 1:nrow(UserInput.db))
            {
              ratio.diff = 0
              TSI      = as.numeric(as.character(UserInput.db[i,5]))
              EPI      = as.numeric(as.character(UserInput.db[i,6]))
              PR       = as.numeric(as.character(UserInput.db[i,7]))
              
              if(is.na(TSI) || (TSI == 0)){TSI_flag = 0; TSI = 0}
              if(is.na(EPI) || (EPI == 0)){EPI_flag = 0; EPI = 0}
              if(is.na(PR)  || (PR == 0)) {PR_flag  = 0; PR = 0}
              
              if(TSI > 0) {TSI_flag = 1}
              if(EPI > 0) {EPI_flag = 1}
              if(PR > 0)  {PR_flag  = 1}


              Not_Blank = TSI_flag + EPI_flag + PR_flag 
              if(Not_Blank == 0) {UserInput.db[i,8:9] = 'TSI / EPI & Premium not reported'}

              EPI_PR = EPI_flag + PR_flag
              if((TSI_flag == 0) && (EPI_PR == 1)){UserInput.db[i,8:9] = 'Insufficient TSI Computation Parameters'}

              if(PR_flag == 1) {if (as.numeric(PR) > 1) {UserInput.db[i,8:9] = 'Premium rate greater than 100%'}}
              if(PR_flag == 1) {if (as.numeric(PR) < 0) {UserInput.db[i,8:9] = 'Premium rate less than 0%'}}

              if((EPI_flag == 1) && (TSI_flag == 1)) {if (EPI >= TSI) {UserInput.db[i,8:9] = 'Premium Income greater than Total Sum Insured'}}

              if((EPI_flag == 1) && (TSI_flag == 1) && (PR_flag == 1))
                  {
                    tmp.tsi    = as.numeric(EPI) / as.numeric(PR)
                    diff.tsi   = as.numeric(TSI) - tmp.tsi
                    ratio.diff = diff.tsi/as.numeric(TSI)
         
                    if(ratio.diff > 0.05) {UserInput.db[i,8:9] = 'Difference between computed TSI (EPI * Premium Rate) and reported TSI greater than 5%'}
                  }
       
              if((EPI_flag == 1) && (TSI_flag == 0) && (PR_flag == 1))
                  {
                   tmp.tsi = as.numeric(EPI) / as.numeric(PR)
                   UserInput.db[i,5] = as.numeric(tmp.tsi)
                  }
           }
        }
   #..........................................................................................................
   
   
   
   
   
   
   
   #..........................................................................................................
   # combine all array's
   
   
   final_checked_userinput.db <- rbind(State_mismatch,
                                       District_mismatch,
                                       Crop_mismatch,
                                       Season_mismatch,
                                       UserInput.db)

   return(final_checked_userinput.db)
   #..........................................................................................................
   
 }
 #...............................................................................
 
 #...............................................................................
 # Compute data audit output
   Perform_Data_Audit <- function(display_array)
        {
         #...............................................................................
         # isolate good user entries
           display_array[display_array[,8]=='Crop by District not modelled',8] <- 'Good'
           display_array[display_array[,8]=='District mismatch',8] <- 'Good'

           Data_Audit_Name_Array <<- display_array[display_array[,8] == 'Good',]
           Data_Audit_Name_Array <<- Data_Audit_Name_Array[,-8]
         #...............................................................................


         if(nrow(Data_Audit_Name_Array) > 0)
               {
               #...............................................................................
               # Aggregate state by crop
                 State_levels <- lapply(unique(Data_Audit_Name_Array$State_Name), as.character)
                 Crop_levels  <- lapply(unique(Data_Audit_Name_Array$Crop), as.character)
                 cropl        = length(Crop_levels)
                 output_array <- array(, dim=c(length(State_levels),cropl))


                  for(j in 1:length(State_levels))
                     {
                       state_id = toString(State_levels[j])
                       state.db = Data_Audit_Name_Array[Data_Audit_Name_Array[,1] == state_id,]
 
 
                       for(k in 1:length(Crop_levels))
                          {
                           crop_id = toString(Crop_levels[k])
                           crop.db = state.db[state.db[,3] == crop_id,]

                           if(nrow(crop.db) > 0)
                             {
                              Total_TSI = as.numeric(crop.db[,5])
                              output_array[j,k] <- sum(as.data.frame(Total_TSI))
                             }
                         }
                     }

                 colnames(output_array) <- c(Crop_levels)
                 rownames(output_array) <- c(State_levels)

                 data_audit_array <- as.data.frame(output_array)
                 return(data_audit_array)
               }
              #...............................................................................
       }
 #...............................................................................

 #...............................................................................
 # Convert all state, district and crop season to ID's
   Convert_Par_to_ID <- function(display_array, adminID.db, Product_type.db)
       {
  
        #display_array=MNAIS_display_array
        
       # Attach Crop Names to the array
         CropNames  =  as.data.frame(unique(Product_type.db[,c(-1,-5,-6)]))
         cropdb     <- unique(cbind(as.character(CropNames[,1]), 
                                    as.character(CropNames[,2]),
                                    as.numeric(as.character(CropNames[,3]))))

         colnames(cropdb) <- c('Crop','Season', 'CropSeasonID')
         cropmerged       =  merge(display_array, cropdb, by=c('Crop', 'Season'))



       # Attach State Names to the Array
         StateID.db  =  unique(adminID.db[,c(-1, -3, -5)])
         State_DistrictID.db  =  unique(adminID.db[,c(-1)])

         dist_not_exists  <- cropmerged[is.na(cropmerged[,4]),]
         dist_exists      <- cropmerged[!is.na(cropmerged[,4]),]

         if(nrow(dist_exists) > 0)     {State_district_crop_merged  =  merge(dist_exists, State_DistrictID.db, by=c('State_Name', 'District_Name'))}
         if(nrow(dist_not_exists) > 0) 
            {
             State_crop_merged = merge(dist_not_exists, StateID.db, by=c('State_Name'))
             State_crop_merged = cbind(State_crop_merged, NA)
            }


       if((nrow(dist_exists) > 0)  && (nrow(dist_not_exists) > 0)) {final = cbind(State_district_crop_merged, State_crop_merged)}
       if((nrow(dist_exists) > 0)  && (nrow(dist_not_exists) == 0)){final = State_district_crop_merged}
       if((nrow(dist_exists) == 0) && (nrow(dist_not_exists) > 0)) {final = State_crop_merged}

      # create a output array  
        converted <- cbind(final[,10:11], final[,9], final[,5:7])
   
 
        colnames(converted) <- c('State_ID','District_ID','CropSeasonID','TSI','EPI','Premium_rate')
   
   
#    #...............................................................................
#    # Compute tsi
#    for(i in 1:nrow(converted))
#    {
#      TSI = converted[i,4]; if(!is.na(TSI)){if(TSI == 0){TSI = NA}}
#      EPI = converted[i,5]; if(!is.na(EPI)){if(EPI == 0){EPI = NA}}
#      PR  = converted[i,6]; if(!is.na(PR)) {if(PR == 0) {PR = NA} }
#      
#      TSI
#    }
#    #...............................................................................
   
   return(converted)
 }
 #...............................................................................
 
 #...............................................................................
 # Convert all state, district and crop season to ID's
 Convert_ID_to_Par_Dissagregate <- function(Dissaggregated_exposure.db, adminID.db, Product_type.db)
 {
   #  Dissaggregated_exposure.db = WBCIS_Dissaggregated_exposure.db
   StateID.Array    = unique(adminID.db[,c(-1,-3,-5)])
   DistrictID.Array = unique(adminID.db[,c(-1,-2,-4)])
   
   tmp.ID <- array(, dim=c(nrow(Dissaggregated_exposure.db),4))
   
   for(i in 1:nrow(Dissaggregated_exposure.db))
   {
     State        = lapply(Dissaggregated_exposure.db[i,1], as.character)
     District     = lapply(Dissaggregated_exposure.db[i,2], as.character)
     Crop         = lapply(Dissaggregated_exposure.db[i,3], as.character)
     
     
     StateID      = StateID.Array   [StateID.Array[,2]    %in% State,   ] 
     if(!is.na(District)){DistrictID  = DistrictID.Array[DistrictID.Array[,2] %in% District,]}
     
     CropID       = Product_type.db[as.numeric(Product_type.db[,4]) %in% as.numeric(Crop),]
     
     tmp.ID[i,1]  = toString(lapply(StateID[1], as.character))
     if(!is.na(District)){ tmp.ID[i,2] = toString(lapply(DistrictID[1], as.character))}
     tmp.ID[i,3]  = toString(lapply(CropID[2], as.character))
     tmp.ID[i,4]  = toString(lapply(CropID[3], as.character))
   }
   
   
   
   
   converted = cbind(tmp.ID, Dissaggregated_exposure.db[,4:7])
   colnames(converted) <- c('State_Name','District_name',
                            'Crop_Name', 'Season_Name','TSI',
                            'Modelled', 'Planted_Area', 'Indemnity')
   
   
   

   
   
   
   return(converted)
 }
 #...............................................................................
 
 #...............................................................................
 # Convert all state, district and crop season to ID's
   Convert_ID_to_Par_Losses <- function(x, adminID.db, Product_type.db)
     {
       StateID.Array    = unique(adminID.db[,c(-1,-3,-5)])
       DistrictID.Array = unique(adminID.db[,c(-1,-2,-4)])

       tmp.ID <- array(, dim=c(nrow(x),4))

       for(i in 1:nrow(x))
          {
            State    = lapply(x[i,1], as.character)
            District = lapply(x[i,2], as.character)
            Crop     = lapply(x[i,3], as.character)

            StateID      = StateID.Array   [StateID.Array[,2] %in% State,]
            if(District != "<NA>"){DistrictID  = DistrictID.Array[DistrictID.Array[,2] %in% District,]}

            CropID       = Product_type.db[as.numeric(Product_type.db[,4]) %in% as.numeric(Crop),]

            tmp.ID[i,1]  = toString(lapply(StateID[1], as.character))
            if(District != "<NA>"){ tmp.ID[i,2] = toString(lapply(DistrictID[1], as.character))}
            tmp.ID[i,3]  = toString(lapply(CropID[2], as.character))
            tmp.ID[i,4]  = toString(lapply(CropID[3], as.character))
          }

         converted = cbind(tmp.ID, x[,4:ncol(x)])
         colnames(converted) <- c('State_Name','District_name',
                                   'Crop_Name', 'Season_Name','TSI',
                                   'Modelled', 'Planted_Area', 'Indemnity', "Avg_Actual_GY",
                                   "Guaranteed_GY", "Threshold_GY", "Avg_Shortfall_GY", "N",
                                   "LossN", "Loss_Sum", "Loss_min", "Loss_max", 'Burning_Cost')
         return(converted)
       }
 #...............................................................................
 
 #...............................................................................
 # Convert all state, district and crop season to ID's
   Convert_ID_to_Par_detailed_Losses <- function(x, adminID.db, Product_type.db)
     {
       #x=MNAIS_Dissaggregated_exposure.db
       StateID.Array    = unique(adminID.db[,c(-1,-3,-5)])
       DistrictID.Array = unique(adminID.db[,c(-1,-2,-4)])
   
       tmp.ID <- array(, dim=c(nrow(x),4))
   
       for(i in 1:nrow(x))
          {
           State    = as.numeric(as.character(x[i,1]))
           District = lapply(x[i,2], as.character)
           Crop     = lapply(x[i,3], as.character)
     
           StateID      = StateID.Array   [StateID.Array[,2] %in% State,]
           if(District != "<NA>"){DistrictID  = DistrictID.Array[DistrictID.Array[,2] %in% District,]}
     
           CropID       = Product_type.db[as.numeric(Product_type.db[,4]) %in% as.numeric(Crop),]
     
           tmp.ID[i,1]  = toString(lapply(StateID[1], as.character))
           if(District != "<NA>"){ tmp.ID[i,2] = toString(lapply(DistrictID[1], as.character))}
           tmp.ID[i,3]  = toString(lapply(CropID[2], as.character))
           tmp.ID[i,4]  = toString(lapply(CropID[3], as.character))
         }
   
        converted = cbind(tmp.ID, x[,4:ncol(x)])
        colnames(converted) <- c('State_Name','District_name', 'Crop_Name', 'Season_Name','TSI',
                                  'Modelled', 'Planted_Area', 'Indemnity', 'Year', 'Actual_GY',
                                  'Guaranteed_GY', 'Threshold_GY', 'Shortfall_GY', 'Indemnity_loss')
        return(converted)
       }
 #...............................................................................
    
#...............................................................................
# get mutually exclusive modelled states
# ASSUMPTION USER INPUT DOES NOT CONTAIN ANY UNMODELLED DISTRICTS ANY MORE
  get_mutually_exclusive_exposure <- function(UserInput.db, Exposure.db)
       {
        db.flag = 0
        UI_CropSeasonID     = unique(UserInput.db$CropSeasonID)
        UI_StateID          = unique(UserInput.db$State_ID)

        for(i in 1:length(UI_CropSeasonID))
           {
            CS_id       = UI_CropSeasonID[i]
            Crop.db     <- Exposure.db[Exposure.db[,3] == CS_id,]
           

            for(j in 1:length(UI_StateID))
                {
                   st_id         = UI_StateID[j]
                   state_crop.db <- Crop.db[Crop.db[,2] == st_id,]
 
                   if(db.flag == 1) {tmp.Exposure.db = rbind(tmp.Exposure.db, state_crop.db)}
                   if(db.flag == 0) {tmp.Exposure.db = state_crop.db; db.flag = 1}
                 }
            }

         Exposure.db = tmp.Exposure.db
         return(Exposure.db)
        }
#...............................................................................

#...............................................................................
# Disaggregate exposure
  disaggregate_exposure <- function(Exposure.db, UserInput.db)
      {
        
    #   UserInput.db = MNAIS_display_array
    
      #aggregate data at respective admin level to avoid 
        x=UserInput.db[,c(-5,-6)] #remove EPI and PR
        agg_flag = 0
        
        x[x == -9999] <- NA
        
        CropSeason_levels  = unique(x$CropSeasonID)
        for(i in 1:length(CropSeason_levels))
            {
             cs_id = as.numeric(as.character(CropSeason_levels[i]))
             cropseason.db = x[x[,3] == cs_id,]
             state_levels     = unique(cropseason.db$State_ID)

             for(j in 1:length(state_levels))
                {
                 s_id     = as.numeric(as.character(state_levels[j]))
                 state.db = cropseason.db[cropseason.db[,1] == s_id,]
  
                 #state.db[,2][is.na(state.db[,2])] <- -9999
                 District_levels  = unique(state.db$District_ID)

                for(k in 1:length(District_levels))
                   {

                     d_id = as.numeric(as.character(District_levels[k]))
                     district.db = state.db[state.db[,2] == d_id,]

                     sumTSI = sum(as.numeric(district.db[,4]))
                     tmp = district.db[1,]
                     tmp[,4] = sumTSI
                     #tmp[tmp == -9999] <- NA

                     if(agg_flag == 1) {tmp.x = rbind(tmp.x, tmp)}
                     if(agg_flag == 0) {tmp.x = tmp; agg_flag = 1}
                    }
                  }
            }
    
          UserInput.db = tmp.x

         #Initiate data
          db.flag = 0
          UI_CropSeasonID  = unique(UserInput.db$CropSeasonID)

          for(i in 1:length(UI_CropSeasonID))
             {
              CS_id      = as.numeric(as.character(UI_CropSeasonID[i]))
              ui.Crop.db <- UserInput.db[UserInput.db[,3] == CS_id,]
              ex.Crop.db <- Exposure.db [Exposure.db [,3] == CS_id,]

              UI_StateID = unique(ui.Crop.db$State_ID)
 
              for(j in 1:length(UI_StateID))
                  {
                   TRUE_Count  = 0;
                   FALSE_Count = 0;

                   st_id            =  as.numeric(as.character(UI_StateID[j]))
                   ui.state_crop.db <- ui.Crop.db[ui.Crop.db[,1] == st_id,]
                   ex.state_crop.db <- ex.Crop.db[ex.Crop.db[,2] == st_id,]

                  #Join Exposure and User input together
                    tmp_contract_id  =  ui.Crop.db$Contract_Number[1]
                    tmp.db           = merge(ui.state_crop.db, ex.state_crop.db, by='District_ID')
                    tmp.district <- as.numeric(as.character(tmp.db$District_ID))

                    state_crop.db    = cbind(st_id, tmp.district, CS_id, tmp.db$TSI, 
                                            tmp.db[,8], tmp.db$Planted_Area, tmp.db[,9])
                                           #  Modelled                         Indemnity
                    colnames(state_crop.db) <- c('State_ID', 'District_ID', 'CropSeasonID',  'TSI', 'Modelled', 'Planted_Area', 'Indemnity')


                  #determine if it is reported only at state / state & district / district
                    options(warn=-1)
                    rm(st_cr_district, district_test, tmp.flag.TRUE, tmp.flag.FALSE, TRUE_count, reporting_char)
                    options(warn=0)  

                    st_cr_district  = as.numeric(as.character(ui.state_crop.db$District_ID))

                    district_test   = is.na(st_cr_district)
                    district_test   = as.data.frame(table(district_test))

                    tmp.flag.TRUE   = district_test[district_test[,1] == 'TRUE',]
                    tmp.flag.FALSE  = district_test[district_test[,1] == 'FALSE',]

                    if(nrow(tmp.flag.TRUE) > 0) {TRUE_count  = district_test[district_test[,1] == 'TRUE',] ; TRUE_Count = TRUE_count[1,2]}
                    if(nrow(tmp.flag.FALSE) > 0){FALSE_count = district_test[district_test[,1] == 'FALSE',]; FALSE_Count = FALSE_count[1,2]}

                 # TRUE count refers entries where districts are NOT reported, 
                 # False count refers to number entries where districts are reported
                    if((TRUE_Count > 0 ) & (FALSE_Count > 0) ) {reporting_char = 1} #Both State level and District level are reported
                    if((TRUE_Count == 0) & (FALSE_Count > 0) ) {reporting_char = 2} #All entires are reported at district level
                    if((TRUE_Count > 0 ) & (FALSE_Count == 0)) {reporting_char = 3} #All entires are reported at state level 


                    if(reporting_char == 1)
                      {
                      #isolate state level entries
                        state_level_entry    = state_crop.db[is.na(state_crop.db[,2]),,drop = FALSE]
 
                      #Isolate modelled crops
                        modelled_entries = state_crop.db[state_crop.db[,5] > 0,,drop = FALSE]
                        modelled_entries = modelled_entries[complete.cases(modelled_entries[,1]),,drop = FALSE]
                        modelled_entries[,4][is.na(modelled_entries[,4])] <- 0
 
                      #Isolate non modelled crops
 #                       non_modelled_entries = state_crop.db[state_crop.db[,8] == 0,]
 #                       non_modelled_entries = non_modelled_entries[complete.cases(non_modelled_entries[,2]),]
 #                       non_modelled_entries[,5][is.na(non_modelled_entries[,5])] <- 0

                      #deduct all TSI from modelled at State level
                       state_level_entry[4] = as.numeric(state_level_entry[4]) - sum(as.numeric(modelled_entries[,4]))
 
                      #isolate modelled crops where TSI = 0
                       to_distribute_tsi = modelled_entries[modelled_entries[,4] == 0,,drop = FALSE]
                       existing_tsi      = modelled_entries[modelled_entries[,4] > 0,,drop = FALSE]
 
                      #compute ratio
                       ratio_array = as.numeric(to_distribute_tsi[,6])
                       for(a in 1:length(ratio_array)) { to_distribute_tsi[a,4] = (ratio_array[a]/sum(ratio_array)) * as.numeric(state_level_entry[4]) }
 
                      #combine arrays
                       distributed.ui.state_crop.db = rbind(existing_tsi,to_distribute_tsi)
                      }


                    if(reporting_char == 2)
                       {
                      #confirm with premal, that all TSI from non modelled districts get thrown out or not
                      #isolate state level entries
                       state_level_entry    = state_crop.db[1,,drop = FALSE]
                       state_level_entry[4:7] = 0

                       #Isolate modelled crops
                        modelled_entries = state_crop.db[state_crop.db[,5] > 0,,drop = FALSE]
                        modelled_entries = modelled_entries[complete.cases(modelled_entries[,1]),,drop = FALSE]
                        modelled_entries[,4][is.na(modelled_entries[,4])] <- 0

                       #Isolate non modelled crops
                        non_modelled_entries = state_crop.db[state_crop.db[,5] == 0,,drop = FALSE]
                        non_modelled_entries = non_modelled_entries[complete.cases(non_modelled_entries[,1]),,drop = FALSE]
                        non_modelled_entries[,4][is.na(non_modelled_entries[,4])] <- 0

                      #Add all TSI from non-modelled at State level
                       if(nrow(non_modelled_entries) > 0){state_level_entry[4] = sum(as.numeric(non_modelled_entries[,4]))}

                      #isolate modelled crops where TSI = 0
                        to_distribute_tsi = modelled_entries[modelled_entries[,4] == 0,,drop = FALSE]
                        existing_tsi      = modelled_entries[modelled_entries[,4] > 0,,drop = FALSE]

                      #compute ratio
                        if(state_level_entry[4] > 0)
                           {
                             if(nrow(to_distribute_tsi) > 0)
                               {
                                 ratio_array = as.numeric(to_distribute_tsi[,6])
                                 for(a in 1:length(ratio_array)) {to_distribute_tsi[a,4] = (ratio_array[a]/sum(ratio_array)) * as.numeric(state_level_entry[4]) }
                               }  
                            #combine arrays
                             distributed.ui.state_crop.db = rbind(existing_tsi,to_distribute_tsi)
                           } 
                     
#                      #combine arrays     
                       if(state_level_entry[4] == 0){ distributed.ui.state_crop.db = existing_tsi }
                         
                      }


                    if(reporting_char == 3)
                       {
                      #isolate state level entries
                        state_level_entry    = state_crop.db[is.na(state_crop.db[,7]),,drop = FALSE]
                        

                      #Isolate modelled crops
                         modelled_entries = state_crop.db[state_crop.db[,5] > 0,,drop = FALSE]
                         modelled_entries = modelled_entries[complete.cases(modelled_entries[,1]),,drop = FALSE]
                         modelled_entries[,4][is.na(modelled_entries[,4])] <- 0

                      #isolate modelled crops where TSI = 0
                        to_distribute_tsi = modelled_entries[modelled_entries[,4] == 0,,drop = FALSE]
 
                      #compute ratio

                         ratio_array = as.numeric(to_distribute_tsi[,6])
                         for(a in 1:length(ratio_array)) { to_distribute_tsi[a,4] = (ratio_array[a]/sum(ratio_array)) * state_level_entry[4] }
 
                       #combine arrays
                         distributed.ui.state_crop.db = to_distribute_tsi
                      }

            if(db.flag == 1) {deaggregated.UserInput.db = rbind(deaggregated.UserInput.db, distributed.ui.state_crop.db)}
            if(db.flag == 0) {deaggregated.UserInput.db = distributed.ui.state_crop.db; db.flag = 1}

           }
        }

        
        return(deaggregated.UserInput.db)
        
     }
#...............................................................................
  
   #...............................................................................
   # Disaggregate exposure
   disaggregate_exposure_WBCIS <- function(Exposure.db, UserInput.db)
   {
     
     #   UserInput.db = MNAIS_display_array
     
     #aggregate data at respective admin level to avoid 
     x=UserInput.db[,c(-5,-6)] #remove EPI and PR
     agg_flag = 0
     
     x[x == -9999] <- NA
     
     CropSeason_levels  = unique(x$CropSeasonID)
     for(i in 1:length(CropSeason_levels))
     {
       cs_id = as.numeric(as.character(CropSeason_levels[i]))
       cropseason.db = x[x[,3] == cs_id,]
       state_levels     = unique(cropseason.db$State_ID)
       
       for(j in 1:length(state_levels))
       {
         s_id     = as.numeric(as.character(state_levels[j]))
         state.db = cropseason.db[cropseason.db[,1] == s_id,]
         
         #state.db[,2][is.na(state.db[,2])] <- -9999
         District_levels  = unique(state.db$District_ID)
         
         for(k in 1:length(District_levels))
         {
           
           d_id = as.numeric(as.character(District_levels[k]))
           district.db = state.db[state.db[,2] == d_id,]
           
           sumTSI = sum(as.numeric(district.db[,4]))
           tmp = district.db[1,]
           tmp[,4] = sumTSI
           #tmp[tmp == -9999] <- NA
           
           if(agg_flag == 1) {tmp.x = rbind(tmp.x, tmp)}
           if(agg_flag == 0) {tmp.x = tmp; agg_flag = 1}
         }
       }
     }
     
     UserInput.db = tmp.x
     
     #Initiate data
     db.flag = 0
     UI_CropSeasonID  = unique(UserInput.db$CropSeasonID)
     
     for(i in 1:length(UI_CropSeasonID))
     {
       CS_id      = as.numeric(as.character(UI_CropSeasonID[i]))
       ui.Crop.db <- UserInput.db[UserInput.db[,3] == CS_id,]
       ex.Crop.db <- Exposure.db [Exposure.db [,3] == CS_id,]
       
       UI_StateID = unique(ui.Crop.db$State_ID)
       
       for(j in 1:length(UI_StateID))
       {
         TRUE_Count  = 0;
         FALSE_Count = 0;
         
         st_id            =  as.numeric(as.character(UI_StateID[j]))
         ui.state_crop.db <- ui.Crop.db[ui.Crop.db[,1] == st_id,]
         ex.state_crop.db <- ex.Crop.db[ex.Crop.db[,2] == st_id,]
         
         #Join Exposure and User input together
         tmp_contract_id  =  ui.Crop.db$Contract_Number[1]
         tmp.db           = merge(ui.state_crop.db, ex.state_crop.db, by='District_ID')
         tmp.district <- as.numeric(as.character(tmp.db$District_ID))
         
         state_crop.db    = cbind(st_id, tmp.district, CS_id, tmp.db$TSI, 
                                  tmp.db[,11], tmp.db$Planted_Area, tmp.db[,9])
         #  Modelled                         Indemnity
         colnames(state_crop.db) <- c('State_ID', 'District_ID', 'CropSeasonID',  'TSI', 'Modelled', 'Planted_Area', 'Indemnity')
         
         
         #determine if it is reported only at state / state & district / district
         options(warn=-1)
         rm(st_cr_district, district_test, tmp.flag.TRUE, tmp.flag.FALSE, TRUE_count, reporting_char)
         options(warn=0)  
         
         st_cr_district  = as.numeric(as.character(ui.state_crop.db$District_ID))
         
         district_test   = is.na(st_cr_district)
         district_test   = as.data.frame(table(district_test))
         
         tmp.flag.TRUE   = district_test[district_test[,1] == 'TRUE',]
         tmp.flag.FALSE  = district_test[district_test[,1] == 'FALSE',]
         
         if(nrow(tmp.flag.TRUE) > 0) {TRUE_count  = district_test[district_test[,1] == 'TRUE',] ; TRUE_Count = TRUE_count[1,2]}
         if(nrow(tmp.flag.FALSE) > 0){FALSE_count = district_test[district_test[,1] == 'FALSE',]; FALSE_Count = FALSE_count[1,2]}
         
         # TRUE count refers entries where districts are NOT reported, 
         # False count refers to number entries where districts are reported
         if((TRUE_Count > 0 ) & (FALSE_Count > 0) ) {reporting_char = 1} #Both State level and District level are reported
         if((TRUE_Count == 0) & (FALSE_Count > 0) ) {reporting_char = 2} #All entires are reported at district level
         if((TRUE_Count > 0 ) & (FALSE_Count == 0)) {reporting_char = 3} #All entires are reported at state level 
         
         
         if(reporting_char == 1)
         {
           #isolate state level entries
           state_level_entry    = state_crop.db[is.na(state_crop.db[,2]),,drop = FALSE]
           
           #Isolate modelled crops
           modelled_entries = state_crop.db[state_crop.db[,5] > 0,,drop = FALSE]
           modelled_entries = modelled_entries[complete.cases(modelled_entries[,1]),,drop = FALSE]
           modelled_entries[,4][is.na(modelled_entries[,4])] <- 0
           
           #Isolate non modelled crops
           #                       non_modelled_entries = state_crop.db[state_crop.db[,8] == 0,]
           #                       non_modelled_entries = non_modelled_entries[complete.cases(non_modelled_entries[,2]),]
           #                       non_modelled_entries[,5][is.na(non_modelled_entries[,5])] <- 0
           
           #deduct all TSI from modelled at State level
           state_level_entry[4] = as.numeric(state_level_entry[4]) - sum(as.numeric(modelled_entries[,4]))
           
           #isolate modelled crops where TSI = 0
           to_distribute_tsi = modelled_entries[modelled_entries[,4] == 0,,drop = FALSE]
           existing_tsi      = modelled_entries[modelled_entries[,4] > 0,,drop = FALSE]
           
           #compute ratio
           ratio_array = as.numeric(to_distribute_tsi[,6])
           for(a in 1:length(ratio_array)) { to_distribute_tsi[a,4] = (ratio_array[a]/sum(ratio_array)) * as.numeric(state_level_entry[4]) }
           
           #combine arrays
           distributed.ui.state_crop.db = rbind(existing_tsi,to_distribute_tsi)
         }
         
         
         if(reporting_char == 2)
         {
           #confirm with premal, that all TSI from non modelled districts get thrown out or not
           #isolate state level entries
           state_level_entry    = state_crop.db[1,,drop = FALSE]
           state_level_entry[4:7] = 0
           
           #Isolate modelled crops
           modelled_entries = state_crop.db[state_crop.db[,5] > 0,,drop = FALSE]
           modelled_entries = modelled_entries[complete.cases(modelled_entries[,1]),,drop = FALSE]
           modelled_entries[,4][is.na(modelled_entries[,4])] <- 0
           
           #Isolate non modelled crops
           non_modelled_entries = state_crop.db[state_crop.db[,5] == 0,,drop = FALSE]
           non_modelled_entries = non_modelled_entries[complete.cases(non_modelled_entries[,1]),,drop = FALSE]
           non_modelled_entries[,4][is.na(non_modelled_entries[,4])] <- 0
           
           #Add all TSI from non-modelled at State level
           if(nrow(non_modelled_entries) > 0){state_level_entry[4] = sum(as.numeric(non_modelled_entries[,4]))}
           
           #isolate modelled crops where TSI = 0
           to_distribute_tsi = modelled_entries[modelled_entries[,4] == 0,,drop = FALSE]
           existing_tsi      = modelled_entries[modelled_entries[,4] > 0,,drop = FALSE]
           
           #compute ratio
           if(state_level_entry[4] > 0)
           {
             if(nrow(to_distribute_tsi) > 0)
             {
               ratio_array = as.numeric(to_distribute_tsi[,6])
               for(a in 1:length(ratio_array)) {to_distribute_tsi[a,4] = (ratio_array[a]/sum(ratio_array)) * as.numeric(state_level_entry[4]) }
             }  
             #combine arrays
             distributed.ui.state_crop.db = rbind(existing_tsi,to_distribute_tsi)
           } 
           
           #                      #combine arrays     
           if(state_level_entry[4] == 0){ distributed.ui.state_crop.db = existing_tsi }
           
         }
         
         
         if(reporting_char == 3)
         {
           #isolate state level entries
           state_level_entry    = state_crop.db[is.na(state_crop.db[,7]),,drop = FALSE]
           
           
           #Isolate modelled crops
           modelled_entries = state_crop.db[state_crop.db[,5] > 0,,drop = FALSE]
           modelled_entries = modelled_entries[complete.cases(modelled_entries[,1]),,drop = FALSE]
           modelled_entries[,4][is.na(modelled_entries[,4])] <- 0
           
           #isolate modelled crops where TSI = 0
           to_distribute_tsi = modelled_entries[modelled_entries[,4] == 0,,drop = FALSE]
           
           #compute ratio
           
           ratio_array = as.numeric(to_distribute_tsi[,6])
           for(a in 1:length(ratio_array)) { to_distribute_tsi[a,4] = (ratio_array[a]/sum(ratio_array)) * state_level_entry[4] }
           
           #combine arrays
           distributed.ui.state_crop.db = to_distribute_tsi
         }
         
         if(db.flag == 1) {deaggregated.UserInput.db = rbind(deaggregated.UserInput.db, distributed.ui.state_crop.db)}
         if(db.flag == 0) {deaggregated.UserInput.db = distributed.ui.state_crop.db; db.flag = 1}
         
       }
     }
     
     
     return(deaggregated.UserInput.db)
     
   }
   #...............................................................................
   
#...............................................................................
# Get mutually exclusive historic and synthetic yield
  get_mutually_exclusive_gy <- function(gy.db, UserInput.db)
      {
        db.flag          = 0
        UserInput.db     = as.data.frame(UserInput.db)

        UI_CropSeasonID  = unique(UserInput.db$CropSeasonID)

        for(i in 1:length(UI_CropSeasonID))
           {
            CS_id    =  UI_CropSeasonID[i]
            Crop.db  <- gy.db[gy.db[,3] == CS_id,]
 
            UI_DistrictID = unique(UserInput.db$District_ID)

            for(j in 1:length(UI_DistrictID))
               {
                 dis_id   = UI_DistrictID[j]
                 District_crop.db <- Crop.db[Crop.db[,1] == dis_id,]

                 if(db.flag == 1) {tmp.gy.db = rbind(tmp.gy.db, District_crop.db)}
                 if(db.flag == 0) {tmp.gy.db = District_crop.db;   db.flag = 1   }
                }
             }

            gy.db = tmp.gy.db
            return(gy.db)
          }
#...............................................................................

#...............................................................................
# Compute average historic yield
  Get_Guaranteed_gy<-function(gy.db, UserInput.db, Exposure.db)
     {

         ui_exposure    = merge(UserInput.db, Exposure.db, by=c('State_ID', 'District_ID', 'CropSeasonID'))
         ui_exposure_gy = merge(ui_exposure, gy.db, by=c('State_ID', 'District_ID', 'CropSeasonID'))
         
         ui_exposure_gy_truncated = ui_exposure_gy[,c(-8, -9, -10, -12, -13, -14)]
         final_gy = cbind(ui_exposure_gy_truncated[,1:7], ui_exposure_gy_truncated[,9], ui_exposure_gy_truncated[,10], ui_exposure_gy_truncated[,8])
         colnames(final_gy) <- c('State_ID','District_ID','CropSeasonID','TSI',
                                 'Modelled','Planted_Area','Indemnity','Year',
                                 'Yield','Guaranteed_GY')
 
         return(final_gy)
      }
#...............................................................................

#...............................................................................
# Compute Indemnity Loss
  Compute_Indemnity_loss <- function(gy.db)
      {
         db.flag         =  0

       # Compute Threshold_GY & Shortfall_GY
         Threshold_GY    =  as.numeric(gy.db$Indemnity)  * as.numeric(gy.db$Guaranteed_GY)
         Shortfall_GY    =  Threshold_GY - as.numeric(gy.db$Yield)
         gy.db  = cbind(gy.db, Threshold_GY, Shortfall_GY)

        
       # assign indemnity loss
         gy.db.no.shortfall = gy.db[gy.db$Shortfall_GY < 0,]
         gy.db.no.shortfall = cbind(gy.db.no.shortfall, 0)
       
       # Compute indemnity loss
         gy.db.shortfall    = gy.db[gy.db$Shortfall_GY > 0,]
         Shortfall          = gy.db.shortfall$Shortfall_GY
         Threshold          = gy.db.shortfall$Threshold_GY
         TSI                = gy.db.shortfall$TSI
         Indemnity_loss     = (Shortfall/Threshold) * TSI
         gy.db.shortfall    = cbind(gy.db.shortfall, Indemnity_loss)

       
       # 
          column_names <- c('State_ID','District_ID','CropSeasonID','TSI','Modelled','Planted_Area','Indemnity','Year','Yield','Guaranteed_GY','Threshold_GY','Shortfall_GY','Indemnity_loss')
          
          colnames(gy.db.shortfall)    <- column_names
          colnames(gy.db.no.shortfall) <- column_names
          
          gy.db = rbind(gy.db.shortfall, gy.db.no.shortfall)
          gy.db = gy.db[gy.db$Yield != 0,]

          return(gy.db)
       }
#...............................................................................

#...............................................................................
# Compute Aggregation
  Compute_Crop_District_aggregate <- function(GY.db)
      {
        #GY.db = IND_LOSS_Historic_gy.db
        Unique_CropSeasonID = unique(GY.db$CropSeasonID)
        db.flag             =  0

        for(i in 1:length(Unique_CropSeasonID))
           {
            CS_ID = as.numeric(Unique_CropSeasonID[i])
            Crop.db <- GY.db[GY.db[,3] == CS_ID,]

            Unique_DistrictID   = unique(Crop.db$District_ID)

            for(j in 1:length(Unique_DistrictID))
                {
                 DS_ID       =  Unique_DistrictID[j]
                 District.db <- Crop.db[Crop.db[,2] == DS_ID,]

                 Avg_Actual_GY    = mean(District.db$Yield)
                 Avg_Shortfall_GY = mean(District.db$Shortfall_GY > 0)
                 N                = nrow(District.db)
                 LossN            = sum(District.db$Indemnity_loss > 0)
                 Loss_Sum         = sum(District.db$Indemnity_loss)
                 Loss_min         = min(District.db$Indemnity_loss)
                 Loss_max         = max(District.db$Indemnity_loss)
                 Burning_cost     = Loss_Sum/N

                 tmp.gy.db =  cbind(District.db[1,1:7], Avg_Actual_GY, District.db[1,10:11], Avg_Shortfall_GY, N, LossN, Loss_Sum, Loss_min, Loss_max, Burning_cost)
       
                 if(db.flag == 1) {tmp.gy.final.db = rbind(tmp.gy.final.db, tmp.gy.db)}
                 if(db.flag == 0) {tmp.gy.final.db = tmp.gy.db; db.flag = 1}
                }
             }
          return(tmp.gy.final.db)
       }
#...............................................................................

#...............................................................................
# LOB Pie Plot
  LOB_Pie_Plot <- function(x, y)
       {
             x[is.na(x)] <- 0
             newdata.x = colSums(x)
             slices.x <- c(newdata.x) 
             lbls.x <- c(names(newdata.x))
             pct.x <- round(slices.x/sum(slices.x)*100)
             lbls.x <- paste(lbls.x, pct.x) # add percents to labels 
             lbls.x <- paste(lbls.x,"%",sep="") # ad % to labels 

             y[is.na(y)] <- 0
             newdata.y = colSums(y)
             slices.y <- c(newdata.y) 
             lbls.y <- c(names(newdata.y))
             pct.y <- round(slices.y/sum(slices.y)*100)
             lbls.y <- paste(lbls.y, pct.y) # add percents to labels 
             lbls.y <- paste(lbls.y,"%",sep="") # ad % to labels 
 
             par(mfrow=c(1,2))
             pie(slices.x,labels = lbls.x, col=rainbow(length(lbls.x)), main="MNAIS Line of Business")
             pie(slices.y,labels = lbls.y, col=rainbow(length(lbls.y)), main="WBCIS Line of Business")

        }
#...............................................................................

#...............................................................................
# LOB Pie Plot
   LOB_Pie_Plot_1 <- function(x, heading)
       {
        x[is.na(x)] <- 0
        newdata =  colSums(x)
        slices  <- c(newdata) 
        lbls    <- c(names(newdata))
        pct     <- round(slices/sum(slices)*100)
        lbls    <- paste(lbls, pct) # add percents to labels 
        lbls    <- paste(lbls,"%",sep="") # ad % to labels 

        pie(slices,labels = lbls, col=rainbow(length(lbls)), main=heading)
   }
#...............................................................................

#...............................................................................
# LOB Pie Plot
  State_TSI_Plot <- function(x, y)
    {    
      x[is.na(x)] <- 0
      y[is.na(y)] <- 0
      
      newdata.x = rowSums(x)
      
      newdata.y = rowSums(y)
      
      a=cbind(names(newdata.x), newdata.x)
      b=cbind(names(newdata.y), newdata.y)
      colnames(a)<- c('state', 'mnais_tsi')
      colnames(b)<- c('state', 'wbcis_tsi')
      all = merge(a,b,by = c('state'), all=TRUE)
      all2 = cbind(as.numeric(all[,2]), as.numeric(all[,3]))
      k<-as.character(all[,1])
      rownames(all2)<- c(k)
      colnames(all2) <- c('MNAIS', 'WBCIS')
      all2[is.na(all2)]<-0
      
      
      barplot(all2, main="Aggregated TSI per State", xlab="State", col=c("darkblue", "red"), beside=TRUE)
    }
#...............................................................................

#...............................................................................
# Round off all numeric numbers
      round_df <- function(x, digits)
                  {
                   # round all numeric variables
                   # x: data frame 
                   # digits: number of digits to round
                   numeric_columns <- sapply(x, mode) == 'numeric'
                   x[numeric_columns] <-  round(x[numeric_columns], digits)
                   return(x)
                  }
#...............................................................................

 #...............................................................................
 # Assemble product type database
   get_Product_type_db <- function(CropSeasons)
       {
         N_Product_type.db         <- as.matrix(CropSeasons)
         Product_type.db           = cbind(NA, N_Product_type.db[,2], N_Product_type.db[,4], N_Product_type.db[,1], N_Product_type.db[,3], N_Product_type.db[,5])
         colnames(Product_type.db) <- c( 'LOB', 'ProductType', 'SeasonType', 'CropSeasonID', 'CropID', 'SeasonID')
         Product_type.db            = as.data.frame(Product_type.db)
         Product_type.db[Product_type.db==-9999]<-0
         return(Product_type.db)
       }
 #...............................................................................
 
 #...............................................................................
 # Assemble AdminID database
   get_adminID_db <- function(Districts,States)
      {
        Merged              <- merge(Districts,States,by=c("State_ID"))
        Merged              <- as.matrix(Merged)

        adminID.db            =  cbind(NA, Merged[,4], Merged[,3], Merged[,1], Merged[,2])
        colnames(adminID.db)  <- c( 'Country_Name','State_Name','District_Name','State_ID','District_ID')
        adminID.db            <- as.data.frame(adminID.db)
        
        #add statenames without districts
        x=adminID.db
        x[,3] = 'All'
        x[,5] = NA
        x=unique(x)
        adminID.db = rbind(adminID.db, x)
        
        
        return(adminID.db)
       }
 #...............................................................................
 
 #...............................................................................  
 # Assemble Exposure DB
   get_exposure_db <- function(Risk_Items, Crops, adminID.db)
         {
          Merged      <- merge(Risk_Items,adminID.db,by=c("District_ID"))
          Merged      <- merge(Merged, Crops, by=c("Crop_ID"))
          Merged      <- Merged[,c(-12,-13,-14,-16)]

          #                   District    State        CropSeason  PlantedArea    Default_Indemnity
          Exposure.db = cbind(Merged[,2], Merged[,12], Merged[,5], Merged[,6:11], Merged[,13])

          colnames(Exposure.db) <- c('District_ID', 'State_ID', 'CropSeasonID', 'Planted_Area',
                                     'Is_MNAIS_Modeled', 'MNAIS_Term_Sheet_Indemnity', 'MNAIS_Average_Yield',
                                     'Is_WBCIS_Modeled', 'WBCIS_Sum_Insured_Ha', 'Default_Indemnity_Level')

          Exposure.db = as.data.frame(Exposure.db)
          Exposure.db[Exposure.db==-9999]<-0

          #Where indemnity is not provided use Default Indemnity
           for(i in 1:nrow(Exposure.db))
             {
               MNAIS_indemnity   = Exposure.db[i,6]
               Default_indemnity = Exposure.db[i,10]

               if(MNAIS_indemnity == 0){Exposure.db[i,6] = Default_indemnity}
             }
          return(Exposure.db)
         }
 #...............................................................................  

 #...............................................................................  
 # Assemble GY DB
   get_gy_db <- function(gy.db, Risk_Items, adminID.db)
        {     
         
          gy.db = merge(gy.db,Risk_Items, by=c('Risk_Item_ID'))
          gy.db = merge(gy.db,adminID.db, by=c('District_ID'))
          
          District  = as.numeric(as.character(gy.db[,1]))
          State     = as.numeric(as.character(gy.db[,17]))
          CS        = as.numeric(as.character(gy.db[,7]))
          Year      = as.numeric(as.character(gy.db[,3]))
          Yield     = as.numeric(as.character(gy.db[,4]))
            
          final_gy.db           = cbind(District, State, CS, Year, Yield)
          colnames(final_gy.db) = c('District_ID','State_ID','CropSeasonID','Year','Yield')

          final_gy.db = as.data.frame(final_gy.db)
          final_gy.db[final_gy.db==-9999]<-0
          return(final_gy.db)
         }
 #............................................................................... 
 
 #...............................................................................
 # Prepare loss data for output
    prepare_loss_data_for_display <- function(gy.db)
         {
          # Indemnity
            gy.db[,7]  <- format(round((as.numeric(as.character(gy.db[,5]))), 2), numeric = TRUE) 
            gy.db[,7]  =  format(gy.db[,5], scientific = FALSE)

          # Yield
            gy.db[,8]  <- format(round((as.numeric(as.character(gy.db[,7]))), 2), numeric = TRUE) 
            gy.db[,8]  =  format(gy.db[,7], scientific = FALSE)

          # Guaranteed_GY
            gy.db[,9]  <- format(round((as.numeric(as.character(gy.db[,9]))), 2), numeric = TRUE)
            gy.db[,9]  =  format(gy.db[,9], scientific = FALSE)

          # Threshold_GY
            gy.db[,10]  <- format(round((as.numeric(as.character(gy.db[,10]))), 2), numeric = TRUE) 
            gy.db[,10]  =  format(gy.db[,10], scientific = FALSE)

          # Shortfall_GY
            gy.db[,11]  <- format(round((as.numeric(as.character(gy.db[,11]))), 2), numeric = TRUE) 
            gy.db[,11]  =  format(gy.db[,11], scientific = FALSE)

          # TSI
            gy.db[,12]  <- format(round((as.numeric(as.character(gy.db[,12]))), 0), numeric = TRUE)
            gy.db[,12]  =  format(gy.db[,12], scientific = FALSE)

          # Planted_Area
            gy.db[,15]  <- format(round((as.numeric(as.character(gy.db[,15]))), 0), numeric = TRUE) 
            gy.db[,15]  =  format(gy.db[,15], scientific = FALSE)

          #Indemnity_loss
            gy.db[,17]  <- format(round((as.numeric(as.character(gy.db[,17]))), 0), numeric = TRUE) 
            gy.db[,17]  =  format(gy.db[,17], scientific = FALSE)

          # Loss/TSI
            gy.db[,18]  <- format(round((as.numeric(as.character(gy.db[,18]))), 3), numeric = TRUE) 
            gy.db[,18]  =  format(gy.db[,18], scientific = FALSE)

            return(gy.db)
          }
 #...............................................................................

     #...............................................................................
     # Compute Aggregation
      State_year_Crop_Season_aggregation <- function(gy.db, Product_type.db)
         {

            gy.db = IND_LOSS_Historic_gy.db
            
          
            # Attach Crop Names to the array
            CropSeasonNames  =  as.data.frame(unique(Product_type.db[,c(-1,-5,-6)]))
            CropSeasondb     <- unique(cbind(as.character(CropSeasonNames[,1]), 
                                             as.character(CropSeasonNames[,2]),
                                             as.numeric(as.character(CropSeasonNames[,3]))))
            colnames(CropSeasondb) <- c('CropName', 'SeasonName','CropSeasonID')
            gy.db            =  merge(gy.db, CropSeasondb, by=c('CropSeasonID'))

            
            # Attach State Names to the Array
            adminID.db       =  unique(adminID.db[,c(-1,-3,-5)])
            gy.db            =  merge(gy.db, adminID.db, by=c('State_ID'))
            
            # Prepare Array                
            gy.db            =  cbind(gy.db[,16], gy.db[,14], gy.db[,2:13])
            colnames(gy.db) <- c('State_Name','Crop_Name','CropSeasonID','District_ID',
                                 'TSI','Modelled','Planted_Area','Indemnity','Year',
                                 'Yield','Guaranteed_GY','Threshold_GY','Shortfall_GY',
                                 'Indemnity_loss')
            
            
            
            # Create Array
            StateNameID  =  unique(as.character(gy.db$State_Name))
            CropNameID   =  unique(as.character(gy.db$Crop_Name))
            output_array <- array(, dim=c(length(StateNameID),length(CropNameID)))
            
            
            
           # include crop and season ID's
            x <-(cbind(as.numeric(Product_type.db[,4]), as.numeric(Product_type.db[,5]), as.numeric(Product_type.db[,6])))
            colnames(x)<- c('CropSeasonID','CropID','SeasonID')
            gy.db <- merge(GY.db, x, by = 'CropSeasonID')
            gy.db <- cbind(gy.db[,2:3], gy.db[,14:15], gy.db[,4:13])



           # GY.db = IND_LOSS_Historic_gy.db
             Unique_StateID = unique(gy.db$State_ID)
             db.flag        =  0

             for(i in 1:length(Unique_StateID))
                {
                 ST_ID    = as.numeric(Unique_StateID[i])
                 State.db <- gy.db[gy.db[,1] == ST_ID,]

                 Unique_CropID   = unique(State.db$CropID)

                 for(j in 1:length(Unique_CropID))
                     {
                       CR_ID       =  Unique_CropID[j]
                       Crop.db     <- State.db[State.db[,3] == CR_ID,]

                       Unique_year <- unique(Crop.db$Year)

                       for(k in 1:length(Unique_year))
                           {
                              YR_ID            =  Unique_year[k]
                              year.db          <- Crop.db[Crop.db[,9] == YR_ID,]

                              Total_TSI        = sum(year.db$TSI)
                              Total_Planted_Area = sum(year.db$Planted_Area)
                              Avg_Indemnity    = mean(year.db$Indemnity)
                              Avg_Actual_GY    = mean(year.db$Yield)
                              Avg_Guaranteed_GY= mean(year.db$Guaranteed_GY)
                              Avg_Threshold_GY = mean(year.db$Threshold_GY)
                              Avg_Shortfall_GY = mean(year.db$Shortfall_GY > 0)
                              N                = nrow(year.db)
                              LossN            = sum(year.db$Indemnity_loss > 0)
                              Loss_Sum         = sum(year.db$Indemnity_loss)
                              Loss_min         = min(year.db$Indemnity_loss)
                              Loss_max         = max(year.db$Indemnity_loss)
                              Burning_cost     = Loss_Sum/N
                              tmp.gy.db        =  cbind(ST_ID, CR_ID, YR_ID, Total_TSI, Total_Planted_Area, Avg_Indemnity, 
                                                         Avg_Actual_GY, Avg_Guaranteed_GY, Avg_Threshold_GY, Avg_Shortfall_GY, 
                                                         N, LossN, Loss_Sum, Loss_min, Loss_max, Burning_cost)
  
                               if(db.flag == 1) {tmp.gy.final.db = rbind(tmp.gy.final.db, tmp.gy.db)}
                               if(db.flag == 0) {tmp.gy.final.db = tmp.gy.db; db.flag = 1}
                             }
                       }
                }
           
            colnames(tmp.gy.final.db) <- c('State_ID','CropID','Year', 'Total_TSI','Total_Planted_Area','Avg_Indemnity','Avg_Actual_GY','Avg_Guaranteed_GY',
                                           'Avg_Threshold_GY','Avg_Shortfall_GY','N', 'LossN', 'Loss_Sum', 'Loss_min', 'Loss_max', 'Burning_cost')
             
             return(tmp.gy.final.db)
     }
     #...............................................................................
 
     #...............................................................................
     # Compute Aggregation
       Compute_State_year_aggregate <- function(gy.db)
             {
                 gy.db          = IND_LOSS_Historic_gy.db
                 Unique_StateID = unique(gy.db$State_ID)
                 db.flag        =  0

                 for(i in 1:length(Unique_StateID))
                     {
                      ST_ID    = as.numeric(Unique_StateID[i])
                      State.db <- gy.db[gy.db[,1] == ST_ID,]

                      Unique_year   = unique(State.db$Year)

                      for(k in 1:length(Unique_year))
                          {
                            YR_ID              =  Unique_year[k]
                            year.db            <- State.db[State.db[,8] == YR_ID,]

                            Total_TSI          =  sum(year.db$TSI)
                            Total_Planted_Area =  sum(year.db$Planted_Area)
                            Avg_Indemnity      =  mean(year.db$Indemnity)
                            Avg_Actual_GY      =  mean(year.db$Yield)
                            Avg_Guaranteed_GY  =  mean(year.db$Guaranteed_GY)
                            Avg_Threshold_GY   =  mean(year.db$Threshold_GY)
                            Avg_Shortfall_GY   =  mean(year.db$Shortfall_GY > 0)
                            N                  =  nrow(year.db)
                            LossN              =  sum(year.db$Indemnity_loss > 0)
                            Loss_Sum           =  sum(year.db$Indemnity_loss)
                            Loss_min           =  min(year.db$Indemnity_loss)
                            Loss_max           =  max(year.db$Indemnity_loss)
                            Burning_cost       =  Loss_Sum/N
                            tmp.gy.db          =  cbind(ST_ID, YR_ID, Total_TSI, Total_Planted_Area, Avg_Indemnity, 
                                                        Avg_Actual_GY, Avg_Guaranteed_GY, Avg_Threshold_GY, Avg_Shortfall_GY, 
                                                        N, LossN, Loss_Sum, Loss_min, Loss_max, Burning_cost)

                            if(db.flag == 1) {tmp.gy.final.db = rbind(tmp.gy.final.db, tmp.gy.db)}
                            if(db.flag == 0) {tmp.gy.final.db = tmp.gy.db; db.flag = 1}
                           }
                      }

                  colnames(tmp.gy.final.db) <- c('State_ID','Year', 'Total_TSI','Total_Planted_Area','Avg_Indemnity','Avg_Actual_GY','Avg_Guaranteed_GY',
                                                 'Avg_Threshold_GY','Avg_Shortfall_GY','N', 'LossN', 'Loss_Sum', 'Loss_min', 'Loss_max', 'Burning_cost')
                  return(tmp.gy.final.db)
            }
     #...............................................................................
 
     #...............................................................................
     # Convert all state, district and crop season to ID's
       Convert_ID_to_Par_detailed_Losses2 <- function(x, adminID.db, Product_type.db)
            {
              x=Historic_State_crop_year_aggregated.db
              StateID.Array    = unique(adminID.db[,c(-1,-3,-5)])
              Product_type.db <- unique(Product_type.db[,c(-1,-3,-4,-6)])   

              tmp.ID <- array(, dim=c(nrow(x),2))
  
              for(i in 1:nrow(x))
                 {
                   State    = lapply(x[i,1], as.character)
                   Crop     = lapply(x[i,2], as.character)

                   StateID      = StateID.Array   [StateID.Array[,2] %in% State,]

                   CropID       = Product_type.db[as.numeric(Product_type.db[,2]) %in% as.numeric(Crop),]
                   tmp.ID[i,1]  = toString(lapply(StateID[1], as.character))
                   tmp.ID[i,2]  = toString(lapply(CropID[2], as.character))
                  }
   
               converted = cbind(tmp.ID, x[,3:ncol(x)])
               colnames(converted) <- c('State_Name','Crop_Name', 'Year','Total_TSI',
                                        'Total_Planted_Area','Avg_Indemnity','Avg_Actual_GY',
                                        'Avg_Guaranteed_GY','Avg_Threshold_GY','Avg_Shortfall_GY',
                                        'N','LossN','Loss_Sum','Loss_min','Loss_max','Burning_cost')
               return(converted)
             }
      #...............................................................................

   #...............................................................................
   # Compute Aggregation ** 
     Compute_aggregate <- function(gy.db, Product_type.db, adminID.db)
            {
              # gy.db            <- IND_LOSS_Historic_gy.db  #Historic_State_crop_year_aggregated.db

              # Attach Crop Names to the array
                CropNames        =  as.data.frame(unique(Product_type.db[,c(-1,-5,-6)]))
                cropdb           <- unique(cbind(as.character(CropNames[,1]),
                                                 as.character(CropNames[,2]),
                                                 as.numeric(as.character(CropNames[,3]))))
                colnames(cropdb) <- c('CropName','SeasonName', 'CropSeasonID')
                gy.db            =  merge(gy.db, cropdb, by=c('CropSeasonID'))

              # Attach State Names to the Array
                adminID.db       =  unique(adminID.db[,c(-1,-3,-5)])
                gy.db            =  merge(gy.db, adminID.db, by=c('State_ID'))

             # Prepare Array    
               gy.db            =  cbind(gy.db[,16], gy.db[,8], gy.db[,14:15], gy.db[,3], gy.db[,7], gy.db[,9:12], gy.db[,4], gy.db[,6], gy.db[,13])
               colnames(gy.db) <- c('State_Name','Year','CropName','SeasonName','District_ID','Indemnity',
                                   'Yield','Guaranteed_GY','Threshold_GY','Shortfall_GY','TSI','Planted_Area','Indemnity_loss')

             # Add Wieghted LC * PA
#                 LC_PA <- gy.db$TSI * gy.db$Planted_Area
#                 gy.db <- cbind(gy.db, LC_PA)
#              
#                 colnames(gy.db) <- c('State_Name','Year','CropName','SeasonName','District_ID','Indemnity',
#                                      'Yield','Guaranteed_GY','Threshold_GY','Shortfall_GY','TSI',
#                                      'Planted_Area','Indemnity_loss', 'LC_PA')
             


               db_flag = 0
               StateName <- unique(gy.db$State_Name)

               for(i in 1:length(StateName))
                   {
                    ST_ID    <- toString(StateName[i])
                    State.db <- gy.db[gy.db[,1] == ST_ID,]

                    means     <- colMeans(State.db[,6:10])
                    sums     <- colSums(State.db[,11:13])
                    LC_TSI   <- sums[3] / sums[1] 
                    

                    level1 <- array(, dim=c(1,15))
                    level1[1,] =  c('level1',ST_ID, 'All', 'All', 'All', 'All', means, sums, LC_TSI)

                    unique_year <- unique(State.db$Year)
               

                     for(j in 1:length(unique_year))
                        {
                          YR_ID   <- as.numeric(unique_year[j])
                          year.db <- State.db[State.db[,2] == YR_ID,]

                          means <- colMeans(year.db[,6:10])
                          sums  <- colSums(year.db[,11:13])
                          LC_TSI   <- sums[3] / sums[1] 
                          

                          level2 <- array(, dim=c(1,15))
                          level2[1,] =  c('level2',ST_ID, YR_ID, 'All', 'All', 'All', means, sums, LC_TSI)

                          unique_crop <- unique(year.db$CropName)

                          for(k in 1:length(unique_crop))
                             {
                               CR_ID   <- toString(unique_crop[k])
                               crop.db <- year.db[year.db[,3] == CR_ID,]

                               means <- colMeans(crop.db[,6:10])
                               sums  <- colSums(crop.db[,11:13])
                               LC_TSI   <- sums[3] / sums[1] 

                               level3 <- array(, dim=c(1,15))
                               level3[1,] =  c('level3',ST_ID, YR_ID, CR_ID, 'All', 'All', means, sums, LC_TSI)

                               unique_season <- unique(crop.db$SeasonName)

                               for(l in 1:length(unique))
                                  {
                                    SD_ID   <- toString(unique_season[l])
                                    season.db <- crop.db[crop.db[,4] == SD_ID,]

                                    means <- colMeans(season.db[,6:10])
                                    sums  <- colSums(season.db[,11:13])
                                    LC_TSI   <- sums[3] / sums[1] 



                                    level4 <- array(, dim=c(1,15))
                                    level4[1,] =  c('level4',ST_ID, YR_ID, CR_ID, SD_ID, 'All', means, sums, LC_TSI)

                                    if(db_flag == 1){ final = rbind(final, level1, level2, level3, level4)}
                                    if(db_flag == 0){ final = rbind(level1, level2, level3, level4); db_flag = 1}
                        }  }   }  }


             colnames(final) <- c('Aggregation_Level','State_Name','Year','CropName','SeasonName','District_ID','Indemnity',
                                  'Yield','Guaranteed_GY','Threshold_GY','Shortfall_GY','TSI','Planted_Area','Indemnity_loss',
                                  'Loss/TSI')
             final <- as.data.frame(final)

             return(final)
     }
   #...............................................................................
   
   #...............................................................................
   # Compute Aggregation ** 
     Compute_display_aggregate <- function(gy.db, Product_type.db, adminID.db)
           {
             # gy.db            <- IND_LOSS_Historic_gy.db  #Historic_State_crop_year_aggregated.db

             # Attach Crop Names to the array
               CropNames        =  as.data.frame(unique(Product_type.db[,c(-1,-5,-6)]))
               cropdb           <- unique(cbind(as.character(CropNames[,1]),
                                                as.character(CropNames[,2]),
                                                as.numeric(as.character(CropNames[,3]))))
               colnames(cropdb) <- c('CropName','SeasonName', 'CropSeasonID')
               gy.db            =  merge(gy.db, cropdb, by=c('CropSeasonID'))

             # Attach State Names to the Array
               adminID.db       =  unique(adminID.db[,c(-1,-3,-5)])
               gy.db            =  merge(gy.db, adminID.db, by=c('State_ID'))

           # Prepare Array    
              gy.db            =  cbind(gy.db[,16], gy.db[,8], gy.db[,14:15], gy.db[,3], gy.db[,7], gy.db[,9:12], gy.db[,4], gy.db[,6], gy.db[,13])
              colnames(gy.db) <- c('State_Name','Year','CropName','SeasonName','District_ID','Indemnity',
                                   'Yield','Guaranteed_GY','Threshold_GY','Shortfall_GY','TSI','Planted_Area','Indemnity_loss')



             db_flag = 0
             StateName <- as.character(unique(gy.db$State_Name))
             CropName  <- as.character(unique(gy.db$CropName))
             final     <- array(, dim=c(length(StateName),length(CropName)))
             rownames(final) <- c(StateName)
             colnames(final) <- c(CropName)
     
             for(i in 1:length(StateName))
                {
                 ST_ID    <- toString(StateName[i])
                 State.db <- gy.db[gy.db[,1] == ST_ID,]

                 for(j in 1:length(CropName))
                          {
                           CR_ID   <- toString(CropName[j])
                           crop.db <- State.db[State.db[,3] == CR_ID,]

                           sums  <- colSums(crop.db[,11:13])
                           LC_TSI   <- sums[3] / sums[1] 

                           LC_TSI  <- format(round((as.numeric(as.character(LC_TSI))), 4), numeric = TRUE) 
                           LC_TSI  =  format(LC_TSI, scientific = FALSE)
                           
                           final[i,j] <- LC_TSI
                           rm(LC_TSI)

              }           }

     final <- as.data.frame(final)

     return(final)
   }
   #...............................................................................

   #...............................................................................
   # Convert all state, district and crop season to ID's
     Convert_ID_to_Par_WBCIS <- function(x, adminID.db, Product_type.db)
          {
           # Attach Crop Names to the array
             CropNames  =  as.data.frame(unique(Product_type.db[,c(-1,-5,-6)]))
             cropdb     <- unique(cbind(as.character(CropNames[,1]), 
                                        as.character(CropNames[,2]),
                                        as.numeric(as.character(CropNames[,3]))))
             
              colnames(cropdb) <- c('CropName','SeasonName', 'CropSeasonID')
              x1               =  merge(x, cropdb, by=c('CropSeasonID'))
       
           # Attach State Names to the Array
              StateID.db       =  unique(adminID.db[,c(-1,-3,-5)])
              DistrictID.db       =  unique(adminID.db[,c(-1,-2,-4)])
              x2            =  merge(x1, StateID.db, by=c('State_ID'))
           
           
              x3 <- x2[is.na(x2[,3]),]
              x4 <- x2[!is.na(x2[,3]),]
              
              dist1 <- merge(x4, DistrictID.db, by=c('District_ID'))
              tmp = colnames(dist1)
              
              if(nrow(x3) > 0) {x3 = cbind(x3, NA); 
                                dist1 = rbind(dist1, x3);
                                colnames(dist1) <- c(tmp)}
           
               converted <- cbind(dist1[,8:9], dist1[,6:7], dist1[4:5])
               colnames(converted) <- c('State_Name','District_name','Crop_Name','Season_Name','Year','Yield')
       
                 return(converted)
               }
   #...............................................................................

   #...............................................................................
   # Compute Aggregation ** 
     Compute_aggregate_WBCIS <- function(gy.db, Product_type.db, adminID.db)
        {
         # x            <- WBCIS.final

           x =  gy.db     
       
           gy.db     =  cbind(x[,1], x[,6], x[,3:4],x[,2],x[,5], x[,7:8])
           colnames(gy.db) <- c('StateName','Year','CropName','SeasonName','DistrictName','TSI','LC','Indemnity_Loss')
           db_flag   =  0
           StateName <- unique(gy.db$StateName)

           for(i in 1:length(StateName))
               {
                 ST_ID    <- toString(StateName[i])
                 State.db <- gy.db[gy.db[,1] == ST_ID,]

                 TSI            <- sum(as.numeric(as.character(State.db$TSI)))
                 LC             <- mean(as.numeric(as.character(State.db$LC)))
                 Indemnity_Loss <- sum(as.numeric(as.character(State.db$Indemnity_Loss)))


                 level1     <- array(, dim=c(1,9))
                 level1[1,] =  c('level1',ST_ID, 'All', 'All', 'All', 'All', TSI, LC, Indemnity_Loss)
                                  #'StateName','Year','CropName','SeasonName','DistrictName','TSI','LC','Indemnity_Loss')
                 unique_year <- unique(State.db$Year)

                 for(j in 1:length(unique_year))
                    {
                       YR_ID   <- as.numeric(unique_year[j])
                       year.db <- State.db[State.db[,2] == YR_ID,]

                       TSI            <- sum(as.numeric(as.character(year.db$TSI)))
                       LC             <- mean(as.numeric(as.character(year.db$LC)))
                       Indemnity_Loss <- sum(as.numeric(as.character(year.db$Indemnity_Loss)))

                       
                       level2     <- array(, dim=c(1,9))
                       level2[1,] =  c('level2',ST_ID, YR_ID, 'All', 'All', 'All', TSI, LC, Indemnity_Loss)

                       unique_crop <- unique(year.db$CropName)

                       for(k in 1:length(unique_crop))
                          {
                            CR_ID   <- toString(unique_crop[k])
                            crop.db <- year.db[year.db[,3] == CR_ID,]

                            TSI            <- sum(as.numeric(as.character(crop.db$TSI)))
                            LC             <- mean(as.numeric(as.character(crop.db$LC)))
                            Indemnity_Loss <- sum(as.numeric(as.character(crop.db$Indemnity_Loss)))

                            level3 <- array(, dim=c(1,9))
                            level3[1,] =  c('level3',ST_ID, YR_ID, CR_ID, 'All', 'All', TSI, LC, Indemnity_Loss)
                            

                             unique_season <- unique(crop.db$SeasonName)

                             for(l in 1:length(unique))
                                 {
                                   SD_ID   <- toString(unique_season[l])
                                   season.db <- crop.db[crop.db[,4] == SD_ID,]

                                   TSI            <- sum(as.numeric(as.character(season.db$TSI)))
                                   LC             <- mean(as.numeric(as.character(season.db$LC)))
                                   Indemnity_Loss <- sum(as.numeric(as.character(season.db$Indemnity_Loss)))

                                   level4 <- array(, dim=c(1,9))
                                   level4[1,] =  c('level4',ST_ID,  YR_ID, CR_ID, SD_ID, 'All', TSI, LC, Indemnity_Loss)
                                   
                                   if(db_flag == 1){ final = rbind(final, level1, level2, level3, level4)}
                                   if(db_flag == 0){ final = rbind(level1, level2, level3, level4); db_flag = 1}
                       }  }   }  }

               colnames(final) <- c('Aggregation_Level','StateName','Year','CropName','SeasonName','DistrictName','TSI','LC','Indemnity_Loss')
               final           <- as.data.frame(final)
               final           <- unique(final)

               return(final)
             }
   #...............................................................................
   
   #...............................................................................
   # Compute Aggregation ** 
   Compute_display_aggregate <- function(gy.db, Product_type.db, adminID.db)
   {
     # gy.db            <- IND_LOSS_Historic_gy.db  #Historic_State_crop_year_aggregated.db
     
     # Attach Crop Names to the array
     CropNames        =  as.data.frame(unique(Product_type.db[,c(-1,-5,-6)]))
     cropdb           <- unique(cbind(as.character(CropNames[,1]),
                                      as.character(CropNames[,2]),
                                      as.numeric(as.character(CropNames[,3]))))
     colnames(cropdb) <- c('CropName','SeasonName', 'CropSeasonID')
     gy.db            =  merge(gy.db, cropdb, by=c('CropSeasonID'))
     
     # Attach State Names to the Array
     adminID.db       =  unique(adminID.db[,c(-1,-3,-5)])
     gy.db            =  merge(gy.db, adminID.db, by=c('State_ID'))
     
     # Prepare Array    
     gy.db            =  cbind(gy.db[,16], gy.db[,8], gy.db[,14:15], gy.db[,3], gy.db[,7], gy.db[,9:12], gy.db[,4], gy.db[,6], gy.db[,13])
     colnames(gy.db) <- c('State_Name','Year','CropName','SeasonName','District_ID','Indemnity',
                          'Yield','Guaranteed_GY','Threshold_GY','Shortfall_GY','TSI','Planted_Area','Indemnity_loss')
     
     
     
     db_flag = 0
     StateName <- as.character(unique(gy.db$State_Name))
     CropName  <- as.character(unique(gy.db$CropName))
     final     <- array(, dim=c(length(StateName),length(CropName)))
     rownames(final) <- c(StateName)
     colnames(final) <- c(CropName)
     
     for(i in 1:length(StateName))
     {
       ST_ID    <- toString(StateName[i])
       State.db <- gy.db[gy.db[,1] == ST_ID,]
       
       for(j in 1:length(CropName))
       {
         CR_ID   <- toString(CropName[j])
         crop.db <- State.db[State.db[,3] == CR_ID,]
         
         sums  <- colSums(crop.db[,11:13])
         LC_TSI   <- sums[3] / sums[1] 
         
         LC_TSI  <- format(round((as.numeric(as.character(LC_TSI))), 4), numeric = TRUE) 
         LC_TSI  =  format(LC_TSI, scientific = FALSE)
         
         final[i,j] <- LC_TSI
         rm(LC_TSI)
         
       }           }
     
     final <- as.data.frame(final)
     
     return(final)
   }
   #...............................................................................