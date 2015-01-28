#...............................................................................
 Check_SingleEntry_UserInput <- function(UserInput.db,adminID.db,Exposure.db,Product_type.db)   
           {
             rowL = nrow(UserInput.db)
             `%ni%` <- Negate(`%in%`)
 
             check_array <- 'Good'
      
            #..........................................................................................................
            #  a) check if all  State_ID & District_ID are valid.
            #  b) Check if all Crop_ID's and Season_ID's are valid.
               Admin_StateID       = unique(adminID.db$State_Name)
               Admin_DistrictID    = unique(adminID.db$District_Name)
               Prod_CropID         = unique(Product_type.db$ProductType)
               Prod_SeasonID       = unique(Product_type.db$SeasonType)
               #Prod_CropID         = lapply(Prod_CropID, as.character)  ; print(Prod_CropID)

               State_UI            = lapply(UserInput.db[2], as.character)
               District_UI         = lapply(UserInput.db[3], as.character)
               Crop_UI             = lapply(UserInput.db[4], as.character)
               Season_UI           = lapply(UserInput.db[5], as.character)

               state_check         <- State_UI  %in% Admin_StateID
               crop_check          <- Crop_UI   %in% Prod_CropID
               season_check        <- Season_UI %in% Prod_SeasonID
               if(!is.na(District_UI)){district_check <- District_UI %in% Admin_DistrictID}

               
               if(state_check      == 'FALSE'){check_array <- 'State mismatch'}
               if(crop_check       == 'FALSE'){check_array <- 'Crop mismatch'}
               if(season_check     == 'FALSE'){check_array <- 'Season mismatch'}
               if(!is.na(District_UI))        {if(district_check == 'FALSE'){check_array <- 'District mismatch'}}
             #..........................................................................................................

             #..........................................................................................................
             #  c) Check if District_ID's, Crop_ID's and Season_ID's are valid as per the MNAIS Term database.
             #  d) Check if all reported entries are modelled crops per district and season (Check with TOM)
                state_id      = lapply(UserInput.db[2], as.character)
                state_name    = adminID.db[adminID.db[,2] == state_id,]
                state_name    = unique(state_name[,c(-1,-3,-5)])

                if(!is.na(District_UI))
                   {
                     district_id   = lapply(UserInput.db[3], as.character)
                     district_name = adminID.db[adminID.db[,3] == district_id,]
                     district_name = unique(district_name[,c(-1,-2,-4)])
                   }

                crop_id       = lapply(UserInput.db[4], as.character)
                crop_name     = Product_type.db[Product_type.db[,2] == crop_id,]
                crop_name     = crop_name[,c(-1)] 
                

                season_id     = lapply(UserInput.db[5], as.character)
                season_name   = crop_name[crop_name[,2] == season_id,]
                season_name   = unique(season_name[,c(-1,-2)])
                CS_id         = as.numeric(season_name[1])
                


                Exposure.db   = Exposure.db[Exposure.db[,5] == 1,] #get all modelled crop
                Exposure.db   = Exposure.db[Exposure.db[,2] == state_name$State_ID,]; #Get state exposure with crops modelled
                look.db       = Exposure.db[Exposure.db[,3] == CS_id,]

                if(nrow(look.db) == 0){check_array = "Crop by State not modelled"}

                if(!is.na(District_UI))
                  {
                   look.db = look.db[look.db[,1]  == district_name$District_ID,]
                   look.db       = look.db[look.db[,3]  == season_name,] #get all state entries where crop season is modelled

                   if((nrow(look.db) == 0) && (check_array != "Crop by State not modelled")){check_array = "Crop by District not modelled"}
                  }
              #..........................................................................................................  


              # #..........................................................................................................
              # #  c) Check if [TSI] or [EPI and Premium rate] are provided
              # #  d) If all 3 (TSI, EPI & Premium rate) are provided, then check if  EPI/ Premium Rate = approximate(TSI) (decide threshold for "approximate". suggested values could be with 0.5% of the difference)
              # #  e) check if premium rate > 1
              # #  f) If EPI & TSI are provided check if EPI >= TSI
              # #  g) Identify entries where TSI is not reported, and compute TSI (FORMULA: EPI / Premium rate = TSI)


                 TSI      = as.numeric(UserInput.db[6])
                 EPI      = as.numeric(UserInput.db[7])
                 PR       = as.numeric(UserInput.db[8])
                 TSI_flag = 1
                 EPI_flag = 1
                 PR_flag  = 1




 
                 if (TSI==0) {TSI_flag = 0};# if(is.na(TSI)) {TSI_flag = 0}
                 if (EPI==0) {EPI_flag = 0};# if(is.na(EPI)) {EPI_flag = 0}
                 if (PR==0) {PR_flag  = 0}; # if(is.na(PR))  {PR_flag = 0}

                 if(PR_flag == 1){PR = as.numeric(PR)/100}

                 Not_Blank = TSI_flag + EPI_flag + PR_flag 
                 if(Not_Blank == 0) {check_array = 'TSI / EPI & Premium not reported'}

                 EPI_PR = EPI_flag + PR_flag;
                 if((TSI_flag == 0) && (EPI_PR == 1)){check_array = 'Insufficient TSI Computation Parameters'}


                 if(PR_flag == 1) {if (PR > 1) {check_array = 'Premium rate greater than 100%'}}
                 if(PR_flag == 1) {if (PR < 0) {check_array = 'Premium rate less than 0%'}}

                 if((EPI_flag == 1) && (TSI_flag == 1)){if(EPI >= TSI){check_array = 'Premium Income greater Total Sum Insured'}}


                     if((Not_Blank == 3))
                        {
                          tmp.tsi = as.numeric(EPI) / as.numeric(PR)
                          diff.tsi = as.numeric(TSI) - tmp.tsi
                          ratio.diff = diff.tsi/as.numeric(TSI)

                          if(ratio.diff > 0.05) {check_array = 'Difference between computed TSI (EPI * Premium Rate) and reported TSI greater than 5%'}
                         }
                    if((EPI_flag == 1) && (TSI_flag == 0) && (PR_flag == 1))
                         {
                          tmp.tsi = as.numeric(EPI) / as.numeric(PR)
                          UserInput.db[6] = as.numeric(tmp.tsi)
                         }

                UserInput.db <- array(UserInput.db, dim=c(1,length(UserInput.db)))  
                if(length(UserInput.db) > 0) {UserInput.db <- cbind(UserInput.db, check_array)}
                colnames(UserInput.db) <- c('Contract_Number','State','District','Crop','Season','TSI','EPI','Premium_rate','Check_Output')
                #print(UserInput.db)
                return(UserInput.db)
          }
#...............................................................................

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

          #UserInput.db    = as.matrix(raw_input)
          UserInput.db    = raw_input
          #UserInput.db    = as.matrix(UserInput.db)
          #adminID.db      = as.matrix(adminID.db)
          #Exposure.db     = as.numeric(Exposure.db)
          #Product_type.db = as.matrix(Product_type.db)
    
         
          rowL            = nrow(UserInput.db)
          `%ni%`          <-Negate(`%in%`)


         UI_StateID      = unique(UserInput.db[,2])
         UI_DistrictID   = unique(UserInput.db[,3])
         UI_CropSeasonID = unique(UserInput.db[,4])
         UI_TSI          = unique(UserInput.db[,5])
         UI_EPI          = unique(UserInput.db[,6])
         UI_Premium_rate = unique(UserInput.db[,7])



      #..........................................................................................................
      #  a) check if all  State_ID & District_ID are valid.
      #  b) Check if all Crop_ID's and Season_ID's are valid.
         Admin_StateID          =   as.numeric(as.character((unique(adminID.db[,4]))))
         Admin_DistrictID       =   as.numeric(as.character((unique(adminID.db[,5]))))
         Prod_CropSeasonID      =   as.numeric(as.character((unique(Product_type.db[,4]))))

         StateID_mismatch       <-  UserInput.db[UserInput.db[,2] %ni% Admin_StateID,]
         UserInput.db           <-  UserInput.db[UserInput.db[,2] %in% Admin_StateID,]

         DistrictID_mismatch    <-  as.data.frame(UserInput.db[UserInput.db[,3] %ni% Admin_DistrictID,])
         UserInput.db           <-  UserInput.db[UserInput.db[,3] %in% Admin_DistrictID,]

         Crop_SeasonID_mismatch <-  as.data.frame(UserInput.db[UserInput.db[,4] %ni% Prod_CropSeasonID,])
         UserInput.db           <-  UserInput.db[UserInput.db[,4] %in% Prod_CropSeasonID,]


      # store all mismatches
         Column_names_output <- c('Contract_Number','State_ID','District_ID','CropSeasonID','TSI','EPI','Premium_rate','Check_Output') 

         if(nrow(StateID_mismatch)      > 0) {StateID_mismatch       <- cbind(StateID_mismatch,       'State mismatch');      colnames(StateID_mismatch)       <- Column_names_output} 

         if(nrow(DistrictID_mismatch)   > 0) {DistrictID_mismatch    <- cbind(DistrictID_mismatch,    'District mismatch');   colnames(DistrictID_mismatch)    <- Column_names_output}

         if(nrow(Crop_SeasonID_mismatch)> 0) {Crop_SeasonID_mismatch <- cbind(Crop_SeasonID_mismatch, 'CropSeason mismatch'); colnames(Crop_SeasonID_mismatch) <- Column_names_output}
         
      # get a clean set of UI parameters
         UI_StateID      = unique(UserInput.db[,2])
         UI_DistrictID   = unique(UserInput.db[,3])
         UI_CropSeasonID = unique(UserInput.db[,4])
         UI_TSI          = unique(UserInput.db[,5])
         UI_EPI          = unique(UserInput.db[,6])
         UI_Premium_rate = unique(UserInput.db[,7])
     #..........................................................................................................
    

     #..........................................................................................................
     #  c) Check if District_ID's, Crop_ID's and Season_ID's are valid as per the MNAIS Term database.
     #  d) Check if all reported entries are modelled crops per district and season (Check with TOM)
        Exposure.db              = Exposure.db[Exposure.db[,5] == 1,] #Is_MNAIS_Modeled
        modelled.ui.flag         = 0
        modelled.mismatch.d.flag = 0
        modelled.mismatch.s.flag = 0

        for(i in 1:length(UI_CropSeasonID))
           {
            # Isolate User contracts and exposure db by crop types
              CS_id = UI_CropSeasonID[i]
              tmp.crop.exposure          =   Exposure.db [Exposure.db[,3]  == CS_id,]  #Crop exposure
              tmp.crop.UserInput.db      =   UserInput.db[UserInput.db[,4] == CS_id,]  #Crop user input
              Admin_StateID              =   tmp.crop.exposure$State_ID  #States present in exposure db for a crop type 
              Admin_DistrictID           =   as.character(tmp.crop.exposure$District_ID)             #Districts present in exposure db for a crop type 
              Admin_DistrictID           =   c(Admin_DistrictID, NA)
              

            # Find modelled state id's
#               CropStateID                =   as.numeric(as.character(unique(tmp.crop.exposure$State_ID))))
#               CropStateID = as.data.frame(CropStateID)

              tmp.Crop.State.mismatch    <-  tmp.crop.UserInput.db[tmp.crop.UserInput.db[,2] %ni% Admin_StateID,]
              crop.UserInput.db          <-  tmp.crop.UserInput.db[tmp.crop.UserInput.db[,2] %in% Admin_StateID,]
 
              if(modelled.mismatch.s.flag == 1) {Crop.State.mismatch = rbind(Crop.State.mismatch, tmp.Crop.State.mismatch)}
              if(modelled.mismatch.s.flag == 0) {Crop.State.mismatch = tmp.Crop.State.mismatch; modelled.mismatch.s.flag = 1}


            # Find modelled District id's
              CropDistrictID              =   unique(tmp.crop.exposure$Admin_DistrictID)
              tmp.Crop.District.mismatch  <-  crop.UserInput.db[crop.UserInput.db[,3] %ni% Admin_DistrictID,]
              crop.UserInput.db           <-  crop.UserInput.db[crop.UserInput.db[,3] %in% Admin_DistrictID,]

#             if(modelled.ui.flag == 1)       {tmp.UserInput.db = rbind(tmp.UserInput.db, crop.UserInput.db)}
#             if(modelled.ui.flag == 0)       {tmp.UserInput.db = crop.UserInput.db; modelled.ui.flag = 1}

              if(modelled.mismatch.d.flag == 1) {Crop.District.mismatch = rbind(Crop.District.mismatch, tmp.Crop.District.mismatch)}
              if(modelled.mismatch.d.flag == 0) {Crop.District.mismatch = tmp.Crop.District.mismatch; modelled.mismatch.d.flag = 1}

              if(modelled.ui.flag == 1)       {tmp.UserInput.db = rbind(tmp.UserInput.db, crop.UserInput.db)}
              if(modelled.ui.flag == 0)       {tmp.UserInput.db = crop.UserInput.db; modelled.ui.flag = 1}
             }


          UserInput.db = tmp.UserInput.db

         if(nrow(Crop.State.mismatch) > 0) {Crop.State.mismatch <- cbind(Crop.State.mismatch, 'Crop by State not modelled'); 
         colnames(Crop.State.mismatch) <- Column_names_output}

         if(nrow(Crop.District.mismatch) > 0) {Crop.District.mismatch <- cbind(Crop.District.mismatch, 'Crop by District not modelled'); 
         colnames(Crop.District.mismatch) <- Column_names_output}
      #..........................................................................................................



      #..........................................................................................................
      #  c) Check if [TSI] or [EPI and Premium rate] are provided
      #  d) If all 3 (TSI, EPI & Premium rate) are provided, then check if  EPI/ Premium Rate = approximate(TSI) (decide threshold for "approximate". suggested values could be with 0.5% of the difference)
      #  e) check if premium rate > 1 or less than 0
      #  f) If EPI & TSI are provided check if EPI >= TSI
      #  g) Identify entries where TSI is not reported, and compute TSI (FORMULA: EPI / Premium rate = TSI)

         check_array <- array('Good', dim=c(nrow(UserInput.db),1))

         if (nrow(UserInput.db) > 0)
            { 
              for(i in 1:nrow(UserInput.db))
                 {
                   TSI      = UserInput.db[i,5]
                   EPI      = UserInput.db[i,6]
                   PR       = UserInput.db[i,7]
                   TSI_flag = 1
                   EPI_flag = 1
                   PR_flag  = 1

#                  if (!is.na(TSI)==0) {TSI <- NA}
#                  if (!is.na(EPI)==0) {EPI <- NA}
#                  if (!is.na(PR) ==0) {PR  <- NA}
#
                   if(is.na(TSI)){TSI_flag = 0}
                   if(is.na(EPI)){EPI_flag = 0}
                   if(is.na(PR)) {PR_flag  = 0}
 
                   Not_Blank = TSI_flag + EPI_flag + PR_flag 
                   if(Not_Blank == 0) {check_array[i] = 'TSI / EPI & Premium not reported'}
 
                   EPI_PR = EPI_flag + PR_flag
                   if((TSI_flag == 0) && (EPI_PR == 1)){check_array[i] = 'Insufficient TSI Computation Parameters'}
 
 
                  if(PR_flag == 1) {if (as.numeric(PR) > 1) {check_array[i] = 'Premium rate greater than 100%'}}
                  if(PR_flag == 1) {if (as.numeric(PR) < 0) {check_array[i] = 'Premium rate less than 0%'}}
 
                  if((EPI_flag == 1) && (TSI_flag == 1)) {if (EPI >= TSI) {check_array[i] = 'Premium Income greater Total Sum Insured'}}
 

                  if((EPI_flag == 1) && (TSI_flag == 1) && (PR_flag == 1))
                     {
                      tmp.tsi = as.numeric(EPI) / as.numeric(PR)
                      diff.tsi = as.numeric(TSI) - tmp.tsi
                      ratio.diff = diff.tsi/as.numeric(TSI)
 
                      if(ratio.diff > 0.05) {check_array[i] = 'Difference between computed TSI (EPI * Premium Rate) and reported TSI greater than 5%'}
                     } 
               
                   if((EPI_flag == 1) && (TSI_flag == 0) && (PR_flag == 1))
                      {
                       tmp.tsi = as.numeric(EPI) / as.numeric(PR)
                       UserInput.db[i,5] = as.numeric(tmp.tsi)
                      }
              
                  }
         }
    
         if(nrow(UserInput.db) > 0) {UserInput.db <- cbind(UserInput.db, check_array); 
          colnames(UserInput.db) <- Column_names_output}
        #..........................................................................................................







        #..........................................................................................................
        # combine all array's

          final_checked_userinput.db <- rbind(StateID_mismatch,
                                              DistrictID_mismatch,
                                              Crop_SeasonID_mismatch,
                                              Crop.State.mismatch,
                                              Crop.District.mismatch,
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
           display_array[display_array[,9]=='Crop by District not modelled',9] <- 'Good'
           display_array[display_array[,9]=='District mismatch',9] <- 'Good'

           Data_Audit_Name_Array <<- display_array[display_array[,9] == 'Good',]
           Data_Audit_Name_Array <<- Data_Audit_Name_Array[,-9]
         #...............................................................................


         if(nrow(Data_Audit_Name_Array) > 0)
               {
               #...............................................................................
               # Aggregate state by crop
                 State_levels <- lapply(unique(Data_Audit_Name_Array$State), as.character)
                 Crop_levels  <- lapply(unique(Data_Audit_Name_Array$Crop), as.character)
                 cropl        = length(Crop_levels)
                 output_array <- array(, dim=c(length(State_levels),cropl))


                  for(j in 1:length(State_levels))
                     {
                       state_id = toString(State_levels[j])
                       state.db = Data_Audit_Name_Array[Data_Audit_Name_Array[,2] == state_id,]
 
 
                       for(k in 1:length(Crop_levels))
                          {
                           crop_id = toString(Crop_levels[k])
                           crop.db = state.db[state.db[,4] == crop_id,]

                           if(nrow(crop.db) > 0)
                             {
                              Total_TSI = as.numeric(crop.db[,6])
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
# Convert parameter ID's to names for display purposes
  Get_Display_UserInput <- function(UserInput.db, adminID.db, Product_type.db)  
         {
          `%ni%` <- Negate(`%in%`)
          Season <- array(, dim=c(nrow(UserInput.db),1))
          display.UserInput.db = cbind(UserInput.db[,1:4], Season, UserInput.db[,5:8])
          colnames(display.UserInput.db) <- c('Contract_Number',
                                              'State',
                                              'District',
                                              'Crop',
                                              'Season',
                                              'TSI',
                                              'EPI',
                                              'Premium_rate',
                                              'Check_Output')

            for(i in 1:nrow(UserInput.db))
                {
                StateID      = UserInput.db[i,2]
                DistrictID   = UserInput.db[i,3]
                CropSeasonID = UserInput.db[i,4]

                tmp.state    = adminID.db[adminID.db[,4] %in% StateID,]
                tmp.state    = unique(tmp.state[,c(-1,-3,-5)])

                tmp.district = adminID.db[adminID.db[,5] %in% DistrictID,]
                tmp.district = unique(tmp.district[,c(-1,-2,-4)])

                tmp.CSID     = Product_type.db[as.numeric(as.character(Product_type.db[,4])) %in% CropSeasonID,]
                tmp.CSID     = unique(tmp.CSID[,-1])

                if(nrow(tmp.state) > 0)   {display.UserInput.db[i,2] = lapply(tmp.state[1]   , as.character)}
                if(nrow(tmp.district) > 0){display.UserInput.db[i,3] = lapply(tmp.district[1], as.character)}
                if(nrow(tmp.CSID) > 0)    {display.UserInput.db[i,4] = lapply(tmp.CSID[1]    , as.character)}
                if(nrow(tmp.CSID) > 0)    {display.UserInput.db[i,5] = lapply(tmp.CSID[2]    , as.character)}
               }
               return(display.UserInput.db)
           }
#...............................................................................

    
 
 #...............................................................................
 # Convert all state, district and crop season to ID's
 Convert_Par_to_ID <- function(display_array, adminID.db, Product_type.db)
     {
      StateID.Array    = unique(adminID.db[,c(-1,-3,-5)])
      DistrictID.Array = unique(adminID.db[,c(-1,-2,-4)])

      tmp.ID <- array(, dim=c(nrow(display_array),3))

      for(i in 1:nrow(display_array))
         {
           State        = toString(lapply(display_array[i,2], as.character))
           Crop         = toString(lapply(display_array[i,4], as.character))
           Season       = toString(lapply(display_array[i,5], as.character))

           District_NA_flag = display_array[i,3]
           if(!is.na(District_NA_flag)){District  = toString(lapply(display_array[i,3], as.character))}
           if(is.na(District_NA_flag)){District   = NA}


           StateID.Array = adminID.db [adminID.db[,2] %in% State,]
           if(!is.na(District)){DistrictID.Array  = StateID.Array[StateID.Array[,3] %in% District,]}

           StateID    = unique(StateID.Array[,c(-1,-3,-5)])
           DistrictID = unique(DistrictID.Array[,c(-1,-2,-4)])

           CropID       = Product_type.db[Product_type.db[,2] %in% Crop,]
           CropSeasonID = CropID[CropID[,3] %in% Season,]

           tmp.ID[i,1]  = as.numeric(lapply(StateID[2], as.character))
           if(!is.na(District)){ tmp.ID[i,2] = as.numeric(lapply(DistrictID[,2], as.character))}
           tmp.ID[i,3] = as.numeric(lapply(CropSeasonID[4], as.character))
         }
   
   
   
   
   converted = cbind(display_array[,1],tmp.ID, display_array[,6:8])
   colnames(converted) <- c('Contract_Number','State_ID','District_ID','CropSeasonID','TSI','EPI','Premium_rate')
   
   
   #...............................................................................
   # Compute tsi
   for(i in 1:nrow(converted))
   {
     TSI = converted[i,5]; if(!is.na(TSI)){if(TSI == 0){TSI = NA}}
     EPI = converted[i,6]; if(!is.na(EPI)){if(EPI == 0){EPI = NA}}
     PR  = converted[i,7]; if(!is.na(PR)) {if(PR == 0) {PR = NA} }
   }
   #...............................................................................
   
   return(converted)
 }
 #...............................................................................
 
 
 
 #...............................................................................
 # Convert all state, district and crop season to ID's
 Convert_ID_to_Par_Dissagregate <- function(Dissaggregated_exposure.db, adminID.db, Product_type.db)
 {
   StateID.Array    = unique(adminID.db[,c(-1,-3,-5)])
   DistrictID.Array = unique(adminID.db[,c(-1,-2,-4)])
   
   tmp.ID <- array(, dim=c(nrow(Dissaggregated_exposure.db),4))
   
   for(i in 1:nrow(Dissaggregated_exposure.db))
   {
     State        = lapply(Dissaggregated_exposure.db[i,2], as.character)
     District     = lapply(Dissaggregated_exposure.db[i,3], as.character)
     Crop         = lapply(Dissaggregated_exposure.db[i,4], as.character)


     StateID      = StateID.Array   [StateID.Array[,2]    %in% State,   ] 
     if(District != "<NA>"){DistrictID  = DistrictID.Array[DistrictID.Array[,2] %in% District,]}

     CropID       = Product_type.db[as.numeric(Product_type.db[,4]) %in% as.numeric(Crop),]

     tmp.ID[i,1]  = toString(lapply(StateID[1], as.character))
     if(District != "<NA>"){ tmp.ID[i,2] = toString(lapply(DistrictID[1], as.character))}
     tmp.ID[i,3]  = toString(lapply(CropID[2], as.character))
     tmp.ID[i,4]  = toString(lapply(CropID[3], as.character))
   }
   
   
   
   
   converted = cbind(Dissaggregated_exposure.db[,1],tmp.ID, Dissaggregated_exposure.db[,5:8])
   colnames(converted) <- c('Contract_Number','State_Name','District_name',
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
        
    #   UserInput.db = display_array
    
      #aggregate data at respective admin level to avoid 
        x=UserInput.db[,c(-6,-7)] #remove EPI and PR
        agg_flag = 0

        CropSeason_levels  = unique(x$CropSeasonID)
        for(i in 1:length(CropSeason_levels))
            {
             cs_id = CropSeason_levels[i]
             cropseason.db = x[x[,4] == cs_id,]
             state_levels     = unique(cropseason.db$State_ID)

             for(j in 1:length(state_levels))
                {
                 s_id     = state_levels[j]
                 state.db = cropseason.db[cropseason.db[,2] == s_id,]
  
                 state.db[,3][is.na(state.db[,3])] <- -9999
                 District_levels  = unique(state.db$District_ID)

                for(k in 1:length(District_levels))
                   {

                     d_id = District_levels[k]
                     district.db = state.db[state.db[,3] == d_id,]

                     sumTSI = sum(as.numeric(district.db[,5]))
                     tmp = district.db[1,]
                     tmp[,5] = sumTSI
                     if(d_id == -9999){tmp[,3] <- NA}

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
              CS_id      = UI_CropSeasonID[i]
              ui.Crop.db <- UserInput.db[UserInput.db[,4] == CS_id,]
              ex.Crop.db <- Exposure.db [Exposure.db [,3] == CS_id,]

              UI_StateID = unique(ui.Crop.db$State_ID)
 
              for(j in 1:length(UI_StateID))
                  {
                   TRUE_Count  = 0;
                   FALSE_Count = 0;

                   st_id            =  UI_StateID[j]
                   ui.state_crop.db <- ui.Crop.db[ui.Crop.db[,2] == st_id,]
                   ex.state_crop.db <- ex.Crop.db[ex.Crop.db[,2] == st_id,]

                  #Join Exposure and User input together
                    tmp_contract_id  =  ui.Crop.db$Contract_Number[1]
                    tmp.db           = merge(ui.state_crop.db, ex.state_crop.db, by='District_ID', all='TRUE')

                    state_crop.db    = cbind(tmp_contract_id, st_id, tmp.db$District_ID, CS_id, tmp.db$TSI, 
                                            tmp.db[,9], tmp.db$Planted_Area, tmp.db[,10])
                                           #  Modelled                         Indemnity
                    colnames(state_crop.db) <- c('Contract_Number', 'State_ID', 'District_ID', 'CropSeasonID',  'TSI', 'Modelled', 'Planted_Area', 'Indemnity')


                  #determine if it is reported only at state / state & district / district
                    options(warn=-1)
                    rm(st_cr_district, district_test, tmp.flag.TRUE, tmp.flag.FALSE, TRUE_count, reporting_char)
                    options(warn=0)  

                    st_cr_district  = ui.state_crop.db$District_ID

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
                        state_level_entry    = state_crop.db[is.na(state_crop.db[,3]),,drop = FALSE]
 
                      #Isolate modelled crops
                        modelled_entries = state_crop.db[state_crop.db[,6] > 0,,drop = FALSE]
                        modelled_entries = modelled_entries[complete.cases(modelled_entries[,2]),,drop = FALSE]
                        modelled_entries[,5][is.na(modelled_entries[,5])] <- 0
 
                      #Isolate non modelled crops
 #                       non_modelled_entries = state_crop.db[state_crop.db[,8] == 0,]
 #                       non_modelled_entries = non_modelled_entries[complete.cases(non_modelled_entries[,2]),]
 #                       non_modelled_entries[,5][is.na(non_modelled_entries[,5])] <- 0

                      #deduct all TSI from modelled at State level
                       state_level_entry[5] = as.numeric(state_level_entry[5]) - sum(as.numeric(modelled_entries[,5]))
 
                      #isolate modelled crops where TSI = 0
                       to_distribute_tsi = modelled_entries[modelled_entries[,5] == 0,,drop = FALSE]
                       existing_tsi      = modelled_entries[modelled_entries[,5] > 0,,drop = FALSE]
 
                      #compute ratio
                       ratio_array = as.numeric(to_distribute_tsi[,7])
                       for(a in 1:length(ratio_array)) { to_distribute_tsi[a,5] = (ratio_array[a]/sum(ratio_array)) * as.numeric(state_level_entry[5]) }
 
                      #combine arrays
                       distributed.ui.state_crop.db = rbind(existing_tsi,to_distribute_tsi)
                      }


                    if(reporting_char == 2)
                       {
                      #confirm with premal, that all TSI from non modelled districts get thrown out or not
                      #isolate state level entries
                       state_level_entry    = state_crop.db[1,,drop = FALSE]
                       state_level_entry[5:8] = 0

                      #Isolate modelled crops
                       modelled_entries = state_crop.db[state_crop.db[,6] > 0,,drop = FALSE]
                       modelled_entries = modelled_entries[complete.cases(modelled_entries[,2]),,drop = FALSE]
                       modelled_entries[,5][is.na(modelled_entries[,5])] <- 0
                      
                      #Isolate non modelled crops
                       non_modelled_entries = state_crop.db[state_crop.db[,6] == 0,,drop = FALSE]
                       non_modelled_entries = non_modelled_entries[complete.cases(non_modelled_entries[,2]),,drop = FALSE]
                       non_modelled_entries[,5][is.na(non_modelled_entries[,5])] <- 0
                      
                      #Add all TSI from non-modelled at State level
                      if(nrow(non_modelled_entries) > 0){state_level_entry[5] = sum(as.numeric(non_modelled_entries[,5]))}
                      
                     #isolate modelled crops where TSI = 0
                       to_distribute_tsi = modelled_entries[modelled_entries[,5] == 0,,drop = FALSE]
                       existing_tsi      = modelled_entries[modelled_entries[,5] > 0,,drop = FALSE]
                     
                     #compute ratio
                      if(state_level_entry[5] > 0)
                         {
                           ratio_array = as.numeric(to_distribute_tsi[,7])
                           for(a in 1:length(ratio_array)) {to_distribute_tsi[a,5] = (ratio_array[a]/sum(ratio_array)) * as.numeric(state_level_entry[5]) }
                     
                           #combine arrays
                           distributed.ui.state_crop.db = rbind(existing_tsi,to_distribute_tsi)
                         } 
                     
                     #combine arrays     
                      if(state_level_entry[5] == 0){ distributed.ui.state_crop.db = existing_tsi }
                        
                     }


                    if(reporting_char == 3)
                       {
                      #isolate state level entries
                        state_level_entry    = state_crop.db[is.na(state_crop.db[,8]),,drop = FALSE]
                        

                      #Isolate modelled crops
                         modelled_entries = state_crop.db[state_crop.db[,6] > 0,,drop = FALSE]
                         modelled_entries = modelled_entries[complete.cases(modelled_entries[,2]),,drop = FALSE]
                         modelled_entries[,5][is.na(modelled_entries[,5])] <- 0

                      #isolate modelled crops where TSI = 0
                        to_distribute_tsi = modelled_entries[modelled_entries[,5] == 0,,drop = FALSE]
 
                      #compute ratio

                         ratio_array = to_distribute_tsi[,7]
                         for(a in 1:length(ratio_array)) { to_distribute_tsi[a,5] = (ratio_array[a]/sum(ratio_array)) * state_level_entry[5] }
 
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

         ui_exposure  = merge(UserInput.db, Exposure.db, by=c('State_ID', 'District_ID', 'CropSeasonID'))
         ui_exposure_gy = merge(ui_exposure, gy.db, by=c('State_ID', 'District_ID', 'CropSeasonID'))
         
         ui_exposure_gy_truncated = ui_exposure_gy[,c(-4, -9, -10, -11, -13, -14, -15)]
         final_gy = cbind(ui_exposure_gy_truncated[,1:7], ui_exposure_gy_truncated[,9], ui_exposure_gy_truncated[,10], ui_exposure_gy_truncated[,8])
         colnames(final_gy) <- c('State_ID','District_ID','CropSeasonID','TSI','Modelled','Planted_Area','Indemnity','Year','Yield','Guaranteed_GY')
 
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
  LOB_Pie_Plot <- function(x)
       {
         x[is.na(x)] <- 0
         newdata = colSums(x)
         slices <- c(newdata) 
         lbls <- c(names(newdata))
         pct <- round(slices/sum(slices)*100)
         lbls <- paste(lbls, pct) # add percents to labels 
         lbls <- paste(lbls,"%",sep="") # ad % to labels 
         pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Pie Chart for Line of Business")
        }
#...............................................................................


#...............................................................................
# LOB Pie Plot
  State_TSI_Plot <- function(x)
    {    
      data_audit_array[is.na(data_audit_array)] <- 0
      newdata = rowSums(data_audit_array)
      slices <- c(newdata)
      lbls   <- c(names(newdata))
      barplot(newdata, main="Aggregated TSI per State", xlab="State", col="darkblue")
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
          # TSI
            gy.db[,5]  <- format(round((as.numeric(as.character(gy.db[,5]))), 0), numeric = TRUE) 
            gy.db[,5]  =  format(gy.db[,5], scientific = FALSE)

          # Planted Area
            gy.db[,7]  <- format(round((as.numeric(as.character(gy.db[,7]))), 0), numeric = TRUE) 
            gy.db[,7]  =  format(gy.db[,7], scientific = FALSE)

          # Avg actual GY
            gy.db[,9]  <- format(round((as.numeric(as.character(gy.db[,9]))), 2), numeric = TRUE)
            gy.db[,9]  =  format(gy.db[,9], scientific = FALSE)

          # Guaranteed GY
            gy.db[,10]  <- format(round((as.numeric(as.character(gy.db[,10]))), 2), numeric = TRUE) 
            gy.db[,10]  =  format(gy.db[,10], scientific = FALSE)

          # Threshold GY
            gy.db[,11]  <- format(round((as.numeric(as.character(gy.db[,11]))), 2), numeric = TRUE) 
            gy.db[,11]  =  format(gy.db[,11], scientific = FALSE)

          # Avg Shortfall GY
            gy.db[,12]  <- format(round((as.numeric(as.character(gy.db[,12]))), 2), numeric = TRUE)
            gy.db[,12]  =  format(gy.db[,12], scientific = FALSE)

          # Loss_Sum
            gy.db[,15]  <- format(round((as.numeric(as.character(gy.db[,15]))), 0), numeric = TRUE) 
            gy.db[,15]  =  format(gy.db[,15], scientific = FALSE)

          # Loss_max
            gy.db[,17]  <- format(round((as.numeric(as.character(gy.db[,17]))), 0), numeric = TRUE) 
            gy.db[,17]  =  format(gy.db[,17], scientific = FALSE)

          # Burning Cost
            gy.db[,18]  <- format(round((as.numeric(as.character(gy.db[,18]))), 0), numeric = TRUE) 
            gy.db[,18]  =  format(gy.db[,18], scientific = FALSE)

            return(gy.db)
          }
 #...............................................................................
 