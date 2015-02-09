
#..........................................................................................................
#  a) check if all  State_ID & District_ID are valid.
#  b) Check if all Crop_ID's and Season_ID's are valid.

Check_UserInput_Name_Mismatch <- function(UserInput.db,adminID.db,Exposure.db,Product_type.db)
{      
  `%ni%`          <-Negate(`%in%`)
  
  #..........................................................................................................
  #  a) check if all  State_ID & District_ID are valid.
  #  b) Check if all Crop_ID's and Season_ID's are valid.
  Admin_State          =   as.character((unique(adminID.db[,2])))
  Admin_District       =   as.character((unique(adminID.db[,3])))
  Prod_Crop            =   as.character((unique(Product_type.db[,2])))
  Prod_Season          =   as.character((unique(Product_type.db[,3])))
  
  State_mismatch       <-  UserInput.db[as.character(UserInput.db[,2]) %ni% Admin_State,]
  UserInput.db         <-  UserInput.db[as.character(UserInput.db[,2]) %in% Admin_State,]
  
  District_mismatch    <-  UserInput.db[as.character(UserInput.db[,3]) %ni% Admin_District,]
  UserInput.db         <-  UserInput.db[as.character(UserInput.db[,3]) %in% Admin_District,]
  
  Crop_mismatch        <-  UserInput.db[as.character(UserInput.db[,4]) %ni% Prod_Crop,]
  UserInput.db         <-  UserInput.db[as.character(UserInput.db[,4]) %in% Prod_Crop,]
  
  Season_mismatch      <-  UserInput.db[as.character(UserInput.db[,5]) %ni% Prod_Season,]
  UserInput.db         <-  UserInput.db[as.character(UserInput.db[,5]) %in% Prod_Season,]
  
  
  # store all mismatches
  Column_names_output <- c('rownumber','State_Name','District_Name','Crop','Season','TSI','EPI','Premium_rate','MNAIS_Check','WBCIS_Check') 
  
  if(nrow(State_mismatch)    > 0) {State_mismatch     <- cbind(State_mismatch,    'State mismatch'   , 'State mismatch')   ; colnames(State_mismatch)    <- Column_names_output} 
  if(nrow(District_mismatch) > 0) {District_mismatch  <- cbind(District_mismatch, 'District mismatch', 'District mismatch'); colnames(District_mismatch) <- Column_names_output}
  if(nrow(Crop_mismatch)     > 0) {Crop_mismatch      <- cbind(Crop_mismatch,     'Crop mismatch'    , 'Crop mismatch')    ; colnames(Crop_mismatch)     <- Column_names_output}
  if(nrow(Season_mismatch)   > 0) {Season_mismatch    <- cbind(Season_mismatch,   'Season mismatch'  , 'Season mismatch')  ; colnames(Season_mismatch)   <- Column_names_output}
  if(nrow(UserInput.db)      > 0) {UserInput.db          <- cbind(UserInput.db,   'Good'  , 'Good')  ; colnames(UserInput.db)   <- Column_names_output}
  UserInput.db <- rbind(UserInput.db,State_mismatch, District_mismatch, Crop_mismatch, Season_mismatch)
  #..........................................................................................................
  
  return(UserInput.db)
}
#..........................................................................................................

#..........................................................................................................
# Prepare Exposure DB
Check_UserInput_Prepare_Exposure_db <- function(adminID.db,Exposure.db,Product_type.db)
{
  # add names to exposure db parameters
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
  
  # prepare Exposure.db for entries where district has not been stated
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
      }      }     }
  
  Exposure.db = rbind(Exposure.db,final.Exposure.db )
  return(Exposure.db)
}
#..........................................................................................................



#..........................................................................................................
# Check if districts and states are modelled
  Check_UserInput_modelled_adminlevel<- function(UserInput.db, Exposure.db)
       {
        UserInput.db2 = as.matrix(UserInput.db)
        #UserInput.db2 = as.matrix(UserInput_name_mismatch)
      # remove all mismatches except district
      # x=UserInput.db2[UserInput.db2[,9]  == 'District mismatch',]
        UserInput.db2[UserInput.db2[,9]  == 'District mismatch',3]  <- 'All'
        UserInput.db2[UserInput.db2[,9]  == 'District mismatch',9]  <- 'Good'
        UserInput.db2[UserInput.db2[,10] == 'District mismatch',10] <- 'Good'
        UserInput.db <- UserInput.db2[UserInput.db2[,9] == 'Good',]
        UserInput.db <- UserInput.db[,c(-9,-10)]

      # merge with user input
        ui = merge(UserInput.db, Exposure.db, by=c('State_Name','District_Name','Crop','Season'), all.x = TRUE)
        ui = cbind(ui[,5],ui[,1:4], ui[,6:10], ui[,9:10])
        colnames(ui) <- c('rownumber', 'State_Name','District_Name','Crop','Season','TSI','EPI','Premium_rate','Is_MNAIS_Modeled','Is_WBCIS_Modeled','State_Is_MNAIS_Modeled','State_Is_WBCIS_Modeled')

       state_levels <- unique(as.character(ui$State_Name))
       db_flag = 0

      for(i in 1:length(state_levels))
         {
          st_l        = state_levels[i]
          state.ui.db = ui[as.character(ui[,2]) == st_l,]
          crop_levels = unique(as.character(state.ui.db$Crop))

          for(j in 1:length(crop_levels))
             {
               cr_l       = crop_levels[j]
               crop.ui.db = state.ui.db[as.character(state.ui.db[,4]) == cr_l,]
               season_levels = unique(as.character(state.ui.db$Season))

            for(k in 1:length(season_levels))
               {
                sn_l = season_levels[k]
                season.ui.db = crop.ui.db[crop.ui.db[,5] == sn_l,]

               if(nrow(season.ui.db)> 0)
                     {
                       mnais <- sum(season.ui.db$State_Is_MNAIS_Modeled)
                       wbcis <- sum(season.ui.db$State_Is_WBCIS_Modeled)

                       if(mnais == 0){season.ui.db[,11] = 0}
                       if(wbcis == 0){season.ui.db[,12] = 0}
                       if(mnais > 0) {season.ui.db[,11] = 1}
                       if(wbcis > 0) {season.ui.db[,12] = 1}
                      }
        
        if(db_flag == 1){ final.ui <- rbind(final.ui, season.ui.db)}    
        if(db_flag == 0){ final.ui <- season.ui.db; db_flag = 1    }
      }    }      }
  
      ui = cbind(final.ui, final.ui[,11:12])

      colnames(ui) <- c('rownames','State_Name','District_Name','Crop','Season','TSI','EPI','Premium_rate',
                        'Is_MNAIS_Modeled','Is_WBCIS_Modeled','State_Is_MNAIS_Modeled','State_Is_WBCIS_Modeled',
                        'MNAIS_Check', 'WBCIS_Check')

      ui = as.matrix(ui)

     # put in flags

      ui[ui[,9] == 0,  13] <- 'Crop by District not modelled'
      ui[ui[,10] == 0,  14] <- 'Crop by District not modelled'

      ui[ui[,11] == 0, 13] <- 'Crop by State not modelled'
      ui[ui[,12] == 0, 14] <- 'Crop by State not modelled'

      UserInput.db = ui[,c(-9,-10,-11,-12)]
      UserInput.db[UserInput.db[,9]  == 1,9] <- 'Good'
      UserInput.db[UserInput.db[,10] == 1,10] <- 'Good'

      return(UserInput.db)
     }
#..........................................................................................................

#..........................................................................................................
#  c) Check if [TSI] or [EPI and Premium rate] are provided
#  d) If all 3 (TSI, EPI & Premium rate) are provided, then check if  EPI/ Premium Rate = approximate(TSI) (decide threshold for "approximate". suggested values could be with 0.5% of the difference)
#  e) check if premium rate > 1 or less than 0
#  f) If EPI & TSI are provided check if EPI >= TSI
#  g) Identify entries where TSI is not reported, and compute TSI (FORMULA: EPI / Premium rate = TSI)

    Check_UserInput_TSI_check <- function(UserInput.db)
         {   

          if (nrow(UserInput.db) > 0)
             { 
              for(i in 1:nrow(UserInput.db))
                  {
                    ratio.diff = 0
                    TSI      = as.numeric(as.character(UserInput.db[i,6]))
                    EPI      = as.numeric(as.character(UserInput.db[i,7]))
                    PR       = as.numeric(as.character(UserInput.db[i,8]))

                    if(is.na(TSI) || (TSI == 0)){TSI_flag = 0; TSI = 0}
                    if(is.na(EPI) || (EPI == 0)){EPI_flag = 0; EPI = 0}
                    if(is.na(PR)  || (PR == 0)) {PR_flag  = 0; PR = 0 }

                    if(TSI > 0) {TSI_flag = 1}
                    if(EPI > 0) {EPI_flag = 1}
                    if(PR > 0)  {PR_flag  = 1}

                    Not_Blank = TSI_flag + EPI_flag + PR_flag 
                    if(Not_Blank == 0) {UserInput.db[i,9:10] = 'TSI / EPI & Premium not reported'}

                     EPI_PR = EPI_flag + PR_flag
                    if((TSI_flag == 0) && (EPI_PR == 1)){UserInput.db[i,9:10] = 'Insufficient TSI Computation Parameters'}

                    if(PR_flag == 1) {if (as.numeric(PR) > 1) {UserInput.db[i,9:10] = 'Premium rate greater than 100%'}}
                    if(PR_flag == 1) {if (as.numeric(PR) < 0) {UserInput.db[i,9:10] = 'Premium rate less than 0%'}}

                    if((EPI_flag == 1) && (TSI_flag == 1)) {if (EPI >= TSI) {UserInput.db[i,9:10] = 'Premium Income greater than Total Sum Insured'}}

                    if((EPI_flag == 1) && (TSI_flag == 1) && (PR_flag == 1))
                          {
                             tmp.tsi    = as.numeric(EPI) / as.numeric(PR)
                             diff.tsi   = as.numeric(TSI) - tmp.tsi
                             ratio.diff = diff.tsi/as.numeric(TSI)

                             if(ratio.diff > 0.05) {UserInput.db[i,9:10] = 'Difference between computed TSI (EPI * Premium Rate) and reported TSI greater than 5%'}
                          }

                    if((EPI_flag == 1) && (TSI_flag == 0) && (PR_flag == 1))
                      {
                        tmp.tsi = as.numeric(EPI) / as.numeric(PR)
                         UserInput.db[i,5] = as.numeric(tmp.tsi)
                       }
                }
             }
  
            return(UserInput.db)
         }
#..........................................................................................................


#...............................................................................
# Check all CSV user input for valid and invalid entires
  Check_UserInput <- function(UserInput.db,adminID.db,Exposure.db,Product_type.db, Check_UserInput_Name_Mismatch,  Check_UserInput_Prepare_Exposure_db, Check_UserInput_modelled_adminlevel, Check_UserInput_TSI_check)
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
         UserInput.db[is.na(UserInput.db[,5]),5] <- 0
         UserInput.db[is.na(UserInput.db[,6]),6] <- 0
         UserInput.db[is.na(UserInput.db[,7]),7] <- 0

         options(warn=-1);UserInput.db[is.na(UserInput.db[,2]),2] <- 'All';options(warn=0);

         UserInput.db = cbind(rownames(UserInput.db), UserInput.db)
         colnames(UserInput.db) <- c('rownumber','State_Name','District_Name','Crop','Season','TSI','EPI','Premium_rate')
  
         #...............................................................................
         #  a) check if all  State_ID & District_ID are valid.
         #  b) Check if all Crop_ID's and Season_ID's are valid.
               UserInput_name_mismatch   <- Check_UserInput_Name_Mismatch(UserInput.db,adminID.db,Exposure.db,Product_type.db)
         #...............................................................................

         #..........................................................................................................
         #  c) Check if District_ID's, Crop_ID's and Season_ID's are valid as per the MNAIS Term database.
         #  d) Check if all reported entries are modelled crops per district and season (Check with TOM)
                Exposure.db    <- Check_UserInput_Prepare_Exposure_db(adminID.db,Exposure.db,Product_type.db)
         #..........................................................................................................


         #..........................................................................................................
         #   Check if districts and states are modelled
         #   Check if all reported entries are modelled crops per district and season
                 UserInput_is_modelled    <- Check_UserInput_modelled_adminlevel(UserInput_name_mismatch, Exposure.db)
         #..........................................................................................................
  
         #..........................................................................................................
         #  c) Check if [TSI] or [EPI and Premium rate] are provided
         #  d) If all 3 (TSI, EPI & Premium rate) are provided, then check if  EPI/ Premium Rate = approximate(TSI) (decide threshold for "approximate". suggested values could be with 0.5% of the difference)
         #  e) check if premium rate > 1 or less than 0
         #  f) If EPI & TSI are provided check if EPI >= TSI
         #  g) Identify entries where TSI is not reported, and compute TSI (FORMULA: EPI / Premium rate = TSI)
                UserInput_TSI_check    <- Check_UserInput_TSI_check(UserInput_is_modelled)
         #..........................................................................................................
  
               UserInput_name_mismatch = as.data.frame(UserInput_name_mismatch)
               UserInput_is_modelled   = as.data.frame(UserInput_is_modelled)
               UserInput_TSI_check     = as.data.frame(UserInput_TSI_check)
  
         #..........................................................................................................
         # combine and prioritise check messages
               final_flag = 0
               for(i in 1:rowL)
                 {
                    name_mismatch       <- as.matrix(UserInput_name_mismatch[as.numeric(as.character(UserInput_name_mismatch[,1])) == i,,drop = FALSE])
                    MNAIS_name_mismatch <- as.character(name_mismatch[1,9,drop=FALSE])
                    WBCIS_name_mismatch <- as.character(name_mismatch[1,10,drop=FALSE])


                     is_modelled         <- as.matrix(UserInput_is_modelled[as.numeric(as.character(UserInput_is_modelled[,1])) == i,,drop = FALSE])
                     if(nrow(is_modelled) > 0)
                        {
                          MNAIS_is_modelled   <- as.character(is_modelled[1,9,drop=FALSE])
                          WBCIS_is_modelled   <- as.character(is_modelled[1,10,drop=FALSE])
                         }

                     TSI_Check           <- as.matrix(UserInput_TSI_check[as.numeric(as.character(UserInput_TSI_check[,1])) == i,,drop = FALSE])
                     if(nrow(TSI_Check) > 0)
                         {
                           MNAIS_TSI_Check     <- as.character(TSI_Check[1,9,drop=FALSE])
                           WBCIS_TSI_Check     <- as.character(TSI_Check[1,10,drop=FALSE])
                         }

                     if(nrow(is_modelled) > 0){ if(MNAIS_is_modelled != 'Good'){name_mismatch[1,9] <- as.character(MNAIS_is_modelled)}}
                     if(nrow(TSI_Check) > 0) { if(MNAIS_TSI_Check   != 'Good'){name_mismatch[1,9] <- as.character(MNAIS_TSI_Check)}}

                     if(nrow(is_modelled) > 0){ if(WBCIS_is_modelled != 'Good'){name_mismatch[1,10] <- as.character(WBCIS_is_modelled)}}
                     if(nrow(TSI_Check) > 0) { if(WBCIS_TSI_Check   != 'Good'){name_mismatch[1,10] <- as.character(WBCIS_TSI_Check)}}
                     

                    if(final_flag == 1){ final.ui <- rbind(final.ui, name_mismatch)}    
                    if(final_flag == 0){ final.ui <- name_mismatch; final_flag = 1 }
                  }
  
              final_checked_userinput.db = as.data.frame(final.ui[,-1])
              return(final_checked_userinput.db)
          #..........................................................................................................
        }
#...............................................................................
