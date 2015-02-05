setwd('C:/Nirav/IARP/Beta_Model_BI_V2/Beta_Model_BI_V2/data')

Crops       <- read.csv('Crops.csv'        , header = TRUE)
saveRDS(Crops, file="Crops.Rds")

CropSeasons <- read.csv('CropSeasons.csv'  , header = TRUE)
saveRDS(CropSeasons, file="CropSeasons.Rds")

Districts   <- read.csv('Districts.csv'    , header = TRUE)
saveRDS(Districts, file="Districts.Rds")

Risk_Items  <- read.csv('Risk_Items.csv'   , header = TRUE)
saveRDS(Risk_Items, file="Risk_Items.Rds")

Seasons     <- read.csv('Seasons.csv'      , header = TRUE)
saveRDS(Seasons, file="Seasons.Rds")

States      <- read.csv('States.csv'       , header = TRUE)
saveRDS(States, file="States.Rds")


Risk_Items_YearWise_Historical_Yields <- read.csv('Risk_Items_YearWise_Historical_Yields.csv' , header = TRUE)
saveRDS(Risk_Items_YearWise_Historical_Yields, file="WBCIS_yearwise_LC.Rds")

Risk_Items_YearWise_LossCosts         <- read.csv('Risk_Items_YearWise_LossCosts.csv'         , header = TRUE)
saveRDS(Risk_Items_YearWise_LossCosts, file="Risk_Items_YearWise_LossCosts.Rds")

Risk_Items_YearWise_Synthetic_Yields  <- read.csv('Risk_Items_YearWise_Synthetic_Yields.csv'  , header = TRUE)
saveRDS(Risk_Items_YearWise_Synthetic_Yields, file="Risk_Items_YearWise_Synthetic_Yields.Rds")

