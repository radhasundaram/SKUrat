#Get the data for Segment - Block at a Brand level calculate SKU % 
rm(list=ls())
setwd("D:/Mars/SKU/Codes")
df <- read.csv("D:/Mars/SKU/Data/Import_LDESC.csv")
require(dplyr)
library(lubridate)
library(tidyr)

Block_Segment=dplyr::filter(df, grepl('Block', Segment))
Block_Segment <- data.frame(Block_Segment)
saveRDS(Block_Segment,"Block_Segment.RData")

model_regularization_parameter <- 1
#Cut Off/ Thresholds for Data Filtering- PPGs to exclude from model set consideration
min_revenue_threshold <- 0 
#Minimum Data Present
min_prct_data_points <- 0.9
# Low Variance Cutoff
#Qty. Sold
sd_by_mean_sales_vol_threshold <- 0.01
# Avg. price
sd_by_mean_avg_price_threshold <- 0.01
#Price Minimum Correlation Cut Off
model_correlation_feature_shortlist <- 0.05
#TPR Minimum Correlation Cut Off
model_correlation_feature_shortlist_tpr <- (-0.05)
#Price Trend Acting Item Correlation Cut Off
model_correlation_price_trend_cutoff <- 0.5
#R Square Threshold to account vol. contri into cannibalisation
cannibalisation_cor_cutoff <- 0.5
#ACV Cutoff
Retailer_ACV_CUTOFF <- 15
ROM_ACV_CUTOFF <- 15
#Interactions cutoff
min_interaction_rp_cutoff <- 0.1
max_interaction_rp_cutoff <- 0.9
####Dates
end_date <- "2020-03-08"
start_date <- "2017-04-02"
#start_date <- "2017-06-04"
Retailer_filt <- "Retailer"
Retailer_filt_1 <- "Coles"
#################################################### Input Files ######################################
#Retailer and LTA Pos: US Total X PPG X Week data merged with TMT Weekly Data
#merged_ustotal_ppg_ip_filename <- paste("AUSTOTALPPGWeek_",Category_filt,".rData",sep = "")
#################################################### Output Files ######################################
# Output filter file
#filtered_model_dataset_filename <- paste("Model_",Segment1,"_",Retailer_filt_1,sep = "")
#filtered_ppg_filename <- paste("Filtered_PPG_Details_",Segment1,"_",Retailer_filt_1,".csv",sep = "")
####################################Filtering Data based on Constraints################################
source("Functions.R")

merged_ustotal_ppg_ip_filename<-paste("Block_Segment",".rData",sep = "")
data_filtered_after_constraints(
  Block_Segment,
  min_revenue_threshold,
  min_prct_data_points,
  sd_by_mean_avg_price_threshold,
  sd_by_mean_sales_vol_threshold,
  filtered_model_dataset_filename,
  Retailer_ACV_CUTOFF,
  ROM_ACV_CUTOFF,
  filtered_ppg_filename,
  start_date,end_date,Retailer_filt)
 
