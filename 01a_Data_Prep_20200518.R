#####################Libraries######################################
rm(list = ls())

library(rstudioapi)
library(data.table)
library(dplyr)
library(lubridate)
library(readxl)
library(readr)
library(zoo)
library(data.table)
library(openxlsx)
library(stringr)

setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path)))
source("Functions.R")
setwd("../Data/")
################### Read Master Mapping File data ############################
Dat1 <- read_excel("../MappingFile/Mapping.xlsx",sheet = "Data")
Dat1 <- Dat1[,c("Description","Category","Sub Category","Segment","Sub Segment","Manufacturer","Packsize","Product Type","Promo Group","Brand","Sub Brand","Flavour","APN")]
# unique(Dat1$Segment) %like% "Chocolate"
# Dat1$Segment = ifelse(Dat1$Segment %like% "Chocolate", Dat1$Segment, paste("Chocolate",Dat1$Segment,sep = " "))
Dat1$Description <- as.character(Dat1$Description)
Dat1$desc <- Dat1$Description
Dat1$desc <- tolower(gsub(" ","",Dat1$desc))
Dat1 <- unique(Dat1)
#nrow(Dat1)
# Dat1$desc[Dat1$Description == "Mars Pods Mint 160g"] <- "podsmint160g"
# Dat1$desc[Dat1$Description == "Mars Snickers Funsize Sharepck 12pk 216g"] <- "snickersfunsizesharepack12pk216g"
# Dat1$`Promo Group`[Dat1$desc == "maltesersvanillashake140g"] <- "MW Med Bitesize Malts"
# Dat1$desc[Dat1$Description == "Mars Snickers Sharepack 20pk 360g" ] <- "snickerssharepack20pk360g"

Dat1$`Promo Group`[is.na(Dat1$`Promo Group`)] = "N/A"

# Dat1 = Dat1[!(Dat1$`Promo Group`=="N/A" & is.na(Dat1$Packsize)),]

################## Nielsen Data ######################
# Files <- c("Metric_Val Sales.csv","Metric_Vol Sales.csv","Metric_Wtd Selling Dist.csv")
# 
# dat1 <- fread("Metric_Unit Sales.csv")
# for (i in Files) {
#   dat2 <- fread(i)
#   dat1 <- merge(dat1,dat2,by = c("LDESC","Date","Retailer"))
# }


sheets <- c("Val Sales","Vol Sales","Unit Sales","Wtd Selling Dist")
dat1 <- data.table()
for (sheet in sheets) {
  dat <- read_excel("Choc_Nielsen_Data.xlsx",sheet = sheet)
  week_names <- names(dat)[4:length(names(dat))]
  names_processed <- unlist(lapply(week_names, process_name))
  names(dat)[4:length(names(dat))] <- c(names_processed)
  names(dat)[1:3] <- c("Retailer", "LDESC","Metric")
  dat <- data.table(dat)
  dat <- melt(dat,variable.name = "Date",value.name = sheet,id.vars = c("Retailer", "LDESC","Metric"))
  dat <- data.table(dat)
  dat[,Metric :=  NULL]
  dat[,Date := as.Date(Date,format = "%d/%m/%Y")]
  if (nrow(dat1) == 0) {
    dat1 <- dat
  }else{
    dat1 <- merge(dat1,dat,by = c("Retailer","LDESC","Date"),all.x = T)
  }
  
}
dat1 <- dat1[`Val Sales` > 0]
dat2 <- NULL
dat <- NULL
dat1$LDESC <- as.character(dat1$LDESC)
dat1$ldesc <- tolower(gsub(" ","",dat1$LDESC))

dat1 <- merge(dat1,Dat1,by.x = "ldesc",by.y = "desc",all.x = T)
#dat1 <- dat1[!is.na(Category) & `Val Sales` > 0,]

dat1$Date_2 <- as.Date(dat1$Date,format = "%d/%m/%Y")

dat1_chk = copy(dat1)

dat1_chk <- dat1_chk[,c("Retailer","LDESC","Category","Sub Category","Segment",
                        "Sub Segment","Manufacturer","Brand","Sub Brand","Packsize",
                        "Product Type","Flavour","Promo Group","APN",
                        "Date_2",
                        'Unit Sales','Val Sales','Vol Sales','Wtd Selling Dist'
                        )]

#Rename Columns
setnames(dat1_chk,c("LDESC","Date_2"),
         c("Description","Date")
         )
nrow(dat1_chk)

#data = copy(dat1_chk)

###Reading Nielsen Fresh Data, LDESC level#####
Nielsen_LDESC_data <- copy(dat1_chk)
LDESC_to_Pack_mapping <- read_excel("LDESC_Promo_Group_MAPPING_TO_PROMO_FILE.xlsx")
LDESC_to_Pack_mapping = unique(LDESC_to_Pack_mapping[,c("Retailer","Segment","Brand","Promo Group","PackSize")])

Total_Category_data <- merge(Nielsen_LDESC_data,LDESC_to_Pack_mapping,by = c("Retailer","Segment","Brand","Promo Group"),all.x = T)
Total_Category_data$Manufacturer <-ifelse(Total_Category_data$Manufacturer=="Mars Wrigley","Mars Wrigley","Non-Mars Wrigley")

#Rolled up at Promo Group + PackSize
Total_PromoGroup_data <- Total_Category_data %>% group_by(Retailer,Manufacturer,`Promo Group`,Segment,PackSize,Date) %>% summarise(Sales = sum(`Val Sales`),Unit_sales = sum(`Unit Sales`),Vol_Sales = sum(`Vol Sales`),wtd_distribution = max(`Wtd Selling Dist`)) 
Total_PromoGroup_data$Retailer[Total_PromoGroup_data$Retailer == "Aus Coles Group"] <- "Coles"
Total_PromoGroup_data$Retailer[Total_PromoGroup_data$Retailer == "Aus Woolworths"] <- "Woolworths"
# Total_PromoGroup_data$Manufacturer <-ifelse(Total_PromoGroup_data$Manufacturer=="Mars Wrigley","Mars Wrigley","Non-Mars Wrigley")

#PPG_ITEM mapping (PackSize+Promo Group)
item_no_mapping <- read_excel("../MappingFile/CHOC_PROMO_MAPPING_V3.xlsx")
item_no_mapping <- item_no_mapping[,c("Manufacturer","Promo Group","Retailer","PackSize","Item_No")]
item_no_mapping <- unique(item_no_mapping)

Total_PromoGroup_data <- merge(Total_PromoGroup_data,item_no_mapping ,by = c("Manufacturer","Retailer", "Promo Group","PackSize"),all.x = T )
Total_PromoGroup_data <- setDT(Total_PromoGroup_data)

Total_PromoGroup_data$PPG_Item_No <- paste0("Item_",Total_PromoGroup_data$Item_No)
Total_PromoGroup_data$Date <- as.Date(Total_PromoGroup_data$Date)

setorderv(Total_PromoGroup_data,c("Retailer","PPG_Item_No","Date"))
Total_PromoGroup_data$wk_sold_avg_price_byppg <- Total_PromoGroup_data$Sales/Total_PromoGroup_data$Unit_sales
Total_PromoGroup_data[,paste("lag_price_",1:7,sep = "") := shift(wk_sold_avg_price_byppg,1:7),
                      by = list(Retailer,PPG_Item_No)]
Total_PromoGroup_data[,max_price_prev := max(lag_price_1,
                                             lag_price_2,
                                             lag_price_3,
                                             lag_price_4,
                                             lag_price_5,
                                             lag_price_6,
                                             lag_price_7,na.rm = TRUE),by = list(Retailer,PPG_Item_No,Date)]
Total_PromoGroup_data[,lag_price_1 := ifelse(abs((lag_price_1 - max_price_prev)/max_price_prev) <= 0.05,lag_price_1,NA)]
Total_PromoGroup_data[,lag_price_2 := ifelse(abs((lag_price_2 - max_price_prev)/max_price_prev) <= 0.05,lag_price_2,NA)]
Total_PromoGroup_data[,lag_price_3 := ifelse(abs((lag_price_3 - max_price_prev)/max_price_prev) <= 0.05,lag_price_3,NA)]
Total_PromoGroup_data[,lag_price_4 := ifelse(abs((lag_price_4 - max_price_prev)/max_price_prev) <= 0.05,lag_price_4,NA)]
Total_PromoGroup_data[,lag_price_5 := ifelse(abs((lag_price_5 - max_price_prev)/max_price_prev) <= 0.05,lag_price_5,NA)]
Total_PromoGroup_data[,lag_price_6 := ifelse(abs((lag_price_6 - max_price_prev)/max_price_prev) <= 0.05,lag_price_6,NA)]
Total_PromoGroup_data[,lag_price_7 := ifelse(abs((lag_price_7 - max_price_prev)/max_price_prev) <= 0.05,lag_price_7,NA)]
Total_PromoGroup_data[,median_baseprice := median(c(lag_price_1,
                                                    lag_price_2,
                                                    lag_price_3,
                                                    lag_price_4,
                                                    lag_price_5,
                                                    lag_price_6,
                                                    lag_price_7),na.rm = TRUE),by = list(Retailer,PPG_Item_No,Date)]

Total_PromoGroup_data[,median_baseprice := ifelse((median_baseprice -
                                                     wk_sold_avg_price_byppg)/median_baseprice <= 0.05,
                                                  wk_sold_avg_price_byppg,median_baseprice)]
setorderv(Total_PromoGroup_data,c("Retailer","PPG_Item_No","Date"))

#Add retailer
Total_PromoGroup_data <- Total_PromoGroup_data %>% group_by(PPG_Item_No,Retailer) %>% mutate(Final_BasePrice = rollapply(median_baseprice,width = 26,FUN = max,na.rm = T,partial = T,align = c("right")))
Total_PromoGroup_data$Final_BasePrice[Total_PromoGroup_data$Final_BasePrice == -Inf] <- Total_PromoGroup_data$median_baseprice[Total_PromoGroup_data$Final_BasePrice == -Inf] 
Total_PromoGroup_data$Final_BasePrice[is.na(Total_PromoGroup_data$Final_BasePrice)] <- Total_PromoGroup_data$wk_sold_avg_price_byppg[is.na(Total_PromoGroup_data$Final_BasePrice)] 


Total_PromoGroup_data <- setDT(Total_PromoGroup_data)
Total_PromoGroup_data[,median_baseprice := ifelse(abs((Final_BasePrice - median_baseprice)/Final_BasePrice) >= 0.05,Final_BasePrice*.95,median_baseprice)]

##############################Seasonality Index###############################
print(min(Total_PromoGroup_data$Date))
print(max(Total_PromoGroup_data$Date))
si_index <- Total_PromoGroup_data %>% filter( Date >= min(Total_PromoGroup_data$Date) & Date <= max(Total_PromoGroup_data$Date)) %>% group_by(Segment,Date) %>% summarise(Sales = sum(Sales,na.rm = T))
si_index <- setDT(si_index)
si_index <- setorderv(si_index,c("Segment","Date"))
si_index$Year <- rep(c(rep(1,52),rep(2,52),rep(3,52)),length(unique(si_index$Segment)))
si_index$week <- rep(rep(c(1:52),3),length(unique(si_index$Segment)))
si_index <- si_index %>% group_by(Segment,Year) %>% mutate(mean_sales = mean(Sales,na.rm = T),SI = Sales/mean_sales)
si_index <- si_index %>% group_by(Segment,week) %>% mutate(SI = mean(SI,na.rm = T))
names(si_index)[names(si_index) %in% "Sales"] <- "Segment_sales" 
Total_PromoGroup_data <- merge(Total_PromoGroup_data,si_index[,c("Date","Segment","SI")],by = c("Segment","Date"),all.x = T)



Total_PromoGroup_data$PPG_MFG <- ifelse(Total_PromoGroup_data$Manufacturer == "Mars Wrigley","Mars Wrigley","Non-Mars Wrigley")
Total_PromoGroup_data$PPG_Retailer <- Total_PromoGroup_data$Retailer
Total_PromoGroup_data$PPG_Seg <- Total_PromoGroup_data$Segment
Total_PromoGroup_data$PPG_Description <- Total_PromoGroup_data$`Promo Group`

Total_PromoGroup_data$wk_sold_doll_byppg <- Total_PromoGroup_data$Sales
Total_PromoGroup_data$wk_sold_qty_byppg <- Total_PromoGroup_data$Unit_sales
Total_PromoGroup_data$wk_sold_doll_base_byppg <- Total_PromoGroup_data$Unit_sales * Total_PromoGroup_data$Final_BasePrice
Total_PromoGroup_data$wk_sold_qty_base_byppg <- Total_PromoGroup_data$Unit_sales
Total_PromoGroup_data$ACV_Selling <- Total_PromoGroup_data$wtd_distribution
Total_PromoGroup_data$wk_avg_price_perunit_byppg <- Total_PromoGroup_data$Sales/Total_PromoGroup_data$Unit_sales
Total_PromoGroup_data$wk_base_price_perunit_byppg <- Total_PromoGroup_data$Final_BasePrice
Total_PromoGroup_data <- data.table(Total_PromoGroup_data)


RMS_Data_PPG_2 <- Total_PromoGroup_data[,.(PPG_Item_No,PPG_Retailer,PPG_Seg,PPG_MFG,PackSize,Date,wk_sold_doll_byppg,wk_sold_qty_byppg,wk_sold_doll_base_byppg,wk_sold_qty_base_byppg,ACV_Selling,wk_avg_price_perunit_byppg,wk_base_price_perunit_byppg,SI,`Promo Group`)]
RMS_Data_PPG_2 <- data.frame(RMS_Data_PPG_2)


#Total_PromoGroup_data <- Total_PromoGroup_data[`Promo Group` != "N/A",]

saveRDS(Total_PromoGroup_data,"../Output/Nielsen_Data_Processed.RData")
write.csv(Total_PromoGroup_data,"../Output/Nielsen_Data_Processed.csv")
