#####################Libraries######################################
rm(list = ls())

library(rstudioapi)
library(data.table)
library(dplyr)
library(lubridate)
library(readxl)
library(readr)
library(zoo)
library('magrittr')
library('tidyr')
library(data.table)
library(openxlsx)
library(stringr)

setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path)))
setwd("../Data/")
################### Read Master Mapping File data ############################
data <- read.csv("Import_LDESC.csv")

fil_data=data%>%filter(Description!='Double D Sugar Fre Choc&Jely Ronds 1x70g',!is.na(APN))
fil_data$Date <-  as.Date(fil_data$Date, format="%Y-%m-%d")

fil_data=data%>%filter(Description!='Double D Sugar Fre Choc&Jely Ronds 1x70g',!is.na(APN))
fil_data%<>%mutate(Date= as.Date(Date, format="%Y-%m-%d"))

fil_data$Month<-month(fil_data$Date)
fil_data$Year<-year(fil_data$Date)
fil_data$Month_Yr <- format(as.Date(fil_data$Date), "%Y-%m")
fil_data$Month_Yr <- format(as.Date(paste0(fil_data$Month_Yr,"-01")), "%Y-%m")
## Create mapping ####

cat_cols=c("APN","Description","Category","Sub.Category","Segment","Sub.Segment","Manufacturer","Brand",
           "Sub.Brand","Packsize","Product.Type","Flavour","Promo.Group")
map=fil_data %>% select(all_of(cat_cols))%>%unique()

# cap ends of cat variables with low frequencies
CapLowFreq<-function(col,prop=0.01){
  #col=map$Sub.Segment
  #prop=0.02
  #data.frame(prop.table(table(col_changed)))
  
  freq_dist=data.frame(prop.table(table(col)))
  col_changed<-ifelse(col%in%c(NA,""," ",NULL)|col%in%freq_dist[freq_dist$Freq<prop,]$col,'Others',col)
  return(col_changed)
}
map$Sub.Category<-CapLowFreq(map$Sub.Category,0.02)
map$Segment<-CapLowFreq(map$Segment,0.03)
map$Sub.Segment<-CapLowFreq(map$Sub.Segment,0.02)
map$Product.Type<-CapLowFreq(map$Product.Type,0.01)
map$Flavour<-CapLowFreq(map$Flavour,0.00125)
map$Packsize<-sapply(map$Packsize,FUN = function(x)(as.numeric(sub(x,pattern = 'g',replacement=''))))

#Check
#data.frame(prop.table(table(map$Packsize))) %>% View()
#a=sapply(map$Packsize,FUN = function(x)(as.numeric(sub(x,pattern = 'g',replacement=''))))
#summary(a)


## Monthly rollup #### 
roll_monthly=fil_data%>%
  group_by(APN,Month_Yr,Year)%>%
  summarise(wk_sold_avg_price_bysku=mean(Price.per.Unit),
            wk_sold_qty_bysku=sum(Unit.Sales),
            wk_sold_doll_bysku=sum(Val.Sales),
            Vol_Sales=sum(Vol.Sales))
roll_monthly$Month<-month(roll_monthly$Date)
roll_monthly$Year<-year(roll_monthly$Date)

#join with map
final_set=roll_monthly %>% left_join(map)

####Need to create month year SI index and create avg_price vars 

## Creating Seasonality Index at segment level ####
si_index <- setDT(final_set %>%group_by(Segment,Month_Yr,Year) %>% summarise(Sales = sum(wk_sold_qty_bysku,na.rm = T)))
si_index <- setorderv(si_index,c("Segment",'Month_Yr','Year'))

si_index <- si_index %>% group_by(Segment,Year) %>% mutate(mean_sales = mean(Sales,na.rm = T),SI = Sales/mean_sales) %>% ungroup()
si_index <- si_index %>% group_by(Segment,Month_Yr) %>% mutate(SI = mean(SI,na.rm = T)) %>% ungroup()

final_set <- final_set %>% left_join(si_index[,c("Segment","Month_Yr","SI")])


# creating brand and category sales
brandsales = fil_data %>% group_by(Brand,Month_Yr) %>% summarise(BrandSales=sum(Unit.Sales,na.rm=T))
cateorysales = fil_data %>% group_by(Category,Month_Yr) %>% summarise(CategorySales=sum(Unit.Sales,na.rm=T))

final_set<-final_set %>% left_join(brandsales) %>% left_join(cateorysales)


 
# writing it out
write.csv(final_set,"Aus_SKUWeek_Data.csv",row.names = F)

