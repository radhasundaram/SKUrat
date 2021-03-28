#registerDoParallel(cores=2)
rm(list = ls())
set.seed(2020)

#Setting Directory
# cdir<-(dirname(rstudioapi::getSourceEditorContext()$path))
cdir <- dirname(getwd())
setwd(cdir)
options(scipen=999)
library(Hmisc)
require(easypackages)


#Loading libraries
list_of_pkgs <-  c("zoo","dplyr","doParallel","magrittr","fastDummies","glmnet","openxlsx","data.table","tidyr","feather","MLmetrics")
list_of_pkgs_bool <- unlist(lapply(list_of_pkgs,require,character.only = TRUE))
new_pkgs <- list_of_pkgs[list_of_pkgs_bool == FALSE]
for (pkg in new_pkgs){
  print(paste("installing package:",pkg))
  install.packages(pkg)
}
libraries(list_of_pkgs)

############ Reading the required file ##############

# Prepared file
data<-read.xlsx("Input/Aus_SKUMonthYr_data_v1.xlsx",detectDates = T) %>% mutate(APN=as.character(APN))
n_distinct(data$APN) # 1601
nrow(data) # 37858


data_segment = data %>% 
  filter(Segment=="Bitesize",Manufacturer=='Mars Wrigley') %>% 
  arrange(APN,Date) 
n_distinct(data_segment$APN) # 64
nrow(data_segment) # 1477


contribution_skus<-data_segment %>% group_by(Date) %>% 
  mutate(total_sales_month=sum(total_VolSales)) %>% ungroup() %>% 
  group_by(APN,Date) %>% 
  mutate(sales_contribution=total_VolSales/total_sales_month) %>% 
  ungroup() %>% 
  select(APN,Date,sales_contribution)

# Mapping file
map<-read.xlsx("Input/DescriptionMapping.xlsx") %>% 
  select(-Month,-Year) %>% 
  unique()

# reading the SKUs required to model
model_skus<-read.csv("Input/sku_Summary_file.csv")
model_skus<-model_skus %>% filter(Segment=="Bitesize",Manufacturer=='Mars Wrigley',Reason=="can be modelled")
model_skus$sku<-sapply(model_skus$PPG_Item_No,FUN=function(x)strsplit(x=x,split = "_")[[1]][2])

model_skus_list<-unique(model_skus$sku)


############ Model Data prep and clean ##########

data_segment_temp=data_segment %>% 
  group_by(APN) %>% 
  slice(.,3:n()) %>% 
  ungroup()
n_distinct(data_segment_temp$APN) #64
nrow(data_segment_temp) # 1350

# cleaning, number data points (atleast 12 points)
data_segment_temp<-data_segment_temp %>% 
  group_by(APN) %>%
  mutate(num_months=n()) %>% ungroup() %>% filter(num_months>=12) %>% select(-num_months) 
n_distinct(data_segment_temp$APN) #45
nrow(data_segment_temp)# 1265

mars_skus_master<-data_segment_temp%>%filter(Manufacturer=='Mars Wrigley')%>%.$APN%>%unique()%>%as.character()


############ Filter for modellable SKUs ##############

data_segment_temp<-data_segment_temp %>% filter(APN %in% model_skus_list,Date<="2019-12-31")

############ Feature Engineering ################

# segment wise number of active skus in month
active_skus<- data_segment_temp %>% group_by(Date) %>% summarise(num_marsskus_month=n_distinct(APN)) 

min_max_norm <- function(x){
  return((x- min(x)) /(max(x)-min(x)))
}


cat_attr=c("Sub.Category","Sub.Segment","Brand","Sub.Brand","Packsize","Promo.Group","Product.Type","Flavour")
con_attr = c("total_UnitSales",'total_ValueSales','total_VolSales','BrandSales','CategorySales','price_per_vol')

data_segment_temp = data_segment_temp %>% select(APN,SKU,Date,cat_attr,con_attr) %>%  mutate_at(con_attr,min_max_norm) 


############# TAE SIMILARITY ################


active_skus_df = data_segment_temp %>% select(APN) %>% unique() %>% mutate(key = 1)
active_skus_df = merge(active_skus_df,active_skus_df,by = "key",all = T)
active_skus_df = active_skus_df %>% filter(APN.x!=APN.y) %>%
  merge(data_segment_temp,by.x = "APN.x",by.y = "APN",all.x = T) %>%
  merge(data_segment_temp,by.x = "APN.y",by.y = "APN",all.x = T) %>% filter(Date.x == Date.y) %>%
  merge(contribution_skus,by.x = c("APN.x","Date.x"), by.y = c("APN",'Date'),all.x = T) %>%
  merge(contribution_skus,by.x = c("APN.y","Date.y"), by.y = c("APN",'Date'),all.x = T) %>%
  merge(active_skus,by.x = "Date.x", by.y = "Date",all.x = T)

sim_df = active_skus_df %>% 
  mutate(
    Sub.Category.sim = ifelse(Sub.Category.x == Sub.Category.y ,1,0),
    Sub.Segment.sim = ifelse(Sub.Segment.x == Sub.Segment.y,1,0),
    Brand.sim = ifelse(Brand.x == Brand.y,1,0),
    Sub.Brand.sim = ifelse(Sub.Brand.x == Sub.Brand.y,1,0),
    Packsize.sim = ifelse(Packsize.x == Packsize.y,1,0),
    Promo.Group.sim = ifelse(Promo.Group.x == Promo.Group.y,1,0),
    Product.Type.sim = ifelse(Product.Type.x == Product.Type.y,1,0),
    Flavour.sim = ifelse(Flavour.x == Flavour.y,1,0),
    total_UnitSales.sim = 1-abs(total_UnitSales.x - total_UnitSales.y),
    total_ValueSales.sim = 1-abs(total_ValueSales.x - total_ValueSales.y),
    total_VolSales.sim = 1-abs(total_VolSales.x - total_VolSales.y),
    BrandSales.sim = 1-abs(BrandSales.x-BrandSales.y),
    CategorySales.sim = 1-abs(CategorySales.x - CategorySales.y),
    price_per_vol.sim = 1-abs(price_per_vol.x - price_per_vol.y)
  ) %>%
  mutate(avg_sim_score = rowMeans(select(.,ends_with('.sim')))) %>%
  mutate(wt_avg_sim_score = sales_contribution.y*avg_sim_score) %>%
  group_by(select(.,ends_with('.x')))  %>% summarise(TAE = sum(wt_avg_sim_score)) 

################ Assortment Elasticity  ################
corr_df = sim_df %>% group_by(APN.x) %>% summarise(Correlation = cor(sales_contribution.x,TAE))

write.csv(sim_df,paste0("./Output/TAE_Similarity_",Sys.Date(),".csv"),row.names = FALSE)