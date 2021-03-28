#registerDoParallel(cores=2)
rm(list = ls())
set.seed(2020)

#Setting Directory
cdir<-(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(cdir)
options(scipen=999)
library(Hmisc)
require(easypackages)

#Loading libraries
libraries(c("lubridate","zoo","dplyr","doParallel","magrittr","fastDummies","glmnet","openxlsx","data.table","tidyr","feather","MLmetrics"))

# Functions


# Test Data Preparation
# In test_prep function, bucketing is segment specific,needs to modified accordingly to how we've given to train data
test_prep<-function(df,clusterinfo,dummycolnames,flavours){
  
  # segment wise number of active skus in month
  active_skus<-df %>% group_by(Date) %>% dplyr::summarize(num_marsskus_month=n_distinct(APN)) 
  
  # Get new sim scores bases on categorical features
  cat_attr=c("Sub.Segment","Brand","Sub.Brand","Promo.Group","Product.Type","Flavour")
  newsimscore_df = df %>% 
    select(APN,SKU,Date,price_per_vol,all_of(cat_attr)) %>% 
    filter(price_per_vol !=0) %>% 
    group_by(Date) %>% mutate(SKUcount = length(unique(SKU))) %>% ungroup %>%
    group_by(Date,Sub.Segment) %>% mutate(SubSegmentSKUcount = length(unique(SKU))) %>% ungroup %>%
    group_by(Date,Brand) %>% mutate(BrandSKUcount = length(unique(SKU))) %>% ungroup %>%
    group_by(Date,Sub.Brand) %>% mutate(SubBrandSKUcount = length(unique(SKU))) %>% ungroup %>%
    group_by(Date,Promo.Group) %>% mutate(PromoGroupSKUcount = length(unique(SKU))) %>% ungroup %>%
    group_by(Date,Product.Type) %>% mutate(ProductTypeSKUcount = length(unique(SKU))) %>% ungroup %>%
    group_by(Date,Flavour) %>% mutate(FlavourSKUcount = length(unique(SKU))) %>% ungroup %>%
    mutate(SubSegmentScore = SubSegmentSKUcount/SKUcount,
           BrandScore = BrandSKUcount/SKUcount,
           SubBrandScore = SubBrandSKUcount/SKUcount,
           PromoGroupScore = PromoGroupSKUcount/SKUcount,
           ProductTypeScore = ProductTypeSKUcount/SKUcount,
           FlavourScore = FlavourSKUcount/SKUcount) %>%
    select(APN,Date,ends_with("Score"))
  newsimscore_df$new_sim_score<-rowSums(x = newsimscore_df %>% select(-APN,-Date),na.rm = T)
  newsimscore_df<-newsimscore_df %>% select(APN,Date,new_sim_score)
  
  # joining the features
  df<-df %>% 
    left_join(newsimscore_df) %>% 
    left_join(active_skus)
  
  # adding price_per_vol_log
  df<-df %>% mutate(price_per_vol_log=log(price_per_vol))
  
  ##Here bucketing is segment specific,needs to modified accordingly
  
  #prop.table(table(df$Product.Type)) %>% View(.)
  df$Product.Type<-ifelse(!(df$Product.Type%in%c("Fruit","Mint")),"Other",df$Product.Type)
  
  #prop.table(table(df$Sub.Segment)) %>% View(.)
  #df$Sub.Segment<-ifelse(!(df$Sub.Segment%in%c("Choc Covered")),"Others",df$Sub.Segment)
  
  #prop.table(table(df$Flavour)) %>% View(.)
  #df$Product.Type<-ifelse(!(df$Product.Type%in%c("Fruit","Mint")),"Other",df$Product.Type)
  df$Flavour<-ifelse(!(df$Flavour%in%flavours),"Other Flavour",df$Flavour)
  
  #prop.table(table(df$Packsize)) %>% View(.)
  #prop.table(table(df$Packsize_grouped)) %>% View(.)
  #unique(data_segment_mars$Packsize_grouped)
  df$Packsize=sapply(df$Packsize,function(x)as.numeric(sub(x,pattern = "g",replacement = "")))
  df %<>% mutate(Packsize_grouped=ifelse(Packsize<64,"<64",">=64"))
  
  #prop.table(table(df$Packsize_grouped)) %>% View(.)
  
  df<-df %>% left_join(clusterinfo %>% select(APN,cluster))
  
  dummycolnames_test=dummycolnames
  dummycols=df %>% select(all_of(dummycolnames_test)) %>% dummy_cols(.) %>% select(-all_of(dummycolnames_test))
  
  df_fin<-cbind(df,dummycols)
  dummies_not_present<-setdiff(vars_used$IDV,colnames(df_fin))
  
  if(rlang::is_empty(dummies_not_present)){
    df_test=df_fin
  }else{
    df_test=eval(parse(text=paste0("df_fin %>% mutate(",paste0("'",dummies_not_present,"'=0",collapse=","),")")))
  }
  
  return(df_test)
}

##Calculating the Model performance metrics 
eval_results <- function(true, predicted, df,flag=NA,Info=NA) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  Mape=mean((abs(true-predicted)/true)*100)
  wmape=(sum(abs(true-predicted))/sum(true))*100
  
  # Model performance metrics
  data.frame(
    Info=Info,
    Data=flag,
    RMSE = RMSE,
    Rsquare = R_square,
    Mape=Mape,
    Wmape=wmape
  )
  
}

##Model Training 
modelfit<-function(x_train,y_train,train_cutoff_date){
  
  # try model lasso reg
  lambdas <- 10^seq(2, -3, by = -.1)
  # Setting alpha = 1 implements lasso regression
  set.seed(2020)
  lasso_reg <- cv.glmnet(data.matrix(x_train), 
                         y_train, 
                         alpha = 1, 
                         lambda = lambdas, 
                         standardize = F, 
                         nfolds = 3,
                         lower.limits = c(Low),
                         upper.limits = c(High))
  # Best 
  lambda_best <- lasso_reg$lambda.min
  print(lambda_best)
  
  # Actual model fit
  set.seed(2020)
  lasso_model <- glmnet(data.matrix(x_train), 
                        y_train, 
                        alpha = 1, 
                        lambda = lambda_best, 
                        standardize = F,
                        lower.limits = c(Low),
                        upper.limits = c(High))
  
  # Evaluation train and test
  set.seed(2020)
  predictions_train <- exp(predict(lasso_model, s = lambda_best, newx = data.matrix(x_train)))
  predictions_train<-ifelse(predictions_train<0,0,predictions_train)
  trn_perf=eval_results(true = exp(y_train), predicted = predictions_train, df = x_train,flag='train',Info = paste0("Trained till ",train_cutoff_date," and ",length(y_train)))
  
  
  return(list(lasso_model,trn_perf,predictions_train))
  
}

# Getting the Co-efficients from the Lasso Train
get_coeff<-function(model){
  coefficient<-data.frame(as.matrix(coefficients(model))) %>% mutate(Variable=rownames(.)) %>% filter(s0!=0)
  return(coefficient)
}

clean_skusim<-function(sim_scores){
  # get similar skus to the discontinued sju
  skudat<-sim_scores %>% t(.) %>% data.frame()
  skudat$APN<-rownames(skudat)
  skudat<-skudat[2:nrow(skudat),] 
  colnames(skudat)<-c("score","APN")
  skudat$score<-as.numeric(skudat$score)
  return(skudat)
}

##Flagging the discontinued SKU's 
get_disc_sku<-function(df){
  
  df1<-df %>% mutate(low_sales_flag=ifelse(total_VolSales<=mean(c(lag(total_VolSales,1),
                                                                  lag(total_VolSales,2),
                                                                  lag(total_VolSales,3),
                                                                  lag(total_VolSales,4)),na.rm=T)*0.3,1,0))
  df1$probable_discontinue_flag<-ifelse((lag(df1$low_sales_flag,1)&
                                           lag(df1$low_sales_flag,2)&
                                           lag(df1$low_sales_flag,3))==1,1,0)
  
  df1 %<>%mutate(final_disc_Date=!(duplicated(probable_discontinue_flag))&probable_discontinue_flag) 
  return(df1 %>% select(APN,Date,total_VolSales,low_sales_flag,probable_discontinue_flag,final_disc_Date))
}

# Predict clusters for the new SKU's
predict.kmeans <- function(object,newdata,method=c("centers", "classes")) {
 
  method <- match.arg(arg = method,choices=c("centers", "classes"))
  
  centers <- object$centers
  ss_by_center <- apply(centers, 1, function(x) {
    #print(x)
    #print()
    colSums((t(newdata) - x) ^ 2)
  })
  best_clusters <- apply(ss_by_center, 1, which.min)
  
  if (method == "centers") {
    centers[best_clusters, ]
  } else {
    paste0("cluster_",best_clusters)
  }
}



############ Reading the required file ##############

# Train data till the date 
train_cutoff_date=as.Date("2019-12-01")

# Segment Filtering 
Segment_fil="Gum"

# Prepared file(Month Level Rolled up data)
data<-read.xlsx("Input/Aus_SKUMonthYr_data_v1.xlsx",detectDates = T) %>% mutate(APN=as.character(APN))
n_distinct(data$APN) # 1601
nrow(data) # 37858

data_segment = data %>% 
  filter(Segment==Segment_fil,Manufacturer=='Mars Wrigley') %>% 
  arrange(APN,Date) 
n_distinct(data_segment$APN) # 65
nrow(data_segment) # 1769

# Mapping file(SKU level attributes)
map<-read.xlsx("Input/DescriptionMapping.xlsx") %>% select(-Month,-Year) %>% unique()

# reading the SKUs required to model(Has to be generated based on filters used in TPO)
model_skus<-read.csv("Input/sku_Summary_file.csv",stringsAsFactors = F)
model_skus$sku<-sapply(model_skus$PPG_Item_No,FUN=function(x)strsplit(x=x,split = "_")[[1]][2])
model_skus<-model_skus %>% filter(Segment==Segment_fil,Manufacturer=='Mars Wrigley',Reason=="can be modelled")
model_skus_list<-unique(model_skus$sku)
n_distinct(model_skus_list)#28

############ Model Data prep and clean ##########

# removing first few points with low sales
data_segment_temp=data_segment%>% 
  group_by(APN) %>% 
  slice(.,3:n()) %>% 
  ungroup()
n_distinct(data_segment_temp$APN) #65
nrow(data_segment_temp) # 1642

# cleaning, number data points (atleast 12 points)
data_segment_temp<-data_segment_temp %>% 
  group_by(APN) %>%
  mutate(num_months=n()) %>% ungroup() %>% filter(num_months>=12) %>% select(-num_months) 
n_distinct(data_segment_temp$APN) #52
nrow(data_segment_temp)# 1574

mars_skus_master<-data_segment_temp%>%filter(Manufacturer=='Mars Wrigley')%>%.$APN%>%unique()%>%as.character()


############ Getting the discontinued SKUs ##############

ModelData1<-data_segment_temp%>% group_by(APN) %>% do(get_disc_sku(.)) %>% ungroup() 

discontinued_skus<-ModelData1%>%filter(final_disc_Date==1)
all_discontinued_skus <- unique(discontinued_skus$APN)
normal_skus <- intersect(model_skus_list,setdiff(mars_skus_master,all_discontinued_skus))

n_distinct(all_discontinued_skus) #17
n_distinct(normal_skus) #28

all_discontinued_skus_old<-all_discontinued_skus

# preparing the discontinued sku df
discontinued_skus_mod<-discontinued_skus %>% 
  mutate(ActualDiscDate=as.Date(as.yearmon(format(discontinued_skus$Date,"%Y-%m"))-(4/12)))%>% 
  filter(APN %in% all_discontinued_skus) %>% 
  select("APN","Date"=Date,ActualDiscDate)

rm(discontinued_skus)
gc()

# Removing disc skus with less data before discontinuation
pre_dis_data<-data_segment_temp %>% filter(APN %in% all_discontinued_skus)
final_disc_skus<-c()
for(i in seq_along(all_discontinued_skus)){
 
  sku_temp=all_discontinued_skus[i]
  print(sku_temp)
  disc_date<-discontinued_skus_mod %>% filter(APN==sku_temp) %>% .$ActualDiscDate
  fil_temp<-pre_dis_data %>% filter(APN == sku_temp,Date<=disc_date,total_VolSales>10)%>%count(.)
  if(fil_temp<=10){
    next
  }else{
    final_disc_skus<-append(final_disc_skus,sku_temp)
  }
  
}
pre_dis_data<-pre_dis_data %>% filter(APN %in%final_disc_skus)
all_discontinued_skus<-final_disc_skus
discontinued_skus_mod<-discontinued_skus_mod %>% filter(APN %in% all_discontinued_skus)
length(all_discontinued_skus)#6

pre_normal_data<-data_segment_temp %>% filter(APN %in% normal_skus)
data_segment_temp<-rbind(pre_dis_data,pre_normal_data)
n_distinct(data_segment_temp$APN) # 34
nrow(data_segment_temp)# 1127

rm(pre_dis_data,final_disc_skus,pre_normal_data)
gc()


############ Filter till train cut off date ##############

data_segment_temp<-data_segment_temp %>% filter(Date<=train_cutoff_date)
n_distinct(data_segment_temp$APN) # 34
nrow(data_segment_temp)# 1031


############ Feature Engineering ################

# segment wise number of active skus in month
active_skus<-data_segment_temp %>% group_by(Date) %>% dplyr::summarize(num_marsskus_month=n_distinct(APN)) 


# Get new sim scores bases on categorical features
cat_attr=c("Sub.Segment","Brand","Sub.Brand","Promo.Group","Product.Type","Flavour")
newsimscore_df = data_segment_temp %>% 
  select(APN,SKU,Date,price_per_vol,all_of(cat_attr)) %>% 
  filter(price_per_vol !=0) %>% 
  group_by(Date) %>% mutate(SKUcount = length(unique(SKU))) %>% ungroup %>%
  group_by(Date,Sub.Segment) %>% mutate(SubSegmentSKUcount = length(unique(SKU))) %>% ungroup %>%
  group_by(Date,Brand) %>% mutate(BrandSKUcount = length(unique(SKU))) %>% ungroup %>%
  group_by(Date,Sub.Brand) %>% mutate(SubBrandSKUcount = length(unique(SKU))) %>% ungroup %>%
  group_by(Date,Promo.Group) %>% mutate(PromoGroupSKUcount = length(unique(SKU))) %>% ungroup %>%
  group_by(Date,Product.Type) %>% mutate(ProductTypeSKUcount = length(unique(SKU))) %>% ungroup %>%
  group_by(Date,Flavour) %>% mutate(FlavourSKUcount = length(unique(SKU))) %>% ungroup %>%
  mutate(SubSegmentScore = SubSegmentSKUcount/SKUcount,
         BrandScore = BrandSKUcount/SKUcount,
         SubBrandScore = SubBrandSKUcount/SKUcount,
         PromoGroupScore = PromoGroupSKUcount/SKUcount,
         ProductTypeScore = ProductTypeSKUcount/SKUcount,
         FlavourScore = FlavourSKUcount/SKUcount) %>%
  select(APN,Date,ends_with("Score"))
newsimscore_df$new_sim_score<-rowSums(x = newsimscore_df %>% select(-APN,-Date),na.rm = T)
newsimscore_df<-newsimscore_df %>% select(APN,Date,new_sim_score)

#check
#newsimscore_df %>% group_by(APN) %>% summarise(a=n_distinct(new_sim_score)) %>% View(.)

# joining the features
data_segment_with_features<-data_segment_temp %>% 
  left_join(newsimscore_df) %>% 
  left_join(active_skus)

rm(newsimscore_df,active_skus)
gc()

############ Prep model data #####################

data_segment_mars<-data_segment_with_features %>% filter(Manufacturer=='Mars Wrigley')
n_distinct(data_segment_mars$APN) # 34
nrow(data_segment_mars) # 1031

rm(data_segment_temp)
gc()

numcols<-data_segment_mars %>% 
  select("APN",'Date',"Month","Year","BrandSales","CategorySales","SI",
         "price_per_vol","new_sim_score",starts_with("mars"),"num_marsskus_month",
         "total_VolSales")

##This bucketing needs to be modified according to the Segment 

#prop.table(table(data_segment_mars$Product.Type)) %>% View(.)
data_segment_mars$Product.Type<-ifelse(!(data_segment_mars$Product.Type%in%c("Fruit","Mint")),"Other",data_segment_mars$Product.Type)

#prop.table(table(data_segment_mars$Sub.Segment)) %>% View(.)
#data_segment_mars$Sub.Segment<-ifelse(!(data_segment_mars$Sub.Segment%in%c("Choc Covered")),"Others",data_segment_mars$Sub.Segment)

#prop.table(table(data_segment_mars$Flavour)) %>% View(.)
flavours<-setdiff(unique(data_segment_mars$Flavour),c("Other Flavour"))

#prop.table(table(data_segment_mars$Packsize)) %>% View(.)
#prop.table(table(data_segment_mars$Packsize_grouped)) %>% View(.)
data_segment_mars$Packsize=sapply(data_segment_mars$Packsize,function(x)as.numeric(sub(x,pattern = "g",replacement = "")))
data_segment_mars$Packsize_grouped <-as.character(cut2(data_segment_mars$Packsize, g =2))
data_segment_mars$Packsize_grouped<-ifelse(data_segment_mars$Packsize_grouped=="[14, 64)","<64",">=64")

# Clusters to differentiate between SKU's
data_segment_mars_cp<-data_segment_mars

rest_of_data<-data_segment_mars_cp %>% filter(!(APN %in% discontinued_skus_mod$APN)) 
discontinue_sku_df<-data_segment_mars_cp %>% filter(APN %in% discontinued_skus_mod$APN)
data_till_discontinue<-data.frame()
for(i in seq_along(all_discontinued_skus)){
  #i=1
  sku_temp=all_discontinued_skus[i]
  print(sku_temp)
  disc_date<-discontinued_skus_mod %>% filter(APN==sku_temp) %>% .$ActualDiscDate
  fil_temp<-discontinue_sku_df %>% 
    filter(APN == sku_temp,Date<=disc_date)
  data_till_discontinue = rbind(data_till_discontinue,fil_temp)
  
}
data_segment_mars_cp<-rbind(data_till_discontinue,rest_of_data)
summary_sales<-data_segment_mars_cp %>%
  select(APN,total_VolSales) %>%
  group_by(APN) %>%
  summarise(mean=mean(total_VolSales),
            percentile1=quantile(total_VolSales,0.25),
            median=quantile(total_VolSales,0.5),
            percentile2=quantile(total_VolSales,0.75),
            max=max(total_VolSales),
            min=min(total_VolSales),
  )
set.seed(2020)
pc_cluster <-kmeans(summary_sales %>% select(-APN),10,nstart = 20)
summary_sales$cluster<-paste0("cluster_",pc_cluster$cluster)
data_segment_mars<-data_segment_mars %>% left_join(summary_sales %>% select(APN,cluster))

# Creating required dummy variables : Change according to the fill rate
dummycolnames=c("cluster","Promo.Group","Product.Type","Packsize_grouped","Flavour")
dummycols=data_segment_mars %>% select(all_of(dummycolnames)) %>% dummy_cols(.) %>% select(-all_of(dummycolnames))

ModelData<-cbind(numcols,dummycols)%>%
  filter(!(total_VolSales==0)) %>% 
  arrange(APN,Date) %>% 
  mutate(price_per_vol_log=log(price_per_vol),
         total_VolSales_log=log(total_VolSales)) %>% 
  select(-price_per_vol)


#n_distinct(ModelData$APN)
#rm(data_segment_mars,manual_cluster)
#gc()

############ Add transition flag #####################

rest_of_data<-ModelData %>% filter(!(APN %in% discontinued_skus_mod$APN)) 
rest_of_data$trans_flag<-0

discontinue_sku_df<-ModelData %>% filter(APN %in% discontinued_skus_mod$APN)
data_till_discontinue<-data.frame()
for(i in seq_along(all_discontinued_skus)){
  #i=1
  sku_temp=all_discontinued_skus[i]
  print(sku_temp)
  disc_date<-discontinued_skus_mod %>% filter(APN==sku_temp) %>% .$ActualDiscDate
  fil_temp<-ModelData %>% 
    filter(APN == sku_temp)%>% 
    mutate(trans_flag=ifelse(Date>disc_date,1,0))
  
  data_till_discontinue = rbind(data_till_discontinue,fil_temp)
}

# combining the clean data
clean_modeldata_segment<-rbind(data_till_discontinue,rest_of_data)
rm(data_till_discontinue,rest_of_data,discontinue_sku_df,fil_temp)
gc()

############ Model Train ###########################

traindata<-na.omit(clean_modeldata_segment)

# renaming required variable to dv
traindata %<>%rename(DV=total_VolSales_log) 
colnames(traindata)

# defining columns that needs to be removed
rm_cols<-c("APN","Date","Month","Year","total_VolSales","DV")

# creating x and y train/test
traindata_Y<-traindata$DV
traindata_X<-traindata %>% 
  select(-all_of(rm_cols),-starts_with("mars_")) %>% 
  select("BrandSales","CategorySales","SI","new_sim_score","price_per_vol_log",everything())

# Finding the importance of the IDV's
library(ranger)
set.seed(2020)
rf=ranger(x=traindata_X,y=traindata_Y,importance = 'impurity')
imp=data.frame(importance(rf))%>%mutate(variables=row.names(.)) %>% arrange(desc(importance.rf.))

#Taking only top 25 important variables
imp_variables<-imp %>% head(25) %>% .$variables
traindata_X<-traindata_X %>% select(all_of(imp_variables))
traindata_X %<>%select("BrandSales","CategorySales","SI","new_sim_score","price_per_vol_log",everything())

# Setting Constraints for Lasso Model
other_cols<-length(setdiff(colnames(traindata_X),c("BrandSales","CategorySales","SI","new_sim_score","price_per_vol_log")))
Low = c (  0,   0,-Inf,   -Inf, -Inf, rep(-Inf,other_cols))
High = c(Inf, Inf, Inf,   Inf,   0, rep(Inf,other_cols))
ranges_df = data.frame(col=colnames(traindata_X), low=as.character(Low), high = as.character(High))

artefacts<-modelfit(traindata_X,traindata_Y,train_cutoff_date)

model<-artefacts[[1]]
performance<-artefacts[[2]]
traindata$Prediction<-artefacts[[3]]
traindata$Actual<-exp(traindata_Y)
coeffs<-get_coeff(model)
vars_used<-data.frame("IDV"=colnames(traindata_X),"DV"="total_VolSales_log")
traindata$`%Error`<-(abs(traindata$total_VolSales-traindata$Prediction)/traindata$total_VolSales)*100
traindata%<>%left_join(discontinued_skus_mod %>% select(APN,"Discontinue Date"=ActualDiscDate)) 

rm(artefacts,traindata_X,traindata_Y)
gc()

############ Predict on all SKU & calc retention #####

# getting test period
test_date=train_cutoff_date
month(test_date) <- month(test_date) + 1
test_date

# get test data & prepare features to predict
data_segment_test<-data_segment # %>% filter(APN %in% c(all_discontinued_skus,normal_skus))
n_distinct(data_segment_test$APN)

test_data<-data_segment_test %>% filter(Date==test_date)
test_skus<-test_data$APN

# create new clusters for the new APNs : skus which have history to calculate the clusters
df_clust<-data_segment_test %>% filter(APN %in% test_skus,Date<=train_cutoff_date) %>%
  group_by(APN) %>%
  summarise(mean=mean(total_VolSales),
            percentile1=quantile(total_VolSales,0.25),
            median=quantile(total_VolSales,0.5),
            percentile2=quantile(total_VolSales,0.75),
            max=max(total_VolSales),
            min=min(total_VolSales))
df_clust$cluster<-predict.kmeans(object = pc_cluster,newdata = df_clust %>% select(-APN),method = "classes")
old_clusters=summary_sales %>% select(APN,cluster_old=cluster)
df_clust=df_clust %>% left_join(old_clusters)
df_clust$cluster_old<-ifelse(is.na(df_clust$cluster_old),df_clust$cluster,df_clust$cluster_old)
df_clust<-df_clust %>% select(-cluster,cluster=cluster_old)

completely_new_skus<-setdiff(test_data$APN,df_clust$APN)
test_data<-data_segment_test %>% filter(APN %in% df_clust$APN,Date==test_date)
n_distinct(test_data$APN)#41

test_data_contribution<-test_data %>% group_by(Date) %>% 
  mutate(total_sales_month=sum(total_VolSales)) %>% ungroup() %>% 
  group_by(APN,Date) %>% 
  mutate(sales_contribution=(total_VolSales/total_sales_month)*100) %>% 
  ungroup() %>% 
  select(APN,Date,sales_contribution)
sum(test_data_contribution$sales_contribution)

# preparing the test data with all the features
test_data_prepared<-test_prep(test_data,df_clust,dummycolnames,flavours)
test_data_x_zeroflag<-test_data_prepared %>% select(all_of(vars_used$IDV)) 
test_data_x_oneflag<-test_data_prepared %>% select(all_of(vars_used$IDV)) %>% mutate(trans_flag=1) 

# predicting with and without trans flag for getting the lost sales
test_data_prepared$Actual<-test_data_prepared$total_VolSales
set.seed(2020)
test_data_prepared$`Prediction_oldassort_zeroflag`=exp(predict(model,newx = as.matrix(test_data_x_zeroflag)))
set.seed(2020)
test_data_prepared$`Prediction_oldassort_oneflag`=exp(predict(model,newx = as.matrix(test_data_x_oneflag)))
test_data_prepared$`Sales Lost`<-test_data_prepared$Prediction_oldassort_zeroflag-test_data_prepared$Prediction_oldassort_oneflag

#performance<-rbind(performance,
#                   eval_results(test_data_prepared$Actual,test_data_prepared$Prediction_oldassort_zeroflag,test_data_prepared,flag = "test",Info = paste0("Test on ",test_date," and ",nrow(test_data))))


# loop to remove one sku at a time to re prepare and predict
final_retention<-data.frame()
retention_base_discsku<-data.frame()
for(i in seq_along(test_data_prepared$APN)){
  
  #i=1
  APN_UC=test_data_prepared$APN[i]
  test_data_newassort<-test_prep(test_data %>% filter(APN!=APN_UC),df_clust,dummycolnames,flavours)
  test_data_x_newassort<-test_data_newassort  %>% select(all_of(vars_used$IDV)) 
  test_data_newassort$Prediction_newassort=exp(predict(model,newx = as.matrix(test_data_x_newassort)))
  
  old<-test_data_prepared %>% select(APN,Date,Actual,
                                     "Prediction(Old WithZeroFlag)"=Prediction_oldassort_zeroflag,
                                     "Prediction(Old WithOneFlag)"=Prediction_oldassort_oneflag,
                                     "Sim Score Before"=new_sim_score,
                                     "Sales Lost")
  new<-test_data_newassort %>% select(APN,Date,
                                      "Prediction(New WithZeroFlag)"=Prediction_newassort,
                                      "Sim Score After"=new_sim_score)  
  
  joined<-left_join(old,new)
  joined$score_reduction_flag<-ifelse(joined$`Sim Score After`<joined$`Sim Score Before`,1,0)
  joined$Increment_flag<-ifelse(joined$`Prediction(New WithZeroFlag)`>joined$`Prediction(Old WithZeroFlag)`,1,0)
  joined$model_group<-paste0("Group_",i)
  joined<-joined %>% left_join(map)
  retention_base_discsku<-rbind(retention_base_discsku,joined)
  
  
  increment<-joined %>% filter(!is.na(`Prediction(New WithZeroFlag)`),Increment_flag==1,score_reduction_flag==1)
  lifts<-increment$`Prediction(New WithZeroFlag)`-increment$`Prediction(Old WithZeroFlag)`
  lifts=sum(lifts)
  similar_skus<-sum(increment$Increment_flag)
  similar_sku_avgsales=mean(increment$Actual,na.rm=T)
  
  APN_UC_df=joined %>% select(APN,Date,Actual,"Prediction(Old WithZeroFlag)","Prediction(Old WithOneFlag)","Prediction(New WithZeroFlag)","Sales Lost") %>% filter(APN==APN_UC) 
  APN_UC_df$`TotalSales(OldAssort)`<-sum(old$`Prediction(Old WithZeroFlag)`)
  APN_UC_df$`TotalSales(NewAssort)`<-sum(new$`Prediction(New WithZeroFlag)`)
  APN_UC_df$Lift=lifts
  APN_UC_df$`%Retention`=(APN_UC_df$Lift/APN_UC_df$`Sales Lost`)*100
  APN_UC_df$`#Similar SKUs`<-similar_skus
  APN_UC_df$`Average Sales Similar SKUs`<-similar_sku_avgsales
  
  final_retention<-rbind(final_retention,APN_UC_df)
  
}
final_retention<-final_retention %>% 
  left_join(data.frame(APN=all_discontinued_skus_old) %>% mutate(disck_flag=1)) %>% 
  left_join(test_data_contribution) %>% 
  left_join(df_clust) %>%
  left_join(retention_base_discsku %>% filter(is.na(`Prediction(New WithZeroFlag)`)) %>% select(APN,model_group)) %>% 
  left_join(map)



############ final write #########################

list=list("Performance"=performance,
          "Retentions"=final_retention,
          "Retentions_meta"=retention_base_discsku,
          "Predictions"=traindata %>% mutate(Date=as.character(Date)),
          "Coeff"=coeffs,
          "Variables"=vars_used,
          "DiscontinuedSKUs"=discontinued_skus_mod,
          "DiscontinueLogic"=ModelData1,
          "Importance"=imp,
          "TopSelected"=data.frame(imp_variables),
          "SalesDistribution"=summary_sales,
          "Constraints"=ranges_df)

write.xlsx(list,paste0("Output/",Segment_fil,"_modelresults_(General)_re1.xlsx"))
