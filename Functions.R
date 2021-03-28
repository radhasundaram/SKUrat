#####################Functions############################################
#Processing Names
process_name <- function(x){a <- unlist(strsplit(x," "))
x <- a[2]}





acting_item_filter_edlp <- function(model_data_set_1,i,action_ppgs_for_model_ppg_1){
  
  
  cor_data <- c()
  for(j in action_ppgs_for_model_ppg_1){
    
    model_vol<-model_data_set_1[PPG_Item_No == i]$wk_sold_qty_byppg_log
    names(model_vol)<-model_data_set_1[PPG_Item_No == i]$Date
    
    
    act_base_price<-model_data_set_1[PPG_Item_No == j]$wk_sold_median_base_price_byppg_log
    names(act_base_price)<-model_data_set_1[PPG_Item_No == j]$Date
    act_base_price<-act_base_price[names(act_base_price) %in% names(model_vol)]
    model_vol_base<-model_vol[names(model_vol) %in% names(act_base_price)]
    
    
    temporary_1 <-  cor(model_vol_base,
                        act_base_price,use="pairwise.complete.obs")
    
    cor1 <- data.frame(Item=j,value=temporary_1)
    cor_data <- rbind(cor_data,cor1)
    
    
  }
  cor_data <- cor_data[order(abs(cor_data$value),decreasing=T),]
  cor_data$Item <- as.character(cor_data$Item)
  action_ppgs_for_model_ppg <- c(head(cor_data$Item,5))
  
  
  temporary_1 <- model_data_set_1[PPG_Item_No %in% action_ppgs_for_model_ppg]  
  temporary_1 <- temporary_1[,c("PPG_Item_No","Date","wk_sold_median_base_price_byppg_log")] 
  
  if(nrow(temporary_1)>0){
    
    action_ppg <- dcast(temporary_1, Date ~ PPG_Item_No, value.var = "wk_sold_median_base_price_byppg_log")
    colnames(action_ppg)[!colnames(action_ppg) %in% c("Date")]<-paste(colnames(action_ppg)[!colnames(action_ppg) %in% c("Date")],"_RegularPrice",sep="")
    return(action_ppg)
  }else{
    return(NULL)
  }
}
  
  
acting_item_filter_tpr <- function(model_data_set_1,i,
                                   action_ppgs_for_model_ppg_1){
    
    cor_data <- c()
    
    for (j in action_ppgs_for_model_ppg_1) {
      
      model_vol <- model_data_set_1[PPG_Item_No == i]$wk_sold_qty_byppg_log
      names(model_vol) <- model_data_set_1[PPG_Item_No == i]$Date
      
      act_tpr <- model_data_set_1[PPG_Item_No == j]$tpr_discount_byppg
      names(act_tpr) <- model_data_set_1[PPG_Item_No == j]$Date
      act_tpr <- act_tpr[names(act_tpr) %in% names(model_vol)]
      
      model_vol_tpr <- model_vol[names(model_vol) %in% names(act_tpr)]
      
      temporary_1_tpr <-  cor(model_vol_tpr,
                              act_tpr,use = "pairwise.complete.obs")
      
      cor1 <- data.frame(Item = j,value = temporary_1_tpr)
      cor_data <- rbind(cor_data,cor1)
      
    }
    cor_data <- cor_data[order(abs(cor_data$value),decreasing = T),]
    cor_data$Item <- as.character(cor_data$Item)
    
    action_ppgs_for_model_ppg_tpr <- c(head(cor_data$Item,5))
    
    temporary_1 <- model_data_set_1[PPG_Item_No %in% action_ppgs_for_model_ppg_tpr]  
    temporary_1_tpr <- temporary_1[,c("PPG_Item_No","Date","tpr_discount_byppg")] 
    
    if (nrow(temporary_1_tpr) > 0) {
      action_ppg_tpr <- dcast(temporary_1_tpr, Date ~ PPG_Item_No, value.var = "tpr_discount_byppg")
      colnames(action_ppg_tpr)[!colnames(action_ppg_tpr) %in% c("Date")] <- paste(colnames(action_ppg_tpr)[!colnames(action_ppg_tpr) %in% c("Date")],"_PromotedDiscount",sep = "")
      return(action_ppg_tpr)
    }else{
      return(NULL)
    }
  }
    
cannibalisation_module <- function(cannibal_ppg,
                                 box_xmat,
                                 model,
                                 model_data_set_1,i,
                                 model_coefficients_check_pantry){
  
  model_lambda <- model$lambda.1se
  pred_vol<-exp(predict(model,data.matrix(box_xmat),s=model_lambda))
  ppg_model_cannibal_data<-data.frame()
  iter<-lapply(cannibal_ppg,function(x){
    ppg_x<-gsub("(\\_RegularPrice)|(\\_PromotedDiscount)","",x)
    box_xmat_new<-data.frame(box_xmat)
    box_xmat_new$Date <-as.character(rownames(box_xmat))
    if(x %like% "_PromotedDiscount"){
      #Computing Cannibalisation Qty. for TPR effects
      box_xmat_new[x]<-0
      box_xmat_new["Date"]<-NULL
      cannibal_vol<-exp(predict(model,data.matrix(box_xmat_new),s=model_lambda))
      ppg_measure<-"Promoted Discount"
    }else{
      #Computing Cannibalisation Qty. for EDLP effects
      box_xmat_new<-merge(box_xmat_new,
                          unique(model_data_set_1[which(model_data_set_1$PPG_Item_No==gsub("\\_(PromotedDiscount|RegularPrice)","",x)),
                                                  c("Date","Final_baseprice")]),
                          by="Date",all.x=TRUE)
      box_xmat_new[,x]<-log(as.numeric(box_xmat_new[,"Final_baseprice"]))
      box_xmat_new["Final_baseprice"]<-NULL
      box_xmat_new["Date"]<-NULL
      cannibal_vol<-exp(predict(model,data.matrix(box_xmat_new),s=model_lambda))
      ppg_measure<-"Regular Price"
    }
    #Comput Doll. Cannibalised
    cannibal_vol<-(cannibal_vol-pred_vol)*model_data_set_1$wk_sold_avg_price_byppg[which(model_data_set_1$PPG_Item_No==i)]
    cannibal_vol<-ifelse(cannibal_vol<0,0,cannibal_vol)
    cannibal_dat<-cbind(ppg_x,i,ppg_measure,rownames(box_xmat),cannibal_vol)
    colnames(cannibal_dat)<-c("Cannibal_PPG","Cannibalised_PPG","Measure","Date","Cannibal_Doll")
    
    ppg_model_cannibal_data<<-rbind(ppg_model_cannibal_data,cannibal_dat)
    return(TRUE)
  })
  
  #----------Canibalisation from Pantry Loading--------------
  if(length(model_coefficients_check_pantry)>0 & ! (i %like% "ROM")){
    box_xmat_new<-data.frame(box_xmat)
    pred_vol<-exp(predict(model,data.matrix(box_xmat_new),s=model_lambda))
    if("tpr_discount_byppg_lag1" %in% colnames(box_xmat_new))
    {
      box_xmat_new$tpr_discount_byppg_lag1<-0
      cannibal_vol<-exp(predict(model,data.matrix(box_xmat_new),s=model_lambda))
      cannibal_vol<-(cannibal_vol-pred_vol)*model_data_set_1$wk_sold_avg_price_byppg[which(model_data_set_1$PPG_Item_No==i)]
      cannibal_vol<-ifelse(cannibal_vol<0,0,cannibal_vol)
      
    }else
    {cannibal_vol=rep(0,nrow(box_xmat_new))}  
    ppg_measure<-"Pantry Loading 1"
    cannibal_vol<-c(cannibal_vol[2:length(cannibal_vol)],0)
    cannibal_dat<-cbind(i,i,ppg_measure,rownames(box_xmat),cannibal_vol)
    colnames(cannibal_dat)<-c("Cannibal_PPG","Cannibalised_PPG","Measure","Date","Cannibal_Doll")
    ppg_model_cannibal_data<-rbind(ppg_model_cannibal_data,cannibal_dat)
    
    box_xmat_new<-data.frame(box_xmat)
    if("tpr_discount_byppg_lag2" %in% colnames(box_xmat_new))
    {
      box_xmat_new$tpr_discount_byppg_lag2<-0
      cannibal_vol<-exp(predict(model,data.matrix(box_xmat_new),s=model_lambda))
      cannibal_vol<-(cannibal_vol-pred_vol)*model_data_set_1$wk_sold_avg_price_byppg[which(model_data_set_1$PPG_Item_No==i)]
      cannibal_vol<-ifelse(cannibal_vol<0,0,cannibal_vol)
      
    }else
    {cannibal_vol=rep(0,nrow(box_xmat_new))} 
    ppg_measure<-"Pantry Loading 2"
    cannibal_vol<-c(cannibal_vol[3:length(cannibal_vol)],0,0)
    cannibal_dat<-cbind(i,i,ppg_measure,rownames(box_xmat),cannibal_vol)
    colnames(cannibal_dat)<-c("Cannibal_PPG","Cannibalised_PPG","Measure","Date","Cannibal_Doll")
    ppg_model_cannibal_data<-rbind(ppg_model_cannibal_data,cannibal_dat)
    
  }
  
  return(ppg_model_cannibal_data)
}

cross_validation <- function(box_xmat, Low_cv, High_cv, i, temporary_2, temporary_df){
  
  box_xmat <- cbind(box_xmat, temporary_df)
  
  box_xmat$tpr_flag <- ifelse(box_xmat$tpr_temp>0,1,0)
  box_xmat$tpr_flag <- factor(box_xmat$tpr_flag)
  setDT(box_xmat)
  for(folds in 1:5){
    set.seed(123)
    flds <- createFolds(box_xmat$tpr_flag, k = 5, list = TRUE, returnTrain = FALSE)
    TestData  = box_xmat[flds[[folds]],]
    TrainData = box_xmat[!flds[[folds]],]
    
    TestData = data.frame(TestData)
    TrainData = data.frame(TrainData)
    
    box_xmat_train = TrainData
    TrainData_edlp = TrainData[TrainData[,"tpr_temp"]==0,]
    TrainData_tpr = TrainData[TrainData[,"tpr_temp"]>0,]
    
    box_xmat_train_edlp = box_xmat_train[box_xmat_train[,"tpr_temp"] ==0,]
    box_xmat_train_tpr = box_xmat_train[box_xmat_train[,"tpr_temp"] >0,]
    box_xmat_train= within(box_xmat_train, rm("DV", "tpr_flag","tpr_temp"))
    box_xmat_train=data.matrix(box_xmat_train)
    box_xmat_train_edlp= within(box_xmat_train_edlp, rm("DV", "tpr_flag","tpr_temp"))
    box_xmat_train_edlp=data.matrix(box_xmat_train_edlp)
    box_xmat_train_tpr= within(box_xmat_train_tpr, rm("DV", "tpr_flag","tpr_temp"))
    box_xmat_train_tpr=data.matrix(box_xmat_train_tpr)
    
    box_xmat_test = TestData
    TestData_edlp = TestData[TestData[,"tpr_temp"]==0,]
    TestData_tpr = TestData[TestData[,"tpr_temp"]>0,]
    
    box_xmat_test_edlp = box_xmat_test[box_xmat_test[,"tpr_temp"] ==0,]
    box_xmat_test_tpr = box_xmat_test[box_xmat_test[,"tpr_temp"] >0,]
    box_xmat_test = within(box_xmat_test, rm("DV", "tpr_flag","tpr_temp"))
    box_xmat_test = data.matrix(box_xmat_test)
    box_xmat_test_edlp = within(box_xmat_test_edlp, rm("DV", "tpr_flag","tpr_temp"))
    box_xmat_test_edlp = data.matrix(box_xmat_test_edlp)
    box_xmat_test_tpr = within(box_xmat_test_tpr, rm("DV", "tpr_flag","tpr_temp"))
    box_xmat_test_tpr = data.matrix(box_xmat_test_tpr)
    
    setDT(TrainData)
    temporary_100 = colnames(TrainData[,-c("DV", "tpr_flag","tpr_temp"), with = FALSE])
    
    pen_check <- sum(as.numeric(temporary_100 != "wk_sold_median_base_price_byppg_log" & temporary_100 != "tpr_discount_byppg" & temporary_100 != "tpr_discount_byppg_lag1" & temporary_100 != "tpr_discount_byppg_lag2" &
                                  !temporary_100 %like% ifelse(grepl("_ROM",i),paste0(gsub("_ROM","_Retailer",i)),paste0(gsub("_Retailer","_ROM",i)))))
    
    if(pen_check > 0){
      set.seed(123)
      model <- cv.glmnet(data.matrix(box_xmat_train), 
                         TrainData$DV, 
                         alpha = model_regularization_parameter,
                         lower.limits=Low_cv,
                         upper.limits =High_cv,
                         penalty.factor= as.numeric(temporary_100 != "wk_sold_median_base_price_byppg_log" & temporary_100 != "tpr_discount_byppg" & temporary_100 != "tpr_discount_byppg_lag1" & temporary_100 != "tpr_discount_byppg_lag2" &
                                                      !temporary_100 %like% ifelse(grepl("_ROM",i),paste0(gsub("_ROM","_Retailer",i)),paste0(gsub("_Retailer","_ROM",i)))))
    }else{
      set.seed(123)
      model <- cv.glmnet(data.matrix(box_xmat_train), 
                         TrainData$DV, 
                         alpha = model_regularization_parameter,
                         lower.limits=Low_cv,
                         upper.limits =High_cv)
    }
    model_lambda <- model$lambda.1se
    #R Square
    model_RSq<- model$glmnet.fit$dev.ratio[which(model$glmnet.fit$lambda == model_lambda)]
    #CV Error Mean
    cv_err_mean<-model$cvm[model$lambda==model$lambda.1se]
    #CV Error SD
    cv_err_sd<-model$cvsd[model$lambda==model$lambda.1se]
    
    #pred_vol<-exp(predict(model,box_xmat,s = model_lambda))
    tpr_events_train = nrow(TrainData_tpr)
    tpr_events_test = nrow(TestData_tpr)
    
    train_pred_vol_edlp<-data.frame(Pred=exp(predict(model,box_xmat_train_edlp,s = model_lambda)))
    train_pred_vol_edlp$Date=TrainData_edlp$Date
    train_pred_vol_edlp$PPG_Item_No=i
    if(tpr_events_train>0){
      train_pred_vol_tpr<-data.frame(Pred=exp(predict(model,box_xmat_train_tpr,s = model_lambda)))
      train_pred_vol_tpr$Date=TrainData_tpr$Date
      train_pred_vol_tpr$PPG_Item_No=i
    }
    
    test_pred_vol_edlp<-data.frame(Pred=exp(predict(model,box_xmat_test_edlp,s = model_lambda)))
    test_pred_vol_edlp$Date=TestData_edlp$Date
    test_pred_vol_edlp$PPG_Item_No=i
    if(tpr_events_test>0){
      test_pred_vol_tpr<-data.frame(Pred=exp(predict(model,box_xmat_test_tpr,s = model_lambda)))
      test_pred_vol_tpr$Date=TestData_tpr$Date
      test_pred_vol_tpr$PPG_Item_No=i
    }
    
    train_pred_vol<-data.frame(Pred=exp(predict(model,box_xmat_train,s = model_lambda)))
    train_pred_vol$Date=TrainData$Date
    train_pred_vol$PPG_Item_No=i
    test_pred_vol<-data.frame(Pred=exp(predict(model,box_xmat_test,s = model_lambda)))
    test_pred_vol$Date=row.names(test_pred_vol)
    test_pred_vol$PPG_Item_No=i
    
    train_MAPE=regr.eval(exp(TrainData$DV),train_pred_vol$X1)[4]
    test_MAPE=regr.eval(exp(TestData$DV),test_pred_vol$X1)[4]
    
    train_edlp_MAPE=regr.eval(exp(TrainData_edlp$DV),train_pred_vol_edlp$X1)[4]
    test_edlp_MAPE=regr.eval(exp(TestData_edlp$DV),test_pred_vol_edlp$X1)[4]
    
    if(tpr_events_train>0){
      train_tpr_MAPE = regr.eval(exp(TrainData_tpr$DV),train_pred_vol_tpr$X1)[4]
    }else{
      train_tpr_MAPE = "No tpr event"
    }
    if(tpr_events_test>0){
      test_tpr_MAPE = regr.eval(exp(TestData_tpr$DV),test_pred_vol_tpr$X1)[4]
    }else{
      test_tpr_MAPE = "No tpr event"
    }
    
    temporary_4 <- unique(temporary_2[,c("PPG_Cat","PPG_MFG","PPG_Item_No", "PPG_Description"),with = FALSE])
    temporary_4[, model_RSq := model$glmnet.fit$dev.ratio[which(model$glmnet.fit$lambda == model_lambda)]]
    temporary_4[, model_CVErrorMean :=cv_err_mean]
    temporary_4[, model_CVErrorSD :=cv_err_sd]
    
    temporary_4$TrainMAPE_edlp=train_edlp_MAPE
    temporary_4$TrainMAPE_tpr=train_tpr_MAPE
    
    temporary_4$TestMAPE_edlp=test_edlp_MAPE
    temporary_4$TestMAPE_tpr=test_tpr_MAPE
    temporary_4$TrainMape_overall = train_MAPE
    temporary_4$TestMape_overall = test_MAPE
    temporary_4$no_of_tpr_events_train = tpr_events_train
    temporary_4$no_of_tpr_events_test = tpr_events_test
    
    temporary_4$fold=folds
    
    if(folds == 1){
      model_results_cv <- copy(temporary_4)
    } else{
      model_results_cv <- rbind(model_results_cv,temporary_4)
    }
  }
  return(model_results_cv)
}

get_contrib_multiplicative <- function(data, coeffs, actual, base_vars,
                                       scale_to_actuals = T,
                                       no_base_contrib = T) {
  model_vars_temp = row.names(coeffs)
  data <- data.table(data)
  data[,"(Intercept)" := exp(1)]
 
  coeffs <- c(coeffs[-1],coeffs[1])
  
  model_vars_temp <- model_vars_temp[!model_vars_temp %in% "(Intercept)"]
  model_vars <- c(model_vars_temp,"(Intercept)")
  
  regular_price_vars <- c(names(data)[names(data) %like% "log"],names(data)[names(data) %like% "Regular"],names(data)[names(data) %like% "Promoted"],names(data)[names(data) %like% "Promo_"],names(data)[names(data) %like% "Flag_"])
  
  
  if (length(regular_price_vars) != 0) {
  data[,c(regular_price_vars) := lapply(.SD,function(x){exp(x)}),.SDcols = regular_price_vars]}
  
  exp_data = data[, model_vars, with = F]
  
  
  exp_data[exp_data <= 0] = 1
  
  exp_data = t(apply(exp_data, 1, function(x) x^coeffs)) # apply at row level
  
  predicted = apply(exp_data, 1, prod)
  
  if (scale_to_actuals) {
    scale_to_value = actual
  } else {
    scale_to_value = predicted
  }
  
  base = apply(exp_data[, base_vars, drop = F], 1, prod)
  
  if (no_base_contrib) {
    scale_to_value = scale_to_value / base
  } else {
    scale_to_value = scale_to_value - base
  }
  
  # raw contribution
  non_base_vars = setdiff(model_vars, base_vars)
  raw_contrib = (1 - 1/exp_data[, non_base_vars, drop = F]) * scale_to_value
  # adjusted contribution
  difference = (scale_to_value - rowSums(raw_contrib))
  adj_contrib = raw_contrib + difference * abs(raw_contrib) / rowSums(abs(raw_contrib))
  adj_contrib = as.data.frame(adj_contrib)
  
  if (no_base_contrib) {
    adj_contrib = adj_contrib * base
  } else {
    adj_contrib[, "Base"] = base
    adj_contrib = adj_contrib[, c("Base", non_base_vars), drop = F]
  }
  
  return(adj_contrib)
}



data_filtered_after_constraints <- function(category,
                                            merged_ustotal_ppg_ip_filename,
                                            min_revenue_threshold,
                                            min_prct_data_points,
                                            sd_by_mean_avg_price_threshold,
                                            sd_by_mean_sales_vol_threshold,
                                            filtered_model_dataset_filename,
                                            Retailer_ACV_CUTOFF,
                                            ROM_ACV_CUTOFF,
                                            filtered_ppg_filename,
                                            start_date,end_date,Retailer_filt){
  
  
  #Load input files onto R environment
  RMS_Data_PPG_2 <- readRDS(file = merged_ustotal_ppg_ip_filename)
  RMS_Data_PPG_2$PPG_Retailer <- as.character(RMS_Data_PPG_2$PPG_Retailer)
  if (Retailer_filt != "Retailer") {
  RMS_Data_PPG_2 <- RMS_Data_PPG_2[RMS_Data_PPG_2$PPG_Retailer == Retailer_filt,]}
  sales_dat <- RMS_Data_PPG_2
  rm(RMS_Data_PPG_2)
  setDT(sales_dat)
  input_sales_data_1 <- copy(sales_dat)
  
  setDT(input_sales_data_1)
  input_sales_data_1 <- input_sales_data_1[PPG_Cat == Category_filt & Date <= as.Date(end_date) & Date >= as.Date(start_date)]
  
  #Include only non-zero positive sales records from Retailer and ROM
  input_sales_data_1 <- input_sales_data_1[(wk_sold_doll_byppg >= 0 &
                                              wk_sold_qty_byppg >= 0) ]
  
  #Create Qtr. flags to caputre seasonality in sales for PPGs
  input_sales_data_1[,flag_qtr2 := 0]
  input_sales_data_1[quarter(Date) == 2, flag_qtr2 := 1]
  input_sales_data_1[,flag_qtr3 := 0]
  input_sales_data_1[quarter(Date) == 3, flag_qtr3 := 1]
  input_sales_data_1[,flag_qtr4 := 0]
  input_sales_data_1[quarter(Date) == 4, flag_qtr4 := 1]						
  
  #Tranform dataset to consider Retailer and ROM of each PPG as an individual PPG unit/ candidate for modelling and acting item set
  Dates=unique(input_sales_data_1$Date)
  data_range_quarters <- n_distinct(paste0(year(Dates),quarter(Dates)))
  min_num_quarter_sales <- 0.75* data_range_quarters
  min_qtr_sales_share <- 0.16*(1/data_range_quarters)
  min_sales_quarters <- 0.5*min_num_quarter_sales
  weeks_in_data <- length(Dates)
  
  # Modelling dataset preparation
  temporary_1 <- copy(input_sales_data_1)
  temporary_1<-temporary_1[wk_sold_doll_byppg>=0 &
                             wk_sold_qty_byppg>=0 &
                             !is.na(wk_sold_doll_byppg) &
                             !is.na(wk_sold_qty_byppg), ]
  temporary_1[,":="(wk_sold_avg_price_byppg=wk_avg_price_perunit_byppg)]						
  
  temporary_1[,":="(
    wk_avg_price_perunit_byppg=NULL)]
  temporary_1$PPG_Item_No=paste(temporary_1$PPG_Item_No,temporary_1$PPG_Retailer,sep="_")
  
  input_sales_data_1 <- temporary_1
  rm(temporary_1)
  
  #Revenue Cut Off Filter  
  temporary_1 <- input_sales_data_1[,list(avg_qtrly_sales = sum(wk_sold_doll_byppg)/data_range_quarters,
                                          avg_qtrly_qty = sum(wk_sold_qty_byppg)/data_range_quarters)
                                    ,by = c("PPG_Cat","PPG_MFG","PPG_Item_No")]
  temporary_1[,avg_rough_ppg_price_point := avg_qtrly_sales/avg_qtrly_qty]
  low_revenue_filtered_ppgs <- temporary_1[PPG_Cat == Category_filt & 
                                             avg_qtrly_sales <= min_revenue_threshold]$PPG_Item_No
  
  input_sales_data_2 <- input_sales_data_1[!(PPG_Item_No %in% low_revenue_filtered_ppgs)]
  
  filtered_ppgs <- data.table()
  if(length(low_revenue_filtered_ppgs) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =low_revenue_filtered_ppgs,Reason = c("Low Revenue")))
  }
  
  rm(temporary_1)
  
  #Consistency Sales Cutoff Filter
  
  temporary_1 <- copy(input_sales_data_2)
  
  temporary_1[,":="(week_year = year(Date),
                    week_quarter = quarter(Date))]
  
  temporary_2 <- temporary_1[,list(sum_sales = sum(wk_sold_doll_byppg),
                                   sum_qty = sum(wk_sold_qty_byppg)) ,by = c("PPG_Item_No","week_year","week_quarter")]
  
  temporary_2[,":="(tot_sum_sales = sum(sum_sales),
                    tot_sum_qty = sum(sum_qty))
              ,by = c("PPG_Item_No")]
  
  temporary_2[,sales_share_in_qtr := round(sum_sales/tot_sum_sales,digits = 4)]
  temporary_2[,quantity_share_in_qtr := round(sum_qty/tot_sum_qty,digits = 4)]
  
  setorderv(temporary_2,c("PPG_Item_No","week_year","week_quarter"))
  
  temporary_2[,count_num_quarters := .N,by = "PPG_Item_No"]
  
  low_count_sales_num_quarters_filtered_ppgs <- unique(temporary_2[count_num_quarters < min_num_quarter_sales]$PPG_Item_No)
  
  low_count_sufficient_sale_quarters_filtered_ppgs <- temporary_2[!(PPG_Item_No %in% low_count_sales_num_quarters_filtered_ppgs) & (sales_share_in_qtr <= min_qtr_sales_share),
                                                                  list(count_num_quarters = .N),by = "PPG_Item_No"][count_num_quarters >= min_sales_quarters]$PPG_Item_No
  
  input_sales_data_3 <- input_sales_data_2[!(PPG_Item_No %in% union(low_count_sales_num_quarters_filtered_ppgs,low_count_sufficient_sale_quarters_filtered_ppgs))]
  
  if(length(union(low_count_sales_num_quarters_filtered_ppgs,low_count_sufficient_sale_quarters_filtered_ppgs)) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =union(low_count_sales_num_quarters_filtered_ppgs,low_count_sufficient_sale_quarters_filtered_ppgs),Reason = c("Inconsistent Sales")))
  }
  
  rm(temporary_1,temporary_2)
  
  # Outlier Detection and Removal
  
  input_sales_data_3[,":="(wk_sold_qty_contribution = wk_sold_qty_byppg/sum(wk_sold_qty_byppg))
                     ,by = c("PPG_Item_No")]
  
  input_sales_data_4 <- input_sales_data_3[wk_sold_qty_contribution >0]
  input_sales_data_4[,wk_sold_qty_contribution := NULL]
  input_sales_data_3[,wk_sold_qty_contribution := NULL]
  
  temporary_1 <- input_sales_data_4[,list(count_of_weeks = .N),by = c("PPG_Item_No")]
  
  low_data_filtered_ppgs <- temporary_1[count_of_weeks <= weeks_in_data*min_prct_data_points]$PPG_Item_No
  
  input_sales_data_5 <- input_sales_data_3[!(PPG_Item_No %in% low_data_filtered_ppgs)]
  
  if(length(low_data_filtered_ppgs) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =low_data_filtered_ppgs,Reason = c("Low Data")))
  }
  
  
  rm(temporary_1)
  
  
  input_sales_data_6 <- input_sales_data_5
  
  #Min Variance CutOff Filter
  
  temporary_1 <- copy(input_sales_data_6)
  
  temporary_2 <- temporary_1[,
                             list(sd_by_mean_sales_vol = (sqrt((length(wk_sold_qty_byppg)-1)/length(wk_sold_qty_byppg)) * sd(wk_sold_qty_byppg,na.rm = T))/mean(wk_sold_qty_byppg,na.rm = T),
                                  sd_by_mean_avg_price = (sqrt((length(wk_sold_avg_price_byppg)-1)/length(wk_sold_avg_price_byppg)) * sd(wk_sold_avg_price_byppg,na.rm = T))/mean(wk_sold_avg_price_byppg,na.rm = T)),
                             by = PPG_Item_No]
  
  low_variance_filtered_ppgs <- temporary_2[sd_by_mean_sales_vol <= sd_by_mean_sales_vol_threshold & sd_by_mean_avg_price <= sd_by_mean_avg_price_threshold]$PPG_Item_No
  
  input_sales_data_7 <- input_sales_data_6[!(PPG_Item_No %in% low_variance_filtered_ppgs)] 
  
  if(length(low_variance_filtered_ppgs) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =low_variance_filtered_ppgs,Reason = c("Low Variance")))
  }
  
  rm(temporary_1,temporary_2)
  
  #Remove ROM PPGs less than ACV Cutoff
  ROMFilter=input_sales_data_7%>%filter(PPG_Item_No%like%"_ROM")%>%group_by(PPG_Item_No)%>%summarise(ACV_Selling=mean(ACV_Selling,na.rm=T))
  ROMFilter1=ROMFilter$PPG_Item_No[ROMFilter$ACV_Selling>ROM_ACV_CUTOFF]
  # 
  # #Remove Retailer PPGs less than ACV cutoff
  RetailerFilter=input_sales_data_7%>%filter(!PPG_Item_No%like%"ROM")%>%group_by(PPG_Item_No)%>%summarise(ACV_Selling=mean(ACV_Selling,na.rm=T))
  RetailerFilter1=RetailerFilter$PPG_Item_No[RetailerFilter$ACV_Selling>Retailer_ACV_CUTOFF]
  
  low_acv_filtered_ppgs <- unique(input_sales_data_7[!(PPG_Item_No %in% c(ROMFilter1,RetailerFilter1))]$PPG_Item_No)
  
  input_sales_data_7<-input_sales_data_7[input_sales_data_7$PPG_Item_No %in% c(ROMFilter1,RetailerFilter1),]
  
  if(length(low_acv_filtered_ppgs) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =low_acv_filtered_ppgs,Reason = c("Low ACV")))
  }
  
  
  #Remove Die down PPGs
  
  temporary_1 <- copy(input_sales_data_7)
  
  setorderv(temporary_1,c("PPG_Item_No","Date"))
  
  check_date <- unique(temporary_1$Date)[length(unique(temporary_1$Date)) - 26]
  
  #Flagging last 26 weeks for comparison
  temporary_1[,flag := 0]
  temporary_1[Date > check_date,flag := 1]
  
  temporary_2 <- temporary_1[,list(avg_sales = mean(wk_sold_doll_byppg,na.rm = T),
                                   avg_acv  = mean(ACV_Selling,na.rm = T)),by = c("PPG_Item_No","flag")]
  
  
  temporary_3 <- dcast.data.table(temporary_2,PPG_Item_No~flag,value.var = c("avg_acv","avg_sales"))
  
  temporary_3[,flag := ifelse(avg_acv_1 >= 15 & avg_sales_1 >= (0.2 * avg_sales_0),0,1)]
  
  temporary_3[,ratio_sales_1 := avg_sales_1/avg_sales_0]
  
  remove_dying_ppgs <- unique(temporary_3[flag == 1]$PPG_Item_No)
  
  input_sales_data_7 <- input_sales_data_7[!(input_sales_data_7$PPG_Item_No %in% remove_dying_ppgs),]
  
  if(length(remove_dying_ppgs) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =remove_dying_ppgs,Reason = c("Dying PPGs")))
  }
  
  setDT(input_sales_data_7)
  
  #Filter out PPGs after Correlation check
  corr=input_sales_data_7%>%group_by(PPG_Item_No)%>%summarise(corr=cor(wk_sold_avg_price_byppg,wk_sold_qty_byppg))
  remove_ppgs_with_pos_correlation=corr$PPG_Item_No[corr$corr>0.20]
  input_sales_data_7=input_sales_data_7[!input_sales_data_7$PPG_Item_No%in%remove_ppgs_with_pos_correlation,]
  
  if(length(remove_ppgs_with_pos_correlation) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =remove_ppgs_with_pos_correlation,Reason = c("PPGs with Positive Correlation b/n Price & Sales Units")))
  }
  
  
  temp = input_sales_data_1[,c("PPG_Item_No","wk_sold_doll_byppg","PPG_MFG","PPG_Brand", "PPG_Retailer","PPG_Description")] %>%
    group_by(PPG_Item_No, PPG_MFG,PPG_Brand, PPG_Retailer,PPG_Description) %>% summarise(TotalSales = sum(wk_sold_doll_byppg))
  PPG_Filter_Summary = merge(temp, filtered_ppgs, by = "PPG_Item_No", all.x = T)
  save(input_sales_data_7,file=paste(filtered_model_dataset_filename,".RData",sep=""))
  write.csv(PPG_Filter_Summary,filtered_ppg_filename)
  remove(temp, PPG_Filter_Summary)
  return(TRUE)
}

data_filtered_after_constraints1 <- function(category,
                                            merged_ustotal_ppg_ip_filename,
                                            min_revenue_threshold,
                                            min_prct_data_points,
                                            sd_by_mean_avg_price_threshold,
                                            sd_by_mean_sales_vol_threshold,
                                            filtered_model_dataset_filename,
                                            Retailer_ACV_CUTOFF,
                                            ROM_ACV_CUTOFF,
                                            filtered_ppg_filename,
                                            start_date,end_date){
  
  
  #Load input files onto R environment
  RMS_Data_PPG_2 <- readRDS(file = merged_ustotal_ppg_ip_filename)
  sales_dat <- RMS_Data_PPG_2
  rm(RMS_Data_PPG_2)
  setDT(sales_dat)
  input_sales_data_1 <- copy(sales_dat)
  
  setDT(input_sales_data_1)
  input_sales_data_1 <- input_sales_data_1[PPG_Cat == Category & Date <= as.Date(end_date) & Date >= as.Date(start_date)]
  
  #Include only non-zero positive sales records from Retailer and ROM
  input_sales_data_1 <- input_sales_data_1[(wk_sold_doll_byppg >= 0 &
                                              wk_sold_qty_byppg >= 0) ]
  
  #Create Qtr. flags to caputre seasonality in sales for PPGs
  input_sales_data_1[,flag_qtr2 := 0]
  input_sales_data_1[quarter(Date) == 2, flag_qtr2 := 1]
  input_sales_data_1[,flag_qtr3 := 0]
  input_sales_data_1[quarter(Date) == 3, flag_qtr3 := 1]
  input_sales_data_1[,flag_qtr4 := 0]
  input_sales_data_1[quarter(Date) == 4, flag_qtr4 := 1]						
  
  #Tranform dataset to consider Retailer and ROM of each PPG as an individual PPG unit/ candidate for modelling and acting item set
  Dates=unique(input_sales_data_1$Date)
  data_range_quarters <- n_distinct(paste0(year(Dates),quarter(Dates)))
  min_num_quarter_sales <- 0.75* data_range_quarters
  min_qtr_sales_share <- 0.16*(1/data_range_quarters)
  min_sales_quarters <- 0.5*min_num_quarter_sales
  weeks_in_data <- length(Dates)
  
  # Modelling dataset preparation
  temporary_1 <- copy(input_sales_data_1)
  temporary_1<-temporary_1[wk_sold_doll_byppg>=0 &
                             wk_sold_qty_byppg>=0 &
                             !is.na(wk_sold_doll_byppg) &
                             !is.na(wk_sold_qty_byppg), ]
  temporary_1[,":="(wk_sold_avg_price_byppg=wk_avg_price_perunit_byppg)]						
  
  temporary_1[,":="(
    wk_avg_price_perunit_byppg=NULL)]
  temporary_1$PPG_Item_No=paste(temporary_1$PPG_Item_No,temporary_1$PPG_Retailer,sep="_")
  
  input_sales_data_1 <- temporary_1
  rm(temporary_1)
  
  #Revenue Cut Off Filter  
  temporary_1 <- input_sales_data_1[,list(avg_qtrly_sales = sum(wk_sold_doll_byppg)/data_range_quarters,
                                          avg_qtrly_qty = sum(wk_sold_qty_byppg)/data_range_quarters)
                                    ,by = c("PPG_Cat","PPG_MFG","PPG_Item_No")]
  temporary_1[,avg_rough_ppg_price_point := avg_qtrly_sales/avg_qtrly_qty]
  low_revenue_filtered_ppgs <- temporary_1[PPG_Cat == Category & 
                                             avg_qtrly_sales <= min_revenue_threshold]$PPG_Item_No
  
  input_sales_data_2 <- input_sales_data_1[!(PPG_Item_No %in% low_revenue_filtered_ppgs)]
  
  filtered_ppgs <- data.table()
  if(length(low_revenue_filtered_ppgs) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =low_revenue_filtered_ppgs,Reason = c("Low Revenue")))
  }
  
  rm(temporary_1)
  
  #Consistency Sales Cutoff Filter
  
  temporary_1 <- copy(input_sales_data_2)
  
  temporary_1[,":="(week_year = year(Date),
                    week_quarter = quarter(Date))]
  
  temporary_2 <- temporary_1[,list(sum_sales = sum(wk_sold_doll_byppg),
                                   sum_qty = sum(wk_sold_qty_byppg)) ,by = c("PPG_Item_No","week_year","week_quarter")]
  
  temporary_2[,":="(tot_sum_sales = sum(sum_sales),
                    tot_sum_qty = sum(sum_qty))
              ,by = c("PPG_Item_No")]
  
  temporary_2[,sales_share_in_qtr := round(sum_sales/tot_sum_sales,digits = 4)]
  temporary_2[,quantity_share_in_qtr := round(sum_qty/tot_sum_qty,digits = 4)]
  
  setorderv(temporary_2,c("PPG_Item_No","week_year","week_quarter"))
  
  temporary_2[,count_num_quarters := .N,by = "PPG_Item_No"]
  
  low_count_sales_num_quarters_filtered_ppgs <- unique(temporary_2[count_num_quarters < min_num_quarter_sales]$PPG_Item_No)
  
  low_count_sufficient_sale_quarters_filtered_ppgs <- temporary_2[!(PPG_Item_No %in% low_count_sales_num_quarters_filtered_ppgs) & (sales_share_in_qtr <= min_qtr_sales_share),
                                                                  list(count_num_quarters = .N),by = "PPG_Item_No"][count_num_quarters >= min_sales_quarters]$PPG_Item_No
  
  input_sales_data_3 <- input_sales_data_2[!(PPG_Item_No %in% union(low_count_sales_num_quarters_filtered_ppgs,low_count_sufficient_sale_quarters_filtered_ppgs))]
  
  if(length(union(low_count_sales_num_quarters_filtered_ppgs,low_count_sufficient_sale_quarters_filtered_ppgs)) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =union(low_count_sales_num_quarters_filtered_ppgs,low_count_sufficient_sale_quarters_filtered_ppgs),Reason = c("Inconsistent Sales")))
  }
  
  rm(temporary_1,temporary_2)
  
  # Outlier Detection and Removal
  
  input_sales_data_3[,":="(wk_sold_qty_contribution = wk_sold_qty_byppg/sum(wk_sold_qty_byppg))
                     ,by = c("PPG_Item_No")]
  
  input_sales_data_4 <- input_sales_data_3[wk_sold_qty_contribution >0]
  input_sales_data_4[,wk_sold_qty_contribution := NULL]
  input_sales_data_3[,wk_sold_qty_contribution := NULL]
  
  temporary_1 <- input_sales_data_4[,list(count_of_weeks = .N),by = c("PPG_Item_No")]
  
  low_data_filtered_ppgs <- temporary_1[count_of_weeks <= weeks_in_data*min_prct_data_points]$PPG_Item_No
  
  input_sales_data_5 <- input_sales_data_3[!(PPG_Item_No %in% low_data_filtered_ppgs)]
  
  if(length(low_data_filtered_ppgs) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =low_data_filtered_ppgs,Reason = c("Low Data")))
  }
  
  
  rm(temporary_1)
  
  
  input_sales_data_6 <- input_sales_data_5
  
  #Min Variance CutOff Filter
  
  temporary_1 <- copy(input_sales_data_6)
  
  temporary_2 <- temporary_1[,
                             list(sd_by_mean_sales_vol = (sqrt((length(wk_sold_qty_byppg)-1)/length(wk_sold_qty_byppg)) * sd(wk_sold_qty_byppg,na.rm = T))/mean(wk_sold_qty_byppg,na.rm = T),
                                  sd_by_mean_avg_price = (sqrt((length(wk_sold_avg_price_byppg)-1)/length(wk_sold_avg_price_byppg)) * sd(wk_sold_avg_price_byppg,na.rm = T))/mean(wk_sold_avg_price_byppg,na.rm = T)),
                             by = PPG_Item_No]
  
  low_variance_filtered_ppgs <- temporary_2[sd_by_mean_sales_vol <= sd_by_mean_sales_vol_threshold & sd_by_mean_avg_price <= sd_by_mean_avg_price_threshold]$PPG_Item_No
  
  input_sales_data_7 <- input_sales_data_6[!(PPG_Item_No %in% low_variance_filtered_ppgs)] 
  
  if(length(low_variance_filtered_ppgs) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =low_variance_filtered_ppgs,Reason = c("Low Variance")))
  }
  
  rm(temporary_1,temporary_2)
  
  #Remove ROM PPGs less than ACV Cutoff
  ROMFilter=input_sales_data_7%>%filter(PPG_Item_No%like%"_ROM")%>%group_by(PPG_Item_No)%>%summarise(ACV_Selling=mean(ACV_Selling,na.rm=T))
  ROMFilter1=ROMFilter$PPG_Item_No[ROMFilter$ACV_Selling>ROM_ACV_CUTOFF]
  # 
  # #Remove Retailer PPGs less than ACV cutoff
  RetailerFilter=input_sales_data_7%>%filter(!PPG_Item_No%like%"ROM")%>%group_by(PPG_Item_No)%>%summarise(ACV_Selling=mean(ACV_Selling,na.rm=T))
  RetailerFilter1=RetailerFilter$PPG_Item_No[RetailerFilter$ACV_Selling>Retailer_ACV_CUTOFF]
  
  low_acv_filtered_ppgs <- unique(input_sales_data_7[!(PPG_Item_No %in% c(ROMFilter1,RetailerFilter1))]$PPG_Item_No)
  
  input_sales_data_7<-input_sales_data_7[input_sales_data_7$PPG_Item_No %in% c(ROMFilter1,RetailerFilter1),]
  
  if(length(low_acv_filtered_ppgs) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =low_acv_filtered_ppgs,Reason = c("Low ACV")))
  }
  
  
  #Remove Die down PPGs
  
  temporary_1 <- copy(input_sales_data_7)
  
  setorderv(temporary_1,c("PPG_Item_No","Date"))
  
  check_date <- unique(temporary_1$Date)[length(unique(temporary_1$Date)) - 26]
  
  #Flagging last 26 weeks for comparison
  temporary_1[,flag := 0]
  temporary_1[Date > check_date,flag := 1]
  
  temporary_2 <- temporary_1[,list(avg_sales = mean(wk_sold_doll_byppg,na.rm = T),
                                   avg_acv  = mean(ACV_Selling,na.rm = T)),by = c("PPG_Item_No","flag")]
  
  
  temporary_3 <- dcast.data.table(temporary_2,PPG_Item_No~flag,value.var = c("avg_acv","avg_sales"))
  
  temporary_3[,flag := ifelse(avg_acv_1 >= 15 & avg_sales_1 >= (0.2 * avg_sales_0),0,1)]
  
  temporary_3[,ratio_sales_1 := avg_sales_1/avg_sales_0]
  
  remove_dying_ppgs <- unique(temporary_3[flag == 1]$PPG_Item_No)
  
  input_sales_data_7 <- input_sales_data_7[!(input_sales_data_7$PPG_Item_No %in% remove_dying_ppgs),]
  
  if(length(remove_dying_ppgs) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =remove_dying_ppgs,Reason = c("Dying PPGs")))
  }
  
  setDT(input_sales_data_7)
  
  #Filter out PPGs after Correlation check
  corr=input_sales_data_7%>%group_by(PPG_Item_No)%>%summarise(corr=cor(wk_sold_avg_price_byppg,wk_sold_qty_byppg))
  remove_ppgs_with_pos_correlation=corr$PPG_Item_No[corr$corr>0.20]
  input_sales_data_7=input_sales_data_7[!input_sales_data_7$PPG_Item_No%in%remove_ppgs_with_pos_correlation,]
  
  if(length(remove_ppgs_with_pos_correlation) > 0){
    filtered_ppgs <- rbind(filtered_ppgs,data.table(PPG_Item_No =remove_ppgs_with_pos_correlation,Reason = c("PPGs with Positive Correlation b/n Price & Sales Units")))
  }
  
  
  temp = input_sales_data_1[,c("PPG_Item_No","wk_sold_doll_byppg","PPG_MFG","PPG_Brand", "PPG_Retailer","PPG_Description")] %>%
    group_by(PPG_Item_No, PPG_MFG,PPG_Brand, PPG_Retailer,PPG_Description) %>% summarise(TotalSales = sum(wk_sold_doll_byppg))
  PPG_Filter_Summary = merge(temp, filtered_ppgs, by = "PPG_Item_No", all.x = T)
  save(input_sales_data_7,file=paste(filtered_model_dataset_filename,".RData",sep=""))
  write.csv(PPG_Filter_Summary,filtered_ppg_filename)
  remove(temp, PPG_Filter_Summary)
  return(TRUE)
}

