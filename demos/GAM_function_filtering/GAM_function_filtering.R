library(pacman)
pacman::p_load(readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata,gridExtra) 


setwd(here::here("demos/GAM_function_filtering"))




####gam loop function####
GAM_LOOP_FUN<-function(Edata,k,correlated_vars1,correlated_vars2,correlated_vars3,correlated_vars4,correlated_vars5,correlated_vars6,folder_name,familyXYZ,number_vars_in_mod){
  
  #create all combinations of predictors
  predictor_combinations <- lapply(1:number_vars_in_mod, FUN = function(x){
    #create combination
    combination <- combn(predictors, m = x) |> as.data.table()
    #add s() to all for gam
    combination <- sapply(combination, FUN = function(y) paste0("s(", y, ",",k,")")) |> as.data.table()
    #collapse
    combination <- summarize_all(combination, .funs = paste0, collapse = "+")
    #unlist
    combination <- unlist(combination)
    #remove names
    names(combination) <- NULL
    #return
    return(combination)
  })
  #create all combinations of predictors
  predictor_combinations1 <- sapply(predictors, FUN = function(y) paste0("s(", y, ",",k,")"))|> as.data.table()
  rownames(predictor_combinations1) <- 1:nrow(predictor_combinations1) 
  #merge combinations of predictors as vector
  predictor_combinations <- do.call(c, predictor_combinations)
  predictor_combinations1 <- do.call(c, predictor_combinations1)
  predictor_combinations <- as.data.frame(predictor_combinations)
  predictor_combinations1 <- as.data.frame(predictor_combinations1)
  names(predictor_combinations1)[1]="predictor_combinations"
  predictor_combinations <- rbind(predictor_combinations,predictor_combinations1)
  
  ### remove list elements that contain duplicate/correlated independent variables
  ## see correlated_vars character list
  predictor_combinations <-predictor_combinations[!grepl("bt_anomaly", predictor_combinations$predictor_combinations)| !grepl("sst_anomaly" ,predictor_combinations$predictor_combinations),]
  
  if(correlated_vars1!="NA"||correlated_vars2!="NA"){
    #
    predictor_combinations <- as.data.frame(predictor_combinations)
    predictor_combinations <-predictor_combinations[!grepl(correlated_vars1, predictor_combinations$predictor_combinations)| !grepl(correlated_vars2,predictor_combinations$predictor_combinations),]
  }
  if(correlated_vars3!="NA"||correlated_vars4!="NA"){
    #
    predictor_combinations <- as.data.frame(predictor_combinations)
    predictor_combinations <-predictor_combinations[!grepl(correlated_vars3, predictor_combinations$predictor_combinations)| !grepl(correlated_vars4,predictor_combinations$predictor_combinations),]
  }
  if(correlated_vars5!="NA"||correlated_vars6!="NA"){
    #
    predictor_combinations <- as.data.frame(predictor_combinations)
    predictor_combinations <-predictor_combinations[!grepl(correlated_vars5, predictor_combinations$predictor_combinations)| !grepl(correlated_vars6,predictor_combinations$predictor_combinations),]  
  }
  #create folder to save results to
  if(!dir.exists("trial_results")){
    dir.create("trial_results")
  }
  if(!dir.exists(paste0("trial_results/",folder_name))){
    dir.create(paste0("trial_results/",folder_name))
  }
  if(!dir.exists(paste0("trial_results/",folder_name,"/models"))){
    dir.create(paste0("trial_results/",folder_name,"/models"))
  }
  
  #create and save hypergrid (all combinations of targets and predictors combinations)
  #create hypergrid and save to trial_results/folder_name
  hypergrid <- expand.grid(target = targets, predictors = predictor_combinations) |> as.data.table()
  #add identifier
  hypergrid[, model := paste0("model", 1:nrow(hypergrid))]
  #save to dev
  fwrite(hypergrid, file = paste0("trial_results/",folder_name,"/hypergrid.csv"))
  #if file exists read
  hypergrid <- fread(paste0("trial_results/",folder_name,"/hypergrid.csv"))
  
  #loop through hypergrid, create GAM models
  #progressbar
  pb <- txtProgressBar(min = 1, max = nrow(hypergrid), style = 3)
  for(i in 1:nrow(hypergrid)){
    #update progressbar
    setTxtProgressBar(pb, i)
    #select target
    target <- hypergrid[i,]$target
    #select predictors
    predictors <- hypergrid[i,]$predictors
    #create formula
    gam.formula <- as.formula(paste0(target, "~", predictors))
    #run gam
    gam.model <- gam(gam.formula, familyXYZ,method = "REML",Edata)
    #save gam model do trial_results/folder_name/model
    saveRDS(gam.model, file = paste0("trial_results/", folder_name,"/models/", hypergrid[i,]$model, ".RDS"))
  }
  
  #example where you extract model performances
  for(i in 1:nrow(hypergrid)){
    #read the right model
    rel.model <- readRDS(paste0("trial_results/",folder_name,"/models/", hypergrid[i,]$model, ".RDS"))
    
    #extract model performance, add to hypergrid
    hypergrid[i, AIC := round(rel.model$aic,digits=3)]
    hypergrid[i, s.pv := list(round(summary(rel.model)[["s.pv"]],digits=3))]
    hypergrid[i, dev.expl := round(summary(rel.model)[["dev.expl"]],digits=3)]
    hypergrid[i, family := rel.model$family[1]]
  }
  
  #arrange hypergrid and see resulting df showing model diognisc comparisons
  hypergrid<- dplyr::arrange(hypergrid, hypergrid$target, desc(hypergrid$AIC))
  .GlobalEnv$hypergrid <- hypergrid
}

#### load environmental data to use #####
distribution_fall<-read.csv(("distribution_fall.csv"))

####Testing Fall depth & lat models in function######
targets <- c("COG_Lat_fall","COG_depth_fall")
predictors <- colnames(distribution_fall)[!(colnames(distribution_fall) %in% c("COG_Lat_fall","COG_depth_fall", "Year"))]
correlated_vars <- c("bt_anomaly","sst_anomaly","Heatwave","GSI")

GAM_LOOP_FUN(
  Edata              = distribution_fall,
  k                  = "k=10",
  correlated_vars1   = correlated_vars[1],
  correlated_vars2   = correlated_vars[3],
  correlated_vars3   = correlated_vars[2],
  correlated_vars4   = correlated_vars[3],
  correlated_vars5   = correlated_vars[1],
  correlated_vars6   = correlated_vars[4],
  folder_name        = "cod_fall_depth",
  familyXYZ          =  "family=gaussian()",
  number_vars_in_mod = (length(predictors)-4))

# Formatting steps:
hypergrid$s.pv <- as.character(hypergrid$s.pv)
hypergrid_gaus <- as.data.frame(hypergrid,stringsAsFactors = F)
hypergrid_gaus <- hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]



# Splitting 2.pv to check if they are all significant:
library(tidyverse)

# This is the test case
test_df <- data.frame("text_col" = c("0.025", "c(0.05, 0.03)", "c(0.03, 0.04, 0.02)")) %>% 
  mutate(new_col = text_col)

# This is how you can do it for all of them:
# Trying it first by splitting it up and addressing it individually
hypergrid_gaus %>% 
  split(.$predictors) %>% 
  map(function(x){
    
    # make it a numeric vector instead of one string, by parsing the values
    pval_set <- eval(parse(text = x$s.pv))
  
    # Check that all are below or above
    pval_check <- all(pval_set < 0.05)

    # Add that TRUE/FALSE variable back in
    x <- mutate(x, all_signif = pval_check)
  })
  

# Can it be done without map(), maybe not, maybe so!
hypergrid_gaus %>% 
  mutate(
    all_signif = all(eval(parse(text = s.pv)) <= 0.05),
    some_signif = any(eval(parse(text = s.pv)) <= 0.05)
  ) %>% 
  filter(some_signif)


# Things I tried first that failed:
# stringr: string_remove_all, str_split, separate





# png(("fall_depth_lat.png"),height= 22*nrow(hypergrid_gaus), width = 170*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
# dev.off()

saveRDS(file = "hypergrid_gaus.RDS", hypergrid_gaus)

