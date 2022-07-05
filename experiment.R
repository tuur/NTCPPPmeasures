library(haven)
library(rlist)
library(gdata)
library(rms)

# Internal code
source("~/Desktop/Research/NTCP - uitkomstmaten/code/Models LIPPV2_2.R")
source("~/Desktop/Research/NTCP - uitkomstmaten/code/MBS simulation.R")

# Loads and prepares the data: provides val_data and mbs_data
source("~/Desktop/Research/NTCP - uitkomstmaten/code/data_preparation.R")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

outputfilename = "outputfilename.rds" #results_bs1_imp1_100steps_02052022.rds"

no_steps = 200 # higher no steps plots more points

citl_interval = c(-1.5, 1.5)
slope_interval = c(-.9, 1.1)
auc_interval = c(0, 1)

# No bootstrap samples (per imputation set)
n_bs=1 
n_impsets=1 # already predefined in this data: max 10 (best set to 10)
impsets = c(4) #1:n_impsets

# helping function, possibly better to place in a utility file or something
combine_results_into_list = function(experimental_results, perturbation_info, setting_info){
  combined_result = as.list(setting_info)
  for (per in perturbation_info){
    pername = paste(c(per$model, per$metric,"step"),collapse='_')
    combined_result[[pername]] = per$step
  }
  combined_result = append(combined_result, list.flatten(exp_results, use.names = TRUE, classes = "ANY"))
  return(combined_result)
}

rounds_done = 0
t0 = Sys.time()
# conducting the experiment (across the imputation sets)
final_results = list()
source("~/Desktop/Research/NTCP - uitkomstmaten/code/MBS simulation.R")
for (impset_index in impsets){
  for (bs_index in 1:n_bs){
    print(c('>>> impset',impset_index,'bs_index',bs_index))
    if(bs_index > 1 | impset_index > 1){ # just to give some time indication
      rounds_left = (n_bs * n_impsets) - rounds_done
      est_time_per_round = (Sys.time() - t0) / rounds_done
      est_time_left = rounds_left * est_time_per_round
      print('Est time left:')
      print(est_time_left)
    }
    
    if (bs_index == 1){
      val_df <- val_data[[impset_index]]
      mbs_df <- mbs_data[[impset_index]]
    }else{
      val_df <- sample_n(val_data[[impset_index]], nrow(val_data[[impset_index]]), replace = TRUE)
      mbs_df <- sample_n(mbs_data[[impset_index]], nrow(mbs_data[[impset_index]]), replace = TRUE) 
    }
   
    res_indices <- list(bs_index=bs_index, impset_index=impset_index)
    for (model in c('xer2','xer3','dys2','dys3')){
      print(c('> model',model))
      # 1. CITL perturbations
      print('citl')
      for (i in 0:no_steps){
        citl_step = citl_interval[[1]] + i*abs(citl_interval[[1]]-citl_interval[[2]])/no_steps
        original_models <- get_LIPPV2_2_models()
        perturbations = list(list(model=model,metric="citl",step=citl_step)) # to extend
        exp_results = conduct_perturbed_val_mbs_experiment(val_df, mbs_df, original_models, perturbations)
        results = combine_results_into_list(exp_results, perturbations, list(bs_index=bs_index, impset_index=impset_index))
        final_results = append(final_results, list(results))
      }
      print('slope')
      #2. SLOPE perturbations
      for (i in 0:no_steps){
        slope_step = slope_interval[[1]] + i*abs(slope_interval[[1]]-slope_interval[[2]])/no_steps
        original_models <- get_LIPPV2_2_models()
        perturbations = list(list(model=model,metric="slope",step=slope_step)) # to extend
        exp_results = conduct_perturbed_val_mbs_experiment(val_df, mbs_df, original_models, perturbations)
        results = combine_results_into_list(exp_results, perturbations, list(bs_index=bs_index, impset_index=impset_index))
        final_results = append(final_results, list(results))
      }
      print('auc')
      #3. AUC perturbations
      for (i in 0:no_steps){
        auc_step = auc_interval[[1]] + i*abs(auc_interval[[1]]-auc_interval[[2]])/no_steps
        original_models <- get_LIPPV2_2_models()
        perturbations = list(list(model=model,metric="auc",step=auc_step)) # to extend
        exp_results = conduct_perturbed_val_mbs_experiment(val_df, mbs_df, original_models, perturbations)
        results = combine_results_into_list(exp_results, perturbations, list(bs_index=bs_index, impset_index=impset_index))
        final_results = append(final_results, list(results))
      }
    }
  rounds_done = rounds_done + 1
  }
}

unique_names = list()
for (row in final_results){
  unique_names = union(unique_names, names(row))
}

get_template = function(used_names){
  template <- as.list(rep(NA, length(used_names)))
  names(template) <- used_names
  return(template)
}

temp = get_template(unique_names)
for (i in 1:length(final_results)){
  temp = get_template(unique_names)
  final_results[[i]] = update.list(temp, final_results[[i]])
}


final_results_df = as.data.frame(do.call(rbind, final_results))
saveRDS(final_results_df, file = outputfilename)



# ========================================================== 
# ========== Baseline table ================================
# ==========================================================

library(tableone)
#variables_in_baseline_table = c("Age","Primary_tumor_location_oral_cavity","Primary_tumor_location_larynx","Primary_tumor_location_pharynx", "Dysphagia_at_M6_2plus", "Dysphagia_at_M6_3plus", "Xerostomia_at_M6_2plus", "Xerostomia_at_M6_2plus")

# val_df / citor_data #1145
# mbs_df / mba_data # 289

# date ranges Sval
sort(citor_data[[1]]$RTSTART)[[1]]
sort(citor_data[[1]]$RTSTART)[[1132]]
# date ranges Smbs
sort(mba_data[[1]]$RTSTART)[[1]]
sort(mba_data[[1]]$RTSTART)[[289]]


listVar = c("GESLACHT","AGE",'LOCTUM_cat',"Q41_M06","DYSFAGIE_UMCGshortv2_M06","TSTAD_DEF","TSTAD_2cat")
catVar = c("GESLACHT","LOCTUM_cat","TSTAD_2cat")
SvalTable1 <- CreateTableOne(vars = listVar, data = citor_data[[1]], factorVars = catVar)
SmbsTable1 <- CreateTableOne(vars = listVar, data = mba_data[[1]], factorVars = catVar)

SvalTable1
SmbsTable1
