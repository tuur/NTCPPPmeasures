#install.packages("remotes")
#remotes::install_github("BavoDC/CalibrationCurves")
library(CalibrationCurves)
library(rms)
library(stats)

# Simulate the model based selection procedure
source("~/Desktop/Research/NTCP - uitkomstmaten/code/Models LIPPV2_2.R")

conduct_perturbed_val_mbs_experiment <- function(val_df, mbs_df, original_models, perturbations){
  perturbed_models <- obtain_perturbed_models(original_models, perturbations, val_df)
  
  val_results <- conduct_perturbed_validation(val_df, perturbed_models, perturbations)
  mbs_results <- simulate_mbs(mbs_df, perturbed_models, get_LIPPV2_2_models())

  return (list(mbs_results=mbs_results, val_results=val_results))
}

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

conduct_perturbed_validation <- function(df, models, perturbations){
  lp_predictions <- list()
  val_results <- list() 
  
  for (model_name in names(models)){
    #print(model_name)
    lp_predictions[[model_name]] = apply_model(df, models[[model_name]], DOSE="RT", LP=TRUE) # RT as we are validating here (no MBS)
    true_y = as.numeric(df[[models[[model_name]]$y_name]])
    val_res = quiet(val.prob.ci.2(y=true_y, logit=lp_predictions[[model_name]], pl=FALSE, smooth=FALSE))
    val_results[[model_name]] = as.list(val_res)
  }
  
  return(val_results)
  
}

obtain_perturbed_models <- function(models, perturbations, val_df){
  for (perturbation in perturbations){
    if (perturbation$metric == 'citl'){
      # increase the intercept with the indicated step (perturbation$step)
      models[[perturbation$model]]$set_parameters(intercept=models[[perturbation$model]]$get_model_intercept()+perturbation$step)
    }
    if (perturbation$metric == 'slope'){
      # get predictions of original model on val set 
      val_preds = apply_model(val_df, models[[perturbation$model]], DOSE='RT', LP=FALSE)

      # increase/decrease the slope with the indicated step (perturbation$step)
      models[[perturbation$model]]$set_parameters(slope=models[[perturbation$model]]$get_model_slope()+perturbation$step)

      # refit intercept on original validation predictions
      lp_min_intercept = apply_model(val_df, models[[perturbation$model]], DOSE='RT', LP=TRUE) - models[[perturbation$model]]$get_model_intercept()
      new_intercept = glm(formula = val_preds ~ offset(lp_min_intercept), family = "quasibinomial")$coefficients
      models[[perturbation$model]]$set_parameters(intercept=new_intercept)

    }
    if (perturbation$metric == 'auc'){
      
      # V1 interpolate linear predictor with noise
      # get predictions of original model on val set 
      val_preds = apply_model(val_df, models[[perturbation$model]], DOSE='RT', LP=FALSE)
      
      #V2 interpolate coefficients with a random model
      current_coefficients = models[[perturbation$model]]$coeffs
      sampled_random_coefficients = torch_reshape(torch_tensor(rnorm(length(current_coefficients), 0, 1)),list(length(models[[perturbation$model]]$coeffs),1))
      
      updated_coefficients = as.numeric((current_coefficients*(1-perturbation$step)) + (perturbation$step * sampled_random_coefficients))
      names(updated_coefficients)=models[[perturbation$model]]$X_names

      models[[perturbation$model]]$set_parameters(coeffs=updated_coefficients)

      
      # RECALIBRATION
      # adding noise to the linear predictor results in more extreme predictions; so recalibration is needed
      # refit intercept and slope on original validation predictions
      lp_min_intercept_div_slope = (apply_model(val_df, models[[perturbation$model]], DOSE='RT', LP=TRUE)-models[[perturbation$model]]$get_model_intercept())/models[[perturbation$model]]$get_model_slope()
      calib_model = glm(formula = val_preds ~ lp_min_intercept_div_slope, family = "quasibinomial")
      new_intercept = calib_model$coefficients[['(Intercept)']]
      new_slope = calib_model$coefficients[['lp_min_intercept_div_slope']]
      models[[perturbation$model]]$set_parameters(intercept=new_intercept, slope=new_slope)
    }
  }
  return(models)
}
  
simulate_mbs <- function(mbs_df, models, original_models){
  #metrics to return:
  # - no. preselected
  # - no. RT
  # - no. PT
  # - no. RT>PT
  # - no. PT>RT
  # - mean NTCP RT
  # - mean NTCP PT
  # - mean delta NTCP
  
  # >>>>>>>>>>>>>> Original models <<<<<<<<<<<<<<<<<
  pt_probs_o = list()
  rt_probs_o = list()
  delta_ntcp_o = list()
  delta_lp_o = list()
  for (model_name_o in names(original_models)){
    pt_probs_o[[model_name_o]] = apply_model(mbs_df, original_models[[model_name_o]], DOSE='PT', LP=FALSE)
    rt_probs_o[[model_name_o]] = apply_model(mbs_df, original_models[[model_name_o]], DOSE='RT', LP=FALSE)
    delta_ntcp_o[[model_name_o]] = rt_probs_o[[model_name_o]] - pt_probs_o[[model_name_o]]
    #delta_lp_o[[model_name_o]] = apply_model(mbs_df, original_models[[model_name_o]], DOSE='RT', LP=TRUE) - apply_model(mbs_df, original_models[[model_name_o]], DOSE='PT', LP=TRUE)
  }
  
  
  preselected_for_plan_comparison_o = which(rt_probs_o$xer2 >= 0.1 | rt_probs_o$dys2 >=.1 | ((sum(rt_probs_o$xer2 + rt_probs_o$dys2) >= 0.15) & (rt_probs_o$xer2 >= 0.05 & rt_probs_o$dys2 >=.05)) | rt_probs_o$xer3 >= 0.05 | rt_probs_o$dys3 >= 0.05 | ((sum(rt_probs_o$xer3 + rt_probs_o$dys3) >= 0.075) & (rt_probs_o$xer2 >= 0.025 & rt_probs_o$dys2 >=.025)))
  pt_selected_incl_not_preselected_o = which(delta_ntcp_o$xer2 >= 0.1 | delta_ntcp_o$dys2 >=.1 | ((sum(delta_ntcp_o$xer2 + delta_ntcp_o$dys2) >= 0.15) & (delta_ntcp_o$xer2 >= 0.05 & delta_ntcp_o$dys2 >=.05)) | delta_ntcp_o$xer3 >= 0.05 | delta_ntcp_o$dys3 >= 0.05 | ((sum(delta_ntcp_o$xer3 + delta_ntcp_o$dys3) >= 0.075) & (delta_ntcp_o$xer2 >= 0.025 & delta_ntcp_o$dys2 >=.025)))
  
  had_pt = pt_selected_incl_not_preselected_o #intersect(preselected_for_plan_comparison_o, )
  had_rt = setdiff(seq(1, nrow(mbs_df), by=1), had_pt)
    
  # >>>>>>>>>>>>>>  Perturbed models <<<<<<<<<<<<<<<<<
  pt_probs = list()
  rt_probs = list()
  delta_ntcp = list()
  delta_lp = list()
  for (model_name in names(models)){
    pt_probs[[model_name]] = apply_model(mbs_df, models[[model_name]], DOSE='PT', LP=FALSE)
    rt_probs[[model_name]] = apply_model(mbs_df, models[[model_name]], DOSE='RT', LP=FALSE)
    delta_ntcp[[model_name]] = rt_probs[[model_name]] - pt_probs[[model_name]]
    delta_lp[[model_name]] = apply_model(mbs_df, models[[model_name]], DOSE='RT', LP=TRUE) - apply_model(mbs_df, models[[model_name]], DOSE='PT', LP=TRUE)
  }
  
  # TODO: double check this with LIPPV2.2
  preselected_for_plan_comparison = which(rt_probs$xer2 >= 0.1 | rt_probs$dys2 >=.1 | ((sum(rt_probs$xer2 + rt_probs$dys2) >= 0.15) & (rt_probs$xer2 >= 0.05 & rt_probs$dys2 >=.05)) | rt_probs$xer3 >= 0.05 | rt_probs$dys3 >= 0.05 | ((sum(rt_probs$xer3 + rt_probs$dys3) >= 0.075) & (rt_probs$xer2 >= 0.025 & rt_probs$dys2 >=.025)))
  pt_selected_incl_not_preselected = which(delta_ntcp$xer2 >= 0.1 | delta_ntcp$dys2 >=.1 | ((sum(delta_ntcp$xer2 + delta_ntcp$dys2) >= 0.15) & (delta_ntcp$xer2 >= 0.05 & delta_ntcp$dys2 >=.05)) | delta_ntcp$xer3 >= 0.05 | delta_ntcp$dys3 >= 0.05 | ((sum(delta_ntcp$xer3 + delta_ntcp$dys3) >= 0.075) & (delta_ntcp$xer2 >= 0.025 & delta_ntcp$dys2 >=.025)))
  
  selected_for_pt = pt_selected_incl_not_preselected #intersect(preselected_for_plan_comparison, )
  selected_for_rt = setdiff(seq(1, nrow(mbs_df), by=1), selected_for_pt)
  
  n_preselected = length(preselected_for_plan_comparison)
  n_pt = length(selected_for_pt)
  n_rt = length(selected_for_rt)
  

  #print(c('INT',intersect(selected_for_pt, had_rt),'nowPT', selected_for_pt,'hadRT',had_rt))
  n_rt_to_pt = length(intersect(selected_for_pt, had_rt))
  n_rt_to_rt = length(intersect(selected_for_rt, had_rt))
  n_pt_to_pt = length(intersect(selected_for_pt, had_pt))
  
  n_pt_to_rt = length(intersect(selected_for_rt, had_pt))

  mean_ntcp_rt_xer2 = mean(rt_probs[['xer2']])
  mean_ntcp_pt_xer2 = mean(pt_probs[['xer2']])
  mean_delta_ntcp_xer2 = mean(delta_ntcp[['xer2']])
  mean_delta_lp_xer2 = mean(delta_lp[['xer2']])

  mean_ntcp_rt_xer3 = mean(rt_probs[['xer3']])
  mean_ntcp_pt_xer3 = mean(pt_probs[['xer3']])
  mean_delta_ntcp_xer3 = mean(delta_ntcp[['xer3']])
  mean_delta_lp_xer3 = mean(delta_lp[['xer3']])
  
  mean_ntcp_rt_dys2 = mean(rt_probs[['dys2']])
  mean_ntcp_pt_dys2 = mean(pt_probs[['dys2']])
  mean_delta_ntcp_dys2 = mean(delta_ntcp[['dys2']])
  mean_delta_lp_dys2 = mean(delta_lp[['dys2']])

  mean_ntcp_rt_dys3 = mean(rt_probs[['dys3']])
  mean_ntcp_pt_dys3 = mean(pt_probs[['dys3']])
  mean_delta_ntcp_dys3 = mean(delta_ntcp[['dys3']]) 
  mean_delta_lp_dys3 = mean(delta_lp[['dys3']])
  
  result = list(mean_delta_lp_dys3=mean_delta_lp_dys3, mean_delta_lp_dys2=mean_delta_lp_dys2, mean_delta_lp_xer3=mean_delta_lp_xer3, mean_delta_lp_xer2=mean_delta_lp_xer2, pt=selected_for_pt, rt=selected_for_rt,pt_probs=pt_probs, rt_probs=rt_probs, delta_ntcp=delta_ntcp, n_rt_to_rt=n_rt_to_rt, n_pt_to_pt=n_pt_to_pt, n_pt=n_pt, n_rt=n_rt, n_preselected=n_preselected, n_rt_to_pt=n_rt_to_pt, n_pt_to_rt=n_pt_to_rt, mean_ntcp_rt_xer2=mean_ntcp_rt_xer2, mean_ntcp_pt_xer2=mean_ntcp_pt_xer2, mean_delta_ntcp_xer2=mean_delta_ntcp_xer2, mean_ntcp_rt_xer3=mean_ntcp_rt_xer3, mean_ntcp_pt_xer3=mean_ntcp_pt_xer3, mean_delta_ntcp_xer3=mean_delta_ntcp_xer3, mean_ntcp_rt_dys2=mean_ntcp_rt_dys2, mean_ntcp_pt_dys2=mean_ntcp_pt_dys2, mean_delta_ntcp_dys2=mean_delta_ntcp_dys2, mean_ntcp_rt_dys3=mean_ntcp_rt_dys3, mean_ntcp_pt_dys3=mean_ntcp_pt_dys3, mean_ntcp_pt_dys3=mean_ntcp_pt_dys3, mean_delta_ntcp_dys3=mean_delta_ntcp_dys3)
  return(result)
  }
  