# NTCP models from the 2019 Dutch National protocol for model-based selection of HNC patients for proton therapy
library(stringr)
source("~/Desktop/Research/NTCP - uitkomstmaten/code/LR.R")


# Function to apply the NTCP models to either RT or PT data 
apply_model <- function(data, model, DOSE, LP=FALSE){ # DOSE is RT or DOSE is PT
  # remember neutral coefficient names (with DOSE_ prefix)
  oldnames <- model$X_names
  # set what variables should be used (RT or PT) by replacing the DOSE_ prefix with RT or PT
  newnames <- lapply(oldnames, function(x) str_replace(x, 'DOSE', DOSE))
  model$X_names <- newnames
  # make predictions
  predictions <- model$predict(data, LP=LP)
  # make the model neutral again (resetting to original state: using DOSE_ prefixes)
  model$X_names <- oldnames
  return(predictions)
}


# Xerostomia grade >= II: primary setting

xer2_coeffs_list <- list(
  list('DOSE_sqrt_Dmean_ipsilateral_parotid_sqrt_Dmean_contralateral_parotid', 0.0996),
  list('DOSE_Dmean_both_submandibulars', 0.0182),
  list('Xerostomia_at_baseline_2', 0.4950), # een beetje
  list('Xerostomia_at_baseline_3_4', 1.2070) # nogal of heel erg
)
xer2_intercept = -2.2951
xer2_outcome = 'Xerostomia_at_M6_2plus'

xer2_coeffs =  data.frame(matrix(NA, ncol = length(xer2_coeffs_list), nrow = 0))  
xer2_coeffs <- setNames(lapply(xer2_coeffs_list, function(x) x[[2]]), lapply(xer2_coeffs_list, function(x) x[[1]]))

# xer2_model <- torch_lr(names(xer2_coeffs), xer2_outcome)
# xer2_model$set_parameters(xer2_coeffs, xer2_intercept)


# Xerostomia grade >= III: primary setting

xer3_coeffs_list <- list(
  list('DOSE_sqrt_Dmean_ipsilateral_parotid_sqrt_Dmean_contralateral_parotid', 0.0855),
  list('DOSE_Dmean_both_submandibulars', 0.0156),
  list('Xerostomia_at_baseline_2', 0.4249), # een beetje
  list('Xerostomia_at_baseline_3_4', 1.0361) # nogal of heel erg
)
xer3_intercept = -3.7286
xer3_outcome = 'Xerostomia_at_M6_3plus'

xer3_coeffs =  data.frame(matrix(NA, ncol = length(xer3_coeffs_list), nrow = 0))  
xer3_coeffs <- setNames(lapply(xer3_coeffs_list, function(x) x[[2]]), lapply(xer3_coeffs_list, function(x) x[[1]]))

# xer3_model <- torch_lr(names(xer3_coeffs), xer3_outcome)
# xer3_model$set_parameters(xer3_coeffs, xer3_intercept)

# Dysphagia grade >= II: primary setting

dys2_coeffs_list <- list(
  list('DOSE_Dmean_oral_cavity', 0.0300),
  list('DOSE_Dmean_PCM_superior', 0.0236),
  list('DOSE_Dmean_PCM_medius', 0.0095),
  list('DOSE_Dmean_PCM_inferior', 0.0133),
  list('Dysphagia_at_baseline_2', 0.9382), # grade 0-1 is reference
  list('Dysphagia_at_baseline_3_5', 1.2900),
  list('Primary_tumor_location_pharynx', -0.6281), # oral cavity is reference
  list('Primary_tumor_location_larynx', -0.7711)
  )
dys2_intercept = -4.0536
dys2_outcome = 'Dysphagia_at_M6_2plus'

dys2_coeffs =  data.frame(matrix(NA, ncol = length(dys2_coeffs_list), nrow = 0))  
dys2_coeffs <- setNames(lapply(dys2_coeffs_list, function(x) x[[2]]), lapply(dys2_coeffs_list, function(x) x[[1]]))

# dys2_model <- torch_lr(names(dys2_coeffs), dys2_outcome)
# dys2_model$set_parameters(dys2_coeffs, dys2_intercept)

# Dysphagia grade >= III: primary setting

dys3_coeffs_list <- list(
  list('DOSE_Dmean_oral_cavity', 0.0259),
  list('DOSE_Dmean_PCM_superior', 0.0203),
  list('DOSE_Dmean_PCM_medius', 0.0303),
  list('DOSE_Dmean_PCM_inferior', 0.0341),
  list('Dysphagia_at_baseline_2', 0.5738), # grade 0-1 is reference
  list('Dysphagia_at_baseline_3_5', 1.4718),
  list('Primary_tumor_location_pharynx', 0.0387), # oral cavity is reference
  list('Primary_tumor_location_larynx', -0.5303)
)
dys3_intercept = -7.6174
dys3_outcome = 'Dysphagia_at_M6_3plus'

dys3_coeffs =  data.frame(matrix(NA, ncol = length(dys3_coeffs_list), nrow = 0))  
dys3_coeffs <- setNames(lapply(dys3_coeffs_list, function(x) x[[2]]), lapply(dys3_coeffs_list, function(x) x[[1]]))

# dys3_model$set_parameters(dys3_coeffs, dys3_intercept)
# dys3_model <- torch_lr(names(dys3_coeffs), dys3_outcome)


get_LIPPV2_2_models <- function(){
  models <- list()
  models$xer2 = torch_lr(names(xer2_coeffs), xer2_outcome)
  models$xer2$set_parameters(xer2_coeffs, xer2_intercept)
  
  models$xer3 <- torch_lr(names(xer3_coeffs), xer3_outcome)
  models$xer3$set_parameters(xer3_coeffs, xer3_intercept)
  
  models$dys2 <- torch_lr(names(dys2_coeffs), dys2_outcome)
  models$dys2$set_parameters(dys2_coeffs, dys2_intercept)
  
  models$dys3 <- torch_lr(names(dys3_coeffs), dys3_outcome)
  models$dys3$set_parameters(dys3_coeffs, dys3_intercept)
  
  return(models)
}
