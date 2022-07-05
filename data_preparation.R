library(plyr)
library(dplyr)
library(mice)
library(haven)


# ==========================================================
#        LOAD RAW DATA     
# ==========================================================

# External Validation Data (already imputed)
load("~/Desktop/Research/NTCP - Updating/code/data/imp.ext.all.RData")
load("~/Desktop/Research/NTCP - Updating/code/data/CITOR.data.imputed.RData")

for (i in 1:10){
  CITOR.data.imputed[[i]]$DYSFAGIE_UMCGshortv2_M06 = revalue(CITOR.data.imputed[[i]]$DYSFAGIE_UMCGshort_M06, c("Grade 0-1 (regular diet)"="Grade 0-1 (regular diet)", "Grade 2 (soft food)"="Grade 2 (soft food)", "Grade 3 (liquids only )"="Grade 3-5 (liquids only or tube feeding dependent)", "Grade 4-5 (tube feeding dependent)"="Grade 3-5 (liquids only or tube feeding dependent)"))
  CITOR.data.imputed[[i]]$DYSFAGIE_UMCGshortv2_BSL = revalue(CITOR.data.imputed[[i]]$DYSFAGIE_UMCGshort_BSL, c("Grade 0-1 (regular diet)"="Grade 0-1 (regular diet)", "Grade 2 (soft food)"="Grade 2 (soft food)", "Grade 3 (liquids only )"="Grade 3-5 (liquids only or tube feeding dependent)", "Grade 4-5 (tube feeding dependent)"="Grade 3-5 (liquids only or tube feeding dependent)"))
}
overlapping_citor_columns = intersect(colnames(imp.ext.all[[1]]),colnames(CITOR.data.imputed[[1]]))
citor_data = list()
for (i in 1:10){
  citor_data[[i]] <- rbind(imp.ext.all[[i]][,overlapping_citor_columns],CITOR.data.imputed[[i]][,overlapping_citor_columns])
}

# External validation data (not imputed_but more variables) - not needed?
#citor_val <- readRDS("~/Desktop/Research/NTCP - Updating/code/data/CITOR.validatie.data.RDS")

# MBS Data (but without proton plans for RT patients)
load("~/Desktop/Research/NTCP - Updating/code/data/mba_data.RData") 

# proton plans for RT patients (in a separate file)
ptplans_rtps = read_sav("~/Desktop/Research/NTCP - uitkomstmaten/code/data/IMPT_DVH_data_van_fotonen patienten_UPDATE19072021.sav")

# ==========================================================
# add pt data from ptplans_rtps to mba_data 
# ==========================================================

impt_dose_columns = colnames(ptplans_rtps)[grep('_LIPIMPT', colnames(ptplans_rtps))]
sel_ptdf =  ptplans_rtps[,c('PAID', impt_dose_columns)]
for (imp_index in 1:10){
  # 1. add PT plans to data (_LIPIMPT for RT patients)
  mba_data[[imp_index]] <- merge(x=mba_data[[imp_index]], y=sel_ptdf, by="PAID", all.x=TRUE)
  # 2. set _LIPIMPT for PT patients (using _Clin info, as for these patients the IMPT plan is the clinical plan)
  for (c in impt_dose_columns){
    clin_column <- str_replace(c, "_LIPIMPT", "_Clin")
    if (clin_column %in% colnames(mba_data[[imp_index]])){
      mba_data[[imp_index]][[c]][sort(which(mba_data[[imp_index]]$RT_Technique=='IMPT'))] = mba_data[[imp_index]][[clin_column]][sort(which(mba_data[[imp_index]]$RT_Technique=='IMPT'))]
    }
  }
}

# ==========================================================
#   Assign dataframes to use for validation and mbs data     
# ==========================================================

val_input_df <-citor_data # as read from the data files (set to imp.ext.all if only 395)
mbs_input_df <- mba_data # as read from the data files

# ==========================================================
#   DEFINE IMPORTANT COLUMNS       
# ==========================================================

relevant_val_columns <- list(
  'Age',
  'RTSTART',
  'Geslacht',
  'LOCTUM_cat',
  'TSTAD_DEF',
  'Primary_tumor_location_oral_cavity',  #oral_cavity, pharynx, larynx
  'Primary_tumor_location_larynx',  #oral_cavity, pharynx, larynx
  'Primary_tumor_location_pharynx',  #oral_cavity, pharynx, larynx
  'Dysphagia_at_baseline_0_1',                # physician rated (0_1 vs. 2 vs. 3_5)
  'Dysphagia_at_baseline_2',                # physician rated (0_1 vs. 2 vs. 3_5)
  'Dysphagia_at_baseline_3_5',                # physician rated (0_1 vs. 2 vs. 3_5)
  'Dysphagia_at_M6_2plus',                      # physician rated (0_1 vs. 2 vs. 3_5) grade >= 2
  'Dysphagia_at_M6_3plus',                      # physician rated (0_1 vs. 2 vs. 3_5) grade >= 2
  'Xerostomia_at_baseline_1',               # patient-rated (1 vs. 2 vs. 3_4) EORTC QLQ-H&N35 - Q41
  'Xerostomia_at_baseline_2',               # patient-rated (1 vs. 2 vs. 3_4) EORTC QLQ-H&N35 - Q41
  'Xerostomia_at_baseline_3_4',               # patient-rated (1 vs. 2 vs. 3_4) EORTC QLQ-H&N35 - Q41
  'Xerostomia_at_M6_2plus',                     # patient-rated (1 vs. 2 vs. 3_4) EORTC QLQ-H&N35 - Q41
  'Xerostomia_at_M6_3plus',                     # patient-rated (1 vs. 2 vs. 3_4) EORTC QLQ-H&N35 - Q41
  'RT_Dmean_parotid_low',
  'RT_Dmean_parotid_high',
  'RT_sqrt_Dmean_ipsilateral_parotid_sqrt_Dmean_contralateral_parotid',
  'RT_Dmean_both_submandibulars',
  'RT_Dmean_oral_cavity',
  'RT_Dmean_PCM_superior',
  'RT_Dmean_PCM_medius',
  'RT_Dmean_PCM_inferior')


additionally_relevant_mbs_columns <- list(
  'Preselected_for_plan_comparison', # From here only those for MBS data
  'Indication_for_PT',
  'PT_Dmean_parotid_low',
  'PT_Dmean_parotid_high',
  'PT_sqrt_Dmean_ipsilateral_parotid_sqrt_Dmean_contralateral_parotid',
  'PT_Dmean_both_submandibulars',
  'PT_Dmean_oral_cavity',
  'PT_Dmean_PCM_superior',
  'PT_Dmean_PCM_medius',
  'PT_Dmean_PCM_inferior'
)

relevant_mbs_columns <- c(relevant_val_columns, additionally_relevant_mbs_columns)



# ==========================================================
#        PREPARE VAL DATA       
# ==========================================================

prep_val_df <- function(df){
  RT_suffix <- ''
  prepped_df <- data.frame(matrix(NA, ncol = length(relevant_val_columns), nrow = nrow(df))) 
  colnames(prepped_df) <- relevant_val_columns
  prepped_df["Age"] = lapply(select(df,paste('AGE',RT_suffix,sep='')), function(x) as.numeric(x)) 
  prepped_df["Geslacht"] = lapply(select(df,paste('GESLACHT',RT_suffix,sep='')), function(x) x)  
  prepped_df["LOCTUM_cat"] = lapply(select(df,paste('LOCTUM_cat',RT_suffix,sep='')), function(x) x)  
  prepped_df["TSTAD_DEF"] = lapply(select(df,paste('TSTAD_DEF',RT_suffix,sep='')), function(x) x)   
  prepped_df["Primary_tumor_location_oral_cavity"] = lapply(select(df,'LOCTUM_cat'), function(x) as.logical(x=='Oral cavity'))  
  prepped_df["Primary_tumor_location_larynx"] = lapply(select(df,'LOCTUM_cat'), function(x) as.logical(x=='Larynx'))  
  prepped_df["Primary_tumor_location_pharynx"] = lapply(select(df,'LOCTUM_cat'), function(x) as.logical(x %in% c('Oropharynx','Nasopharynx','Hypopharynx','Neopharynx'))  )  
  prepped_df["Dysphagia_at_baseline_0_1"] = lapply(select(df,'DYSFAGIE_UMCGshortv2_BSL'), function(x) as.logical(x=='Grade 0-1 (regular diet)'))  
  prepped_df["Dysphagia_at_baseline_2"] = lapply(select(df,'DYSFAGIE_UMCGshortv2_BSL'), function(x) as.logical(x=='Grade 2 (soft food)'))  
  prepped_df["Dysphagia_at_baseline_3_5"] = lapply(select(df,'DYSFAGIE_UMCGshortv2_BSL'), function(x) as.logical(x=='Grade 3-5 (liquids only or tube feeding dependent)'))   
  prepped_df["Dysphagia_at_M6_2plus"] = lapply(select(df,'DYSFAGIE_UMCGshortv2_M06'), function(x) x %in% c('Grade 2 (soft food)','Grade 3-5 (liquids only or tube feeding dependent)'))  
  prepped_df["Dysphagia_at_M6_3plus"] = lapply(select(df,'DYSFAGIE_UMCGshortv2_M06'), function(x) as.logical(x=='Grade 3-5 (liquids only or tube feeding dependent)'))  
  prepped_df["Xerostomia_at_baseline_1"] = lapply(select(df,'Q41_BSL'), function(x) as.logical(x=='Helemaal niet'))  
  prepped_df["Xerostomia_at_baseline_2"] = lapply(select(df,'Q41_BSL'), function(x) as.logical(x=='Een beetje'))
  prepped_df["Xerostomia_at_baseline_3_4"] = lapply(select(df,'Q41_BSL'), function(x) as.logical(x %in% c('Nogal','Heel erg')))  

  prepped_df["Xerostomia_at_M6_2plus"] = lapply(select(df,'Q41_M06'), function(x) as.logical(x %in% c('Nogal','Heel erg')))   
  prepped_df["Xerostomia_at_M6_3plus"] = lapply(select(df,'Q41_M06'), function(x) as.logical(x %in% c('Heel erg')))  

  #prepped_df["Xerostomia_at_M6_2plus"] = lapply(select(df,'XER_M06'), function(x) as.logical(x %in% c('Grade 2','Grade 2 (moderate xerostomia, altered diet)','Grade 3','Grade 3 (complete dryness)','Grade 4','Grade 5')))   
  #prepped_df["Xerostomia_at_M6_3plus"] = lapply(select(df,'XER_M06'), function(x) as.logical(x %in% c('Grade 3','Grade 3 (complete dryness)','Grade 4','Grade 5')))  
  
    prepped_df["RT_Dmean_parotid_low"] = lapply(select(df,paste('Parotid_low_Dmean',RT_suffix,sep='')), function(x) x)  
  prepped_df["RT_Dmean_parotid_high"] = lapply(select(df,paste('Parotid_high_Dmean',RT_suffix,sep='')), function(x) x)  

  prepped_df["RT_sqrt_Dmean_ipsilateral_parotid_sqrt_Dmean_contralateral_parotid"] = sqrt(select(prepped_df,"RT_Dmean_parotid_low"))+sqrt(select(prepped_df,"RT_Dmean_parotid_high"))
  prepped_df["RT_Dmean_both_submandibulars"] = lapply(select(df,paste('Submandibulars_Dmean',RT_suffix,sep='')), function(x) x)  
  prepped_df["RT_Dmean_oral_cavity"] = lapply(select(df,paste('OralCavity_Ext_Dmean',RT_suffix,sep='')), function(x) x)  
  prepped_df["RT_Dmean_PCM_superior"] = lapply(select(df,paste('PCM_Sup_Dmean',RT_suffix,sep='')), function(x) x)  
  prepped_df["RT_Dmean_PCM_medius"] = lapply(select(df,paste('PCM_Med_Dmean',RT_suffix,sep='')), function(x) x)  
  prepped_df["RT_Dmean_PCM_inferior"] = lapply(select(df,paste('PCM_Inf_Dmean',RT_suffix,sep='')), function(x) x)  
  
  return (prepped_df)
}

val_data <- list()
for (impset in val_input_df){
  # prepare data and impute any missings
  prepped <- complete(mice(data=prep_val_df(impset), m = 1 ,seed = sample(1:100, 1, replace=FALSE)))
  val_data <- append(list(prepped),val_data)
}


# ==========================================================
#        PREPARE MBS DATA       
# ==========================================================

prep_mbs_df <- function(df){
  RT_suffix <- '_LIPVMAT'
  #RT_suffix = '_IMRT'
  
  PT_suffix <- '_LIPIMPT'

  prepped_df <- data.frame(matrix(NA, ncol = length(relevant_mbs_columns), nrow = nrow(df)))
  colnames(prepped_df) <- relevant_mbs_columns
  prepped_df["Age"] = lapply(select(df,'AGE'), function(x) as.numeric(x))  
  prepped_df["Geslacht"] = lapply(select(df,'GESLACHT'), function(x) x)  
  prepped_df["TSTAD_DEF"] = lapply(select(df,'TSTAD_2cat'), function(x) x)   
  prepped_df["Primary_tumor_location_oral_cavity"] = lapply(select(df,'LOCTUM_cat'), function(x) as.logical(x=='Oral cavity'))  
  prepped_df["Primary_tumor_location_larynx"] = lapply(select(df,'LOCTUM_cat'), function(x) as.logical(x=='Larynx'))  
  prepped_df["Primary_tumor_location_pharynx"] = lapply(select(df,'LOCTUM_cat'), function(x) as.logical(x %in% c('Oropharynx','Nasopharynx','Hypopharynx','Neopharynx'))  )  
  prepped_df["Dysphagia_at_baseline_0_1"] = lapply(select(df,'DYSFAGIE_UMCGshort_BSL'), function(x) as.logical(x=='Grade 0-1 (regular diet)'))  
  prepped_df["Dysphagia_at_baseline_2"] = lapply(select(df,'DYSFAGIE_UMCGshort_BSL'), function(x) as.logical(x=='Grade 2 (soft food)')) 
  prepped_df["Dysphagia_at_baseline_3_5"] = lapply(select(df,'DYSFAGIE_UMCGshort_BSL'), function(x) x %in% c('Grade 3 (liquids only )','Grade 3 (liquids only)','Grade 3-5 (liquids only or tube feeding dependent)'))   

  # ! No M6 info
  #prepped_df["Dysphagia_at_M6_2plus"] = lapply(select(df,'DYSFAGIE_UMCGshortv2_M06'), function(x) x %in% c('Grade 2 (soft food)','Grade 3_5 (liquids only or tube feeding dependent)'))  
  #prepped_df["Dysphagia_at_M6_3plus"] = lapply(select(df,'DYSFAGIE_UMCGshortv2_M06'), function(x) as.logical(x=='Grade 3_5 (liquids only or tube feeding dependent)'))  
  
  prepped_df["Xerostomia_at_baseline_1"] = lapply(select(df,'Q41_BSL'), function(x) as.logical(x=='Helemaal niet'))  
  prepped_df["Xerostomia_at_baseline_2"] = lapply(select(df,'Q41_BSL'), function(x) as.logical(x=='Een beetje'))
  prepped_df["Xerostomia_at_baseline_3_4"] = lapply(select(df,'Q41_BSL'), function(x) as.logical(x %in% c('Nogal','Heel erg')))  

  # ! No M6 info
  #prepped_df["Xerostomia_at_M6_2plus"] = lapply(select(df,'Q41_M06'), function(x) as.logical(x %in% c('Nogal','Heel erg')))   
  #prepped_df["Xerostomia_at_M6_3plus"] = lapply(select(df,'Q41_M06'), function(x) as.logical(x %in% c('Heel erg')))  
  
  prepped_df["RT_Dmean_parotid_low"] = lapply(select(df,paste('Parotid_low_Dmean',RT_suffix,sep='')), function(x) x)  
  prepped_df["RT_Dmean_parotid_high"] = lapply(select(df,paste('Parotid_high_Dmean',RT_suffix,sep='')), function(x) x)  
  
  prepped_df["RT_sqrt_Dmean_ipsilateral_parotid_sqrt_Dmean_contralateral_parotid"] = sqrt(select(prepped_df,"RT_Dmean_parotid_low"))+sqrt(select(prepped_df,"RT_Dmean_parotid_high"))
  prepped_df["RT_Dmean_both_submandibulars"] = lapply(select(df,paste('Submandibulars_Dmean',RT_suffix,sep='')), function(x) x)  
  prepped_df["RT_Dmean_oral_cavity"] = lapply(select(df,paste('OralCavity_Ext_Dmean',RT_suffix,sep='')), function(x) x)  
  prepped_df["RT_Dmean_PCM_superior"] = lapply(select(df,paste('PCM_Sup_Dmean',RT_suffix,sep='')), function(x) x)  
  prepped_df["RT_Dmean_PCM_medius"] = lapply(select(df,paste('PCM_Med_Dmean',RT_suffix,sep='')), function(x) x)  
  prepped_df["RT_Dmean_PCM_inferior"] = lapply(select(df,paste('PCM_Inf_Dmean',RT_suffix,sep='')), function(x) x)  
  
  # Absent in val data
  prepped_df["Preselected_for_plan_comparison"] = lapply(select(df,'Preselectie'), function(x) x=='Positief')  
  prepped_df["Indication_for_PT"] = lapply(select(df,'Planvergelijking'), function(x) x=='Positief')   
  
  prepped_df["PT_Dmean_parotid_low"] = lapply(select(df,paste('Parotid_low_Dmean',PT_suffix,sep='')), function(x) x)  
  prepped_df["PT_Dmean_parotid_high"] = lapply(select(df,paste('Parotid_high_Dmean',PT_suffix,sep='')), function(x) x)  
  
  prepped_df["PT_sqrt_Dmean_ipsilateral_parotid_sqrt_Dmean_contralateral_parotid"] = sqrt(select(prepped_df,"PT_Dmean_parotid_low"))+sqrt(select(prepped_df,"PT_Dmean_parotid_high"))
  prepped_df["PT_Dmean_both_submandibulars"] = lapply(select(df,paste('Submandibulars_Dmean',PT_suffix,sep='')), function(x) x)  
  prepped_df["PT_Dmean_oral_cavity"] = lapply(select(df,paste('OralCavity_Ext_Dmean',PT_suffix,sep='')), function(x) x)   
  prepped_df["PT_Dmean_PCM_superior"] = lapply(select(df,paste('PCM_Sup_Dmean',PT_suffix,sep='')), function(x) x)  
  prepped_df["PT_Dmean_PCM_medius"] = lapply(select(df,paste('PCM_Med_Dmean',PT_suffix,sep='')), function(x) x) 
  prepped_df["PT_Dmean_PCM_inferior"] = lapply(select(df,paste('PCM_Inf_Dmean',PT_suffix,sep='')), function(x) x)   
  return (prepped_df)
}

mbs_data <- list()

for (impset in mbs_input_df){
  # prepare data and impute any missings
  raw_prepped_data = prep_mbs_df(impset)
  imputed_prepped_data <- complete(mice(data=raw_prepped_data, m = 1 ,seed = sample(1:100, 1, replace=FALSE)))
  mbs_data <- append(list(imputed_prepped_data),mbs_data)

}
