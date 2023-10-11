library(dplyr)
library(stringr)
library(MatchIt)
library(plyr)

################## User choices ###################################

# Disease list --> Choose diseases and their corresponding ICD10 code
disease_list <- list(
  'CD' = c('K50'),
  'UC' = c('K51'),
  'PSO' = c('L40'),
  'SLE' = c('M32'),
  'COPD' = c('J449'),
  'obesity' = c('E66'),
  'T2D' = c('E11'),
  'Atherosclerosis' = c('I70'),
  'RA' = c('M05', 'M06')
  
)

# omics could be either genomics, proteomics of proteomics
omics = 'genomics'

# output folder
output_folder = '...'

# input folder
input_folder = '...'



################## Helping functions ###################################
# extract patients with certain ICD code ####
ukb_icd_subset_by_ICD = function (data, icd.code, icd.version = 10) 
{
  ukb_case <- data %>% dplyr::select(matches(paste("^diagnoses.*icd", 
                                                   icd.version, sep = ""))) %>% 
    purrr::map_df(~grepl(icd.code, ., perl = TRUE)) %>% rowSums() > 0
  data_subset <- data[ukb_case,]
  return(data_subset)
}



################## Read data ###################################
## Choose either genomics, proteomics and metabolomics

if(omics == 'genomics'){
  omics_path = paste(input_folder, '/EUROS_PRS_9_disease_send.csv', sep = '')
  output_file = 'all'
}

if(omics == 'proteomics'){
  omics_path = paste(input_folder, '/Olink_proteomics_data_transposed_decoded2UNIportID_stringvalue_corrected_Martin.txt', sep = '')
  output_file = 'prot' #is used in saving results
}

if(omics == 'metabolomics'){
  omics_path = paste(input_folder, '/nmr_biomarker_data_RemovedTechVariation_secondPHASE_FilteredDuplicateV2.csv', sep = '')
  output_file = 'met'  #is used in saving results
}


if(output_file == 'all'){
  PID_omics = read.table(file = omics_path, sep=';', header = TRUE, row.names = 'eid')
} else {
  PID_omics = read.table(file = omics_path, sep=',', header = TRUE, row.names = 'eid')
}
PID_omics = rownames(PID_omics)

#load all UKBB patients
load(paste(input_folder, '/ukb672643.rda', sep = ''))

#clinical with columns only relevant for finding healthy controls
input_file <- paste(input_folder, '/Decoded_ukb_Matrix_Firoj_ControlHealthyICD9and10codes_only.csv', sep = '')
data <- read.csv(input_file, header = TRUE, stringsAsFactors = FALSE)

# A list of patients that withdrawn from the UKBB
withdrawn_patients = read.table(paste(input_folder, '/withdrawn_patients.csv', sep = ''))
my_ukb_data = my_ukb_data[!my_ukb_data$eid %in% withdrawn_patients$V1,]
data = data[!data$eid %in% withdrawn_patients$V1,]
PID_omics = PID_omics[!PID_omics %in% withdrawn_patients$V1]



################## Preprocess data ###################################
#subset to only those with omics
my_ukb_data<- my_ukb_data[my_ukb_data$eid %in% PID_omics, ]

#find healthy controls 
rows_with_all_na <- data[rowSums(is.na(data[, -1])) == (ncol(data) - 1), ]
Prot_healthy_controls = rows_with_all_na[rows_with_all_na$eid %in% PID_omics,]
ukb_healthy_controls = my_ukb_data[my_ukb_data$eid %in% Prot_healthy_controls$eid,]



################## Choose all patients with the diagnosis ###################################



icd.version = '10'

# Repeat for all diseases
for(j in 1:length(disease_list)){
  
  disease = names(disease_list[j])
  icd.code.list = disease_list[[j]]

  #Pick patients with corresponding ICD10 code
  ukb_disease_subset_list = list()
  ukb_case_list = list()
  for(icd.code in icd.code.list){
      
    # all patients with a given icd.code
    patients_with_disease = ukb_icd_subset_by_ICD(my_ukb_data, icd.version = icd.version, icd.code = icd.code)

    # the below is to remember which columns corresponded to the disease and is later used for creating incident group
    ukb_case_subset <- patients_with_disease %>% dplyr::select(matches(paste("^diagnoses.*icd",
                                                                               icd.version, sep = ""))) %>%
      purrr::map_df(~grepl(icd.code, ., perl = TRUE))
    
    ukb_case_subset = cbind(eid = patients_with_disease$eid,ukb_case_subset)
  
    ukb_case_list = append(ukb_case_list, list(ukb_case_subset))
  
  }
  
  ukb_case = bind_rows(ukb_case_list)
  
  # There might be duplicates in ukb_case (if for example patient has both M05 and M06 in RA)
  # The below code takes only unique values and for each diagnosis returns TRUE if it belongs to either M05 or M06
  coln = colnames(ukb_case)
  ukb_case = ddply(ukb_case, .(eid), function(x) return(as.logical(colSums(x[,2:dim(ukb_case)[2]]))))
  colnames(ukb_case) = coln
  rownames(ukb_case) = ukb_case$eid
  ukb_case = ukb_case[,-1]
  
  ukb_disease_subset = my_ukb_data[my_ukb_data$eid %in% rownames(ukb_case),]
    
  ################## Divide patients between prevalent and incident  ###################################
  
  
  # for each patient, we find all disease related columns, then we map them into corresponding column names for diagnosis time,
  # then remove all columns with secondary disease and select the earliest diagnosis time
  earliest_diagnosis = c()
  for(i in 1:dim(ukb_disease_subset)[1]){
    diagnoses = colnames(ukb_case)[unlist(ukb_case[i,])]
    diagnoses = paste('date_of_first_inpatient_', gsub("diagnoses", "diagnosis", diagnoses), sep = '')
    diagnoses = gsub("41270", "41280", diagnoses)
    diagnoses = gsub("41202", "41262", diagnoses)
    diagnoses_times = ukb_disease_subset[i,diagnoses[!str_detect(diagnoses, 'secondary')]]
    if(length(diagnoses_times) == 1){
      earliest_diagnosis = c(earliest_diagnosis, as.character(diagnoses_times))
    }
    else{
      earliest_diagnosis = c(earliest_diagnosis, min(as.vector(t(diagnoses_times))))
    }
  }
  
  # Compare sampling time and diagnosis time and divide the patients
  days_diagnosis_after_sample = as.Date(earliest_diagnosis) - ukb_disease_subset[,'date_of_attending_assessment_centre_f53_0_0'] 
  ukb_disease_prevalent = ukb_disease_subset[days_diagnosis_after_sample < 0,]
  ukb_disease_incident = ukb_disease_subset[days_diagnosis_after_sample > 0,]
  
  
  ################## Prepare files for Matching based on age and sex  ###################################
  
  ukb_disease_incident = ukb_disease_incident[,c('eid', 'age_at_recruitment_f21022_0_0', 'sex_f31_0_0')]
  ukb_disease_prevalent = ukb_disease_prevalent[,c('eid', 'age_at_recruitment_f21022_0_0', 'sex_f31_0_0')]
  ukb_healthy_controls = ukb_healthy_controls[,c('eid', 'age_at_recruitment_f21022_0_0', 'sex_f31_0_0')]
  ukb_disease_prevalent['group'] = rep('Prevalent', dim(ukb_disease_prevalent)[1])
  ukb_disease_incident['group'] = rep('Incident', dim(ukb_disease_incident)[1])
  ukb_healthy_controls['group'] = rep('HC', dim(ukb_healthy_controls)[1])
  ukb_healthy_controls = na.omit(ukb_healthy_controls) #necessary for Genomics
  
  
  ################## Match healthy to prevalent  ###################################
  
  #using MatchIt function that pairs prevalent with healthy based on age and sex
  
  summary = rbind(ukb_disease_prevalent, ukb_healthy_controls)
  
  a = matchit(group ~ age_at_recruitment_f21022_0_0, data = summary, method = "nearest", exact = ~as.factor(sex_f31_0_0), 
              distance = "euclidean",ratio = 1)
  
  # paired controls
  ukb_healthy_controls_PairedToPrevalent = ukb_healthy_controls[rownames(ukb_healthy_controls) %in% as.vector(a$match.matrix),]
  
  ################## Match healthy to incident  ###################################
  
  
  summary = rbind(ukb_disease_incident, ukb_healthy_controls)
  
  a = matchit(group ~ age_at_recruitment_f21022_0_0, data = summary, method = "nearest", exact = ~as.factor(sex_f31_0_0), 
              distance = "euclidean",ratio = 1)

  # paired controls
  ukb_disease_ToBe_sick_PairedToIncident = ukb_healthy_controls[rownames(ukb_healthy_controls) %in% as.vector(a$match.matrix),]


  ################## Output files  ###################################
  
  file_path= paste(output_folder, "/ukb_", disease, "_", output_file, "_prevalent.csv", sep = '')
  write.csv(ukb_disease_prevalent, file = file_path, row.names = FALSE)
   
  file_path= paste(utput_folder, "/ukb_", disease, "_", output_file, "_incident.csv", sep = '')
  write.csv(ukb_disease_incident, file = file_path, row.names = FALSE)
   
  file_path= paste(utput_folder, "/ukb_", disease, "_", output_file, "_HC_PairedToIncident.csv", sep = '')
  write.csv(ukb_disease_ToBe_sick_PairedToIncident, file = file_path, row.names = FALSE)
  
  file_path= paste(utput_folder, "/ukb_", disease, "_", output_file, "_HC_PairedToPrevalent.csv", sep = '')
  write.csv(ukb_healthy_controls_PairedToPrevalent, file = file_path, row.names = FALSE)

}


