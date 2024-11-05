library(dplyr)
library(rstatix)
library(survival)
library(survminer)
source('Dataset_Construction.R')


rfs_cat = df_clinical %>%
  
  mutate_at(contains('specify', vars = colnames(df_clinical)), as.character) %>%
  
  mutate_at(contains('how many', vars = colnames(df_clinical)), as.character) %>%
  
  mutate_at(contains('number of', vars = colnames(df_clinical)), as.character) %>%
  
  select_if(is.character) %>%
  
  colnames() %>% unlist()
  
  

rfs_numeric = df_clinical %>%
  
  select_if(is.numeric) %>%
  
  select(-`Repeat Instance`, -PNUMBER) %>%
  
  colnames() %>% unlist()

results = list()

results[['Numeric']] = df_clinical %>%
  
  select(all_of(rfs_numeric)) %>%
  
  get_summary_stats() %>%
  
  transmute(
    
    variable = variable,
    `N characterized` = n,
    Median = median,
    IQR = paste0('(', q1, ', ', q3, ')')
    
  )

tmp = df_clinical %>%
  
  gather(., key = 'Variable', value = 'Value', all_of(rfs_cat)) %>%
  
  freq_table(Variable, Value) %>%
  
  group_by(Variable) %>%
  
  mutate(`N characterized` = sum(n)) %>%
  
  ungroup() %>%
  
  rename('N' = 'n', '%' = 'prop')

tmp$`N characterized`[which(duplicated(tmp$Variable))] = NA

results[['Categorical']] = tmp

results[['ADC_per_line']] = df_advanced_therapy %>%
  
  filter(`ADC treatment` %in% c('Trastuzumab deruxtecan', 'Sacituzumab govitecan')) %>%
  
  mutate(`Line therapy` = ifelse(`Line therapy` > 2, 'Subsequent', as.character(`Line therapy`))) %>%
  
  select(PNUMBER, `Line therapy`, `ADC treatment`) %>%
  
  mutate(ID = paste(PNUMBER, `Line therapy`, `ADC treatment`)) %>%
  
  filter(!duplicated(ID)) %>%
  
  freq_table(`ADC treatment`, `Line therapy`)



results[['AEs_per_ADC']] = df_advanced_therapy %>%
  
  filter(`ADC treatment` != 'No') %>%
  
  select(PNUMBER, `ADC treatment`, contains('AE')) %>%
  
  select(-contains('AE grade'), -`The patient knew about this AE and was sensitized`) %>%
  
  gather(., key = 'AE', value = 'AE_value', -PNUMBER, -`ADC treatment`) %>%
  
  freq_table(`ADC treatment`, AE, AE_value) %>%
  
  arrange(`ADC treatment`, desc(prop))


grades = df_advanced_therapy %>%
  
  select(contains('grade')) %>%
  
  select(-"Pneumonitis grade",                                                     
         -"Histology grade  (according to AJCC Cancer Staging Manual 8th Edition)") %>%
  
  colnames()

df_advanced_therapy$grade3_4 = apply(df_advanced_therapy[, grades], 1, function(x){
  
  return(any(x > 2, na.rm = T))
  
})

df_advanced_therapy$grade3_4[which(is.na(df_advanced_therapy$`Clinician's reported Adverse Events (AE)`))] = NA

results[['Grades']] = df_advanced_therapy %>%
  
  select(PNUMBER, `ADC treatment`, grade3_4) %>%
  
  filter(`ADC treatment` != 'No') %>%
  
  ungroup() %>%
  
  freq_table(`ADC treatment`, grade3_4)
  
  



results[['Dose_reduction_AE']] = df_advanced_therapy %>%
  
  group_by(PNUMBER) %>%
  
  summarise(dose_reduction_ADC = any(`Dose modification or delay for ADC treatment (choice=Dose reduction)` == 'Yes')) %>%
  
  freq_table(dose_reduction_ADC)


df_advanced_therapy$follow_up_event = 1 - df_advanced_therapy$death_after_adc

km = survfit(Surv(time = df_advanced_therapy$time_from_adc_to_death, event = df_advanced_therapy$follow_up_event) ~ 1)

results[['Time_to_FUP']] = surv_median(km)

results[['Age at ADC']] = df %>%
  
  filter(`ADC treatment` != 'No') %>%
  
  mutate(ID = paste(`ADC treatment`, PNUMBER)) %>%
  
  filter(!duplicated(ID)) %>%
  
  mutate(Age_70 = `Age at ADC` > 70) %>%
  
  freq_table(`ADC treatment`, Age_70)

myfile_save = gsub('Rscripts', 'Results/', getwd())
openxlsx::write.xlsx(results, paste0(myfile_save, 'Descriptives.xlsx'))
