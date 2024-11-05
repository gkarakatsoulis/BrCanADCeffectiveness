library(dplyr)
library(rstatix)
library(survival)
library(survminer)
source('Dataset_Construction.R')

results = list()

results[['AEs_per_ADC']] = df_advanced_therapy %>%
  
  filter(`ADC treatment` != 'No') %>%
  
  select(PNUMBER, `ADC treatment`, contains('AE')) %>%
  
  select(-contains('AE grade'), -`The patient knew about this AE and was sensitized`) %>%
  
  gather(., key = 'AE', value = 'AE_value', -PNUMBER, -`ADC treatment`) %>%
  
  freq_table(`ADC treatment`, AE, AE_value) %>%
  
  arrange(`ADC treatment`, desc(prop)) %>%
  
  filter(AE_value == 'Yes') %>%
  
  mutate(result = paste0(n, ' (', prop, '%)')) %>%
  
  select(-AE_value, -n, -prop) %>%
  
  mutate(AE = gsub('.*\\(', '', AE),
         AE = gsub('.*\\=', '', AE),
         AE = gsub(')', '', AE)) %>%
  
  filter(AE != 'specify') %>%
  
  spread(., key = 'ADC treatment', value = 'result')


grades = df_advanced_therapy %>%
  
  select(contains('grade')) %>%
  
  select(-"Pneumonitis grade",                                                     
         -"Histology grade  (according to AJCC Cancer Staging Manual 8th Edition)") %>%
  
  colnames()

df_advanced_therapy$grade3_4 = apply(df_advanced_therapy[, grades], 1, function(x){
  
  return(any(x > 2, na.rm = T))
  
})

df_advanced_therapy$grade3_4[which(is.na(df_advanced_therapy$`Clinician's reported Adverse Events (AE)`))] = NA

results[['Grades_per_ADC']] = df_advanced_therapy %>%
  
  select(PNUMBER, `ADC treatment`, grade3_4) %>%
  
  filter(`ADC treatment` != 'No') %>%
  
  ungroup() %>%
  
  freq_table(`ADC treatment`, grade3_4) %>%
  
  filter(grade3_4)

results[['Grades_per_AE_per_ADC']] = df_advanced_therapy %>%
  
  filter(`ADC treatment` != 'No') %>%
  
  select(PNUMBER, `ADC treatment`, all_of(grades)) %>%
  
  gather(., key = 'AE', value = 'Grade', -PNUMBER, -`ADC treatment`) %>%
  
  mutate(grade3_4 = ifelse(Grade >= 3, T, F)) %>%
  
  freq_table(`ADC treatment`, AE, grade3_4) %>%
  
  filter(grade3_4) %>%
  
  arrange(`ADC treatment`, desc(prop)) %>%
  
  mutate(result = paste0(n, ' (', prop, '%)')) %>%
  
  select(-grade3_4, -n, -prop) %>%
  
  spread(., key = 'ADC treatment', value = 'result')

results[['Grades']] = df_advanced_therapy %>%
  
  select(PNUMBER, `ADC treatment`, grade3_4) %>%
  
  filter(`ADC treatment` != 'No') %>%
  
  ungroup() %>%
  
  freq_table(grade3_4)




results[['Dose_reduction_AE']] = df_advanced_therapy %>%
  
  group_by(PNUMBER) %>%
  
  summarise(dose_reduction_ADC = any(`Dose modification or delay for ADC treatment (choice=Dose reduction)` == 'Yes')) %>%
  
  freq_table(dose_reduction_ADC)

results[['Dose_reduction_AE_per_Group']] = df_advanced_therapy %>%
  
  group_by(PNUMBER, `ADC treatment`) %>%
  
  summarise(dose_reduction_ADC = any(`Dose modification or delay for ADC treatment (choice=Dose reduction)` == 'Yes')) %>%
  
  filter(`ADC treatment` != 'No') %>%
  
  ungroup() %>%
  
  freq_table(`ADC treatment`, dose_reduction_ADC)

results[['Dose_discontinuation_AE']] = df_advanced_therapy %>%
  
  group_by(PNUMBER) %>%
  
  summarise(dose_discontinuation_ADC = any(`Discontinuation reason` == 'Toxicity (Not fatal / else mark death)')) %>%
  
  freq_table(dose_discontinuation_ADC)

results[['Dose_disc_AE_per_Group']] = df_advanced_therapy %>%
  
  group_by(PNUMBER, `ADC treatment`) %>%
  
  summarise(dose_discontinuation_ADC = any(`Discontinuation reason` == 'Toxicity (Not fatal / else mark death)')) %>%
  
  filter(`ADC treatment` != 'No') %>%
  
  ungroup() %>%
  
  freq_table(`ADC treatment`, dose_discontinuation_ADC)



df_advanced_therapy$age70 = ifelse(df_advanced_therapy$`Age at ADC` >= 70, '70+', '70-')

results[['AEs_per_Age']] = df_advanced_therapy %>%
  
  select(PNUMBER, age70, contains('AE')) %>%
  
  select(-contains('AE grade'), -`The patient knew about this AE and was sensitized`) %>%
  
  gather(., key = 'AE', value = 'AE_value', -PNUMBER, -age70) %>%
  
  freq_table(age70, AE, AE_value) %>%
  
  arrange(age70, desc(prop)) %>%
  
  filter(AE_value == 'Yes') %>%
  
  mutate(result = paste0(n, ' (', prop, '%)')) %>%
  
  select(-AE_value, -n, -prop) %>%
  
  mutate(AE = gsub('.*\\(', '', AE),
         AE = gsub('.*\\=', '', AE),
         AE = gsub(')', '', AE)) %>%
  
  filter(AE != 'specify') %>%
  
  spread(., key = 'age70', value = 'result')

fisher.test(x = df_advanced_therapy$age70, y = df_advanced_therapy$grade3_4)

tmp = df_advanced_therapy %>%
  
  group_by(PNUMBER) %>%
  
  summarise(discontinuation = any(`Treatment status` == 'Discontinuation'),
            dose_reduction_ADC = any(`Dose modification or delay for ADC treatment (choice=Dose reduction)` == 'Yes'),
            age70) %>%
  
  ungroup()


fisher.test(x = tmp$age70, y = tmp$dose_reduction_ADC)
fisher.test(x = tmp$age70, y = tmp$discontinuation)


df_advanced_therapy$age65 = ifelse(df_advanced_therapy$`Age at ADC` >= 65, '65+', '65-')

results[['AEs_per_Age65']] = df_advanced_therapy %>%
  
  select(PNUMBER, age65, contains('AE')) %>%
  
  select(-contains('AE grade'), -`The patient knew about this AE and was sensitized`) %>%
  
  gather(., key = 'AE', value = 'AE_value', -PNUMBER, -age65) %>%
  
  freq_table(age65, AE, AE_value) %>%
  
  arrange(age65, desc(prop)) %>%
  
  filter(AE_value == 'Yes') %>%
  
  mutate(result = paste0(n, ' (', prop, '%)')) %>%
  
  select(-AE_value, -n, -prop) %>%
  
  mutate(AE = gsub('.*\\(', '', AE),
         AE = gsub('.*\\=', '', AE),
         AE = gsub(')', '', AE)) %>%
  
  filter(AE != 'specify') %>%
  
  spread(., key = 'age65', value = 'result')


results[['ILD']] = df_advanced_therapy %>%
  
  select(`ADC treatment`, contains('pneumonitis')) %>%
  
  select(-contains('date')) %>%
  
  gather(., key = 'Variable', value = 'Value', -`ADC treatment`) %>%
  
  freq_table(`ADC treatment`, Variable, Value) %>%
  
  filter(`ADC treatment` != 'No', Value != 'No')

results[['ILD_Diagnosis']] = df_advanced_therapy %>%
  
  mutate(time_from_adc_to_ild = time_length(difftime(
    
    `Date of diagnosis of pneumonitis`,
    `Start date of treatment line`
    
  ), unit = 'day'))


results[['Educated_ILD']] = df_advanced_therapy %>%
  
  freq_table(`ADC treatment`, `The patient knew about this AE and was sensitized`)


# # Derive the dataset with cases with ILD
# pneum = df_advanced_therapy %>%
# 
#   filter(Pneumonitis == 'Yes')
# 
# myfile_save = gsub('Rscripts', 'Results/', getwd())
# openxlsx::write.xlsx(pneum, paste0(myfile_save, 'ILD_Cases.xlsx'))


myfile_save = gsub('Rscripts', 'Results/', getwd())
openxlsx::write.xlsx(results, paste0(myfile_save, 'Table2_Adverse_Events.xlsx'))
