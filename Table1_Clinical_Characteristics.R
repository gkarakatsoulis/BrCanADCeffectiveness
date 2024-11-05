library(dplyr)
library(rstatix)
library(survival)
library(survminer)
source('Dataset_Construction.R')

df_clinical$Working = ifelse(df_clinical$`Professional status` %in% c('Part-time',
                                                                      'Fulltime',
                                                                      'Salaried position'),
                             'Yes', 'No')

df_clinical$Working = ifelse(is.na(df_clinical$`Professional status`),
                             NA,
                             df_clinical$Working)

df_clinical$neo_adjuvant_Chemo = ifelse(df_clinical$`Adjuvant Chemotherapy` == 'Yes' |
                                          df_clinical$`Neoadjuvant Chemotherapy` == 'Yes',
                                        'Yes', 'No')

df_clinical$First_line_ADC = apply(df_clinical[, c('Lines_before_tdxd', 'Lines_before_sg')], 1, function(x){
  
  z = ifelse(any(!is.na(x)), max(x, na.rm = T) + 1, NA)
  
  return(z)
  
})

df_clinical$First_line_ADC = ifelse(df_clinical$First_line_ADC >= 3,
                                    '>=3',
                                    df_clinical$First_line_ADC)


df_clinical$Concurrent_diseases = ifelse(
  toupper(df_clinical$`Concurrent diseases`) %in% c('0', '-', 'NO',
                                                    'NONE', 'NO OTHER DISEASES',
                                                    'ΝΟ'),
  'No', 'Yes'
  
)

df_clinical$Concurrent_diseases = ifelse(is.na(df_clinical$`Concurrent diseases`),
                                         NA, df_clinical$Concurrent_diseases)

rfs_cat = c(
  
  'Working',
  'Personal history of other cancers',
  'Family history of breast or ovarian cancer',
  'Family history of other cancers',
  'Concurrent_diseases',
  'If Yes specify (Genetic testing)',
  'Menopausal Status (at first diagosis)',
  'Breast cancer diagnosis',
  'Breast surgery at presentation',
  'Adjuvant Radiotherapy',
  'neo_adjuvant_Chemo',
  'sg_indication',
  'tdxd_indication',
  colnames(df_clinical)[grep('Distant ', colnames(df_clinical))],
  # 'Performance Status at the time of ADC initiation',
  'First_line_ADC'
) 

rfs_numeric = 'Age at diagnosis'


# rfs_cat = df_clinical %>%
#   
#   mutate_at(contains('specify', vars = colnames(df_clinical)), as.character) %>%
#   
#   mutate_at(contains('how many', vars = colnames(df_clinical)), as.character) %>%
#   
#   mutate_at(contains('number of', vars = colnames(df_clinical)), as.character) %>%
#   
#   select_if(is.character) %>%
#   
#   select(-tdxd, -sg) %>%
#   
#   colnames() %>% unlist()
# 
# 
# 
# rfs_numeric = df_clinical %>%
#   
#   select_if(is.numeric) %>%
#   
#   select(-`Repeat Instance`, -PNUMBER) %>%
#   
#   colnames() %>% unlist()

results = list()


tmp_all = df_clinical %>%
  
  select(all_of(rfs_numeric)) %>%
  
  get_summary_stats() %>%
  
  transmute(
    
    Variable = variable,
    Value = 'Median (IQR)',
    Median = median,
    IQR = paste0('(', q1, ', ', q3, ')'),
    ALL = paste(Median, IQR)
    
  ) %>%
  
  select(-Median, -IQR)

tmp_group = df_clinical %>%
  
  select(tdxd, sg, all_of(rfs_numeric)) %>%
  
  gather(., key = 'Treatment', value = 'Received', tdxd, sg) %>%
  
  filter(Received == 'Yes') %>%
  
  select(-Received) %>%
  
  group_by(Treatment) %>%
  
  get_summary_stats() %>%
  
  transmute(
    
    Variable = variable,
    Value = 'Median (IQR)',
    Treatment = Treatment,
    Median = median,
    IQR = paste0('(', q1, ', ', q3, ')'),
    result = paste(Median, IQR)
    
  ) %>%
  
  select(-Median, -IQR) %>%
  
  spread(., key = 'Treatment', value = 'result')

results[['Numeric']] = merge(tmp_all, tmp_group)


tmp_all = df_clinical %>%
  
  gather(., key = 'Variable', value = 'Value', all_of(rfs_cat)) %>%
  
  freq_table(Variable, Value) %>%
  
  mutate(ALL = paste0(n, ' (', prop, '%)')) %>%
  
  select(-n, -prop)

tmp_group = df_clinical %>%
  
  gather(., key = 'Variable', value = 'Value', all_of(rfs_cat)) %>%
  
  gather(., key = 'Treatment', value = 'Received', tdxd, sg) %>%
  
  filter(Received == 'Yes') %>%
  
  freq_table(Treatment, Variable, Value) %>%
  
  mutate(result = paste0(n, ' (', prop, '%)')) %>%
  
  select(-n, -prop) %>%
  
  spread(., key = 'Treatment', value = 'result')

results[['Categorical']] = merge(tmp_all, tmp_group) %>%
  
  mutate(Variable = factor(Variable, levels = rfs_cat)) %>%
  
  arrange(Variable) %>%
  
  mutate(Variable = as.character(Variable),
         Needed = ifelse(Value == 'No' & grepl('Distant', Variable), F, T),
         Variable = gsub('.*\\=', '', Variable),
         Variable = gsub(')', '', Variable)) %>%
  
  filter(Needed) %>%
  
  select(-Needed) %>%
  
  mutate(Variable = ifelse(duplicated(Variable), NA, Variable))

results = data.table::rbindlist(results)


myfile_save = gsub('Rscripts', 'Results/', getwd())
openxlsx::write.xlsx(results, paste0(myfile_save, 'Table1_Clinical_Characteristics_Clean.xlsx'))
