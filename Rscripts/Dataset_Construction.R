library(dplyr)
library(rstatix)
library(lubridate)

myfile_wd = gsub('Rscripts', '', getwd())

df = openxlsx::read.xlsx(paste0(myfile_wd, 'HE11ADC23_14102024.xlsx'),
                         check.names = F,
                         na.strings = c('', 'Missing Data', 'MD', 'Not evaluated'))

# Check the duplicated colnames!
df_colnames = openxlsx::read.xlsx(paste0(myfile_wd, 'Dataset_correction_columns.xlsx'))

df_colnames = df_colnames %>% filter(Column != 'Data Access Group')

any(gsub(' ', '.', df_colnames$Column) != colnames(df)) # All columns are the same

colnames(df) = df_colnames$Corrected_Column

colnames(df) = gsub('\\,', '', colnames(df))

# # Check for duplicated line of therapies
# tmp = df %>%
# 
#   mutate(ID = paste(`Record ID`, `Line therapy`)) %>%
# 
#   filter(!is.na(`Line therapy`), duplicated(ID)) %>%
# 
#   select(`Record ID`, `Line therapy`)
# 
# myfile_save_problems = gsub('Rscripts', 'Results/Problems/', getwd())
# writexl::write_xlsx(tmp, paste0(myfile_save_problems, 'Duplicated_Line_Therapies.xlsx'))

# Fix the duplicated line of therapies
df$`Line therapy`[which(df$`Record ID` %in% c('539-2', '542-1', '550-13', '550-30'))] = 
  
  df$`Repeat Instance`[which(df$`Record ID` %in% c('539-2', '542-1', '550-13', '550-30'))] 


# Check if any non-missing before Line therapy column
tmp = df %>%
  
  filter(is.na(PNUMBER))

apply(tmp[, 4:(grep('Line therapy', colnames(tmp))-1)], 1, function(x){
  return(any(!is.na(x)))
  }) |> table()


# Define the cols for the ADC; Will need it later!
advanced_therapy_cols = colnames(df)[c(
  
  grep('PNUMBER', colnames(df)),
  grep('Line therapy', colnames(df)):(grep('Lost to follow', colnames(df))-1)
  
)]

## Pass all the PNUMBERs to the cases
ids = df %>% select(`Record ID`, PNUMBER) %>% filter(!is.na(PNUMBER))

df = df %>% select(-PNUMBER)

df = merge(df, ids)

# Change the check/uncheck to yes/no
cols_check = which(apply(df, 2, function(x){return(any(x %in% c('Checked', 'Unchecked')))}))

df[, cols_check] = sapply(df[,cols_check], function(x){
  
  x = ifelse(x == 'Checked', 'Yes', 'No')

  return(x)
  
})

# Fix the histological classification
# df[, grep('Histological classification', colnames(df))] = sapply(df[,grep('Histological classification', colnames(df))], function(x){
#   
#   if (any(x == 'Yes', na.rm = T)){
#     
#     x = ifelse(x %in% 'Yes', 'Yes', 'No')
#     
#   }
#   return(x)
# })

# # Check if histological classification appears twice in any case
# tmp = df %>%
#   
#   select(`Record ID`, contains('Histological classification')) %>%
#   
#   gather(., key = 'Variable', value = 'Value', -`Record ID`) %>%
#   
#   filter(Value == 'Yes') %>%
#   
#   # filter(duplicated(`Record ID`)) %>%
#   
#   mutate(Variable = gsub('.*=', '', Variable),
#          Variable = gsub(')', '', Variable)) %>%
#   
#   group_by(`Record ID`) %>%
#   
#   summarise(Checked = paste(Variable, collapse = ', ')) %>%
#   
#   filter(grepl(',', Checked))
# 
# 
# myfile_save_problems = gsub('Rscripts', 'Results/Problems/', getwd())
# writexl::write_xlsx(tmp, paste0(myfile_save_problems, 'Duplicated_Histological_class.xlsx'))

# # IDs for missing HER2+,HER2low, TNBC, HR positive
# # To send to Elena
# mydata = df %>%
# 
#   group_by(PNUMBER) %>%
# 
#   mutate(indication_tdxd = any(!is.na(`In case of trastuzumab deruxtecan treatment indication based on the most recent biopsy`)),
#          indication_sg = any(!is.na(`In case of sacituzumab govitecan treatment indication based on the most recent biopsy`))) %>%
# 
#   ungroup() %>%
# 
#   filter(!indication_sg, !indication_tdxd) %>%
# 
#   select(PNUMBER,
#          `Record ID`,
#          `ADC treatment`,
#          `In case of sacituzumab govitecan treatment indication based on the most recent biopsy`,
#          `In case of trastuzumab deruxtecan treatment indication based on the most recent biopsy`,
#          `New biopsy`,
#          `ADDITIONAL  BIOPSY`,
#          `Estrogen Receptor status (tumor characteristics)`,
#          `Estrogen Receptor status (advanced therapy)`,
#          `Progesterone Receptor status (tumor characteristics)`,
#          `Progesterone Receptor status (advanced therapy)`,
#          `HER2 FISH (tumor characteristics)`,
#          `HER2 FISH (advanced therapy)`,
#          `HER2 IHC (tumor characteristics)`,
#          `HER2 IHC (advanced therapy)`,
#          `HER2 IHC define (tumor characteristics)`,
#          `HER2 IHC define (advanced therapy)`)
# 
# mydata2 = mydata
# 
# ## Update the advanced therapy (new biopsy) from tumor characteristics (first biopsy)
# mydata2$`Estrogen Receptor status (advanced therapy)`[which(is.na(mydata2$`Estrogen Receptor status (advanced therapy)`))] =
# 
#   mydata2$`Estrogen Receptor status (tumor characteristics)`[which(is.na(mydata2$`Estrogen Receptor status (advanced therapy)`))]
# 
# mydata2$`Progesterone Receptor status (advanced therapy)`[which(is.na(mydata2$`Progesterone Receptor status (advanced therapy)`))] =
# 
#   mydata2$`Progesterone Receptor status (tumor characteristics)`[which(is.na(mydata2$`Progesterone Receptor status (advanced therapy)`))]
# 
# mydata2$`HER2 FISH (advanced therapy)`[which(is.na(mydata2$`HER2 FISH (advanced therapy)`))] =
# 
#   mydata2$`HER2 FISH (tumor characteristics)`[which(is.na(mydata2$`HER2 FISH (advanced therapy)`))]
# 
# mydata2$`HER2 IHC (advanced therapy)`[which(is.na(mydata2$`HER2 IHC (advanced therapy)`))] =
# 
#   mydata2$`HER2 IHC (tumor characteristics)`[which(is.na(mydata2$`HER2 IHC (advanced therapy)`))]
# 
# mydata2$`HER2 IHC define (advanced therapy)`[which(is.na(mydata2$`HER2 IHC define (advanced therapy)`))] =
# 
#   mydata2$`HER2 IHC define (tumor characteristics)`[which(is.na(mydata2$`HER2 IHC define (advanced therapy)`))]
# 
# mydata2 = mydata2 %>% select(-contains('tumor characteristics'))
# 
# mydata2 = mydata2 %>%
# 
#   group_by(PNUMBER, `Record ID`) %>%
# 
#   summarise(Treatment = `ADC treatment` |> unique() |> paste(collapse = ', '),
#             Indication_sg = `In case of sacituzumab govitecan treatment indication based on the most recent biopsy` |> unique() |> paste(collapse = ', '),
#             Indication_tdxd = `In case of trastuzumab deruxtecan treatment indication based on the most recent biopsy` |> unique() |> paste(collapse = ', '),
#             ER = `Estrogen Receptor status (advanced therapy)` |> unique() |> paste(collapse = ', '),
#             PR = `Progesterone Receptor status (advanced therapy)` |> unique() |> paste(collapse = ', '),
#             HER2_FISH = `HER2 FISH (advanced therapy)` |> unique() |> paste(collapse = ', '),
#             HER2_IHC = `HER2 IHC (advanced therapy)` |> unique() |> paste(collapse = ', '),
#             HER2_IHC_define = `HER2 IHC define (advanced therapy)` |> unique() |> paste(collapse = ', '))
# 
# tmp = list(mydata, mydata2)
# 
# myfile_save_problems = gsub('Rscripts', 'Results/Problems', getwd())
# writexl::write_xlsx(tmp, paste0(myfile_save_problems, '/IDs_Missing_Groups.xlsx'))


# Fix the HER2 define variable
any(!is.na(df$`HER2 IHC define (advanced therapy)`) & 
      !is.na(df$`HER2 IHC define (tumor characteristics)`)) # either in tumor or in advanced

df$`HER2 IHC define (advanced therapy)` = ifelse(is.na(df$`HER2 IHC define (advanced therapy)`),
                                                 df$`HER2 IHC define (tumor characteristics)`,
                                                 df$`HER2 IHC define (advanced therapy)`)

# Fix the Line therapy
df$`Line therapy` = gsub("\\D", "", df$`Line therapy`) |> as.numeric()

tmp = df %>%
  
  select(PNUMBER,`Line therapy`, `HER2 IHC define (advanced therapy)`) %>%
  
  mutate(id = paste(PNUMBER, `Line therapy`)) %>%
  
  group_by(id) %>%
  
  mutate(result = max(`HER2 IHC define (advanced therapy)`, na.rm = T)) %>%
  
  mutate(result = ifelse(result == '-Inf', NA, as.numeric(result))) %>%
  
  ungroup() %>%
  
  filter(!duplicated(id)) %>%
  
  select(-`HER2 IHC define (advanced therapy)`, -id) %>%
  
  spread(., key = 'Line therapy', value = 'result')
  
tmp$Max_HER = apply(tmp[,-1], 1, max, na.rm = T)
tmp$Max_HER[which(tmp$Max_HER == '-Inf')] = NA
tmp$Min_HER = apply(tmp[,-1], 1, min, na.rm = T)
tmp$Min_HER[which(tmp$Min_HER == 'Inf')] = NA

tmp = tmp %>% select(PNUMBER, Min_HER, Max_HER)

df = merge(df, tmp, all.x = T)

# Check dates of CT scan (They have multiple measurements!!)

ct_scans = df %>%
  
  select(`Record ID`, `ADC treatment`, `Line therapy`, `Dates of CT scans during this treatment`) %>%
  
  filter(`ADC treatment` != 'No',
         !is.na(`Dates of CT scans during this treatment`),
         !`Dates of CT scans during this treatment` %in% c('Not Applicable', 'NA', '0', 'Not applicable yet', 'not done', 'no')) %>%
  
  mutate(date_multiple = `Dates of CT scans during this treatment` |> as.numeric() |> is.na()) %>%
  
  filter(date_multiple, grepl(' ', `Dates of CT scans during this treatment`))

# Check dates of last period. They have 0000-01-01

dates = df %>%
  
  select(contains('Date'))

dates = lapply(dates, function(x){
  
  x = as.Date(as.numeric(x), origin = "1899-12-30")
  
  return(x)
  
}) %>%
  
  as.data.frame()

colnames(dates) = gsub('\\.\\.', ' ', colnames(dates)) 
colnames(dates) = gsub('\\.', ' ', colnames(dates))


 
df = df %>% select(-contains('Date'))


df = cbind(df, dates)

# Pass the year of birth and correct age at diagnosis to the dataset
myfile_wd = gsub('Rscripts', '', getwd())

df_age = openxlsx::read.xlsx(paste0(myfile_wd, 'HE11ADC23_05032024_AGE_KARAK_07032024_Updated.xlsx'),
                             check.names = F)

colnames(df_age) = gsub('\\.', ' ', colnames(df_age))

df$`Age at diagnosis` = NULL

df = merge(df, df_age, all.x = T)

# Calculate age at treatment
df$`Year of ADC` = format(df$`Start date of treatment line`, format = '%Y') %>% as.numeric() 
df$`Age at ADC` = df$`Year of ADC` - df$BirthYear_REDCap

# Update the advanced therapy cols
advanced_therapy_cols = c(advanced_therapy_cols, 'Year of ADC', 'Age at ADC', 'BirthYear_REDCap')

# Keep only the clinical (non-duplicated)
df_clinical = df %>% select(-all_of(advanced_therapy_cols[-1])) %>% filter(is.na(`Repeat Instrument`))

## Fix the times to progress and to last contact

myfile_wd_times = gsub('Rscripts', '', getwd())

df_times = readxl::read_xlsx(paste0(myfile_wd_times, 'Dataset_Corrections_Queries_1_5.xlsx'))

colnames(df_times)[-c(1:2)] = paste0(colnames(df_times)[-c(1:2)], '_corrected')

df_times$`Patient dead_corrected` = ifelse(df_times$`Patient dead_corrected` == 'Alive',
                                           'No', df_times$`Patient dead_corrected`)

df_times = df_times %>%
  
  mutate_all(as.character)

df_clinical = merge(df_clinical, df_times, all.x = T)


df_clinical$`Date of last contact` = ifelse(is.na(df_clinical$`Date of last contact_corrected`),
                                            as.character(df_clinical$`Date of last contact`),
                                            df_clinical$`Date of last contact_corrected`)

df_clinical$`Date of last contact` = as.Date(df_clinical$`Date of last contact`)

df_clinical$`Patient dead` = ifelse(is.na(df_clinical$`Patient dead_corrected`),
                                            df_clinical$`Patient dead`,
                                            df_clinical$`Patient dead_corrected`)

# Pass the clinical to the duplicated cases
df_advanced_therapy = df %>% filter(!is.na(`Repeat Instrument`)) %>%
  
  select(all_of(advanced_therapy_cols))

df_advanced_therapy = merge(df_advanced_therapy, df_clinical, all.x = T)

df_advanced_therapy$`If yes date of progression` = ifelse(is.na(df_advanced_therapy$`If yes, date of progression_corrected`),
                                                          df_advanced_therapy$`If yes date of progression` |> as.character(),
                                                          df_advanced_therapy$`If yes, date of progression_corrected`) |>
  
  as.Date()

df_advanced_therapy = df_advanced_therapy %>% select(-contains('_corrected'))

# Time to follow_up
df_advanced_therapy$`Date of last contact` = apply(
  
  df_advanced_therapy[,c('Date of last contact', 'Date of death')],
  1,
  max, na.rm = T)

df_advanced_therapy$time_from_adc_to_fup = time_length(difftime(
  
  df_advanced_therapy$`Date of last contact`,
  df_advanced_therapy$`Start date of treatment line`
  
), unit = 'month')

df_advanced_therapy$time_from_adc_to_fup[which(df_advanced_therapy$`ADC treatment` == 'No')] = NA

# # Cases with non-positive time to fup
# tmp = df_advanced_therapy %>% filter(time_from_adc_to_fup <= 0) %>%
# 
#   select(PNUMBER, `Record ID`)
# 
# myfile_save_problems = gsub('Rscripts', 'Results/Problems/', getwd())
# writexl::write_xlsx(tmp, paste0(myfile_save_problems, 'Non-positive_fup_after_adc.xlsx'))

df_advanced_therapy$time_from_adc_to_fup = ifelse(df_advanced_therapy$time_from_adc_to_fup <= 0,
                                                  NA,
                                                  df_advanced_therapy$time_from_adc_to_fup)

# Time from ADC to Progression
df_advanced_therapy$progress_after_adc = ifelse(
  
  df_advanced_therapy$`Progression on ADC treatment` == 'Yes' |
    
    df_advanced_therapy$`Patient dead` == 'Yes', 1, 0)

df_advanced_therapy$progression_date = apply(
  
  df_advanced_therapy[, c('If yes date of progression', 'Date of death')],
  1, min, na.rm = T
  
)

df_advanced_therapy$time_from_adc_to_prog = time_length(difftime(
  
  df_advanced_therapy$progression_date,
  df_advanced_therapy$`Start date of treatment line`
  
), unit = 'month')

df_advanced_therapy$time_from_adc_to_prog[which(df_advanced_therapy$progress_after_adc == 0)] = time_length(difftime(
  
  df_advanced_therapy$`Date of last contact`,
  df_advanced_therapy$`Start date of treatment line`
  
), unit = 'month')[which(df_advanced_therapy$progress_after_adc == 0)]


df_advanced_therapy$time_from_adc_to_prog[which(df_advanced_therapy$`ADC treatment` == 'No')] = NA

# # Cases with non-positive time to progression
# tmp = df_advanced_therapy %>% filter(time_from_adc_to_prog <= 0, progress_after_adc == 1) %>%
# 
#   select(PNUMBER, `Record ID`)
# 
# myfile_save_problems = gsub('Rscripts', 'Results/Problems/', getwd())
# writexl::write_xlsx(tmp, paste0(myfile_save_problems, 'Non-positive_prog_after_adc.xlsx'))

df_advanced_therapy$time_from_adc_to_prog = ifelse(df_advanced_therapy$time_from_adc_to_prog <= 0,
                                                   NA,
                                                   df_advanced_therapy$time_from_adc_to_prog)

# Time from ADC to Death
df_advanced_therapy$death_after_adc = ifelse(df_advanced_therapy$`Patient dead` == 'Yes', 1, 0)

df_advanced_therapy$time_from_adc_to_death = time_length(difftime(
  
  df_advanced_therapy$`Date of death`,
  df_advanced_therapy$`Start date of treatment line`
  
), unit = 'month')

df_advanced_therapy$time_from_adc_to_death[which(df_advanced_therapy$death_after_adc == 0)] = time_length(difftime(
  
  df_advanced_therapy$`Date of last contact`,
  df_advanced_therapy$`Start date of treatment line`
  
), unit = 'month')[which(df_advanced_therapy$death_after_adc == 0)]

df_advanced_therapy$time_from_adc_to_death[which(df_advanced_therapy$`ADC treatment` == 'No')] = NA

# # Cases with non-positive time to death
# tmp = df_advanced_therapy %>% filter(time_from_adc_to_death <= 0) %>%
# 
#   select(PNUMBER, `Record ID`)
# 
# myfile_save_problems = gsub('Rscripts', 'Results/Problems/', getwd())
# writexl::write_xlsx(tmp, paste0(myfile_save_problems, 'Non-positive_death_after_adc.xlsx'))

df_advanced_therapy$time_from_adc_to_death[which(df_advanced_therapy$time_from_adc_to_death <= 0)] = NA

# Adverse events: Fix the data
# ## Fatigue:
# fatigue = apply(df_advanced_therapy %>% select(contains('Specify the Other')), 1, function(x){
#   
#   grepl('FATIGUE', toupper(x)) |> any()
#   
# }) |> which()
# 
# df_advanced_therapy$`Clinician's reported AEs (choice=Fatigue)`[fatigue] = 'Yes'
# ## Abdominal
# abdominal = apply(df_advanced_therapy %>% select(contains('Specify the Other')), 1, function(x){
#   
#   grepl('ABDOMINAL', toupper(x)) |> any()
#   
# }) |> which()
# 
# df_advanced_therapy$`Clinician's reported AEs (choice=Abdominal Pain)`[abdominal] = 'Yes'
# ## Headache
# headache = apply(df_advanced_therapy %>% select(contains('Specify the Other')), 1, function(x){
#   
#   grepl('HEADACHE', toupper(x)) |> any()
#   
# }) |> which()
# 
# df_advanced_therapy$`Clinician's reported AEs (choice=Headache)`[headache] = 'Yes'

# # Cases with AE missmatches
# tmp = df_advanced_therapy[c(abdominal, headache, fatigue), ] %>%
#   
#   select(`Record ID`) %>%
#   
#   mutate(Differences = c(rep('Abdominal', length(abdominal)),
#                          rep('Headache', length(headache)),
#                          rep('Fatigue', length(fatigue))))
# 
# myfile_save_problems = gsub('Rscripts', 'Results/Problems/', getwd())
# writexl::write_xlsx(tmp, paste0(myfile_save_problems, 'AE_differences.xlsx'))



# Check if all have received ADC at least once
df_advanced_therapy %>%
  
  group_by(PNUMBER) %>%
  
  summarise(
    
    received_ADC = ifelse(
      
      any(`ADC treatment` %in% c('Trastuzumab deruxtecan', 'Sacituzumab govitecan')),
          'Yes', 'No'
      
      )
    
    ) %>% freq_table(received_ADC)
  

adc_specific = df_advanced_therapy %>%
  
  group_by(PNUMBER) %>%
  
  summarise(tdxd = ifelse(
    
    any(`ADC treatment` %in% c('Trastuzumab deruxtecan')),
    'Yes', 'No'
    
    ),
    
    sg = ifelse(
      
      any(`ADC treatment` %in% c('Sacituzumab govitecan')),
      'Yes', 'No'
      
    ),
    
    both_tdxd_sg = ifelse(tdxd == 'Yes' & sg == 'Yes', 'Yes', 'No')
  )

df_clinical = merge(df_clinical, adc_specific, all.x = T)

df_advanced_therapy$Previous_lines_before_ADC = df_advanced_therapy$`Line therapy` - 1
df_advanced_therapy$Previous_lines_before_ADC[which(df_advanced_therapy$`ADC treatment` == 'No')] = NA

prev_lines = df_advanced_therapy %>%
  
  filter(`ADC treatment` != 'No') %>%
  
  filter(!duplicated(paste(PNUMBER, `ADC treatment`))) %>%
  
  select(PNUMBER, `ADC treatment`, Previous_lines_before_ADC) %>%
  
  spread(., key = 'ADC treatment', value = 'Previous_lines_before_ADC')

colnames(prev_lines)[-1] = c('Lines_before_sg', 'Lines_before_tdxd')

df_clinical = merge(df_clinical, prev_lines, all.x = T)


# Fix the Indication
df_indication = df %>%
  
  group_by(`Record ID`) %>%
  
  summarise(sg_indication = `In case of sacituzumab govitecan treatment indication based on the most recent biopsy` |> unique() |> paste(collapse = '++'),
            tdxd_indication = `In case of trastuzumab deruxtecan treatment indication based on the most recent biopsy` |> unique() |> paste(collapse = '++')) %>%
  
  mutate(tdxd_indication = gsub('NA\\+\\+', '', tdxd_indication),
         tdxd_indication = gsub('\\+\\+NA', '', tdxd_indication),
         tdxd_indication = ifelse(tdxd_indication == 'NA', NA, tdxd_indication),
         sg_indication = gsub('NA\\+\\+', '', sg_indication),
         sg_indication = gsub('\\+\\+NA', '', sg_indication),
         sg_indication = ifelse(sg_indication == 'NA', NA, sg_indication)) %>%
  
  ungroup()

df_indication$tdxd_indication[which(df_indication$`Record ID` == '539-15')] = 'HER2-LOW'

df_clinical = merge(df_clinical, df_indication, all.x = T)




# # Check if anyone has no indication
# tmp = df_indication %>%
# 
#   mutate(both_missing = is.na(sg_indication) & is.na(tdxd_indication)) %>%
# 
#   filter(both_missing)
# 
# myfile_save_problems = gsub('Rscripts', 'Results/Problems/', getwd())
# writexl::write_xlsx(tmp, paste0(myfile_save_problems, 'Missing_Indication.xlsx'))


# # ----------------- Don't need it anymore ------------------------
# df_indication = readxl::read_xlsx(paste0(myfile_wd, 'IDs_Missing_Groups (2).xlsx'), 
#                                   sheet = 'Correct') %>%
#   
#   select(-Treatment)
# 
# df = merge(df, df_indication, all.x = T)
# 
# df$`In case of sacituzumab govitecan treatment indication based on the most recent biopsy`[which(is.na(df$`In case of sacituzumab govitecan treatment indication based on the most recent biopsy`))] =
#   
#   df$SG_Group[which(is.na(df$`In case of sacituzumab govitecan treatment indication based on the most recent biopsy`))]
# 
# df$`In case of trastuzumab deruxtecan treatment indication based on the most recent biopsy`[which(is.na(df$`In case of trastuzumab deruxtecan treatment indication based on the most recent biopsy`))] =
#   
#   df$Tdxd_Group[which(is.na(df$`In case of trastuzumab deruxtecan treatment indication based on the most recent biopsy`))]
# # ---------------------------------------------------------------

df_advanced_therapy = merge(df_advanced_therapy, df_clinical %>% select(PNUMBER, sg_indication, tdxd_indication),
                            all.x = T)

