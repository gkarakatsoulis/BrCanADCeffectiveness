library(dplyr)
library(rstatix)
library(survival)
library(survminer)
library(data.table)
library(ggsurvfit)
source('Dataset_Construction.R')



df = df_advanced_therapy %>%
  
  filter(`ADC treatment` != 'No')

df$time_from_adc_to_death = df$time_from_adc_to_death
df$time_from_adc_to_prog = df$time_from_adc_to_prog


myfile_save = gsub('Rscripts', 'Results/KM-curves/', getwd())

# -------------------------------------------------------------
# OS - Tdxd
# -------------------------------------------------------------


df$Group = df$tdxd_indication |>
  
  as.factor()

mydata = df %>% filter(Group == 'HER2-LOW') %>% droplevels()

km = survfit(Surv(time = mydata$time_from_adc_to_death, event = mydata$death_after_adc) ~ mydata$Group)

twelve_month = surv_summary(km, data = mydata) %>%
  
  filter(time >= 12) %>%
  
  filter(time == min(time))  %>%
  
  transmute(result = paste0('12 month-OS \n', (100*surv) |> round(1), '%')) %>%
  
  unlist()
  

p = ggsurvfit(km, color = 'steelblue') +
  
  labs(y = 'Survival probability',
       x = 'Time (in months)',
       title = paste0(mydata$Group |> levels())) +
  
  add_risktable(
    risktable_stats = "{n.risk} \n {n.event}",
    stats_label = "N. at Risk \n N. Events") +
  
  add_risktable_strata_symbol(symbol = "\U25CF", size = 10) +
  
  theme(legend.position = 'top') +
  
  add_legend_title("Group") +
  
  add_quantile(y_value = 0.5, linetype = "dotted", 
               color = "black", linewidth = 0.8) +
  
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  
  geom_label(aes(x = 25, y = 0.75, label = twelve_month), label.size = NA)

ggsave(paste0(myfile_save, 'OS_her2low.jpeg'), p, width = 8, height = 7)


# ----------------------------------------------------------------
#                 HER2 POSITIVE
# ----------------------------------------------------------------

mydata = df %>% filter(Group != 'HER2-LOW') %>% droplevels()

km = survfit(Surv(time = mydata$time_from_adc_to_death, event = mydata$death_after_adc) ~ mydata$Group)

twelve_month = surv_summary(km, data = mydata) %>%
  
  filter(time >= 12) %>%
  
  filter(time == min(time))  %>%
  
  transmute(result = paste0('12 month-OS \n', (100*surv) |> round(1), '%')) %>%
  
  unlist()

p = ggsurvfit(km, color = 'steelblue') +
  
  labs(y = 'Survival probability',
       x = 'Time (in months)',
       title = paste0(mydata$Group |> levels())) +
  
  add_risktable(
    risktable_stats = "{n.risk} \n {n.event}",
    stats_label = "N. at Risk \n N. Events ") +
  
  add_risktable_strata_symbol(symbol = "\U25CF", size = 10) +
  
  theme(legend.position = 'top') +
  
  add_legend_title("Group") +
  
  add_quantile(y_value = 0.5, linetype = "dotted", 
               color = "black", linewidth = 0.8) +
  
  scale_x_continuous(breaks = seq(0, 75, 5)) +
  
  geom_label(aes(x = 40, y = 0.75, label = twelve_month), label.size = NA)

ggsave(paste0(myfile_save, 'OS_her2pos.jpeg'), p, width = 8, height = 7)


# --------------------------------------------------------
# OS - SG
# --------------------------------------------------------

df$Group = df$sg_indication |>
  
  as.factor()


# ----------------------------------------------------------------
#                 HR POSITIVE/HER2 NEGATIVE
# ----------------------------------------------------------------

mydata = df %>% filter(Group == 'HR POSITIVE/HER2 NEGATIVE') %>% droplevels()

km = survfit(Surv(time = mydata$time_from_adc_to_death, event = mydata$death_after_adc) ~ mydata$Group)


twelve_month = surv_summary(km, data = mydata) %>%
  
  filter(time >= 12) %>%
  
  filter(time == min(time))  %>%
  
  transmute(result = paste0('12 month-OS \n', (100*surv) |> round(1), '%')) %>%
  
  unlist()


p = ggsurvfit(km, color = 'steelblue') +
  
  labs(y = 'Survival probability',
       x = 'Time (in months)',
       title = paste0(mydata$Group |> levels())) +
  
  add_risktable(
    risktable_stats = "{n.risk} \n {n.event}",
    stats_label = "N. at Risk \n N. Events") +
  
  theme(legend.position = 'top') +
  
  add_legend_title("Group") +
  
  add_quantile(y_value = 0.5, linetype = "dotted", 
               color = "black", linewidth = 0.8) +
  
  scale_x_continuous(breaks = seq(0, 75, 5)) +
  
  geom_label(aes(x = 25, y = 0.75, label = twelve_month), label.size = NA)

ggsave(paste0(myfile_save, 'OS_hr2pos_her2neg.jpeg'), p, width = 8, height = 7)


# --------------------------------------------------------
#                       TNBC
# --------------------------------------------------------


mydata = df %>% filter(Group != 'HR POSITIVE/HER2 NEGATIVE') %>% droplevels()

km = survfit(Surv(time = mydata$time_from_adc_to_death, event = mydata$death_after_adc) ~ mydata$Group)

twelve_month = surv_summary(km, data = mydata) %>%
  
  filter(time >= 12) %>%
  
  filter(time == min(time))  %>%
  
  transmute(result = paste0('12 month-OS \n', (100*surv) |> round(1), '%')) %>%
  
  unlist()


p = ggsurvfit(km, color = 'steelblue') +
  
  labs(y = 'Survival probability',
       x = 'Time (in months)',
       title = paste0(mydata$Group |> levels())) +
  
  add_risktable(
    risktable_stats = "{n.risk} \n {n.event}",
    stats_label = "N. at Risk \n N. Events") +
  
  add_risktable_strata_symbol(symbol = "\U25CF", size = 10) +
  
  theme(legend.position = 'top') +
  
  add_legend_title("Group") +
  
  add_quantile(y_value = 0.5, linetype = "dotted", 
               color = "black", linewidth = 0.8) +
  
  scale_x_continuous(breaks = seq(0, 75, 5)) +
  
  geom_label(aes(x = 25, y = 0.75, label = twelve_month), label.size = NA)

ggsave(paste0(myfile_save, 'OS_tnbc.jpeg'), p, width = 8, height = 7)


# --------------------------------------------------------
# PFS - Tdxd
# --------------------------------------------------------

df$Group = df$tdxd_indication |>
  
  as.factor()

mydata = df %>% filter(Group == 'HER2-LOW') %>% droplevels()

km = survfit(Surv(time = mydata$time_from_adc_to_prog, event = mydata$progress_after_adc) ~ mydata$Group)


twelve_month = surv_summary(km, data = mydata) %>%
  
  filter(time >= 12) %>%
  
  filter(time == min(time))  %>%
  
  transmute(result = paste0('12 month-PFS \n', (100*surv) |> round(1), '%')) %>%
  
  unlist()


p = ggsurvfit(km, color = 'steelblue') +
  
  labs(y = 'Progression free survival probability',
       x = 'Time (in months)',
       title = paste0(mydata$Group |> levels())) +
  
  add_risktable(
    risktable_stats = "{n.risk} \n {n.event}",
    stats_label = "N. at Risk \n N. Events") +
  
  add_risktable_strata_symbol(symbol = "\U25CF", size = 10) +
  
  theme(legend.position = 'top') +
  
  add_legend_title("Group") +
  
  add_quantile(y_value = 0.5, linetype = "dotted", 
               color = "black", linewidth = 0.8) +
  
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  
  geom_label(aes(x = 25, y = 0.75, label = twelve_month), label.size = NA)

ggsave(paste0(myfile_save, 'PFS_her2low.jpeg'), p, width = 8, height = 7)




mydata = df %>% filter(Group != 'HER2-LOW') %>% droplevels()

km = survfit(Surv(time = mydata$time_from_adc_to_prog, event = mydata$progress_after_adc) ~ mydata$Group)

twelve_month = surv_summary(km, data = mydata) %>%
  
  filter(time >= 12) %>%
  
  filter(time == min(time))  %>%
  
  transmute(result = paste0('12 month-PFS \n', (100*surv) |> round(1), '%')) %>%
  
  unlist()


p = ggsurvfit(km, color = 'steelblue') +
  
  labs(y = 'Progression free survival probability',
       x = 'Time (in months)',
       title = paste0(mydata$Group |> levels())) +
  
  add_risktable(
    risktable_stats = "{n.risk} \n {n.event}",
    stats_label = "N. at Risk \n N. Events") +
  
  add_risktable_strata_symbol(symbol = "\U25CF", size = 10) +
  
  theme(legend.position = 'top') +
  
  add_legend_title("Group") +
  
  add_quantile(y_value = 0.5, linetype = "dotted", 
               color = "black", linewidth = 0.8) +
  
  scale_x_continuous(breaks = seq(0, 75, 5)) +
  
  geom_label(aes(x = 35, y = 0.75, label = twelve_month), label.size = NA)


ggsave(paste0(myfile_save, 'PFS_her2pos.jpeg'), p, width = 8, height = 7)



# -----------------------------------------------------------
# PFS - SG
# -----------------------------------------------------------


df$Group = df$sg_indication |>
  
  as.factor()

mydata = df %>% filter(Group == 'HR POSITIVE/HER2 NEGATIVE') %>% droplevels()

km = survfit(Surv(time = mydata$time_from_adc_to_prog, event = mydata$progress_after_adc) ~ mydata$Group)

twelve_month = surv_summary(km, data = mydata) %>%
  
  filter(time >= 12) %>%
  
  filter(time == min(time))  %>%
  
  transmute(result = paste0('12 month-PFS \n', (100*surv) |> round(1), '%')) %>%
  
  unlist()


p = ggsurvfit(km, color = 'steelblue') +
  
  labs(y = 'Progression free survival probability',
       x = 'Time (in months)',
       title = paste0(mydata$Group |> levels())) +
  
  add_risktable(
    risktable_stats = "{n.risk} \n {n.event}",
    stats_label = "N. at Risk \n N. Events") +
  
  add_risktable_strata_symbol(symbol = "\U25CF", size = 10) +
  
  theme(legend.position = 'top') +
  
  add_legend_title("Group") +
  
  add_quantile(y_value = 0.5, linetype = "dotted", 
               color = "black", linewidth = 0.8) +
  
  scale_x_continuous(breaks = seq(0, 75, 5)) +
  
  geom_label(aes(x = 20, y = 0.75, label = twelve_month), label.size = NA)

ggsave(paste0(myfile_save, 'PFS_hr2pos_her2neg.jpeg'), p, width = 8, height = 7)



mydata = df %>% filter(Group != 'HR POSITIVE/HER2 NEGATIVE') %>% droplevels()

km = survfit(Surv(time = mydata$time_from_adc_to_prog, event = mydata$progress_after_adc) ~ mydata$Group)

twelve_month = surv_summary(km, data = mydata) %>%
  
  filter(time >= 12) %>%
  
  filter(time == min(time))  %>%
  
  transmute(result = paste0('12 month-PFS \n', (100*surv) |> round(1), '%')) %>%
  
  unlist()


p = ggsurvfit(km, color = 'steelblue') +
  
  labs(y = 'Progression free survival probability',
       x = 'Time (in months)',
       title = paste0(mydata$Group |> levels())) +
  
  add_risktable(
    risktable_stats = "{n.risk} \n {n.event}",
    stats_label = "N. at Risk \n N. Events") +
  
  add_risktable_strata_symbol(symbol = "\U25CF", size = 10) +
  
  theme(legend.position = 'top') +
  
  add_legend_title("Group") +
  
  add_quantile(y_value = 0.5, linetype = "dotted", 
               color = "black", linewidth = 0.8) +
  
  scale_x_continuous(breaks = seq(0, 75, 5)) +
  
  geom_label(aes(x = 20, y = 0.75, label = twelve_month), label.size = NA)

ggsave(paste0(myfile_save, 'PFS_tnbc.jpeg'), p, width = 8, height = 7)



