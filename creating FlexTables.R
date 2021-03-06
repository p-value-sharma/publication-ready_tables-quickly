# packages and setup  #####
library(checkpoint)
checkpoint(snapshotDate = '2018-08-27')

library(dplyr)
library(tidyr)
library(flextable)
library(readxl)
library(here)


dir.create(path = here::here('outputs'))

# importing data ###### 
raw_data <- read_excel(here::here('data/CDI 2009-2015_selected variables.xlsx'))

# user functions ####
bracketizer <- function(x) {
  paste0('(', x, ')')
}
indent <- function(x) {
  paste0('    ', x)
}
# data cleaning ####
cleaned_data <- raw_data %>% 
  mutate(sex = ifelse(grepl(pattern = 'female', x = sex, ignore.case = T), 'Female', 'Male')) %>% 
  rename(admin_date = `date of admission`, c_diff_test_date = `date of c.diff test`) %>% 
  mutate_at(vars(admin_date, c_diff_test_date), funs(as.Date)) %>% 
  mutate(year_admin = format(admin_date, '%Y'),
         month_c_diff_test = format(c_diff_test_date, format = '%b'),
         agegrp = cut(age_final, breaks = c(0, 17, 64, Inf), labels = c('0-17', '18-64', '>65')))

# study period aggregated - table components ####
tabcomp_overall_patient_count <- cleaned_data %>% 
  count() %>% 
  mutate(n = formatC(n, format = 'd', big.mark = ','), 
         characteristic = 'No. of patients') %>% 
  select(characteristic, everything()) %>% 
  rename(value = n)

tabcomp_gender_percentage <- cleaned_data %>% 
  group_by(sex) %>% 
  count() %>%
  ungroup() %>% 
  mutate(n.y = sum(n)) %>% 
  filter(sex == 'Female') %>% 
  mutate(prop = 100*n/n.y, 
         prop = bracketizer(formatC(prop, digits = 1, format = 'f')),
         n = formatC(n, format = 'd', big.mark = ','),
         n = paste(n, prop, sep = ' ')) %>% 
  select(-c(sex, n.y, prop)) %>% 
  mutate(characteristic = 'Sex, female, no. (%)') %>% 
  select(characteristic, everything()) %>% 
  rename(value = n)


# age year +/- sd
tabcomp_age_year_sd <- cleaned_data %>% 
  summarise(mean = mean(age_final, na.rm = T), sd = sd(age_final, na.rm = T)) %>% 
  mutate_if(is.numeric, funs(formatC), format = 'f', digits = 1) %>% 
  mutate(mean = paste(mean, sd, sep = '±')) %>% 
  select(-sd) %>% 
  mutate(characteristic = 'Age, yr, mean ± SD') %>% 
  select(characteristic, everything()) %>% 
  rename(value = mean)
  


# age year group 18-64, 65+ n (%)
tabcomp_age_yeargrp <- cleaned_data %>% 
  group_by(agegrp) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Overalln = sum(n), 
         prop = 100*n/Overalln, 
         prop = bracketizer(formatC(prop, digits = 1, format = 'f')),
         n = formatC(n, format = 'd', big.mark = ','),
         n = paste(n, prop, sep = ' ')) %>% 
  ungroup() %>% 
  filter(!is.na(agegrp)) %>% 
  select(-c(Overalln, prop)) %>% 
  rename(value = n, characteristic = agegrp) %>% 
  select(characteristic, everything()) %>% 
  mutate(characteristic = indent(characteristic))
  
  
# time to c diff test, median (q1q3)
#  Personal communication with Choi, Kelly (PHAC/ASPC) 
# 'Time to C.difficile - please exclude cases missing date of admission when computing this'
tab_comp_cdifftime <- cleaned_data %>% 
  filter(!is.na(admin_date)) %>% 
  mutate(date_diff = as.numeric(c_diff_test_date - admin_date)) %>% 
  summarise(median = median(date_diff), 
            q1 = quantile(date_diff, probs = 0.25),
            q3 = quantile(date_diff, probs = 0.75)) %>% 
  mutate_if(is.numeric, formatC, format = 'd') %>% 
  mutate(q1q3 = bracketizer(paste0(q1, '-', q3)),
         median = paste0(median, ' ', q1q3)) %>% 
  select(-contains('q')) %>% 
  mutate(characteristic = 'Time to C. difficile test from the date of admission, days, median (IQR)') %>% 
  select(characteristic, everything()) %>% 
  rename(value = median)
  
  


# march April, n cases
tabcomp_marApr_N_cases <- cleaned_data %>% 
  filter(month_c_diff_test %in% c('Mar', 'Apr')) %>% 
  count() %>% 
  mutate_if(is.numeric, formatC, big.mark = ',', format = 'd') %>% 
  mutate(characteristic = 'No. of cases') %>% 
  select(characteristic, everything()) %>% 
  rename(value = n) 
  

# march April, clincal results, temperature
tabcomp_marApr_temp <- cleaned_data %>% 
  filter(month_c_diff_test %in% c('Mar', 'Apr')) %>% 
  summarise(mean = mean(temp, na.rm = T), 
            sd = sd(temp, na.rm = T)) %>% 
  mutate_if(is.numeric, formatC, format = 'f', digits = 1) %>% 
  mutate(mean = paste0(mean, '±', sd)) %>% 
  select(-sd) %>% 
  mutate(characteristic = 'Temperature, °C, mean ± SD') %>% 
  select(characteristic, everything()) %>% 
  rename(value = mean)



# march April, clincal results, WBC
tabcomp_marApr_wbc <- cleaned_data %>% 
  filter(month_c_diff_test %in% c('Mar', 'Apr')) %>% 
  summarise(median = median(WBC, na.rm = T), 
            q1 = quantile(WBC, probs = 0.25, na.rm = T),
            q3 = quantile(WBC, probs = 0.75, na.rm = T)) %>% 
  mutate_if(is.numeric, formatC, format = 'f', digits = 1) %>% 
  mutate(median = paste0(median, ' (', q1, '-', q3, ')')) %>% 
  select(-contains('q')) %>% 
  mutate(characteristic = '     Leukocyte count, x109/L, median (IQR)') %>% 
  select(characteristic, everything()) %>% 
  rename(value = median)

# flexTable creation ####
tabcomponents_combined <- add_row(tabcomp_overall_patient_count, .before = 1, 
                                  characteristic = 'Year-round surveillance', value = ' ') %>% 
  bind_rows(tabcomp_gender_percentage) %>% 
  bind_rows(tabcomp_age_year_sd) %>% 
  add_row(characteristic = 'Age group, yr, no. (%)', value = ' ') %>% 
  bind_rows(tabcomp_age_yeargrp) %>%
  bind_rows(tab_comp_cdifftime) %>%
  add_row(characteristic = 'March and April targeted surveillance', value = ' ') %>% 
  bind_rows(tabcomp_marApr_N_cases) %>%
  add_row(characteristic = 'Clinical results', value = ' ') %>% 
  bind_rows(tabcomp_marApr_temp) %>% 
  bind_rows(tabcomp_marApr_wbc)  

headers_typology <- data.frame(col_keys = c('characteristic', 'value'),
                      colA = c('Variable', '2009-2015')) %>% 
  mutate_all(as.character)

final_flexTable <- flextable(tabcomponents_combined, 
                             col_keys = names(tabcomponents_combined)) %>% 
  set_header_df(., mapping = headers_typology, key = 'col_keys') %>% 
  theme_zebra() %>% 
  align(j = 1, align = 'left', part = 'all') %>% 
  bold(i = grepl(pattern = 'surv', tabcomponents_combined$characteristic)) %>% 
  autofit()

flextable_title <- 'Table 1: Patient characteristics, clinical results and severe outcomes of 17 202 adults diagnosed with health care–associated
Clostridium difficile infection from 2009 to 2015*'

output_doc <- officer::read_docx() %>% 
  officer::body_add_par(flextable_title) %>% 
  body_add_flextable(final_flexTable) 

print(output_doc, target = here::here('outputs', 'table1.docx'))


