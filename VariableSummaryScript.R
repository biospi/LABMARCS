library(readxl)
library(ggplot2)
library(tibble)
library(data.table)
library(lubridate)

data_path = 'C:/Users/bs16044/OneDrive - University of Bristol/HDR-UK-AMR/LABMARCS/brian/'
fn = 'FullGLM_18Models_VariableSummary.xlsx'
xls_data = read_excel(paste(data_path,fn,sep = ''))  



psig_idx = (xls_data$`P-value` <= .05) & (xls_data$`P-value` != 0)

data_focus_icu = xls_data[psig_idx,]
data_focus_icu = data_focus_icu[data_focus_icu$outcome=='icu',]

data_focus_death = xls_data[psig_idx,]
data_focus_death = data_focus_death[data_focus_death$outcome=='death',]
#frequency table

print(table(data_focus_death$Description))

print(table(data_focus_icu$Description))
