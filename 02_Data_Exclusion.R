
library("stringr")
library("magrittr")
library("data.table")
library('scales')


setwd("C:/Users/qiuyu/Documents/SPH5104_GroupAssignment")

# 50920 ICU Patients' First Admission into ICU

DT <- fread("Dataset.csv")

minLOS <- 4

#===============================================================================
# Exclude Length of Stay in ICU less than 3/4 Days
rowsX <- 50920
DT <- DT[!DT$p_los<minLOS, ]
rowsY <- nrow(DT)

"Exclude Length of Stay in ICU less than" %>% 
  paste(minLOS, "Days\n") %>%
  paste0(rowsX - rowsY, " Excluded ---> ", rowsY, " Remaining\n") %>%
  paste0("------------------------------------------------------\n") %>% cat()


#===============================================================================
# Exclude Patients less than 18 Years of Age
rowsX <- nrow(DT)
DT <- DT[!DT$p_age<18, ]
rowsY <- nrow(DT)

"Exclude Patients less than 18 Years of Age\n" %>%
  paste0(rowsX - rowsY, " Excluded ---> ", rowsY, " Remaining\n") %>%
  paste0("------------------------------------------------------\n") %>% cat()

#===============================================================================
# Exclude Patients whose First Braden Mobility Score in ICU is more than 2
rowsX <- nrow(DT)
DT <- DT[!DT$p_bm1>2, ]
rowsY <- nrow(DT)

"Exclude Patients whose First Braden Mobility Score" %>%
  paste("in ICU is more than '2'\n") %>%
  paste0(rowsX - rowsY, " Excluded ---> ", rowsY, " Remaining\n") %>%
  paste0("------------------------------------------------------\n") %>% cat()


#===============================================================================
# Exclude Patients who Developed Pressure Ulcers in first 48hrs of ICU Stay
rowsX <- nrow(DT)
DT <- DT[!DT$p_npu>0, ]
rowsY <- nrow(DT)

"Exclude Patients who Developed Pressure Ulcers" %>% 
  paste("in the First 48hrs of ICU Stay\n") %>%
  paste0(rowsX - rowsY, " Excluded ---> ", rowsY, " Remaining\n") %>%
  paste0("------------------------------------------------------\n") %>% cat()


#===============================================================================
# Case and Control Grouping
DT$Cohort <- ifelse(DT$i_vpu, "CASE", "CONTROL")

case_count <- DT[DT$Cohort=="CASE", ] %>% nrow()
ctrl_count <- DT[DT$Cohort=="CONTROL", ] %>% nrow()
ovrl_count <- nrow(DT)

paste0("Case (received VPR)          :   ", case_count, "(", 
       percent(case_count/ovrl_count, accuracy=0.01), ")\n") %>%
  paste0("Control (did not receive VPR):   ", ctrl_count, "(", 
         percent(ctrl_count/ovrl_count, accuracy=0.01), ")\n") %>%
  paste0("Total                        :   ", ovrl_count, "\n") %>%
  paste0("------------------------------------------------------\n") %>% cat()



#===============================================================================
# Save to File
filename <- paste0("Dataset_min", minLOS, "days.csv")
fwrite(DT, filename, quote=FALSE)


