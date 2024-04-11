
library("stringr")
library("magrittr")
library("data.table")

setwd("C:/Users/qiuyu/Documents/SPH5104_GroupAssignment")


itm <- fread("MIMIC_IV/icu/d_items.csv")

#===============================================================================
# Population
icu <- fread("MIMIC_IV/icu/icustays.csv")
pat <- fread("MIMIC_IV/hosp/patients.csv")
adm <- fread("MIMIC_IV/hosp/admissions.csv")
unique_patients <- icu$subject_id %>% unique()
# 50920 unique patients

DT <- data.frame(matrix(NA, nrow=length(unique_patients), ncol=52))
colnames(DT) <- c("p_idx", "p_fcu", "p_hsp", "p_icu", "p_str", "p_los", "p_age", 
                  "p_bm1", "p_pup", "p_npu", "p_txt", "i_vpu", "i_vpt", 
                  "i_pch", "i_prd", "i_prm", "c_age", "c_gdr", "c_rce", 
                  "c_bmi", "c_hls", "c_ils", "c_doi", "c_bf1", "c_bm1",
                  "c_bm2", "c_bm3", "c_bm4", "c_dms", "c_rhd", "c_htd", 
                  "c_ihd", "c_pcd", "c_ohd", "c_aac", "c_rfl", "c_ane",
                  "c_sps", "c_imv", "c_npu", "c_txt", "c_nte", "o_dpu",
                  "o_cpu", "o_max", "o_txt", "o_ext", "o_nxt", "o_sms", 
                  "o_ems", "o_sti", "o_eti")

DT$p_idx <- unique_patients
remove(unique_patients)

#--------------------------------------
for(i in 1:nrow(DT)){

  p_idx <- DT$p_idx[i]
  patX <- pat[pat$subject_id==p_idx, ]
  icuX <- icu[icu$subject_id==p_idx, ]  
  icuX$weekday <- weekdays(icuX$intime)
  
  first_icu <- min(icuX$intime)

  DT[i, c("p_age", "c_age", "c_gdr")] <- 
    patX[1, c("anchor_age", "anchor_age", "gender")]
  
  DT[i, c("p_fcu", "p_hsp", "p_icu", "p_str", "p_los", "c_ils", "c_doi")] <- 
    icuX[icuX$intime==first_icu, 
         c("first_careunit", "hadm_id", "stay_id", "intime", "los", "los", "weekday")]

  admX <- adm[adm$hadm_id==DT$p_hsp[i], ]
  admX$hosp_los <- (admX$dischtime - admX$admittime) %>% as.numeric()
  
  DT[i, c("c_rce", "c_hls")] <- admX[1, c("race", "hosp_los")]
  
  if(i%%100==0){
    cat(i)
    cat(".")
  }
}
cat("\n")
remove(i, p_idx, patX, icuX, admX, first_icu)
remove(icu, pat, adm)

# Length of Stay More Than 4 Days
DT <- DT[DT$p_los >= 4, ]

units <- c("Medical Intensive Care Unit (MICU)",
           "Medical/Surgical Intensive Care Unit (MICU/SICU)",
           "Surgical Intensive Care Unit (SICU)",
           "Trauma SICU (TSICU)")
DT <- DT[DT$p_fcu %in% units, ]


# 3-days: 15440 unique patients first stay at ICU
# 4-days: 10924 unique patients first stay at ICU
#===============================================================================
#===
#==
#=


#===============================================================================
# BMI
bmi <- fread("MIMIC_IV/hosp/omr.csv")
bmi <- bmi[str_detect(bmi$result_name, "BMI") & bmi$subject_id %in% DT$p_idx, ]

#--------------------------------------
for(i in 1:nrow(DT)){
  
  p_idx <- DT$p_idx[i]
  p_str <- DT$p_str[i]
  
  bmiX <- bmi[bmi$subject_id==p_idx, ]
  
  if(nrow(bmiX)!=0){
    bmiX$datediff <- 
      (as.POSIXct(bmiX$chartdate, tz="UTC") - as.POSIXct(p_str, tz="UTC")) %>%
      as.numeric() %>% abs()
    bmiX <- bmiX[bmiX$datediff==min(bmiX$datediff), ]
    DT[i, c("c_bmi")] <- bmiX$result_value[1]
  }

  if(i%%100==0){
    cat(i)
    cat(".")
  }
}
cat("\n")
remove(i, p_idx, p_str, bmiX)

# Clear Memory 
remove(bmi)
#===============================================================================
#===
#==
#=

#===============================================================================
# Input Events - Vasopressors
ipt <- fread("MIMIC_IV/icu/inputevents.csv")
VP_list <- c("222315", "221749", "229630", "229631", "229632", "229789", 
             "221289", "229617", "221906", "221662", "229709", "229764")
ipt <- ipt[(ipt$stay_id %in% DT$p_icu) & (ipt$itemid %in% VP_list), ]

#--------------------------------------
for(i in 1:nrow(DT)){
  
  p_icu <- DT$p_icu[i]
  p_str <- DT$p_str[i]
  p_str1 <- p_str + 86400
  
  iptX <- ipt[ipt$stay_id==p_icu & (ipt$starttime >= p_str) & 
                (ipt$starttime < p_str1), ]           
  
  if(nrow(iptX)!=0){
    DT[i, c("i_vpu")] <- TRUE
    DT[i, c("i_vpt")] <- min(iptX$starttime)
  } else {
    DT[i, c("i_vpu")] <- FALSE
    DT[i, c("i_vpt")] <- NA
  }
  
  if(i%%100==0){
    cat(i)
    cat(".")
  }
}
cat("\n")
remove(i, p_icu, p_str, p_str1, iptX, VP_list)

# Clear Memory 
remove(ipt)
#===============================================================================
#===
#==
#=

#===============================================================================
# Diagnoses
dia <- fread("MIMIC_IV/hosp/diagnoses_icd.csv")
dia <- dia[dia$subject_id %in% DT$p_idx, ]
dia$icd_code_all <- substr(dia$icd_code, 1, 3)

DMS09 <- c("249", "250", "648")
DMS10 <- c("E08", "E09", "E10", "E11", "E13", "E14", "O24")
RHD09 <- c("393", "394", "395", "396", "397", "398")
RHD10 <- c("I05", "I06", "I07", "I08", "I09")
HTD09 <- c("401", "402", "403", "404", "405")
HTD10 <- c("I10", "I11", "I12", "I13", "I15")
IHD09 <- c("410", "411", "412", "413", "414")
IHD10 <- c("I20", "I21", "I22", "I23", "I24", "I25")
PCD09 <- c("415", "416", "417")
PCD10 <- c("I26", "I27", "I28")
OHD09 <- c("420", "421", "422", "423", "424", "425", "426", "427", "428","429")
OHD10 <- c("I30", "I31", "I32", "I33", "I34", "I35", "I36", "I37", "I38", "I39", 
           "I40", "I41", "I42", "I43", "I44", "I45", "I46", "I47", "I48", "I49",
           "I50", "I51", "I52")
AAC09 <- c("420", "421", "422", "423", "424", "425", "426", "427", "428", "429")
AAC10 <- c("I70", "I71", "I72", "I73", "I74", "I77", "I78", "I79")
RFL09 <- c("584", "585", "586")
RFL10 <- c("N17", "N18", "N19")
ANE09 <- c("280", "281", "282", "283", "284", "285")
ANE10 <- c("D50", "D51", "D52", "D53", "D55", "D56", "D57", "D58", "D59", "D60", 
           "D61", "D62", "D63", "D64")
SPS09 <- c("78552")
SPS10 <- c("R572", "R6521")
PUA09 <- c("707")
PUA10 <- c("L89")

#--------------------------------------
for(i in 1:nrow(DT)){
  
  p_hsp <- DT$p_hsp[i]

  diaX <- dia[dia$hadm_id==p_hsp, ] 
  diaX09 <- diaX$icd_code_all[diaX$icd_version==9]           
  diaX10 <- diaX$icd_code_all[diaX$icd_version==10]   
  
  DT[i, c("c_dms")] <-
    (length(intersect(diaX09, DMS09))>0) | (length(intersect(diaX10, DMS10))>0)
  DT[i, c("c_rhd")] <-
    (length(intersect(diaX09, RHD09))>0) | (length(intersect(diaX10, RHD10))>0)
  DT[i, c("c_htd")] <-
    (length(intersect(diaX09, HTD09))>0) | (length(intersect(diaX10, HTD10))>0)
  DT[i, c("c_ihd")] <-
    (length(intersect(diaX09, IHD09))>0) | (length(intersect(diaX10, IHD10))>0)
  DT[i, c("c_pcd")] <-
    (length(intersect(diaX09, PCD09))>0) | (length(intersect(diaX10, PCD10))>0)
  DT[i, c("c_ohd")] <-
    (length(intersect(diaX09, OHD09))>0) | (length(intersect(diaX10, OHD10))>0)
  DT[i, c("c_aac")] <-
    (length(intersect(diaX09, AAC09))>0) | (length(intersect(diaX10, AAC10))>0)
  DT[i, c("c_rfl")] <-
    (length(intersect(diaX09, RFL09))>0) | (length(intersect(diaX10, RFL10))>0)
  DT[i, c("c_ane")] <-
    (length(intersect(diaX09, ANE09))>0) | (length(intersect(diaX10, ANE10))>0)
  DT[i, c("c_sps")] <-
    (length(intersect(diaX$icd_code[diaX$icd_version==9], SPS09))>0) | 
    (length(intersect(diaX$icd_code[diaX$icd_version==10], SPS10))>0)
  DT[i, c("o_dpu")] <-
    (length(intersect(diaX09, PUA09))>0) | (length(intersect(diaX10, PUA10))>0)

  if(i%%100==0){
    cat(i)
    cat(".")
  }
}
cat("\n")
remove(i, p_hsp, diaX, diaX09, diaX10)
remove(DMS09, RHD09, HTD09, IHD09, PCD09, OHD09,
       AAC09, RFL09, ANE09, SPS09, PUA09)
remove(DMS10, RHD10, HTD10, IHD10, PCD10, OHD10, 
       AAC10, RFL10, ANE10, SPS10, PUA10)

# Clear Memory 
remove(dia)
#===============================================================================
#===
#==
#=

#===============================================================================
# Procedure Events - Mechanical Ventilation (Invasive Ventilation)
pcd <- fread("MIMIC_IV/icu/procedureevents.csv")
pcd <- pcd[(pcd$stay_id %in% DT$p_icu) & pcd$itemid==225792, ]

#--------------------------------------
for(i in 1:nrow(DT)){
  
  p_icu <- DT$p_icu[i]
  p_str <- DT$p_str[i]
  
  pcdX <- pcd[pcd$stay_id==p_icu & (pcd$charttime < (p_str + 86400)), ]

  DT[i, c("c_imv")] <- (nrow(pcdX) > 0)
  
  if(i%%100==0){
    cat(i)
    cat(".")
  }
}
cat("\n")
remove(i, p_icu, p_str, pcdX)

# Clear Memory 
remove(pcd)
#===============================================================================
#===
#==
#=


#===============================================================================
# Chart Events
cht <- fread("MIMIC_IV/icu/chartevents.csv")
cht <- cht[cht$stay_id %in% DT$p_icu, ]

#--------------------------------------
# Max Number of Pressure Injuries
PU_list <- c("228506", "228507", "228508", "228509", "228510", 
             "228511", "228512", "228513", "228514", "228515")
anatomy <- c("Abdominal"="N", "Ankle"="X", "Ankle Lateral"="X", 
             "Ankle Medial"="X", "Axilla"="N", "Back"="N", "Breast"="N", 
             "Chest"="N", "Chin"="N", "Coccyx"="N", "Ear"="N", "Elbow"="X", 
             "Eye"="N", "Facial"="N", "Fingers"="X", "Flank"="N", "Foot"="X", 
             "Gluteal"="N", "Groin"="N", "Hand"="X", "Head"="N", "Heel"="X", 
             "Hip"="N", "Iliac Crest"="N", "Ischial"="N", "Knee"="X",
             "Labia"="N", "Lip"="N", "LLQ"="N", "Lower arm"="X", 
             "Lower leg"="X", "LUQ"="N", "Mouth"="N", "Nare"="N", "Neck"="N", 
             "Nose"="N", "Occipital"="N", "Oral"="N", "Penis"="N",
             "Perianal"="N", "Perineum"="N", "Rectal"="N", "RLQ"="N",
             "RUQ"="N", "Sacrum"="N", "Scapula"="N", "Scrotum"="N",
             "Shoulder"="N", "Sternum"="N", "Toes"="X", "Tongue"="N", 
             "Torso"="N", "Tracheostomy"="N", "Upper arm"="X",
             "Upper leg"="X", "Wrist"="X")
cht_pu <- cht[cht$itemid %in% PU_list, ]
for(i in 1:nrow(DT)){
  
  p_icu <- DT$p_icu[i]
  p_str <- DT$p_str[i]
  num_of_pu_first2days <- 0
  num_of_pu_after2days <- 0
  txt_of_pu_first2days <- "None"
  txt_of_pu_after2days <- "None"
  txt_of_pu_all <- NULL
  PU_ext <- FALSE
  PU_nxt <- FALSE
  
  chtX <- cht_pu[cht_pu$stay_id==p_icu, ]
  chtX_first2days <- chtX[chtX$charttime < (p_str + 172800), ]
  chtX_after2days <- chtX[chtX$charttime >= (p_str + 172800), ]
  
  if(nrow(chtX_first2days)!=0){
    num_of_pu_first2days <- chtX_first2days$itemid %>% max() - 228505
    txt_of_pu_first2days <- chtX_first2days$value %>% unique()
    txt_of_pu_all <- c(txt_of_pu_all, txt_of_pu_first2days)
  }
  
  if(nrow(chtX_after2days)!=0){
    num_of_pu_after2days <- chtX_after2days$itemid %>% max() - 228505
    txt_of_pu_after2days <- chtX_after2days$value %>% unique()
    txt_of_pu_all <- c(txt_of_pu_all, txt_of_pu_after2days)
    
    PU_site_after2days <- ""
    for(site in txt_of_pu_after2days){
      if(PU_ext & PU_nxt) { break }
      site <- site %>% str_split(pattern="-")
      site <- site[[1]][1] %>% str_trim()
      if(site=="") { next }
      if(anatomy[site]=="X"){
        PU_ext <- TRUE
      } else if(anatomy[site]=="N"){ 
        PU_nxt <- TRUE 
      }
    }
    remove(site)
  }  
  
  txt_of_pu_first2days <- txt_of_pu_first2days %>% paste(collapse="; ")
  txt_of_pu_after2days <- txt_of_pu_after2days %>% paste(collapse="; ")
  txt_of_pu_all <- txt_of_pu_all %>% unique() %>% paste(collapse="; ")
  
  DT[i, c("p_npu", "o_max", "c_npu")] <- 
    c(num_of_pu_first2days, num_of_pu_after2days, 
      max(num_of_pu_first2days, num_of_pu_after2days)) 
  
  DT[i, c("p_txt", "o_txt", "c_txt")] <- 
    c(txt_of_pu_first2days, txt_of_pu_after2days, txt_of_pu_all)
  
  DT[i, c("o_cpu", "o_ext", "o_nxt")] <- 
    c((num_of_pu_after2days>0), PU_ext, PU_nxt) 
  
  if(i%%100==0){
    cat(i)
    cat(".")
  }
}
cat("\n")
remove(i, p_icu, p_str, chtX, chtX_first2days, chtX_after2days,
       num_of_pu_first2days, num_of_pu_after2days, 
       txt_of_pu_first2days, txt_of_pu_after2days, txt_of_pu_all, 
       PU_site_after2days, PU_ext, PU_nxt, PU_list, anatomy)
remove(cht_pu)


#--------------------------------------
# Braden Score - Mobility
cht_bs <- cht[cht$itemid==224057, ]
for(i in 1:nrow(DT)){
  
  p_icu <- DT$p_icu[i]
  p_str <- DT$p_str[i]
  chtX <- cht_bs[cht_bs$stay_id==p_icu, ]
  first_score <- chtX$valuenum[chtX$charttime==min(chtX$charttime)]
  
  # First 24hrs
  min_score_1 <- chtX$valuenum[(chtX$charttime < (p_str + 86400))] %>% min()
  
  # Second 24hrs
  min_score_2 <- chtX$valuenum[(chtX$charttime >= (p_str + 86400)) &
                                 (chtX$charttime < (p_str + 172800))] %>% min()
  
  # Third 24hrs
  min_score_3 <- chtX$valuenum[(chtX$charttime >= (p_str + 172800)) &
                                 (chtX$charttime < (p_str + 259200))] %>% min()
  
  # Fourth 24hrs
  min_score_4 <- chtX$valuenum[(chtX$charttime >= (p_str + 259200)) &
                                 (chtX$charttime < (p_str + 345600))] %>% min()
  
  if(length(first_score)==0){ first_score <- NA }
  if(min_score_1==Inf){ min_score_1 <- NA }
  if(min_score_2==Inf){ min_score_2 <- NA }
  if(min_score_3==Inf){ min_score_3 <- NA }
  if(min_score_4==Inf){ min_score_4 <- NA }
  
  DT[i, c("p_bm1", "c_bf1", "c_bm1", "c_bm2", "c_bm3", "c_bm4")] <- 
    c(min_score_1, first_score, min_score_1, min_score_2, min_score_3, 
      min_score_4)
  
  if(i%%100==0){
    cat(i)
    cat(".")
  }
}
cat("\n")
remove(i, p_icu, p_str, chtX, first_score, 
       min_score_1, min_score_2, min_score_3, min_score_4)
remove(cht_bs)


#--------------------------------------
# Deterioration of Pressure Ulcers
SV_list <- c("228813", "228814", "228815", "228816", "228817", 
             "228818", "228819", "228820", "228821", "228822")
cht_sv <- cht[cht$itemid %in% SV_list, ]
for(i in 1:nrow(DT)){
  
  p_icu <- DT$p_icu[i]
  p_str <- DT$p_str[i]
  max_first2days <- 0
  max_after2days <- 0
  dti_first2days <- FALSE
  dti_after2days <- FALSE
  ukw_first2days <- FALSE
  ukw_after2days <- FALSE
  
  chtX <- cht_sv[cht_sv$stay_id==p_icu, ]
  chtX$Stage <- chtX$value %>% substr(1, 1)
  
  chtX_first2days <- chtX[chtX$charttime < (p_str + 172800), ]
  if(nrow(chtX_first2days)!=0){
    stages <- chtX_first2days$Stage
    dti_first2days <- any(stages=="S")
    ukw_first2days <- any(stages=="U")
    
    stages <- stages[stages!="S" & stages!="U"] 
    if(length(stages)==0){
      if(ukw_first2days){
        max_first2days <- "Unknown"
      }
    } else {
      max_first2days <- stages[stages!="S" & stages!="U"] %>% max()
    }
    remove(stages)
  }
  
  chtX_after2days <- chtX[chtX$charttime >= (p_str + 172800), ]
  if(nrow(chtX_after2days)!=0){
    stages <- chtX_after2days$Stage
    dti_after2days <- any(stages=="S")
    ukw_after2days <- any(stages=="U")
    
    stages <- stages[stages!="S" & stages!="U"] 
    if(length(stages)==0){
      if(ukw_after2days){
        max_after2days <- "Unknown"
      }
    } else {
      max_after2days <- stages[stages!="S" & stages!="U"] %>% max()
    }
    remove(stages)
  }
  
  
  DT[i, c("o_sms", "o_ems")] <- c(max_first2days, max_after2days)
  DT[i, c("o_sti", "o_eti")] <- c(dti_first2days, dti_after2days)

  if(i%%100==0){
    cat(i)
    cat(".")
  }
}
cat("\n")
remove(i, p_icu, p_str, max_first2days, max_after2days, 
       dti_first2days, dti_after2days, ukw_first2days, ukw_after2days,
       chtX, chtX_first2days, chtX_after2days, SV_list)
remove(cht_sv)


#--------------------------------------
# Other Pressure Ulcers Variables
OT_list <- c("224066", "224088", "228649", "229868", "229858")
cht_ot <- cht[cht$itemid %in% OT_list, ]
for(i in 1:nrow(DT)){
  
  p_icu <- DT$p_icu[i]
  p_str <- DT$p_str[i]
  p_pup <- FALSE
  i_pch <- 0
  i_prd <- "None"
  i_prm <- FALSE
  c_nte <- "None"
  chtX <- cht_ot[cht_ot$stay_id==p_icu, ]
  
  # Pressure Ulcer Present Adm History/FHPA
  chtY <- chtX[chtX$itemid=="228649" & chtX$charttime < (p_str + 86400), ]
  if(nrow(chtY)!=0){
    chtY_val <- chtY$value %>% as.numeric() %>% max()
    if(chtY_val==1){
      p_pup <- TRUE
    } else {
      p_pup <- FALSE
    }
    remove(chtY_val)
  } else {
    p_pup <- as.logical(NA)
  }
  remove(chtY)
  
  # Position Change
  chtY <- chtX[chtX$itemid=="224066" & value=="Done", ]
  if(nrow(chtY)!=0){
    chtY_min <- chtY$charttime %>% min()
    chtY_max <- chtY$charttime %>% max()
    chtY_dur <- (chtY_max - chtY_min) %>% as.numeric()
    
    i_pch <- nrow(chtY) / chtY_dur
    remove(chtY_min, chtY_max, chtY_dur)
  }
  remove(chtY)
  
  # Pressure Reducing Device
  chtY <- chtX[chtX$itemid=="224088" & value!="None", ]
  if(nrow(chtY)!=0){
    i_prd <- chtY$value %>% unique() %>% paste(collapse="; ")
  }
  remove(chtY)
  
  # Therapeutic Bed - Pressure Redistribution Mattress
  chtY <- chtX[chtX$itemid=="229858" & 
                 chtX$value=="Pressure Redistribution Mattress", ]
  if(nrow(chtY)!=0){
    i_prm <- TRUE
  }
  remove(chtY)
  
  # Notes on Pressure Injury Risk Factors
  chtY <- chtX[chtX$itemid=="229868", ]
  if(nrow(chtY)!=0){
    c_nte <- chtY$value %>% unique() %>% paste(collapse="; ")
  }
  remove(chtY)
  
  DT[i, c("p_pup", "i_prm")] <- c(p_pup, i_prm)
  DT[i, "i_pch"] <- i_pch
  DT[i, c("i_prd", "c_nte")] <- c(i_prd, c_nte)
  
  if(i%%100==0){
    cat(i)
    cat(".")
  }
}
cat("\n")
remove(i, p_icu, p_str, chtX, p_pup, i_pch, i_prd, i_prm, c_nte, OT_list)
remove(cht_ot)

# Clear Memory 
remove(cht)
#===============================================================================
#===
#==
#=





remove(itm)
#===============================================================================
# Save to File
fwrite(DT, "Dataset.csv", quote=FALSE)






