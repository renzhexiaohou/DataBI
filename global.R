
library(dplyr)
library(tibble)
library(stringr)
library(lubridate)

library(DT)
library(ggplot2)

# library(readr)
# library(readxl)
# library(writexl)

dm <- read.csv("Demographics-REFMAL628_20.csv", stringsAsFactors = FALSE)
ae <- read.csv("AE-REFMAL628_1.csv", stringsAsFactors = FALSE)
he <- read.csv("Hematology-REFMAL628_64.csv", stringsAsFactors = FALSE)
ch <- read.csv("Chemistry-REFMAL628_60.csv", stringsAsFactors = FALSE)
pk <- read.csv("PK assessment-Predose REFMAL628_93.csv", stringsAsFactors = FALSE)
tl <- read.csv("Target lesions-REFMAL628_169.csv", stringsAsFactors = FALSE)
co <- read.csv("Concomitant medication-REFMAL628_5.csv", stringsAsFactors = FALSE)


Dm <- dm %>% select(Subject, BRTHYR, SEX, ETHNIC, ETHNIC, RACE1:RACE5) %>% 
  mutate(Age = as.numeric(str_sub(Sys.Date(), 1, 4))  - BRTHYR,
         Subject = as.factor(Subject)) %>% 
  select(Subject, Age, BRTHYR:RACE5)



Ae_1th <- ae %>%
  group_by(Subject)  %>%
  arrange(mdy_hms(AESTDAT)) %>%
  filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(AESTDAT_1th = AESTDAT) %>% 
  select(Subject, AESTDAT_1th)
Ae <- left_join(select(ae, Subject, AESTDAT, AEENDAT, AETERM, AETOXGR, AEACN1, AEOUT),
                Ae_1th) %>% 
  mutate(DAY = as.numeric(mdy_hms(AESTDAT) - mdy_hms(AESTDAT_1th)) /3600/24 + 1,
         Grade_n = as.numeric(str_sub(AETOXGR, 7, 7)))

He_1th <- he %>%
  group_by(Subject)  %>%
  arrange(dmy(LBDAT)) %>%
  filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(LBDAT_1th = LBDAT) %>% 
  select(Subject, LBDAT_1th)
He <- left_join(select(he, Subject, FolderName, LBDAT, RBC, HGB, WBC, ANC, ALC, NEUT, LYM, MONO, BASO, EOS, PLAT),
                He_1th) %>% 
  mutate(DAY = as.numeric(dmy(LBDAT) - dmy(LBDAT_1th)) + 1)


Ch_1th <- ch %>%
  group_by(Subject)  %>%
  arrange(dmy(LBDAT)) %>%
  filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(LBDAT_1th = LBDAT) %>% 
  select(Subject, LBDAT_1th)
Ch <- left_join(select(ch, Subject, FolderName, LBDAT, ALT, AST, BILI, ALP, LDH, BUN, CREAT, EGFR,
                       CA, MG, PHOS, K, SODIUM, CL, CO2, ALB, PROTOT, GLUG, CCK, CHEMUA),
                Ch_1th) %>% 
  mutate(DAY = as.numeric(dmy(LBDAT) - dmy(LBDAT_1th)) + 1)


Pk <- pk %>% select(Subject, FolderName, PCJABDAT_YYYY:PCJABTIM, PCFAST, PCTPT, PCDAT_YYYY:PCTIM) %>% 
  mutate(DoseDTim = str_c(PCJABDAT_YYYY, "-", PCJABDAT_MM, "-", PCJABDAT_DD, " ", PCJABTIM), 
         PKDTim = str_c(PCDAT_YYYY, "-", PCDAT_MM, "-", PCDAT_DD, " ", PCTIM)) %>% 
  select(Subject, FolderName, DoseDTim, PKDTim)


Tl_1th <- tl %>%
  group_by(Subject)  %>%
  arrange(mdy_hms(TLRECDAT)) %>%
  filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(TLRECDAT_1th = TLRECDAT) %>% 
  select(Subject, TLRECDAT_1th)
Tl <- left_join(select(tl, Subject, FolderName, TLLINKID, TLLOC, TLLOCDET, TLMETH, TLRECDAT, TLTEST, TLRECRES),
                Tl_1th) %>% 
  mutate(DAY = as.numeric(mdy_hms(TLRECDAT) - mdy_hms(TLRECDAT_1th)) /3600/24 + 1,
         TLRECRES_n = as.numeric(TLRECRES)) %>% 
  filter(!is.na(TLRECRES))
  


