
library(dplyr)
library(tibble)
library(stringr)

library(ggplot2)

dm <- read.csv("Demographics-REFMAL628_20.csv", stringsAsFactors = FALSE)
ae <- read.csv("AE-REFMAL628_1.csv", stringsAsFactors = FALSE)
he <- read.csv("Hematology-REFMAL628_64.csv", stringsAsFactors = FALSE)
ch <- read.csv("Chemistry-REFMAL628_60.csv", stringsAsFactors = FALSE)
pk <- read.csv("PK assessment-Predose REFMAL628_93.csv", stringsAsFactors = FALSE)
tl <- read.csv("Target lesions-REFMAL628_169.csv", stringsAsFactors = FALSE)
co <- read.csv("Concomitant medication-REFMAL628_5.csv", stringsAsFactors = FALSE)


Dm <- dm %>% select(Subject, BRTHYR, SEX, ETHNIC, ETHNIC, RACE1:RACE5) %>% 
  mutate(Age = as.numeric(str_sub(Sys.Date(), 1, 4))  - BRTHYR) %>% 
  select(Subject, Age, BRTHYR:RACE5)

Ae <- ae %>% select(Subject, AESTDAT, AEENDAT, AETERM, AETOXGR, AEACN1, AEOUT)

He <- he %>% select(Subject, FolderName, LBDAT, RBC, HGB, WBC, ANC, ALC, NEUT, LYM, MONO, BASO, EOS, PLAT)

Ch <- ch %>% select(Subject, FolderName, LBDAT, ALT, AST, BILI, ALP, LDH, BUN, CREAT, EGFR,
                    CA, MG, PHOS, K, SODIUM, CL, CO2, ALB, PROTOT, GLUG, CCK, CHEMUA)

Pk <- pk %>% select(Subject, FolderName, PCJABDAT_YYYY:PCJABTIM, PCFAST, PCTPT, PCDAT_YYYY:PCTIM) %>% 
  mutate(DoseDTim = str_c(PCJABDAT_YYYY, "-", PCJABDAT_MM, "-", PCJABDAT_DD, " ", PCJABTIM), 
         PKDTim = str_c(PCDAT_YYYY, "-", PCDAT_MM, "-", PCDAT_DD, " ", PCTIM)) %>% 
  select(Subject, FolderName, DoseDTim, PKDTim)

Tl <- tl %>% select(Subject, FolderName, TLLINKID, TLLOC, TLLOCDET, TLMETH, TLRECDAT, TLTEST, TLRECRES)
  


