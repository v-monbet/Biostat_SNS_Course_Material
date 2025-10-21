require(SASxport)
library(foreign)
library(tidyverse)

#data downloaded on https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Laboratory&Cycle=2021-2023

DEMO_L <- foreign::read.xport("~/OneDrive - Université de Rennes 1/ENSEIGNEMENT/BIOSTAT_SNS/DONNEES/DEMO_L.XPT")
dim(DEMO_L)
head(DEMO_L)

BMX_L <- foreign::read.xport("~/OneDrive - Université de Rennes 1/ENSEIGNEMENT/BIOSTAT_SNS/DONNEES/BMX_L.XPT")
dim(BMX_L)
head(BMX_L)

DR1TOT_L <- foreign::read.xport("~/OneDrive - Université de Rennes 1/ENSEIGNEMENT/BIOSTAT_SNS/DONNEES/DR1TOT_L.XPT")
dim(DR1TOT_L)
head(DR1TOT_L)

HDL_L <- foreign::read.xport("~/OneDrive - Université de Rennes 1/ENSEIGNEMENT/BIOSTAT_SNS/DONNEES/HDL_L.XPT")
dim(HDL_L)
head(HDL_L)

GLU_L <- foreign::read.xport("~/OneDrive - Université de Rennes 1/ENSEIGNEMENT/BIOSTAT_SNS/DONNEES/GLU_L.XPT")
dim(GLU_L)
head(GLU_L)


SMQ_L <- foreign::read.xport("~/OneDrive - Université de Rennes 1/ENSEIGNEMENT/BIOSTAT_SNS/DONNEES/SMQ_L.XPT")
dim(SMQ_L)
head(SMQ_L)

INQ_L <- foreign::read.xport("~/OneDrive - Université de Rennes 1/ENSEIGNEMENT/BIOSTAT_SNS/DONNEES/INQ_L.XPT")
dim(INQ_L)
head(INQ_L)


# see https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/SMQ_L.htm for the codes 

SMQ1 = SMQ_L %>%
  mutate(smoker   = case_when(SMQ020 == 2                       ~ 1,
                              SMQ020 == 1 & SMQ040 == 3         ~ 2,
                              SMQ020 == 1 & SMQ040 %in% c(1, 2) ~ 3),
         smoker   = factor(smoker, 
                           levels = 1:3, 
                           labels = c("Never", "Past", "Current")))


df.nhanes <- inner_join(BMX_L,GLU_L, by = "SEQN")
df.nhanes <- inner_join(df.nhanes,DEMO_L, by = "SEQN")
df.nhanes <- inner_join(df.nhanes,SMQ1,by = "SEQN")
df.nhanes <- inner_join(df.nhanes,INQ_L,by = "SEQN")


df.nhanes = df.nhanes %>%
  mutate(   race_eth = case_when(RIDRETH3==1 | RIDRETH3== 2  ~ 1,
                                 RIDRETH3==3   ~ 2,
                                 RIDRETH3==4   ~ 3,
                                 RIDRETH3>4    ~ 4),
            race_eth =factor(race_eth ,levels = 1:4, labels = c("Hispanic","Non-Hispanic White","Non-Hispanic Black","Non-Hispanic White other")))

df.nhanes = df.nhanes %>%
  mutate(   genre = case_when(RIAGENDR==1   ~ 1,
                              RIAGENDR==2   ~ 2),
            genre = factor(genre ,levels = 1:2, labels = c("Male","Female")))

df.nhanes = df.nhanes %>%
  mutate(income   = case_when(IND310 == 1  ~ 1,
                              IND310 == 2  ~ 2,
                              IND310 == 3  ~ 3,
                              IND310 == 4  ~ 4,
                              IND310 == 5  ~ 5),
         income   = factor(income, 
                           levels = 1:5, 
                           labels = c("Less than $3000", "$3001- $5000",
                                      "$5001-$10000","$10001-$15000","$15001-$20000")))


df.nhanes.complete <- df.nhanes %>% 
  select(LBDGLUSI, BMXWAIST, smoker, RIDAGEYR,
         genre, race_eth, income) %>% 
  drop_na()