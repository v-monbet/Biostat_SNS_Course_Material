#require(SASxport)
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




# LBDGLUSI: Fasting Glucose (mmol/L)
res = lm(formula = LBDGLUSI ~ BMXWAIST+smoker+ RIDAGEYR +
           genre + race_eth + income, data = df.nhanes.complete)
summary(res)

car::Anova(res, type = 3)

plot(res)

# box-cox transformation
bc = MASS::boxcox(res)
LAMBDA <- bc$x[which(bc$y == max(bc$y))]
LAMBDA

df.nhanes.complete <- df.nhanes.complete %>% 
  mutate(LBDGLUSI_trans = (LBDGLUSI^LAMBDA - 1) / LAMBDA)

res.trans <- lm(LBDGLUSI_trans ~ BMXWAIST + smoker +
                        RIDAGEYR + genre +  race_eth + income,
                      data = df.nhanes.complete)
car::Anova(res.trans, type = 3)
round(summary(res.trans)$coef, 4)

# Vector of WC values at which to predict
X    <- seq(min(df.nhanes.complete$BMXWAIST),
            max(df.nhanes.complete$BMXWAIST))

# Estimate mean outcome for these WC values
# Assume other predictors are at their mean
# or reference level
PRED <- predict(res.trans,
                data.frame(BMXWAIST = X,
                           smoker   = "Never",
                           RIDAGEYR = mean(df.nhanes.complete$RIDAGEYR),
                           genre = "Male",
                           race_eth = "Hispanic",
                           income   = "$15001-$20000"))

# Plot on transformed scale
par(mfrow = c(1, 2))
plot(PRED ~ X, type = "l",
     ylab = "Mean Transformed Fasting Glucose",
     xlab = "Waist Circumference (cm)",
     main = "Transformed Outcome")

# PRED = (LBDGLUSI^LAMBDA - 1) / LAMBDA
# Back-transform to the original scale
FG <- (1 + LAMBDA*PRED)^(1/LAMBDA)
# Plot on original scale
plot(FG ~ X, type = "l",
     ylab = "Median Fasting Glucose (mmol/L)",
     xlab = "Waist Circumference (cm)",
     main = "Original Scale")

### Outliers
car::outlierTest(res.trans, n.max = Inf)

car::influenceIndexPlot(res.trans, vars = "Studentized",
                        id=F, main = "Studentized Residuals")


car::influenceIndexPlot(res.trans, vars = "Cook",
                        id=F, main = "Cook's distance")

### interactions et sélection de variables
df = df.nhanes.complete[,-1] # je retire la variable LBDGLUSI
res.trans <- lm(LBDGLUSI_trans ~ .^2,
                data = df) # modèle avec toutes les interactions
# sélection de variables 
library(MASS)
res.aic = stepAIC(res.trans)

