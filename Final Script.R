# 1: Packages used

#install.packages("haven")
library(haven)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("dplyr")
library(dplyr)
#install.packages("plm")
library(plm)
#install.packages("stargazer")
library(stargazer)
#install.packages("sjPlot")
library(sjPlot)
#install.packages("gtsummary")
library(gtsummary)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("table1")
library(table1)
#install.packages("car")
library(car)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("forecats")
library(forcats)

# 2: Merging waves

# loading in wave 2

P2 <- read_dta("anchor2.dta")

# subsetting data set 

P2F  <- P2 %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income oecd equivalized
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         srs1i1, srs1i2, srs1i3, srs1i4, srs1i5, # importance of life domains (present)
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P2F$wave <- 2

# loading in wave 3

P3 <- read_dta("anchor3.dta")

# subsetting data set

P3F  <- P3 %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         srs1i1, srs1i2, srs1i3, srs1i4, srs1i5, # importance of life domains (present)
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P3F$wave <- 3

# bind data sets

Pairfam <- bind_rows(P2F, P3F)

# loading in wave 4

P4 <- read_dta("anchor4.dta")

# subsetting data set

P4F  <- P4 %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         srs1i1, srs1i2, srs1i3, srs1i4, srs1i5, # importance of life domains (present)
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P4F$wave <- 4

# bind data sets

Pairfam <-bind_rows(Pairfam, P4F)

# loading in wave 5

P5 <- read_dta("anchor5.dta")

# subsetting data set

P5F  <- P5 %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         srs1i1, srs1i2, srs1i3, srs1i4, srs1i5, # importance of life domains (present)
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P5F$wave <- 5

# bind data sets

Pairfam <-bind_rows(Pairfam, P5F)

# loading in wave 2

P6 <- read_dta("anchor6.dta")

# subsetting data set

P6F  <- P6 %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         srs1i1, srs1i2, srs1i3, srs1i4, srs1i5, # importance of life domains (present)
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P6F$wave <- 6

# bind data sets

Pairfam <- bind_rows(Pairfam, P6F)

# loading in wave 7

P7 <- read_dta("anchor7.dta")

# subsetting data set

P7F  <- P7 %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         srs1i1, srs1i2, srs1i3, srs1i4, srs1i5, # importance of life domains (present)
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P7F$wave <- 7

# bind data sets

Pairfam <- bind_rows(Pairfam, P7F)

# loading in wave 8

P8 <- read_dta("anchor8.dta")

# subsetting data set

P8F  <- P8 %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         srs1i1, srs1i2, srs1i3, srs1i4, srs1i5, # importance of life domains (present)
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P8F$wave <- 8

# bind data sets

Pairfam <- bind_rows(Pairfam, P8F)

warnings()

# loading in wave 9

P9 <- read_dta("anchor9.dta")

# subsetting data set

P9F  <- P9 %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         srs1i1, srs1i2, srs1i3, srs1i4, srs1i5, # importance of life domains (present)
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P9F$wave <- 9

# bind data sets

Pairfam <-  bind_rows(Pairfam, P9F)

# loading in wave 10

P10 <- read_dta("anchor10.dta")

# subsetting data set

P10F  <- P10 %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         srs1i1, srs1i2, srs1i3, srs1i4, srs1i5, # importance of life domains (present)
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P10F$wave <- 10

# bind data sets

Pairfam <-  bind_rows(Pairfam, P10F)



# loading in wave 11

P11 <- read_dta("anchor11.dta")

# subsetting data set

P11F  <- P11 %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         srs1i1, srs1i2, srs1i3, srs1i4, srs1i5, # importance of life domains (present)
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P11F$wave <- 11 # note ~ 5,000 more observations

# bind data sets

Pairfam <-  bind_rows(Pairfam, P11F)

# loading in wave 12 cati

P12CT <- read_dta("anchor12_cati.dta")

# subsetting data set

P12CTF  <- P12CT %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         # importance of life domains does not exist (present)
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P12CTF$wave <- 12

# loading in wave 12 capi

P12CP <- read_dta("anchor12_capi.dta")

# subsetting data set

P12CPF  <- P12CP %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         # importance of life domains (present) does not exist
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P12CPF$wave <- 12

# bind data sets

Pairfam <- bind_rows(Pairfam, P12CPF)
Pairfam <- bind_rows(Pairfam, P12CTF)


# loading in wave 13 cati

P13CT <- read_dta("anchor13_cati.dta")

# subsetting data set

P13CTF  <- P13CT %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         # importance of life domains (present) does not exist
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P13CTF$wave <- 13

# loading in wave 13 capi

P13CP <- read_dta("anchor13_capi.dta")

# subsetting data set

P13CPF  <- P13CP %>%
  select(sex_gen, 
         lfs, 
         per2i1, per2i2, per2i3, per2i4, per2i5, per2i6, per2i7, per2i8, per2i9, per2i10,
         age, 
         east, 
         marstat, 
         relstat,
         plfs,
         isced,
         migstatus, ethni,
         siops, #occupational prestige
         incnet, # personal income
         hhincoecd, # household income
         nkidsalv, # number if kids alive
         sat1i1, # job satisfaction
         # importance of life domains (present) does not exist
         job7, # working hours
         job13i4, # subjective wage adequacy
         job14, # employment prospects
         inc28, # satisfaction with economic situation of household
         id)

# creating variable 'wave' as identifier

P13CPF$wave <- 13


# bind data sets

Pairfam <-bind_rows(Pairfam, P13CPF)
Pairfam <- bind_rows(Pairfam, P13CTF)

table(Pairfam$wave)

# save file containing all observations

save(Pairfam, file = "PairfamC.RData")

# load file containing all observations

load(file = "PairfamC.Rdata")


# to remove objects not needed

rm(P10, P10F, P11, P11F, P12CP,P12CPF, P12CT, P12CTF, 
   P13CP,P13CPF, P13CT, P13CTF, P2, P2F, P3, P3F,P4,
   P4F, P5, P5F, P6, P6F, P7, P7F, P8, P8F, P9, P9F)

# 3: Creating analytical sample

# load in file containing all observations

load(file = "PairfamC.Rdata")

# Var: MIGRATION
# 1 = no migration status
# 2 = 1st generation
# 3 = 2nd generation

table(Pairfam$migstatus)

Pairfam <- Pairfam %>%
  filter(migstatus %in% c("1", "2", "3"))

Pairfam$migstatus <-  as.numeric(Pairfam$migstatus)
Pairfam$migstatus[Pairfam$migstatus==1]<-"1- No migratory background"
Pairfam$migstatus[Pairfam$migstatus==2]<-"2- Migratory background"
Pairfam$migstatus[Pairfam$migstatus==3]<-"2- Migratory background"
Pairfam$MIGRATION<-as.factor(Pairfam$migstatus)

table(Pairfam$MIGRATION)


# Var: GR (Gender/Region)

Pairfam$sex_gen <-  as.numeric(Pairfam$sex_gen)
Pairfam$east <-  as.numeric(Pairfam$east)

Pairfam$GR <- Pairfam$sex_gen

Pairfam$GR[Pairfam$sex_gen==1 & Pairfam$east==0] <- "1- Male West German"
Pairfam$GR[Pairfam$sex_gen==2 & Pairfam$east==0] <- "3- Female West German"
Pairfam$GR[Pairfam$sex_gen==1 & Pairfam$east==1] <- "2- Male East German"
Pairfam$GR[Pairfam$sex_gen==2 & Pairfam$east==1] <- "4- Female East German"


Pairfam <- Pairfam %>%
  filter(GR %in% c("1- Male West German", "3- Female West German", "2- Male East German", "4- Female East German"))
Pairfam$GR<-as.factor(Pairfam$GR)

table(Pairfam$GR)

# Var: GENDER

Pairfam <- Pairfam %>%
  filter(sex_gen %in% c("1", "2"))

Pairfam$sex_gen[Pairfam$sex_gen==1]<-"1-male"
Pairfam$sex_gen[Pairfam$sex_gen==2]<-"2-female"
Pairfam$GENDER<-as.factor(Pairfam$sex_gen)

table(Pairfam$GENDER)

# Var: REGION

Pairfam <- Pairfam %>%
  filter(east %in% c("1", "0"))

Pairfam$east[Pairfam$east==0]<-"West Germany"
Pairfam$east[Pairfam$east==1]<-"East Germany"
Pairfam$REGION<-as.factor(Pairfam$east)

table(Pairfam$REGION)

# Var: KIDS

Pairfam$nkidsalv <-  as.numeric(Pairfam$nkidsalv)
Pairfam <-subset(Pairfam, Pairfam$nkidsalv>=0)
Pairfam$nkidsalv[Pairfam$nkidsalv==0]<-"0"
Pairfam$nkidsalv[Pairfam$nkidsalv==1]<-"1"
Pairfam$nkidsalv[Pairfam$nkidsalv==2]<-"2"
Pairfam$nkidsalv[Pairfam$nkidsalv==3]<-"3+"
Pairfam$nkidsalv[Pairfam$nkidsalv==4]<-"3+"
Pairfam$nkidsalv[Pairfam$nkidsalv==5]<-"3+"
Pairfam$nkidsalv[Pairfam$nkidsalv==6]<-"3+"
Pairfam$nkidsalv[Pairfam$nkidsalv==7]<-"3+"
Pairfam$nkidsalv[Pairfam$nkidsalv==8]<-"3+"
Pairfam$nkidsalv[Pairfam$nkidsalv==9]<-"3+"
Pairfam$nkidsalv[Pairfam$nkidsalv==10]<-"3+"
Pairfam$nkidsalv[Pairfam$nkidsalv==11]<-"3+"
Pairfam$nkidsalv[Pairfam$nkidsalv==12]<-"3+"
Pairfam$KIDS <- as.factor(Pairfam$nkidsalv)
table(Pairfam$KIDS)

# Var: AGE

Pairfam$age <-  as.numeric(Pairfam$age)
Pairfam <-subset(Pairfam, Pairfam$age>=16)
Pairfam$AGE <- Pairfam$age
table(Pairfam$AGE)

# Var: EDU

Pairfam$isced <-  as.numeric(Pairfam$isced)
table(Pairfam$isced)
Pairfam <- Pairfam %>%
  filter(isced %in% c("0","1", "2", "3", "4", "5", "6", "7", "8"))
Pairfam$isced[Pairfam$isced==0]<-"8- Currently enrolled"
Pairfam$isced[Pairfam$isced==1]<-"1- No degree"
Pairfam$isced[Pairfam$isced==2]<-"2- Lower secondary education"
Pairfam$isced[Pairfam$isced==3]<-"2- Lower secondary education"
Pairfam$isced[Pairfam$isced==4]<-"3- Upper secondary education vocational"
Pairfam$isced[Pairfam$isced==5]<-"4- Upper secondary education general"
Pairfam$isced[Pairfam$isced==6]<-"5- Post-secondary non tertiary education general"
Pairfam$isced[Pairfam$isced==7]<-"6- First stage of tertiary education"
Pairfam$isced[Pairfam$isced==8]<-"7- Second stage of tertiary education"
Pairfam$EDU<-as.factor(Pairfam$isced)
Pairfam$EDU <- fct_relevel(Pairfam$EDU, "1- No degree", "2- Lower secondary education", "3- Upper secondary education vocational",
                           "4- Upper secondary education general", "5- Post-secondary non tertiary education general",
                           "6- First stage of tertiary education", "7- Second stage of tertiary education", "8- Currently enrolled")
table(Pairfam$EDU)

# Var: INCOME (OECD equivalenced)

table(Pairfam$hhincoecd)

Pairfam <-subset(Pairfam, Pairfam$hhincoecd>=0)
Pairfam$INCOME1 <- Pairfam$hhincoecd
Pairfam$INCOME <- Pairfam$hhincoecd / 1000



# Var: MARITAL

Pairfam$marstat <-  as.numeric(Pairfam$marstat)

Pairfam <- Pairfam %>%
  filter(marstat %in% c("1", "2", "3", "4"))

table(Pairfam$marstat)

Pairfam$marstat[Pairfam$marstat==1]<-"3- Never married"
Pairfam$marstat[Pairfam$marstat==2]<-"1- Married/civil union"
Pairfam$marstat[Pairfam$marstat==3]<-"2- Divorced/dissolved civil union"
Pairfam$marstat[Pairfam$marstat==4]<-"4- Widowed/surviving partner in civil union"
Pairfam$MARITAL<-as.factor(Pairfam$marstat)

table(Pairfam$MARITAL)


# Var: DEP (Depression)

# correct methodology (see p. 183 in Scales Manual)
# per2i2, 7, 8, 9, 10 must be recoded (inversed)
# range from 10 to 40
# cut-off point 25 or higher
# answer range 1= Almost never, 4= Almost always

Pairfam$per2i2 <- recode(Pairfam$per2i2, "4 = 1; 3 = 2; 2 = 3; 1 = 4")
Pairfam$per2i7 <- recode(Pairfam$per2i7, "4 = 1; 3 = 2; 2 = 3; 1 = 4")
Pairfam$per2i8 <- recode(Pairfam$per2i8, "4 = 1; 3 = 2; 2 = 3; 1 = 4")
Pairfam$per2i9 <- recode(Pairfam$per2i9, "4 = 1; 3 = 2; 2 = 3; 1 = 4")
Pairfam$per2i10 <- recode(Pairfam$per2i10, "4 = 1; 3 = 2; 2 = 3; 1 = 4")


Pairfam <- Pairfam %>%
  filter(Pairfam$per2i1 >= 1 & 
           per2i2 >= 1 & 
           per2i3 >= 1 & 
           per2i4 >= 1 &
           per2i5 >= 1 &
           per2i6 >= 1 &
           per2i7 >= 1 &
           per2i8 >= 1 &
           per2i9 >= 1 &
           per2i10 >= 1) 

Pairfam$DEP <- rowSums(Pairfam[,c(3,4,5,6,7,8,9,10,11,12)], na.rm=TRUE)

table(Pairfam$DEP)


# Var: DEPC (Cutoff)
Pairfam$DEPC <- Pairfam$DEP
Pairfam$DEPC[Pairfam$DEP>=25]<-"1- Depression"
Pairfam$DEPC[Pairfam$DEP<=24]<-"0- No Depression"
Pairfam$DEPC<-as.factor(Pairfam$DEPC)
table(Pairfam$DEPC)

########################

save(Pairfam, file = "PairfamD.RData")

########################
# Create OLS Data set

load(file = "PairfamD.Rdata")

# Var: LFSsimp
# 1 = employed
# 2 = unemployed
# 3 = education
# 4 = other
# -7 = excluded

OLSPairfam <- Pairfam
OLSPairfam$lfs <- recode(OLSPairfam$lfs, "1 = 3; 2 = 4; 3 = 4; 4 = 2; 5 = 4; 6 = 4; 7 = 4; 8 = 3; 9 = 1; 10 = 1; 11 = 1; 12 = 1; 13 = 4")

OLSPairfam <- OLSPairfam %>%
  filter(lfs %in% c("1", "2", "3", "4"))

OLSPairfam$lfs <-  as.numeric(OLSPairfam$lfs)

OLSPairfam$lfs[OLSPairfam$lfs==1]<-"1- Employed"
OLSPairfam$lfs[OLSPairfam$lfs==2]<-"2- Unemployed"
OLSPairfam$lfs[OLSPairfam$lfs==3]<-"3- In education"
OLSPairfam$lfs[OLSPairfam$lfs==4]<-"4- Other"
OLSPairfam$LFSsimp<-as.factor(OLSPairfam$lfs)

table(OLSPairfam$LFSsimp)


save(OLSPairfam, file = "Pairfam$LFSsimp.RData")

########################
########################
# Create OLS Data set using more refined Employment statuses

load(file = "PairfamD.Rdata")

# Var: LFSfull

# 1 = full-time employed
# 2 = part-time or marginal employed
# 3 = unemployed
# 4 = education
# 5 = other

FullPairfam <- Pairfam
FullPairfam$lfs <- recode(FullPairfam$lfs,
                          "1=4; 2=5; 3=5; 4=3; 5=5; 6=5; 7=5; 8=4; 9=1; 10=2; 11=2; 12=1; 13=5")

FullPairfam <- FullPairfam %>%
  filter(lfs %in% c("1", "2", "3", "4", "5"))

FullPairfam$lfs <-  as.numeric(FullPairfam$lfs)

FullPairfam$lfs[FullPairfam$lfs==1]<-"1- Employed"
FullPairfam$lfs[FullPairfam$lfs==2]<-"2- Part-time or marginally employed"
FullPairfam$lfs[FullPairfam$lfs==3]<-"3- Unemployed"
FullPairfam$lfs[FullPairfam$lfs==4]<-"4- In education"
FullPairfam$lfs[FullPairfam$lfs==5]<-"5- Other"
FullPairfam$LFSfull<-as.factor(FullPairfam$lfs)

table(FullPairfam$LFSfull)

save(FullPairfam, file = "Pairfam$LFSfull.RData")

# 4: Descriptive Statistics and Figures

load(file = "Pairfam$LFSsimp.RData")
load(file = "Pairfam$LFSfull.RData")


# Figure 1: Unemployment Rate in West and East Germany by Gender
#ID
# 100 = West/men
# 101 = West/women
# 110 = East/men
# 111 = East/women
ALQ <- read.csv("Arbeitslosenqoute.csv")
ALQ$X1995 <- as.numeric(ALQ$X1995)
ALQ <- pivot_longer(ALQ,
                    cols = c(X1995, X1996,
                             X1997, X1998,
                             X1999, X2000,
                             X2001, X2002,
                             X2003, X2004,
                             X2005, X2006,
                             X2007, X2008,
                             X2009, X2010,
                             X2011, X2012,
                             X2013, X2014,
                             X2015, X2016,
                             X2017, X2018,
                             X2019, X2020),
                    names_to = "YEAR",
                    values_to = "ALQ") %>%
  drop_na() %>%
  mutate(YEAR = as.numeric(sub("X", "", YEAR)))

ALQ <- ALQ %>%
  rename(ID = period)

ALQ$ID <- as.factor(ALQ$ID)


UER <- ggplot(ALQ, aes(x=YEAR, y=ALQ, color=ID, group=ID)) +
  geom_line() +
  labs(x="", y="Unemployment Rate [%]") +
  theme_classic() +
  scale_color_brewer(palette = "Paired",
                     labels = c("Men/ West Germany", "Women/ West Germany",
                                "Men/ East Germany", "Women/ East Germany")) +
  scale_x_continuous(breaks=seq(1995, 2020, by=1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size=7),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 4.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
UER

ggsave("F1- UER.png", plot = UER)

# Table 1 - Distribution  

my.render.cont <- function(x)  {
  with(stats.apply.rounding(stats.default(x), digits=4),
       c("","Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))}



FullPairfam$GENDER <- 
  factor(FullPairfam$GENDER, levels=c("1-male","2-female"),
         labels=c("Male", 
                  "Female"))
FullPairfam$EDU <- 
  factor(FullPairfam$EDU, levels=c("1- No degree", 
                                   "2- Lower secondary education", 
                                   "3- Upper secondary education vocational", 
                                   "4- Upper secondary education general",
                                   "5- Post-secondary non tertiary education general",
                                   "6- First stage of tertiary education",
                                   "7- Second stage of tertiary education",
                                   "8- Currently enrolled"),
         labels=c("No degree", 
                  "Lower secondary education",
                  "Upper secondary education vocational",
                  "Upper secondary education general",
                  "Post-secondary non tertiary education general",
                  "First stage of tertiary education",
                  "Second stage of tertiary education",
                  "Currently enrolled"))
FullPairfam$LFSfull <- 
  factor(FullPairfam$LFSfull, levels=c("1- Employed","2- Part-time or marginally employed",
                                       "3- Unemployed", "4- In education", "5- Other"),
         labels=c("Full-time employed", 
                  "Part-time or marginally employed",
                  "Unemployed",
                  "In education",
                  "Other"))
FullPairfam$MARITAL <- 
  factor(FullPairfam$MARITAL, levels=c("1- Married/civil union", "2- Divorced/dissolved civil union", 
                                       "3- Never married", "4- Widowed/surviving partner in civil union"),
         labels=c("Married or in a civil union", 
                  "Divorced or dissolved civil union",
                  "Never married",
                  "Widowed or surviving partner of civil union"))
FullPairfam$MIGRATION <- 
  factor(FullPairfam$MIGRATION, levels=c("1- No migratory background", "2- Migratory background"),
         labels=c("No migratory background", 
                  "Migratory background"))

table1::label(FullPairfam$LFSfull) <- "Labor Force Status"
table1::label(FullPairfam$MARITAL) <- "Marital Status"
table1::label(FullPairfam$MIGRATION) <- "Migratory Background"
table1::label(FullPairfam$DEP) <- "Depression Score (STDS)"
table1::label(FullPairfam$REGION) <- "Region"
table1::label(FullPairfam$EDU) <- "Education"
table1::label(FullPairfam$age) <- "Age"
table1::label(FullPairfam$KIDS) <- "Number of children"
table1::label(FullPairfam$INCOME1) <- "Net-equivalanced Income (OECD)"
table1::label(FullPairfam$GENDER) <- "Gender"


table1::table1(~GENDER+ DEP + REGION + EDU + MARITAL + MIGRATION + age + KIDS + INCOME1
               | LFSfull , data = FullPairfam, render.continuous=my.render.cont)


# Figure 2: Histogram depression

Histo <-ggplot(OLSPairfam, aes(x = DEP)) +
  geom_histogram(binwidth = .5) +
  geom_bar(fill = "skyblue")+
  geom_density(adjust=0.1) +
  labs(x = "Depression Score", y = "Number of individuals")+
  geom_vline(xintercept = 24.5, linetype="dashed", 
             color = "black", linewidth=.5) +
  scale_x_continuous(breaks=seq(10, 40, by=5)) +
  theme_bw()+ 
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size=7),
        axis.title.x = element_text(size = 7))

Histo
ggsave("F2-Histogram - STDS.png", plot = Histo)


HDep <- OLSPairfam %>%
  filter(DEP>=25)

# Figure 3: Average Depression Score by Gender

PairfamGender <-   OLSPairfam %>%
  group_by(GENDER) %>%
  summarise_at(vars(DEP), list(DeprAvg = mean))

F3 <- PairfamGender %>%
  ggplot(aes(x = factor (GENDER), y = DeprAvg)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.65) +
  scale_y_continuous(limit = c(0, 25)) +
  geom_text(aes(label=sprintf("%.2f", DeprAvg)), vjust=0, hjust= 2, size=2, color = "black") +
  labs(x = "", y = "Depression Score") +
  theme(plot.title = element_text(hjust = 0, size = 10))+
  theme(axis.title=element_text(size=8.5)) +
  theme(axis.text=element_text(size=7))+ 
  scale_x_discrete(labels=c('Men', 'Women')) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size=7),
        axis.title.x = element_text(size = 7))


F3
ggsave("F3-Gender descriptive-Bar Chart.png", plot = F3)


# Figure 4: Average Depression Score by Employment Status and Gender

PairfamFemaleR  <- FullPairfam %>%
  filter(FullPairfam$GENDER == "2-female")

PairfamMaleR <- FullPairfam %>%
  filter(FullPairfam$GENDER == "1-male")

PairfamGGFR <-   PairfamFemaleR %>%
  group_by(LFSfull) %>%
  summarise_at(vars(DEP), list(DeprAvg = mean))

PairfamGGFR$Gender <- 2
PairfamGGFR$Gender[PairfamGGFR$Gender==2]<-"Women"
PairfamGGFR$Gender <- as.factor(PairfamGGFR$Gender)

PairfamGGMR <-   PairfamMaleR %>%
  group_by(LFSfull) %>%
  summarise_at(vars(DEP), list(DeprAvg = mean))

PairfamGGMR$Gender <- 1
PairfamGGMR$Gender[PairfamGGMR$Gender==1]<-"Men"
PairfamGGMR$Gender <- as.factor(PairfamGGMR$Gender)

GAvgR <- bind_rows(PairfamGGFR, PairfamGGMR)

F4 <- GAvgR %>%
  ggplot(aes(fill= Gender, x= factor (LFSfull), y= DeprAvg)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(limit = c(0, 25)) +
  scale_fill_manual(values = c("Skyblue", brewer.pal(9, "Blues")[7])) +
  geom_text(aes(label=sprintf("%.2f", DeprAvg)), vjust=0, hjust= 2, size=2, color = "black",
            position = position_dodge(0.9)) +
  labs(x = "", y = "Depression Score") +
  guides(fill = guide_legend(reverse = TRUE))+
  scale_x_discrete(labels=c('Employed', 'Part-time employed', 'Unemployed', 'In education', 'Other')) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size=7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6))

F4

ggsave("F4- Average Depression Score by Employment Status and Gender.png", plot = F4)

# Figure 5: Average Depression Score by Region

H4PairfamWest  <- FullPairfam %>%
  filter(FullPairfam$REGION == "West Germany")

H4PairfamEast <- FullPairfam %>%
  filter(FullPairfam$REGION == "East Germany")

H4PairfamGGWD <-   H4PairfamWest %>%
  group_by(REGION) %>%
  summarise_at(vars(DEP), list(DeprAvg = mean))


H4PairfamGGWD$Region <- 2
H4PairfamGGWD$Region[H4PairfamGGWD$Region==2]<-"West Germany"
H4PairfamGGWD$Region <- as.factor(H4PairfamGGWD$Region)

H4PairfamGGED <-   H4PairfamEast %>%
  group_by(REGION) %>%
  summarise_at(vars(DEP), list(DeprAvg = mean))

H4PairfamGGED$Region <- 1
H4PairfamGGED$Region[H4PairfamGGED$Region==1]<-"East Germany"
H4PairfamGGED$Region <- as.factor(H4PairfamGGED$Region)

H4GAvgRegionD <- bind_rows(H4PairfamGGWD, H4PairfamGGED)

F5 <- H4GAvgRegionD %>%
  ggplot(aes(x= factor (Region), y= DeprAvg)) + #fill= Region, 
  geom_bar(stat = "identity", fill = "skyblue", width = 0.65) +
  scale_y_continuous(limit = c(0, 25)) +
  geom_text(aes(label=sprintf("%.2f", DeprAvg)), vjust=0, hjust= 2, size=2, color = "black",
            position = position_dodge(0.9)) +
  labs(x= "", y = "Depression Score") +
  scale_x_discrete(labels=c('West Germany','East Germany')) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size=7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6))


F5
ggsave("F5 Region dif-Bar Chart.png", plot = F5)

# Figure 6: Average Depression Score by Region and Labor Force Status

H4PairfamWest  <- FullPairfam %>%
  filter(FullPairfam$REGION == "West Germany")

H4PairfamEast <- FullPairfam %>%
  filter(FullPairfam$REGION == "East Germany")

H4PairfamGGW <-   H4PairfamWest %>%
  group_by(REGION, LFSfull) %>%
  summarise_at(vars(DEP), list(DeprAvg = mean))


H4PairfamGGW$Region <- 2
H4PairfamGGW$Region[H4PairfamGGW$Region==2]<-"West Germany"
H4PairfamGGW$Region <- as.factor(H4PairfamGGW$Region)

H4PairfamGGE <-   H4PairfamEast %>%
  group_by(REGION, LFSfull) %>%
  summarise_at(vars(DEP), list(DeprAvg = mean))

H4PairfamGGE$Region <- 1
H4PairfamGGE$Region[H4PairfamGGE$Region==1]<-"East Germany"
H4PairfamGGE$Region <- as.factor(H4PairfamGGE$Region)

H4GAvgRegion <- bind_rows(H4PairfamGGW, H4PairfamGGE)

F6 <- H4GAvgRegion %>%
  ggplot(aes(fill= Region, x= factor (LFSfull), y= DeprAvg)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_color_brewer(palette="Dark2") +
  scale_fill_manual(values = c("Skyblue", brewer.pal(9, "Blues")[7])) +
  geom_text(aes(label=sprintf("%.2f", DeprAvg)), vjust=0, hjust= 2, size=2, color = "black",
            position = position_dodge(0.9)) +
  labs(x = "", y = "Depression Score", fill= "Place of Residence") +
  guides(fill = guide_legend(reverse = TRUE))+
  scale_x_discrete(labels=c('Employed','Part-time employed', 'Unemployed', 'In education', 'Other', 'Homemaker')) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size=7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6))

F6
ggsave("F6- Region simple-Bar Chart.png", plot = F6)

# Figure 7

H4PairfamFemaleR  <- FullPairfam %>%
  filter(FullPairfam$GENDER == "2-female")

H4PairfamMaleR <- FullPairfam %>%
  filter(FullPairfam$GENDER == "1-male")

H4PairfamGGFR <-   H4PairfamFemaleR %>%
  group_by(LFSfull, REGION) %>%
  summarise_at(vars(DEP), list(DeprAvg = mean))


H4PairfamGGFR$Gender <- 2
H4PairfamGGFR$Gender[H4PairfamGGFR$Gender==2]<-"Women"
H4PairfamGGFR$Gender <- as.factor(H4PairfamGGFR$Gender)

H4PairfamGGMR <-   H4PairfamMaleR %>%
  group_by(LFSfull, REGION) %>%
  summarise_at(vars(DEP), list(DeprAvg = mean))

H4PairfamGGMR$Gender <- 1
H4PairfamGGMR$Gender[H4PairfamGGMR$Gender==1]<-"Men"
H4PairfamGGMR$Gender <- as.factor(H4PairfamGGMR$Gender)

H4GAvgR <- bind_rows(H4PairfamGGFR, H4PairfamGGMR)

F7 <- H4GAvgR %>%
  ggplot(aes(fill= Gender, x= factor (LFSfull), y= DeprAvg)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(limit = c(0, 25)) +
  scale_fill_manual(values = c("Skyblue", brewer.pal(9, "Blues")[7])) +
  geom_text(aes(label=sprintf("%.2f", DeprAvg)), vjust=0, hjust= 2, size=2, color = "black",
            position = position_dodge(0.9)) +
  labs(x = "", y = "Depression Score") +
  guides(fill = guide_legend(reverse = TRUE))+
  scale_x_discrete(labels=c('Employed', 'Part-time employed', 'Unemployed', 'In education', 'Other')) +
  facet_wrap(~REGION) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size=7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 7))


F7
ggsave("F7.png", plot = F7)

# 5: Regression Models

load(file = "Pairfam$LFSsimp.RData")
load(file = "Pairfam$LFSfull.RData")


# Model: H1 OLS simple

OLS <- lm(DEP ~ LFSsimp + 
            wave + AGE + GENDER + KIDS + MARITAL +
            REGION + EDU + INCOME + MIGRATION, data = OLSPairfam)
summary(OLS)

# Table: stepwise (INCOME)
OL <- lm(DEP ~ LFSsimp + 
           wave + AGE + GENDER + KIDS + MARITAL +
           REGION + EDU + MIGRATION, data = OLSPairfam)
summary(OL)

stargazer(OL,
          OLS, 
          digits = 2,
          type = "html", out= "OLS-H1step.html", style = "default",
          dep.var.labels   = "Depression Score (STDS)",
          omit.stat = c("rsq", "f"),
          omit = c("wave", "EDU", "AGE", "MARITAL", "KIDS", 
                   "MIGRATION", "GENDER", "Constant"),
          omit.labels = c("wave", "EDU", "AGE", "MARITAL", "KIDS", 
                          "MIGRATION", "GENDER", "Constant"))

# Model: H2 Gender - OLS simple 

OLSPairfam_WOMEN<- subset(OLSPairfam,OLSPairfam$GENDER== "2-female")
OLSPairfam_MEN<- subset(OLSPairfam,OLSPairfam$GENDER== "1-male")

OLS_Women<- lm(DEP ~ LFSsimp + wave + AGE + KIDS + MARITAL +
                 REGION + EDU + INCOME + MIGRATION, data = OLSPairfam_WOMEN)
summary(OLS_Women)

OLS_Men <- lm(DEP ~ LFSsimp + 
                wave + AGE + KIDS + MARITAL +
                REGION + EDU + INCOME + MIGRATION, data = OLSPairfam_MEN)
summary(OLS_Men)

# Table: H2 OLS simple + Gender
# Using stargazer

stargazer(OLS,
          OLS_Women,
          OLS_Men, 
          digits = 2,
          type = "html", out= "OLS-H1-H2.html", style = "default",
          dep.var.labels   = "Depression Score (STDS)",
          column.labels = c("All", "Women", "Men"),
          omit.stat = c("rsq", "f"),
          omit = c( "wave", "EDU", "AGE", "MARITAL", "KIDS", 
                    "MIGRATION", "REGION", "INCOME", "GENDER", "Constant"),
          omit.labels = c("REGION", "wave", "EDU", "AGE", "MARITAL", "KIDS", 
                          "MIGRATION", "INCOME", "GENDER", "Constant"))
# full
stargazer(OLS,
          OLS_Women,
          OLS_Men, 
          digits = 2,
          type = "html", out= "OLS-H1-H2full.html", style = "default",
          dep.var.labels   = "Depression Score (STDS)",
          column.labels = c("All", "Women", "Men"),
          omit.stat = c("rsq", "f"))

# stepwise

OL_Women<- lm(DEP ~ LFSsimp + wave + AGE + KIDS + MARITAL +
                REGION + EDU + MIGRATION, data = OLSPairfam_WOMEN)

OL_Men <- lm(DEP ~ LFSsimp + 
               wave + AGE + KIDS + MARITAL +
               REGION + EDU + MIGRATION, data = OLSPairfam_MEN)

stargazer(OL_Women,
          OLS_Women,
          OL_Men,
          OLS_Men, 
          digits = 2,
          type = "html", out= "OLS-H2step.html", style = "default",
          dep.var.labels   = "Depression Score (STDS)",
          omit.stat = c("rsq", "f"),
          omit = c( "wave", "EDU", "AGE", "MARITAL", "KIDS", 
                    "MIGRATION", "REGION", "GENDER", "Constant"),
          omit.labels = c("REGION", "wave", "EDU", "AGE", "MARITAL", "KIDS", 
                          "MIGRATION", "GENDER", "Constant"),
          column.labels   = c("Women", "Men"),
          column.separate = c(2, 3))

# Model: H3 column.labels = c("Women", "Women", "Men", "Men"),
OLSPairfam_EAST<- subset(OLSPairfam,OLSPairfam$REGION== "East Germany")
OLSPairfam_WEST<- subset(OLSPairfam,OLSPairfam$REGION== "West Germany")

OLSPairfam_EAST_f<- subset(OLSPairfam_EAST,OLSPairfam_EAST$GENDER== "2-female")
OLSPairfam_WEST_f<- subset(OLSPairfam_WEST,OLSPairfam_WEST$GENDER== "2-female")
OLSPairfam_EAST_m<- subset(OLSPairfam_EAST,OLSPairfam_EAST$GENDER== "1-male")
OLSPairfam_WEST_m<- subset(OLSPairfam_WEST,OLSPairfam_WEST$GENDER== "1-male")

OLS_EAST_f<- lm(DEP ~ LFSsimp + wave + AGE + KIDS + MARITAL +
                  EDU + INCOME + MIGRATION, data = OLSPairfam_EAST_f)
OLS_EAST_m<- lm(DEP ~ LFSsimp + wave + AGE + KIDS + MARITAL +
                  EDU + INCOME + MIGRATION, data = OLSPairfam_EAST_m)
OLS_WEST_f<- lm(DEP ~ LFSsimp + wave + AGE + KIDS + MARITAL +
                  EDU + INCOME + MIGRATION, data = OLSPairfam_WEST_f)
OLS_WEST_m<- lm(DEP ~ LFSsimp + wave + AGE + KIDS + MARITAL +
                  EDU + INCOME + MIGRATION, data = OLSPairfam_WEST_m)

# Table: H3
stargazer(OLS_EAST_f,
          OLS_EAST_m,
          OLS_WEST_f,
          OLS_WEST_m,
          digits = 2,
          type = "html", out= "OLS-H3.html", style = "default",
          dep.var.labels   = "Depression Score (STDS)",
          column.labels = c("East German Women", "East German Men",
                            "West German Women", "West German Men"),
          omit.stat = c("rsq", "f"),
          omit = c("wave", "EDU", "AGE", "MARITAL", "KIDS", 
                   "MIGRATION", "INCOME", "Constant"),
          omit.labels = c("wave", "EDU", "AGE", "MARITAL", "KIDS", 
                          "MIGRATION", "INCOME", "Constant"))
# full
stargazer(OLS_EAST_f,
          OLS_EAST_m,
          OLS_WEST_f,
          OLS_WEST_m,
          digits = 2,
          type = "html", out= "OLS-H3full.html", style = "default",
          dep.var.labels   = "Depression Score (STDS)",
          column.labels = c("East German Women", "East German Men",
                            "West German Women", "West German Men"),
          omit.stat = c("rsq", "f"))

# H4
FullPairfam_EAST<- subset(FullPairfam,FullPairfam$REGION== "East Germany")
FullPairfam_WEST<- subset(FullPairfam,FullPairfam$REGION== "West Germany")

FullPairfam_EAST_f<- subset(FullPairfam_EAST,FullPairfam_EAST$GENDER== "2-female")
FullPairfam_WEST_f<- subset(FullPairfam_WEST,FullPairfam_WEST$GENDER== "2-female")
FullPairfam_EAST_m<- subset(FullPairfam_EAST,FullPairfam_EAST$GENDER== "1-male")
FullPairfam_WEST_m<- subset(FullPairfam_WEST,FullPairfam_WEST$GENDER== "1-male")

Full_EAST_f<- lm(DEP ~ LFSfull + wave + AGE + KIDS + MARITAL +
                   EDU + INCOME + MIGRATION, data = FullPairfam_EAST_f)
Full_EAST_m<- lm(DEP ~ LFSfull + wave + AGE + KIDS + MARITAL +
                   EDU + INCOME + MIGRATION, data = FullPairfam_EAST_m)
Full_WEST_f<- lm(DEP ~ LFSfull + wave + AGE + KIDS + MARITAL +
                   EDU + INCOME + MIGRATION, data = FullPairfam_WEST_f)
Full_WEST_m<- lm(DEP ~ LFSfull + wave + AGE + KIDS + MARITAL +
                   EDU + INCOME + MIGRATION, data = FullPairfam_WEST_m)

# Table: H4
stargazer(Full_EAST_f,
          Full_EAST_m,
          Full_WEST_f,
          Full_WEST_m,
          digits = 2,
          type = "html", out= "OLS-H4.html", style = "default",
          dep.var.labels   = "Depression Score (STDS)",
          column.labels = c("East German Women", "East German Men",
                            "West German Women", "West German Men"),
          omit.stat = c("rsq", "f"),
          omit = c("wave", "EDU", "AGE", "MARITAL", "KIDS", 
                   "MIGRATION", "INCOME", "Constant"),
          omit.labels = c("wave", "EDU", "AGE", "MARITAL", "KIDS", 
                          "MIGRATION", "INCOME", "Constant"))
#FE

# Model: H1 FE

FE_A <- plm(DEP~LFSsimp + INCOME + KIDS + as.factor(wave)
            , data=OLSPairfam,index=c("id", "wave"),model= "within")
FE_A

FE_ASTEP <- plm(DEP~LFSsimp + KIDS + as.factor(wave)
                , data=OLSPairfam,index=c("id", "wave"),model= "within")

# Model: H1 DE - stepwise simple

stargazer(FE_ASTEP, FE_A,
          digits = 2,
          type = "html", out= "FEH1step.html", style = "default",
          omit = c("wave", "KIDS"),
          dep.var.labels   = "Depression Score (STDS)",
          omit.labels = c("wave", "KIDS"),
          omit.stat = c("rsq", "f"))

# Model: H2 Fixed effects 
OLSPairfam_WOMEN<- subset(OLSPairfam,OLSPairfam$GENDER== "2-female")
OLSPairfam_MEN<- subset(OLSPairfam,OLSPairfam$GENDER== "1-male")

FE_F <- plm(DEP~LFSsimp + INCOME + KIDS + as.factor(wave)
            , data=OLSPairfam_WOMEN,index=c("id", "wave"),model= "within")
FE_M <- plm(DEP~LFSsimp + INCOME + KIDS + as.factor(wave)
            , data=OLSPairfam_MEN,index=c("id", "wave"),model= "within")
FE_F
FE_M
# Table: H1+H2 Fixed effects
# shortened table
stargazer(FE_A, FE_F, FE_M,
          digits = 2,
          type = "html", out= "FE-H1H2.html", style = "default",
          omit = c("INCOME","KIDS", "wave"),
          dep.var.labels   = "Depression Score (STDS)",
          omit.labels = c("INCOME","KIDS", "wave"),
          column.labels = c("All", "Women", "Men"),
          omit.stat = c("rsq", "f"))
# full table
stargazer(FE_A, FE_F, FE_M,
          digits = 2,
          type = "html", out= "FE-H1H2full.html", style = "default",
          omit = c("wave"),
          dep.var.labels   = "Depression Score (STDS)",
          omit.labels = c("wave"),
          column.labels = c("All", "Women", "Men"),
          omit.stat = c("rsq", "f"))

# Table H2 stepwise
FE_FSTEP <- plm(DEP~LFSsimp + KIDS + as.factor(wave)
                , data=OLSPairfam_WOMEN,index=c("id", "wave"),model= "within")

FE_MSTEP <- plm(DEP~LFSsimp + KIDS + as.factor(wave)
                , data=OLSPairfam_MEN,index=c("id", "wave"),model= "within")

stargazer(FE_FSTEP, FE_F, FE_MSTEP, FE_M,
          digits = 2,
          type = "html", out= "FEH2step.html", style = "default",
          omit = c("KIDS", "wave"),
          dep.var.labels   = "Depression Score (STDS)",
          omit.labels = c("KIDS", "wave"),
          omit.stat = c("rsq", "f"),
          column.labels   = c("Women", "Men"),
          column.separate = c(2, 3))

#column.labels = c("All", "Women", "Men"),
# H3
OLSPairfam_EAST <- subset(OLSPairfam,OLSPairfam$REGION== "East Germany")
OLSPairfam_WEST <- subset(OLSPairfam,OLSPairfam$REGION== "West Germany")

OLSPairfam_EAST_f<- subset(OLSPairfam_EAST,OLSPairfam_EAST$GENDER== "2-female")
OLSPairfam_WEST_f<- subset(OLSPairfam_WEST,OLSPairfam_WEST$GENDER== "2-female")
OLSPairfam_EAST_m<- subset(OLSPairfam_EAST,OLSPairfam_EAST$GENDER== "1-male")
OLSPairfam_WEST_m<- subset(OLSPairfam_WEST,OLSPairfam_WEST$GENDER== "1-male")

FEOLS_EAST_f<- plm(DEP ~ LFSsimp +  as.factor(wave) + INCOME + KIDS, 
                   data = OLSPairfam_EAST_f,index=c("id", "wave"),model= "within")
FEOLS_EAST_m<- plm(DEP ~ LFSsimp +  as.factor(wave) + INCOME + KIDS,
                   data = OLSPairfam_EAST_m,index=c("id", "wave"),model= "within")
FEOLS_WEST_f<- plm(DEP ~ LFSsimp +  as.factor(wave) + INCOME + KIDS, 
                   data = OLSPairfam_WEST_f,index=c("id", "wave"),model= "within")
FEOLS_WEST_m<- plm(DEP ~ LFSsimp +  as.factor(wave) + INCOME + KIDS,
                   data = OLSPairfam_WEST_m,index=c("id", "wave"),model= "within")

# Table H3 
stargazer(FEOLS_EAST_f,
          FEOLS_EAST_m,
          FEOLS_WEST_f,
          FEOLS_WEST_m,
          digits = 2,
          type = "html", out= "FEH3.html", style = "default",
          omit = c("INCOME", "KIDS","wave"),
          dep.var.labels   = "Depression Score (STDS)",
          omit.labels = c("INCOME", "KIDS","wave"),
          column.labels = c("East German Women","East German Men",
                            "West German Women", "West German Men"),
          omit.stat = c("rsq", "f"))

# 6: Model Visualization

# OLS
H4ef <- data.frame(model ="Women", 
                   names(coef(Full_EAST_f)), 
                   coef(Full_EAST_f), 
                   confint(Full_EAST_f))
names(H4ef) <- c('model', 'var', 'coef', 'lwr', 'upr')
H4ef <- cbind(H4ef, Region = c("East Germany"))

H4em <- data.frame(model ="Men",
                   names(coef(Full_EAST_m)),
                   coef(Full_EAST_m),
                   confint(Full_EAST_m))
names(H4em) <- c('model', 'var', 'coef', 'lwr', 'upr')
H4em <- cbind(H4em, Region = c("East Germany"))

H4wf <- data.frame(model ="Women",
                   names(coef(Full_WEST_f)),
                   coef(Full_WEST_f),
                   confint(Full_WEST_f))
names(H4wf) <- c('model', 'var', 'coef', 'lwr', 'upr')
H4wf <- cbind(H4wf, Region = c("West Germany"))

H4wm <- data.frame(model ="Men",
                   names(coef(Full_WEST_m)),
                   coef(Full_WEST_m),
                   confint(Full_WEST_m))
names(H4wm) <- c('model', 'var', 'coef', 'lwr', 'upr')
H4wm <- cbind(H4wm, Region = c("West Germany"))

H4 <- rbind(H4ef, H4em, H4wf, H4wm)

H4 <- subset(H4, (var %in% c("LFSfull2- Part-time or marginally employed",
                             "LFSfull3- Unemployed",
                             "LFSfull4- In education",
                             "LFSfull5- Other"))) 


COLS <- ggplot(H4, aes(var, coef)) +
  facet_grid(model ~Region) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr, shape=model), 
                width = 0, position = position_dodge(width = 0.75)) +
  xlab('') + ylab('Coefficient estimate') +
  labs(title = "") +
  theme_bw() + coord_flip() +
  scale_x_discrete(labels=c('Part-time employed*',
                            'Unemployed*',
                            "In Education*",
                            "Other*"))+
  labs(caption = "*Reference category: Employed",
       shape = "Gender") +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
            fill = NA, color = "black", size = 1, inherit.aes = FALSE,
            alpha = 0.5,  
            width = 0.05, height = 0.05, 
            hjust = 0, vjust = 0,
            show.legend = FALSE) +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size=7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        plot.caption = element_text(size=6),
        strip.text = element_text(size = 7))

COLS

ggsave("OLSCP.png", plot = COLS)

# FE

# H2 - Fixed effects simple
H2sF <- data.frame(model ="Women", names(coef(FE_F)), coef(FE_F), confint(FE_F))
names(H2sF) <- c('model', 'var', 'coef', 'lwr', 'upr')

H2sM <- data.frame(model ="Men", names(coef(FE_M)), coef(FE_M), confint(FE_M))
names(H2sM) <- c('model', 'var', 'coef', 'lwr', 'upr')

H2s <- rbind(H2sF, H2sM)
H2s$var <- sub("1$", "", H2s$var)

H2s <- subset(H2s, (var %in% c("LFSsimp2- Unemployed", 
                               "LFSsimp3- In education",
                               "LFSsimp4- Other"))) #"LFSsimp3- In education","LFSsimp4- Other"

H2FEplot <- ggplot(H2s, aes(var, coef)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_point(aes(shape = model), position = position_dodge(width = 0.75), size = 1.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr, shape=model), 
                width = 0, position = position_dodge(width = 0.75)) +
  coord_flip() + theme_minimal() +
  xlab('') + ylab('Coefficient estimate') +
  labs(caption = "*Reference category: Employed")+ 
  scale_x_discrete(labels=c('Unemployed', 'In education', 'Other'))+
  scale_shape_manual(values = c(19, 17), guide = guide_legend(reverse = TRUE, title = "Gender"))+
  theme_bw()+
  theme(legend.position = c(0.90, 0.85),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.background = element_rect(linewidth=0.5, 
                                         linetype="solid", 
                                         colour ="black"))+
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size=7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        plot.caption = element_text(size=6),
        strip.text = element_text(size = 7))

H2FEplot
ggsave("H2 FE Plot.png", plot = H2FEplot)

# H3

H3wfe <- data.frame(model ="Women", region = "East Germany", 
                    names(coef(FEOLS_EAST_f)), coef(FEOLS_EAST_f), confint(FEOLS_EAST_f))
names(H3wfe) <- c('model','region', 'var', 'coef', 'lwr', 'upr')

H3mfe <- data.frame(model ="Men", region = "East Germany",
                    names(coef(FEOLS_EAST_m)), coef(FEOLS_EAST_m), confint(FEOLS_EAST_m))
names(H3mfe) <- c('model', 'region','var', 'coef', 'lwr', 'upr')

H3wfw <- data.frame(model ="Women", region = "West Germany",
                    names(coef(FEOLS_WEST_f)), coef(FEOLS_WEST_f), confint(FEOLS_WEST_f))
names(H3wfw) <- c('model', 'region','var', 'coef', 'lwr', 'upr')

H3mfw <- data.frame(model ="Men", region = "West Germany",
                    names(coef(FEOLS_WEST_m)), coef(FEOLS_WEST_m), confint(FEOLS_WEST_m))
names(H3mfw) <- c('model', 'region','var', 'coef', 'lwr', 'upr')


H3 <- rbind(H3wfe, H3mfe, H3wfw, H3mfw)

H3 <- subset(H3, (var %in% c("LFSsimp2- Unemployed", 
                             "LFSsimp3- In education",
                             "LFSsimp4- Other"))) 

H3FEplot <- ggplot(H3, aes(var, coef)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_grid(~region) +
  geom_point(aes(shape = model), position = position_dodge(width = 0.75), size = 1.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr, shape=model), 
                width = 0, position = position_dodge(width = 0.75)) +
  xlab('') + ylab('Coefficient estimate') +
  labs(title = "") +
  coord_flip() + theme_minimal() +
  scale_x_discrete(labels=c('', ''))+
  labs(caption = "*Reference category: Employed",
       shape = "Gender") +
  scale_x_discrete(labels=c('Unemployed*', 'In Education*', 'Other*')) +
  theme(legend.position = c(-0.06, 1.02),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.background = element_rect(linewidth=0.5, 
                                         linetype="solid", 
                                         colour ="black"))+
  guides(shape = guide_legend(reverse = TRUE)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_text(size=7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        plot.caption = element_text(size=6),
        strip.text = element_text(size = 7))


H3FEplot
ggsave("H3FEplot.png", plot = H3FEplot)

