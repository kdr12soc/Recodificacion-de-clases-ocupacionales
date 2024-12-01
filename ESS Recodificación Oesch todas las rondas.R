library(occupar) #Abrimos el paquete Occupar para transformar las variables de Isco88 (de la Ronda 1 a la 5) en su código Isco08, y así poder emplear el código de Daniel Oesch para transformar estas avriables en un esquema operativo de clases ocupacionales.

ess2.5.10$isco08b <- isco88to08(ess2.5.10$iscoco)
ess2.5.10 <- ess2.5.10 %>%
  mutate(isco08_final = coalesce(isco08, isco08b))

table(ess2.5.10$isco08_final, ess2.5.10$essround)
ess2.5.10$isco08 <- ess2.5.10$isco08_final
#############################################################################################################################################
# OESCH CLASS SCHEMA
# Create 16-Class schema, 8-Class schema and 5-Class schema
# Data: ESS round 6 - 2012, ESS round 7 - 2014, ESS round 8 - 2016, ESS round 9 - 2018
# October 2020
# Amal Tawfik, University of Lausanne & HESAV
#############################################################################################################################################

#### References:
#### Oesch, D. (2006a) "Coming to grips with a changing class structure" International Sociology 21 (2): 263-288.
#### Oesch, D. (2006b) "Redrawing the Class Map. Stratification and Institutions in Britain, Germany, Sweden and Switzerland", Basingstoke: Palgrave Macmillan.
#### A few minor changes were made with respect to the procedure described in these two sources (decisions taken by Oesch and Tawfik in 2014)

#### 16-Class schema constructed
#1 Large employers
#2 Self-employed professionals
#3 Small business owners with employees
#4 Small business owners without employees
#5 Technical experts
#6 Technicians
#7 Skilled manual
#8 Low-skilled manual
#9 Higher-grade managers and administrators
#10 Lower-grade managers and administrators
#11 Skilled clerks
#12 Unskilled clerks
#13 Socio-cultural professionals
#14 Socio-cultural semi-professionals
#15 Skilled service
#16 Low-skilled service

#### 8-Class schema constructed
#1 Self-employed professionals and large employers
#2 Small business owners
#3 Technical (semi-)professionals
#4 Production workers
#5 (Associate) managers
#6 Clerks
#7 Socio-cultural (semi-)professionals
#8 Service workers

#### 5-Class schema constructed
#1 Higher-grade service class
#2 Lower-grade service class
#3 Small business owners
#4 Skilled workers
#5 Unskilled workers

#### Variables used to construct Oesch class schema: isco08, emplrel, emplno, isco08p, emprelp

########################################################################################
# Respondent's Oesch class position
# Recode and create variables used to construct class variable for respondents
# Variables used to construct class variable for respondents: isco08, emplrel, emplno
########################################################################################

#### Install dplyr, questionr, labelled and esssurvey packages: install.packages(c("dplyr", "questionr", "labelled", "esssurvey"))
#### Import ESS data with the esssurvey package: make  sure  your  email  is  registered  at http://www.europeansocialsurvey.org/ before setting the email in the set_email() function below
#### Set the round you want to download in the import_rounds() function below
#### Example for a country and a round: import_country(country = "Switzerland", rounds = 6, format = "spss")

library(dplyr)
library(questionr)
library(labelled)
library(essurvey)

#### Recode occupation variable (isco08 com 4-digit) for respondents

tail(freq(ess2.5.10$isco08, total = T))

ess2.5.10$isco_mainjob <- ess2.5.10$isco08
ess2.5.10$isco_mainjob[is.na(ess2.5.10$isco_mainjob)] <- -9
var_label(ess2.5.10$isco_mainjob) <- "Current occupation of respondent - isco08 4-digit"

head(freq(ess2.5.10$isco_mainjob, total = T))


#### Recode employment status for respondents

freq(ess2.5.10$emplrel, total = T)
freq(ess2.5.10$emplno, total = T)

ess2.5.10$emplrel_r <- ess2.5.10$emplrel
ess2.5.10$emplrel_r[is.na(ess2.5.10$emplrel_r)] <- 9
val_label(ess2.5.10$emplrel_r, 9) <- "Missing"
freq(ess2.5.10$emplrel_r, total = T)


ess2.5.10$emplno_r <- ess2.5.10$emplno
ess2.5.10$emplno_r[is.na(ess2.5.10$emplno_r)] <- 0
ess2.5.10$emplno_r[ess2.5.10$emplno_r >= 1 & ess2.5.10$emplno_r <= 9] <- 1
ess2.5.10$emplno_r[ess2.5.10$emplno_r >= 10 & ess2.5.10$emplno_r <= 66665] <- 2
val_labels(ess2.5.10$emplno_r) <- c("0 employees" = 0,
                            "1-9 employees" = 1,
                            "10+ employees" = 2)
freq(ess2.5.10$emplno_r, total = T)

ess2.5.10$selfem_mainjob <- NA
ess2.5.10$selfem_mainjob[ess2.5.10$emplrel_r == 1 | ess2.5.10$emplrel_r == 9] <- 1
ess2.5.10$selfem_mainjob[ess2.5.10$emplrel_r == 2 & ess2.5.10$emplno_r == 0] <- 2
ess2.5.10$selfem_mainjob[ess2.5.10$emplrel_r == 3] <- 2
ess2.5.10$selfem_mainjob[ess2.5.10$emplrel_r == 2 & ess2.5.10$emplno_r == 1] <- 3
ess2.5.10$selfem_mainjob[ess2.5.10$emplrel_r == 2 & ess2.5.10$emplno_r == 2] <- 4

val_labels(ess2.5.10$selfem_mainjob) <- c("Not self-employed" = 1,
                                  "Self-empl without employees" = 2,
                                  "Self-empl with 1-9 employees" = 3,
                                  "Self-empl with 10 or more" = 4)
var_label(ess2.5.10$selfem_mainjob) <- "Employment status for respondants"
freq(ess2.5.10$selfem_mainjob, total = T)


#################################################
# Create Oesch class schema for respondents
#################################################

ess2.5.10$class16_r <- -9

# Large employers (1)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 4] <- 1

# Self-employed professionals (2)

ess2.5.10$class16_r[(ess2.5.10$selfem_mainjob == 2 | ess2.5.10$selfem_mainjob == 3) & ess2.5.10$isco_mainjob >= 2000 & ess2.5.10$isco_mainjob <= 2162] <- 2
ess2.5.10$class16_r[(ess2.5.10$selfem_mainjob == 2 | ess2.5.10$selfem_mainjob == 3) & ess2.5.10$isco_mainjob >= 2164 & ess2.5.10$isco_mainjob <= 2165] <- 2
ess2.5.10$class16_r[(ess2.5.10$selfem_mainjob == 2 | ess2.5.10$selfem_mainjob == 3) & ess2.5.10$isco_mainjob >= 2200 & ess2.5.10$isco_mainjob <= 2212] <- 2
ess2.5.10$class16_r[(ess2.5.10$selfem_mainjob == 2 | ess2.5.10$selfem_mainjob == 3) & ess2.5.10$isco_mainjob == 2250] <- 2
ess2.5.10$class16_r[(ess2.5.10$selfem_mainjob == 2 | ess2.5.10$selfem_mainjob == 3) & ess2.5.10$isco_mainjob >= 2261 & ess2.5.10$isco_mainjob <= 2262] <- 2
ess2.5.10$class16_r[(ess2.5.10$selfem_mainjob == 2 | ess2.5.10$selfem_mainjob == 3) & ess2.5.10$isco_mainjob >= 2300 & ess2.5.10$isco_mainjob <= 2330] <- 2
ess2.5.10$class16_r[(ess2.5.10$selfem_mainjob == 2 | ess2.5.10$selfem_mainjob == 3) & ess2.5.10$isco_mainjob >= 2350 & ess2.5.10$isco_mainjob <= 2352] <- 2
ess2.5.10$class16_r[(ess2.5.10$selfem_mainjob == 2 | ess2.5.10$selfem_mainjob == 3) & ess2.5.10$isco_mainjob >= 2359 & ess2.5.10$isco_mainjob <= 2432] <- 2
ess2.5.10$class16_r[(ess2.5.10$selfem_mainjob == 2 | ess2.5.10$selfem_mainjob == 3) & ess2.5.10$isco_mainjob >= 2500 & ess2.5.10$isco_mainjob <= 2619] <- 2
ess2.5.10$class16_r[(ess2.5.10$selfem_mainjob == 2 | ess2.5.10$selfem_mainjob == 3) & ess2.5.10$isco_mainjob == 2621] <- 2
ess2.5.10$class16_r[(ess2.5.10$selfem_mainjob == 2 | ess2.5.10$selfem_mainjob == 3) & ess2.5.10$isco_mainjob >= 2630 & ess2.5.10$isco_mainjob <= 2634] <- 2
ess2.5.10$class16_r[(ess2.5.10$selfem_mainjob == 2 | ess2.5.10$selfem_mainjob == 3) & ess2.5.10$isco_mainjob >= 2636 & ess2.5.10$isco_mainjob <= 2640] <- 2
ess2.5.10$class16_r[(ess2.5.10$selfem_mainjob == 2 | ess2.5.10$selfem_mainjob == 3) & ess2.5.10$isco_mainjob >= 2642 & ess2.5.10$isco_mainjob <= 2643] <- 2

# Small business owners with employees (3)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob >= 1000 & ess2.5.10$isco_mainjob <= 1439] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob == 2163] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob == 2166] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob >= 2220 & ess2.5.10$isco_mainjob <= 2240] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob == 2260] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob >= 2263 & ess2.5.10$isco_mainjob <= 2269] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob >= 2340 & ess2.5.10$isco_mainjob <= 2342] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob >= 2353 & ess2.5.10$isco_mainjob <= 2356] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob >= 2433 & ess2.5.10$isco_mainjob <= 2434] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob == 2620] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob == 2622] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob == 2635] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob == 2641] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob >= 2650 & ess2.5.10$isco_mainjob <= 2659] <- 3
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 3 & ess2.5.10$isco_mainjob >= 3000 & ess2.5.10$isco_mainjob <= 9629] <- 3

# Small business owners without employees (4)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob >= 1000 & ess2.5.10$isco_mainjob <= 1439] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob == 2163] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob == 2166] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob >= 2220 & ess2.5.10$isco_mainjob <= 2240] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob == 2260] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob >= 2263 & ess2.5.10$isco_mainjob <= 2269] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob >= 2340 & ess2.5.10$isco_mainjob <= 2342] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob >= 2353 & ess2.5.10$isco_mainjob <= 2356] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob >= 2433 & ess2.5.10$isco_mainjob <= 2434] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob == 2620] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob == 2622] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob == 2635] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob == 2641] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob >= 2650 & ess2.5.10$isco_mainjob <= 2659] <- 4
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 2 & ess2.5.10$isco_mainjob >= 3000 & ess2.5.10$isco_mainjob <= 9629] <- 4

# Technical experts (5)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2100 &  ess2.5.10$isco_mainjob <= 2162] <- 5
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2164 &  ess2.5.10$isco_mainjob <= 2165] <- 5
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2500 &  ess2.5.10$isco_mainjob <= 2529] <- 5

# Technicians (6)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 3100 &  ess2.5.10$isco_mainjob <= 3155] <- 6
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 3210 &  ess2.5.10$isco_mainjob <= 3214] <- 6
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 3252] <- 6
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 3500 &  ess2.5.10$isco_mainjob <= 3522] <- 6

# Skilled manual (7)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 6000 &  ess2.5.10$isco_mainjob <= 7549] <- 7
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 8310 &  ess2.5.10$isco_mainjob <= 8312] <- 7
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 8330] <- 7
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 8332 &  ess2.5.10$isco_mainjob <= 8340] <- 7
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 8342 &  ess2.5.10$isco_mainjob <= 8344] <- 7

# Low-skilled manual (8)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 8000 &  ess2.5.10$isco_mainjob <= 8300] <- 8
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 8320 &  ess2.5.10$isco_mainjob <= 8321] <- 8
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 8341] <- 8
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 8350] <- 8
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 9200 &  ess2.5.10$isco_mainjob <= 9334] <- 8
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 9600 &  ess2.5.10$isco_mainjob <= 9620] <- 8
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 9622 &  ess2.5.10$isco_mainjob <= 9629] <- 8

# Higher-grade managers and administrators (9)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 1000 &  ess2.5.10$isco_mainjob <= 1300] <- 9
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 1320 &  ess2.5.10$isco_mainjob <= 1349] <- 9
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2400 &  ess2.5.10$isco_mainjob <= 2432] <- 9
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2610 &  ess2.5.10$isco_mainjob <= 2619] <- 9
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 2631] <- 9
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 100 &  ess2.5.10$isco_mainjob <= 110] <- 9

# Lower-grade managers and administrators (10)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 1310 &  ess2.5.10$isco_mainjob <= 1312] <- 10
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 1400 &  ess2.5.10$isco_mainjob <= 1439] <- 10
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2433 &  ess2.5.10$isco_mainjob <= 2434] <- 10
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 3300 &  ess2.5.10$isco_mainjob <= 3339] <- 10
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 3343] <- 10
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 3350 &  ess2.5.10$isco_mainjob <= 3359] <- 10
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 3411] <- 10
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 5221] <- 10
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 200 &  ess2.5.10$isco_mainjob <= 210] <- 10

# Skilled clerks (11)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 3340 &  ess2.5.10$isco_mainjob <= 3342] <- 11
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 3344] <- 11
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 4000 &  ess2.5.10$isco_mainjob <= 4131] <- 11
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 4200 &  ess2.5.10$isco_mainjob <= 4221] <- 11
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 4224 &  ess2.5.10$isco_mainjob <= 4413] <- 11
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 4415 &  ess2.5.10$isco_mainjob <= 4419] <- 11

# Unskilled clerks (12)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 4132] <- 12
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 4222] <- 12
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 4223] <- 12
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 5230] <- 12
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 9621] <- 12

# Socio-cultural professionals (13)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2200 &  ess2.5.10$isco_mainjob <= 2212] <- 13
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 2250] <- 13
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2261 &  ess2.5.10$isco_mainjob <= 2262] <- 13
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2300 &  ess2.5.10$isco_mainjob <= 2330] <- 13
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2350 &  ess2.5.10$isco_mainjob <= 2352] <- 13
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 2359] <- 13
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 2600] <- 13
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 2621] <- 13
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 2630] <- 13
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2632 &  ess2.5.10$isco_mainjob <= 2634] <- 13
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2636 &  ess2.5.10$isco_mainjob <= 2640] <- 13
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2642 &  ess2.5.10$isco_mainjob <= 2643] <- 13

# Socio-cultural semi-professionals (14)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 2163] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 2166] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2220 &  ess2.5.10$isco_mainjob <= 2240] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 2260] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2263 &  ess2.5.10$isco_mainjob <= 2269] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2340 &  ess2.5.10$isco_mainjob <= 2342] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2353 &  ess2.5.10$isco_mainjob <= 2356] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 2620] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 2622] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 2635] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 2641] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 2650 &  ess2.5.10$isco_mainjob <= 2659] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 3200] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 3220 &  ess2.5.10$isco_mainjob <= 3230] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 3250] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 3253 &  ess2.5.10$isco_mainjob <= 3257] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 3259] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 3400 &  ess2.5.10$isco_mainjob <= 3410] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 3412 &  ess2.5.10$isco_mainjob <= 3413] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 3430 &  ess2.5.10$isco_mainjob <= 3433] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 3435] <- 14
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 4414] <- 14

# Skilled service (15)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 3240] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 3251] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 3258] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 3420 &  ess2.5.10$isco_mainjob <= 3423] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 3434] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 5000 &  ess2.5.10$isco_mainjob <= 5120] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 5140 &  ess2.5.10$isco_mainjob <= 5142] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 5163] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 5165] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 5200] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 5220] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 5222 &  ess2.5.10$isco_mainjob <= 5223] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 5241 &  ess2.5.10$isco_mainjob <= 5242] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 5300 &  ess2.5.10$isco_mainjob <= 5321] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 5400 &  ess2.5.10$isco_mainjob <= 5413] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 5419] <- 15
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 8331] <- 15

# Low-skilled service (16)

ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 5130 &  ess2.5.10$isco_mainjob <= 5132] <- 16
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 5150 &  ess2.5.10$isco_mainjob <= 5162] <- 16
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 5164] <- 16
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 5169] <- 16
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 5210 &  ess2.5.10$isco_mainjob <= 5212] <- 16
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 5240] <- 16
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 5243 &  ess2.5.10$isco_mainjob <= 5249] <- 16
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 5322 &  ess2.5.10$isco_mainjob <= 5329] <- 16
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 5414] <- 16
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob == 8322] <- 16
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 9100 &  ess2.5.10$isco_mainjob <= 9129] <- 16
ess2.5.10$class16_r[ess2.5.10$selfem_mainjob == 1 & ess2.5.10$isco_mainjob >= 9400 &  ess2.5.10$isco_mainjob <= 9520] <- 16


ess2.5.10$class16_r[ess2.5.10$class16_r == -9] <- NA
val_labels(ess2.5.10$class16_r) <- c("Large employers" = 1,
                             "Self-employed professionals" = 2,
                             "Small business owners with employees" = 3,
                             "Small business owners without employees" = 4,
                             "Technical experts" = 5,
                             "Technicians" = 6,
                             "Skilled manual" = 7,
                             "Low-skilled manual" = 8,
                             "Higher-grade managers and administrators" = 9,
                             "Lower-grade managers and administrators" = 10,
                             "Skilled clerks" = 11,
                             "Unskilled clerks" = 12,
                             "Socio-cultural professionals" = 13,
                             "Socio-cultural semi-professionals" = 14,
                             "Skilled service" = 15,
                             "Low-skilled service" = 16)
var_label(ess2.5.10$class16_r) <- "Respondent's Oesch class position - 16 classes"
freq(ess2.5.10$class16_r, total = T)


ess2.5.10$class8_r <- NA
ess2.5.10$class8_r[ess2.5.10$class16_r <= 2] <- 1
ess2.5.10$class8_r[ess2.5.10$class16_r == 3 | ess2.5.10$class16_r == 4] <- 2
ess2.5.10$class8_r[ess2.5.10$class16_r == 5 | ess2.5.10$class16_r == 6] <- 3
ess2.5.10$class8_r[ess2.5.10$class16_r == 7 | ess2.5.10$class16_r == 8] <- 4
ess2.5.10$class8_r[ess2.5.10$class16_r == 9 | ess2.5.10$class16_r == 10] <- 5
ess2.5.10$class8_r[ess2.5.10$class16_r == 11 | ess2.5.10$class16_r == 12] <- 6
ess2.5.10$class8_r[ess2.5.10$class16_r == 13 | ess2.5.10$class16_r == 14] <- 7
ess2.5.10$class8_r[ess2.5.10$class16_r == 15 | ess2.5.10$class16_r == 16] <- 8
val_labels(ess2.5.10$class8_r) <- c("Self-employed professionals and large employers" = 1,
                            "Small business owners" = 2,
                            "Technical (semi-)professionals" = 3,
                            "Production workers" = 4,
                            "(Associate) managers" = 5,
                            "Clerks" = 6,
                            "Socio-cultural (semi-)professionals" = 7,
                            "Service workers" = 8)
var_label(ess2.5.10$class8_r) <- "Respondent's Oesch class position - 8 classes"
freq(ess2.5.10$class8_r, total = T)



ess2.5.10$class5_r <- NA
ess2.5.10$class5_r[ess2.5.10$class16_r <= 2 | ess2.5.10$class16_r == 5 | ess2.5.10$class16_r == 9 | ess2.5.10$class16_r == 13] <- 1
ess2.5.10$class5_r[ess2.5.10$class16_r == 6 | ess2.5.10$class16_r == 10 | ess2.5.10$class16_r == 14] <- 2
ess2.5.10$class5_r[ess2.5.10$class16_r == 3 | ess2.5.10$class16_r == 4] <- 3
ess2.5.10$class5_r[ess2.5.10$class16_r == 7 | ess2.5.10$class16_r == 11 | ess2.5.10$class16_r == 15] <- 4
ess2.5.10$class5_r[ess2.5.10$class16_r == 8 | ess2.5.10$class16_r == 12 | ess2.5.10$class16_r == 16] <- 5
val_labels(ess2.5.10$class5_r) <- c("Higher-grade service class" = 1,
                            "Lower-grade service class" = 2,
                            "Small business owners" = 3,
                            "Skilled workers" = 4,
                            "Unskilled workers" = 5)
var_label(ess2.5.10$class5_r) <- "Respondent's Oesch class position - 5 classes"
freq(ess2.5.10$class5_r, total = T)


#######################################################################################
# Partner's Oesch class position
# Recode and create variables used to construct class variable for partners
# Variables used to construct class variable for partners: isco08p, emprelp
#######################################################################################

#### Recode occupation variable (isco88 com 4-digit) for partners


tail(freq(ess2.5.10$isco08p, total = T))

ess2.5.10$isco_partner <- ess2.5.10$isco08p
ess2.5.10$isco_partner[is.na(ess2.5.10$isco_partner)] <- -9
var_label(ess2.5.10$isco_partner) <- "Current occupation of partner - isco08 4-digit"

head(freq(ess2.5.10$isco_partner, total = T))

#### Recode employment status for partners

freq(ess2.5.10$emprelp, total = T)

ess2.5.10$selfem_partner <- NA
ess2.5.10$selfem_partner[ess2.5.10$emprelp == 1 | ess2.5.10$emprelp == 6 | ess2.5.10$emprelp == 7 | ess2.5.10$emprelp == 8 | ess2.5.10$emprelp == 9 | is.na(ess2.5.10$emprelp)] <- 1
ess2.5.10$selfem_partner[ess2.5.10$emprelp == 2 | ess2.5.10$emprelp == 3] <- 2
val_labels(ess2.5.10$selfem_partner) <- c("Not self-employed" = 1,
                                  "Self-employed" = 2)
var_label(ess2.5.10$selfem_partner) <- "Employment status for partners"
freq(ess2.5.10$selfem_partner, total = T)


############################################
# Create Oesch class schema for partners
############################################

ess2.5.10$class16_p <- -9

# Large employers (1)



# Self-employed professionals (2)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2000 & ess2.5.10$isco_partner <= 2162] <- 2
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2164 & ess2.5.10$isco_partner <= 2165] <- 2
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2200 & ess2.5.10$isco_partner <= 2212] <- 2
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner == 2250] <- 2
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2261 & ess2.5.10$isco_partner <= 2262] <- 2
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2300 & ess2.5.10$isco_partner <= 2330] <- 2
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2350 & ess2.5.10$isco_partner <= 2352] <- 2
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2359 & ess2.5.10$isco_partner <= 2432] <- 2
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2500 & ess2.5.10$isco_partner <= 2619] <- 2
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner == 2621] <- 2
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2630 & ess2.5.10$isco_partner <= 2634] <- 2
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2636 & ess2.5.10$isco_partner <= 2640] <- 2
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2642 & ess2.5.10$isco_partner <= 2643] <- 2

# Small business owners with employees (3)



# Small business owners without employees (4)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 1000 & ess2.5.10$isco_partner <= 1439] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner == 2163] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner == 2166] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2220 & ess2.5.10$isco_partner <= 2240] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner == 2260] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2263 & ess2.5.10$isco_partner <= 2269] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2340 & ess2.5.10$isco_partner <= 2342] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2353 & ess2.5.10$isco_partner <= 2356] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2433 & ess2.5.10$isco_partner <= 2434] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner == 2620] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner == 2622] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner == 2635] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner == 2641] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 2650 & ess2.5.10$isco_partner <= 2659] <- 4
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 2 & ess2.5.10$isco_partner >= 3000 & ess2.5.10$isco_partner <= 9629] <- 4

# Technical experts (5)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2100 & ess2.5.10$isco_partner <= 2162] <- 5
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2164 & ess2.5.10$isco_partner <= 2165] <- 5
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2500 & ess2.5.10$isco_partner <= 2529] <- 5


# Technicians (6)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 3100 & ess2.5.10$isco_partner <= 3155] <- 6
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 3210 & ess2.5.10$isco_partner <= 3214] <- 6
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 3252] <- 6
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 3500 & ess2.5.10$isco_partner <= 3522] <- 6

# Skilled manual (7)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 6000 & ess2.5.10$isco_partner <= 7549] <- 7
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 8310 & ess2.5.10$isco_partner <= 8312] <- 7
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 8330] <- 7
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 8332 & ess2.5.10$isco_partner <= 8340] <- 7
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 8342 & ess2.5.10$isco_partner <= 8344] <- 7

# Low-skilled manual (8)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 8000 & ess2.5.10$isco_partner <= 8300] <- 8
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 8320 & ess2.5.10$isco_partner <= 8321] <- 8
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 8341] <- 8
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 8350] <- 8
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 9200 & ess2.5.10$isco_partner <= 9334] <- 8
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 9600 & ess2.5.10$isco_partner <= 9620] <- 8
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 9622 & ess2.5.10$isco_partner <= 9629] <- 8

# Higher-grade managers and administrators (9)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 1000 & ess2.5.10$isco_partner <= 1300] <- 9
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 1320 & ess2.5.10$isco_partner <= 1349] <- 9
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2400 & ess2.5.10$isco_partner <= 2432] <- 9
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2610 & ess2.5.10$isco_partner <= 2619] <- 9
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 2631] <- 9
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 100 & ess2.5.10$isco_partner <= 110] <- 9

# Lower-grade managers and administrators (10)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 1310 & ess2.5.10$isco_partner <= 1312] <- 10
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 1400 & ess2.5.10$isco_partner <= 1439] <- 10
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2433 & ess2.5.10$isco_partner <= 2434] <- 10
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 3300 & ess2.5.10$isco_partner <= 3339] <- 10
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 3343] <- 10
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 3350 & ess2.5.10$isco_partner <= 3359] <- 10
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 3411] <- 10
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 5221] <- 10
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 200 & ess2.5.10$isco_partner <= 210] <- 10

# Skilled clerks (11)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 3340 & ess2.5.10$isco_partner <= 3342] <- 11
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 3344] <- 11
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 4000 & ess2.5.10$isco_partner <= 4131] <- 11
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 4200 & ess2.5.10$isco_partner <= 4221] <- 11
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 4224 & ess2.5.10$isco_partner <= 4413] <- 11
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 4415 & ess2.5.10$isco_partner <= 4419] <- 11

# Unskilled clerks (12)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 4132] <- 12
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 4222] <- 12
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 4223] <- 12
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 5230] <- 12
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 9621] <- 12

# Socio-cultural professionals (13)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2200 & ess2.5.10$isco_partner <= 2212] <- 13
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 2250] <- 13
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2261 & ess2.5.10$isco_partner <= 2262] <- 13
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2300 & ess2.5.10$isco_partner <= 2330] <- 13
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2350 & ess2.5.10$isco_partner <= 2352] <- 13
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 2359] <- 13
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 2600] <- 13
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 2621] <- 13
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 2630] <- 13
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2632 & ess2.5.10$isco_partner <= 2634] <- 13
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2636 & ess2.5.10$isco_partner <= 2640] <- 13
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2642 & ess2.5.10$isco_partner <= 2643] <- 13

# Socio-cultural semi-professionals (14)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 2163] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 2166] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2220 & ess2.5.10$isco_partner <= 2240] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 2260] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2263 & ess2.5.10$isco_partner <= 2269] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2340 & ess2.5.10$isco_partner <= 2342] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2353 & ess2.5.10$isco_partner <= 2356] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 2620] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 2622] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 2635] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 2641] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 2650 & ess2.5.10$isco_partner <= 2659] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 3200] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 3220 & ess2.5.10$isco_partner <= 3230] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 3250] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 3253 & ess2.5.10$isco_partner <= 3257] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 3259] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 3400 & ess2.5.10$isco_partner <= 3410] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 3412 & ess2.5.10$isco_partner <= 3413] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 3430 & ess2.5.10$isco_partner <= 3433] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 3435] <- 14
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 4414] <- 14

# Skilled service (15)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 3240] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 3251] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 3258] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 3420 & ess2.5.10$isco_partner <= 3423] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 3434] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 5000 & ess2.5.10$isco_partner <= 5120] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 5140 & ess2.5.10$isco_partner <= 5142] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 5163] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 5165] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 5200] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 5220] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 5222 & ess2.5.10$isco_partner <= 5223] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 5241 & ess2.5.10$isco_partner <= 5242] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 5300 & ess2.5.10$isco_partner <= 5321] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 5400 & ess2.5.10$isco_partner <= 5413] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 5419] <- 15
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 8331] <- 15

# Low-skilled service (16)

ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 5130 & ess2.5.10$isco_partner <= 5132] <- 16
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 5150 & ess2.5.10$isco_partner <= 5162] <- 16
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 5164] <- 16
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 5169] <- 16
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 5210 & ess2.5.10$isco_partner <= 5212] <- 16
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 5240] <- 16
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 5243 & ess2.5.10$isco_partner <= 5249] <- 16
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 5322 & ess2.5.10$isco_partner <= 5329] <- 16
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 5414] <- 16
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner == 8322] <- 16
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 9100 & ess2.5.10$isco_partner <= 9129] <- 16
ess2.5.10$class16_p[ess2.5.10$selfem_partner == 1 & ess2.5.10$isco_partner >= 9400 & ess2.5.10$isco_partner <= 9520] <- 16


ess2.5.10$class16_p[ess2.5.10$class16_p == -9] <- NA
val_labels(ess2.5.10$class16_p) <- c("Large employers" = 1,
                             "Self-employed professionals" = 2,
                             "Small business owners with employees" = 3,
                             "Small business owners without employees" = 4,
                             "Technical experts" = 5,
                             "Technicians" = 6,
                             "Skilled manual" = 7,
                             "Low-skilled manual" = 8,
                             "Higher-grade managers and administrators" = 9,
                             "Lower-grade managers and administrators" = 10,
                             "Skilled clerks" = 11,
                             "Unskilled clerks" = 12,
                             "Socio-cultural professionals" = 13,
                             "Socio-cultural semi-professionals" = 14,
                             "Skilled service" = 15,
                             "Low-skilled service" = 16)
var_label(ess2.5.10$class16_p) <- "Partner's Oesch class position - 16 classes"
freq(ess2.5.10$class16_p, total = T)


ess2.5.10$class8_p <- NA
ess2.5.10$class8_p[ess2.5.10$class16_p <= 2] <- 1
ess2.5.10$class8_p[ess2.5.10$class16_p == 3 | ess2.5.10$class16_p == 4] <- 2
ess2.5.10$class8_p[ess2.5.10$class16_p == 5 | ess2.5.10$class16_p == 6] <- 3
ess2.5.10$class8_p[ess2.5.10$class16_p == 7 | ess2.5.10$class16_p == 8] <- 4
ess2.5.10$class8_p[ess2.5.10$class16_p == 9 | ess2.5.10$class16_p == 10] <- 5
ess2.5.10$class8_p[ess2.5.10$class16_p == 11 | ess2.5.10$class16_p == 12] <- 6
ess2.5.10$class8_p[ess2.5.10$class16_p == 13 | ess2.5.10$class16_p == 14] <- 7
ess2.5.10$class8_p[ess2.5.10$class16_p == 15 | ess2.5.10$class16_p == 16] <- 8
val_labels(ess2.5.10$class8_p) <- c("Self-employed professionals and large employers" = 1,
                            "Small business owners" = 2,
                            "Technical (semi-)professionals" = 3,
                            "Production workers" = 4,
                            "(Associate) managers" = 5,
                            "Clerks" = 6,
                            "Socio-cultural (semi-)professionals" = 7,
                            "Service workers" = 8)
var_label(ess2.5.10$class8_p) <- "Partner's Oesch class position - 8 classes"
freq(ess2.5.10$class8_p, total = T)


ess2.5.10$class5_p <- NA
ess2.5.10$class5_p[ess2.5.10$class16_p <= 2 | ess2.5.10$class16_p == 5 | ess2.5.10$class16_p == 9 | ess2.5.10$class16_p == 13] <- 1
ess2.5.10$class5_p[ess2.5.10$class16_p == 6 | ess2.5.10$class16_p == 10 | ess2.5.10$class16_p == 14] <- 2
ess2.5.10$class5_p[ess2.5.10$class16_p == 3 | ess2.5.10$class16_p == 4] <- 3
ess2.5.10$class5_p[ess2.5.10$class16_p == 7 | ess2.5.10$class16_p == 11 | ess2.5.10$class16_p == 15] <- 4
ess2.5.10$class5_p[ess2.5.10$class16_p == 8 | ess2.5.10$class16_p == 12 | ess2.5.10$class16_p == 16] <- 5
val_labels(ess2.5.10$class5_p) <- c("Higher-grade service class" = 1,
                            "Lower-grade service class" = 2,
                            "Small business owners" = 3,
                            "Skilled workers" = 4,
                            "Unskilled workers" = 5)
var_label(ess2.5.10$class5_p) <- "Partner's Oesch class position - 5 classes"
freq(ess2.5.10$class5_p, total = T)


####################################################################################################
# Final Oesch class position
# Merge two class variables (respondents and partners)
# Assign the partner's Oesch class position when the respondent's Oesch class position is missing:
####################################################################################################

ess2.5.10$class16 <- ifelse(!is.na(ess2.5.10$class16_r), ess2.5.10$class16_r, ess2.5.10$class16_p)

val_labels(ess2.5.10$class16) <- c("Large employers" = 1,
                           "Self-employed professionals" = 2,
                           "Small business owners with employees" = 3,
                           "Small business owners without employees" = 4,
                           "Technical experts" = 5,
                           "Technicians" = 6,
                           "Skilled manual" = 7,
                           "Low-skilled manual" = 8,
                           "Higher-grade managers and administrators" = 9,
                           "Lower-grade managers and administrators" = 10,
                           "Skilled clerks" = 11,
                           "Unskilled clerks" = 12,
                           "Socio-cultural professionals" = 13,
                           "Socio-cultural semi-professionals" = 14,
                           "Skilled service" = 15,
                           "Low-skilled service" = 16)
var_label(ess2.5.10$class16) <- "Final Oesch class position - 16 classes"
freq(ess2.5.10$class16, total = T)


ess2.5.10$class8 <- NA
ess2.5.10$class8[ess2.5.10$class16 <= 2] <- 1
ess2.5.10$class8[ess2.5.10$class16 == 3 | ess2.5.10$class16 == 4] <- 2
ess2.5.10$class8[ess2.5.10$class16 == 5 | ess2.5.10$class16 == 6] <- 3
ess2.5.10$class8[ess2.5.10$class16 == 7 | ess2.5.10$class16 == 8] <- 4
ess2.5.10$class8[ess2.5.10$class16 == 9 | ess2.5.10$class16 == 10] <- 5
ess2.5.10$class8[ess2.5.10$class16 == 11 | ess2.5.10$class16 == 12] <- 6
ess2.5.10$class8[ess2.5.10$class16 == 13 | ess2.5.10$class16 == 14] <- 7
ess2.5.10$class8[ess2.5.10$class16 == 15 | ess2.5.10$class16 == 16] <- 8
val_labels(ess2.5.10$class8) <- c("Self-employed professionals and large employers" = 1,
                          "Small business owners" = 2,
                          "Technical (semi-)professionals" = 3,
                          "Production workers" = 4,
                          "(Associate) managers" = 5,
                          "Clerks" = 6,
                          "Socio-cultural (semi-)professionals" = 7,
                          "Service workers" = 8)
var_label(ess2.5.10$class8) <- "Final Oesch class position - 8 classes"
freq(ess2.5.10$class8, total = T)


ess2.5.10$class5 <- NA
ess2.5.10$class5[ess2.5.10$class16 <= 2 | ess2.5.10$class16 == 5 | ess2.5.10$class16 == 9 | ess2.5.10$class16 == 13] <- 1
ess2.5.10$class5[ess2.5.10$class16 == 6 | ess2.5.10$class16 == 10 | ess2.5.10$class16 == 14] <- 2
ess2.5.10$class5[ess2.5.10$class16 == 3 | ess2.5.10$class16 == 4] <- 3
ess2.5.10$class5[ess2.5.10$class16 == 7 | ess2.5.10$class16 == 11 | ess2.5.10$class16 == 15] <- 4
ess2.5.10$class5[ess2.5.10$class16 == 8 | ess2.5.10$class16 == 12 | ess2.5.10$class16 == 16] <- 5
val_labels(ess2.5.10$class5) <- c("Higher-grade service class" = 1,
                          "Lower-grade service class" = 2,
                          "Small business owners" = 3,
                          "Skilled workers" = 4,
                          "Unskilled workers" = 5)
var_label(ess2.5.10$class5) <- "Final Oesch class position - 5 classes"
freq(ess2.5.10$class5, total = T)

ess2.5.10 <- subset(ess2.5.10, select = -c(isco_mainjob, emplrel_r, emplno_r, selfem_mainjob, isco_partner, selfem_partner))


####################################################################################################
# Convert all labelled variables (haven_labelled class) to factors
# To convert a specific labelled variable to a factor: ess2.5.10$class16 <- to_factor(ess2.5.10$class16, drop_unused_labels = TRUE)
# The levels argument allows to specify what should be used as the factor levels, the labels (default), the values or the labels prefixed with values
# Example with the labels prefixed with values: ess2.5.10$class16 <- to_factor(ess2.5.10$class16, drop_unused_labels = TRUE, levels = "p")
####################################################################################################

ess2.5.10 <-  unlabelled(ess2.5.10, drop_unused_labels = TRUE)

##################################
# End
##################################

