
## Install the package allowing to use fread() to read the table
## install.packages("data.table")
## install.packages("tidyverse")
## install.packages("openxlsx")
library(data.table)
library(tidyverse)
library(plyr)
library(dplyr)
library(readxl)
library(writexl)

memory.limit(size=100000)
############################Write data table in an Excel file###################
Gotoexcel <- function(data, sheet){
  setwd("N:/Risques et Assurances/Aprecialis/MISSIONS/104 - Produits/MGEFI/02 - Travail/HOZH")
  write_xlsx(data, sheet)
}
################################################################################

# set the work directory
setwd("N:/Risques et Assurances/Aprecialis/MISSIONS/104 - Produits/MGEFI/01 - Documents/MGEFI")

# import data
pop <- fread("population/export.csv", dec = ',')
data2018 <- fread("Survenance 2018.csv", dec = ',')

# 1) Data cleaning
# Check the missing value percentage of each variable
missing_value <- colMeans(is.na(data2018))
missing_value[missing_value!=0] #  209 missing value for "Ann�es Mois naissance" column
data2018 <- head(data2018, -1) #  Remove the last line which represents the sum
names(data2018) <- str_replace_all(names(data2018), c(" "="_")) #  Replace the space by "_" in the names
names(pop) <- str_replace_all(names(pop), c(" "="_"))

# Select the needed data to reduce the data size
produit <- c("MAITRI-SANTE", "MULTI-SANTE-2", "VITA-SANTE-2")
couverture <- c("MAITRI SANTE", "MULTI SANTE 2", "VITA SANTE 2")
  
presta <- data2018 %>% 
  filter(Produit_client %in% produit) %>% 
  select(Identifiant_personne_prot�g�e, Produit_client, Code_majeur, Sous_Code_majeur, `Code_acte_(Libell�)`,
         Montant_Mutuelle, Montant_RO, Montant_Frais_r�els, Montant_de_base_remboursement_RO,
         `Montant_du_d�passement_d'honoraires`, Montant_Ticket_mod�rateur, RAC)

presta2 <- presta %>% 
  filter(presta$Produit_client=="MAITRI-SANTE")

pop2 <- pop %>% 
  filter(Couverture %in% couverture) %>%
  select(Identifiant_personne_prot�g�e, Sexe, `Ann�es-Mois_de_naissance_(AAAAMM)`, Cat�gorie_statutaire,
         Qualit�_mutualiste, Lien_familial, Situation_familiale, Cat�gorie_sociale, `Date_de_d�but_d'effet_de_la_couverture`,
         `Date_de_fin_d'effet_de_la_couverture`, Cotisation_de_maintien, Couverture)

# Get the cover year
pop2 <- pop2 %>%
  mutate(Ann�e_survenance = case_when(
    `Date_de_d�but_d'effet_de_la_couverture`<"2019-01-01" & `Date_de_fin_d'effet_de_la_couverture` > "2018-01-01"      ~ "2018",
    `Date_de_d�but_d'effet_de_la_couverture`<"2020-01-01" & `Date_de_fin_d'effet_de_la_couverture` > "2019-01-01"      ~ "2019",
    `Date_de_d�but_d'effet_de_la_couverture`<"2021-01-01" & `Date_de_fin_d'effet_de_la_couverture` > "2020-01-01"      ~ "2020",
    `Date_de_d�but_d'effet_de_la_couverture`<"2022-01-01" & `Date_de_fin_d'effet_de_la_couverture` > "2021-01-01"      ~ "2021",
    TRUE                                                                                                          ~ '')
  )

pop2 <- pop2 %>% 
  filter(pop2$Cotisation_de_maintien=="Non" & pop2$Ann�e_survenance == "2018" & pop2$Couverture=="MAITRI SANTE") %>%
  distinct(Identifiant_personne_prot�g�e, .keep_all = TRUE)

# Add a new column of the age
pop2 <- pop2 %>%
  mutate(Age = 2018 - as.numeric(substr(pop2$`Ann�es-Mois_de_naissance_(AAAAMM)`,1,4)))

# Set age classment
agebreaks <- c(0, 20, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 100)
agelabels <- c("-21","21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70","70+")
pop2$Tranche_Age <- cut(pop2$Age, breaks = agebreaks, right = FALSE, labels = agelabels)
pop2$Tranche_Age <- as.character(pop2$Tranche_Age)
  
# Classify population data "Agent/Retraite/Enfant"
pop2 <- pop2 %>% 
  mutate(Population = case_when(
    Qualit�_mutualiste=="MP Direct" & Lien_familial=="Chef de Famille" & Situation_familiale=="Chef de famille" &  Cat�gorie_sociale=="Actif"                  ~ 'Agent',
    Qualit�_mutualiste=="MP Direct" & Lien_familial=="Chef de Famille" & Situation_familiale=="Chef de famille" &  Cat�gorie_sociale=="Retraite" | Age > 70   ~ 'Retraite',
    Qualit�_mutualiste=="Ayant Droit" & Lien_familial=="Enfant/Petit Enfant" & Situation_familiale=="Enfant"                                                   ~ 'Enfant',
    TRUE                                                                                                                                                       ~ '')
  )

categorie <- read_excel("N:/Risques et Assurances/Aprecialis/MISSIONS/104 - Produits/MGEFI/02 - Travail/HOZH/Reference_categorie.xlsx", sheet = "sas")
segment <- read_excel("N:/Risques et Assurances/Aprecialis/MISSIONS/104 - Produits/MGEFI/02 - Travail/HOZH/Reference_Segment.xlsx")
pop3 <- left_join(pop2,categorie, by = "Cat�gorie_statutaire")
pop3 <- left_join(pop3, segment, by = c("Population", "Tranche_Age", "Sexe", "Cat�gorie_agent"))

df1 <- left_join(presta2, pop3, by = "Identifiant_personne_prot�g�e", copy = False)



# Grouping the lots
df1 <- df1 %>% 
  mutate(Lot = case_when(
    Montant_Frais_r�els == Montant_de_base_remboursement_RO                            ~ 'Lot1',
    Montant_Mutuelle == Montant_Frais_r�els                                            ~ 'Lot2',
    Montant_RO + Montant_Mutuelle < Montant_de_base_remboursement_RO & Montant_RO > 0  ~ 'Lot0',
    TRUE                                                                               ~ 'Lot3')
  )

result <- df1 %>% 
  group_by(Segment, Population, Tranche_Age, Lot) %>% 
  summarise(N_obs = n(),
            Prestations_Actuelles = sum(Montant_Mutuelle,na.rm=TRUE)
            )

Gotoexcel(result,"Maitri 2018.xlsx")


