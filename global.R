library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)
library(RODBC)

calcul_TS <- function(chemin, sheet1, sheet2){
  ########################## ############Import des données ###########################################################
  #doc <- file.choose()
  #name <- getSheetNames(doc) # Provide name of sheet
  con <- odbcConnectExcel2007(chemin)
  cotisation <- sqlFetch(con, sheet1)
  presta <- sqlFetch(con, sheet2)
  #cotisation <- read_excel(doc, sheet = 3)
  #presta <- read_excel(doc, sheet = 4)
  names(cotisation) <- str_replace_all(names(cotisation), c(" "="_")) #  Replace the space by "_" in the names
  names(presta) <- str_replace_all(names(presta), c(" "="_")) #  Replace the space by "_" in the names
  
  ##################################### Recap Cotisations #############################################################
  recap_cot <- cotisation %>% 
    rename(Pop = EnActivite) %>% 
    group_by(Pop, LienFamilial) %>% 
    summarise(
      Cot_HT = round(sum(Montant_HT, na.rm = TRUE)))
  
  ##################################### Recap Prestations #############################################################
  presta1 <- presta %>% 
    rename(Presta_brute = `SUM_of_ACT/GAR_MTREMBOURSEMENTRC`, Pop = `En_activité?`, LienFamilial = Lien_familial) %>% 
    mutate(Tx = case_when(
      Famille_actes == "Pharmacie" & ACT_CODEPRODUIT == "PROTECTION PH+" & (ACT_TXREMBOURSEMENTRO == 15 | ACT_TXREMBOURSEMENTRO ==30) ~ 1,
      Famille_actes == "Pharmacie" & ACT_CODEPRODUIT == "PRECISION PH+" & ACT_TXREMBOURSEMENTRO == 15  ~ 1,
      Famille_actes == "Pharmacie" & ACT_CODEPRODUIT == "PRECISION PH+" & ACT_TXREMBOURSEMENTRO == 30  ~ 0.2,
      Famille_actes == "Pharmacie" & ACT_CODEPRODUIT == "PRECAUTION PH+" & ACT_TXREMBOURSEMENTRO == 15  ~ 0.5,
      Famille_actes == "Pharmacie" & ACT_CODEPRODUIT == "PRECAUTION PH+" & ACT_TXREMBOURSEMENTRO == 30  ~ 0.2,
      Famille_actes == "Pharmacie" & ACT_CODEPRODUIT == "PERFECTION PH+" & ACT_TXREMBOURSEMENTRO == 15  ~ 0.3,
      Famille_actes == "Pharmacie" & ACT_CODEPRODUIT == "PERFECTION PH+" & ACT_TXREMBOURSEMENTRO == 30  ~ 0.2,
      TRUE ~ 0),
      Presta_retenue =as.numeric(as.character(Presta_brute)) * (1 - Tx))
  
  recap_presta <- presta1 %>% 
    group_by(Pop, LienFamilial) %>% 
    summarise(
      Prestas_retenues = round(sum(Presta_retenue, na.rm = TRUE )))
  
  ######################################## Calcul des TS ##############################################################
  TS <- inner_join(recap_cot, recap_presta, by=c("Pop", "LienFamilial"))
  TS <- TS %>% 
    mutate(Pop_cible = case_when(
      Pop == "Retraité" & LienFamilial == "Membre participant"                   ~ "Retraités MP",
      (Pop == "Actif" | Pop == "ORP/HAN") & LienFamilial == "Membre participant" ~ "Non concerné",
      TRUE                                                                       ~ "Ayants droits"))
  
  recap_TS <- TS %>% 
    filter(Pop_cible != "Non concerné") %>% 
    group_by(Pop_cible) %>% 
    summarise(Cotisations = sum(Cot_HT, na.rm = TRUE),
              Prestations = sum(Prestas_retenues, na.rm = TRUE)) %>% 
    mutate(PSAP = round(Prestations * 0.085),
           Charge_de_prestations = Prestations + PSAP,
           Majoration = round(0.1*Charge_de_prestations),
           MT_TS = Charge_de_prestations + Majoration - Cotisations)
  
  recap_TS <- rbind(recap_TS, c("Total", colSums(recap_TS[,2:7])))
  recap <- list(recap_cot, recap_presta, recap_TS)
  return(recap)
}