#-----------------------Chargement des library et chargement des données-----------
library(tidyverse)
library(haven)
library(questionr)
library(readxl)
library(patchwork)
library(esquisse)
library(ggthemes)
library(lubridate)
library(RColorBrewer)
library(MetBrewer)
library(gtsummary)

setwd("C:/Users/Kevin//Cozy Drive/GitHub/Salaires/data")
sal_ens <- read_excel("indices_1963-2024.xlsx")
sal_ens <- read_csv2("indices_1963-2024.csv")

#-----------------------Arrangement des données-----------
sal_ens <- sal_ens %>% 
  pivot_longer(cols = starts_with(c("primeattractivite", "indice")), names_to=c('.value', "statut","grade","echelon"), 
               names_sep="_")
sal_ens <- sal_ens %>%
  pivot_longer(cols=starts_with("ir_zone"), names_to=c("zone_ir"), values_to="ir")

sal_ens$mois <- ymd(sal_ens$mois)
sal_ens$annee <- year(sal_ens$mois)
iorder(sal_ens$echelon)
sal_ens$echelon <- sal_ens$echelon %>%
  fct_relevel("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "hea1","hea2", "hea3", "heb1", "heb2", "heb3")
sal_ens$grade <- sal_ens$grade %>% fct_relevel("cn", "hc", "ex")

sal_ens$primeattractivite <- sal_ens$primeattractivite %>% replace_na(0)

#--------------Création variables salaires bruts----------------
#Traitement mensuels bruts nominaux
sal_ens <- sal_ens %>% mutate(traitement_brut = indice * pt_indice_euros/12, 
                              traitement_brut_ir = traitement_brut*(1+ir))

#Traitement annuels bruts nominaux
sal_ens <- sal_ens %>% group_by(annee, statut, grade, echelon, zone_ir) %>% 
  mutate(traitement_brut_annuel = sum(traitement_brut, na.rm=TRUE),
         traitement_brut_ir_annuel = sum(traitement_brut_ir, na.rm=TRUE))

#Traitement mensuels bruts moyens nominaux
sal_ens <- sal_ens %>% group_by(annee, statut, grade, echelon, zone_ir) %>% 
  mutate (traitement_brut_moyen = mean(traitement_brut, na.rm=TRUE),
          traitement_brut_ir_moyen = mean(traitement_brut_ir, na.rm=TRUE))

#Traitement mensuels et annuels brut réels en € 2022 (indice IPH = 112.01)
sal_ens <- sal_ens %>% mutate (traitement_brut_reel= traitement_brut / ipc_mensuel*112.01, 
                               traitement_brut_ir_reel= traitement_brut_ir / ipc_mensuel*112.01, 
                               traitement_brut_annuel_reel= traitement_brut_annuel / ipc_annuel*112.01, 
                               traitement_brut_ir_annuel_reel= traitement_brut_ir_annuel / ipc_annuel*112.01, 
                               traitement_brut_moyen_reel= traitement_brut_moyen / ipc_annuel*112.01, 
                               traitement_brut_ir_moyen_reel= traitement_brut_ir_moyen / ipc_annuel*112.01)

#--------------Création/correction variables de primes ----------------
#Calcul du montant de la contribution exceptionnelle de solidarité (1982-2017)
sal_ens$montant_cs = ((sal_ens$traitement_brut*(1+sal_ens$ir) + sal_ens$isoe_fixe) - 
                        (sal_ens$traitement_brut*sal_ens$retenue_pc + sal_ens$tranf_primes_pts + 
                         (sal_ens$isoe_fixe + sal_ens$traitement_brut*sal_ens$ir - sal_ens$tranf_primes_pts)*sal_ens$rafp))*
                      sal_ens$ces_net

#Correction ISOE Stagiaire
sal_ens <- sal_ens %>% mutate(isoe_fixe = case_when(mois < ymd("1992-09-01") ~ 0, 
                                                    (grade=="cn" & echelon =="1") ~ isoe_fixe/2, 
                                                    TRUE ~ isoe_fixe))

#Estimation du montant de l'indemnité compensatrice CSG (2018-)
sal_ens$indemn_csg <- case_when(sal_ens$grade =="cn" & sal_ens$echelon %in% c("1", "2", "3", "4") & sal_ens$mois > ymd("2017-12-01") ~ 
                                  (sal_ens$traitement_brut_ir + sal_ens$isoe_fixe + sal_ens$tranf_primes_pts + sal_ens$primeattractivite)*0.0076, 
                                sal_ens$grade =="ex" ~ ((subset(sal_ens,annee=="2017" & month(mois)=="12")$traitement_brut_annuel*3+
                                                           subset(sal_ens,annee=="2017" & month(mois)=="12")$isoe_fixe*12)*0.016702-
                                                          subset(sal_ens,annee=="2017" & month(mois)=="12")$montant_cs*12)*sal_ens$ind_csg/12, 
                                TRUE ~ ((subset(sal_ens,annee=="2017" & month(mois)=="12")$traitement_brut_annuel+
                                           subset(sal_ens,annee=="2017" & month(mois)=="12")$isoe_fixe*12)*0.016702-
                                          subset(sal_ens,annee=="2017" & month(mois)=="12")$montant_cs*12)*sal_ens$ind_csg/12)

#----------------Création des variables de rémunération brute globale---------------------------------

#Rémunération brute totale mensuelle nominale
sal_ens <- sal_ens %>% mutate(remun_brute = (traitement_brut_ir + isoe_fixe + prime_informatique + primeattractivite + indemn_csg))
                   
#Rémunération annuelle et mensuelle moyenne bruts nominaux
sal_ens <- sal_ens %>% group_by(annee, statut, grade, echelon, zone_ir) %>% mutate(remun_brute_annuelle = sum(remun_brute, na.rm=TRUE),
                                                                                   remun_brute_moyenne = mean(remun_brute, na.rm=TRUE))
                                                                                   
#Rémunération mensuelle et annuelle brute réels en € 2022 (indice IPH = 112.01)
sal_ens <- sal_ens %>% mutate (remun_brute_reelle= remun_brute / ipc_mensuel*112.01, 
                               remun_brute_annuelle_reelle= remun_brute_annuelle / ipc_annuel*112.01,
                               remun_brute_moyenne_reelle= remun_brute_moyenne / ipc_annuel*112.01)


#----------------Création des variables de salaires nets---------------------------------------------
#Calcul des salaires mensuels nets nominaux
sal_ens <- sal_ens %>% mutate (traitement_net = (traitement_brut_ir + isoe_fixe + primeattractivite + prime_informatique) - (traitement_brut*retenue_pc) - 
                                 (traitement_brut*cmdp) - (plafond_ss*cmp) -
                                 (traitement_brut_ir + isoe_fixe + primeattractivite + prime_informatique - tranf_primes_pts)*taux_csg*assiette_csg_crds -
                                 (traitement_brut_ir + isoe_fixe + primeattractivite + prime_informatique - tranf_primes_pts)*taux_crds*assiette_csg_crds -
                                 (isoe_fixe + traitement_brut*ir + primeattractivite + prime_informatique - tranf_primes_pts)*rafp - tranf_primes_pts - montant_cs + indemn_csg)

#Salaires annuels nets et salaires mensuels nets moyens nominaux
sal_ens <- sal_ens %>% group_by(annee, statut, grade, echelon, zone_ir) %>% 
  mutate (traitement_net_annuel = sum(traitement_net, nar.rm=TRUE),
          traitement_net_moyen = mean(traitement_net, nar.rm=TRUE))


#Salaires mensuels et annuels net réels en € 2022 (indice IPH = 112.01)
sal_ens <- sal_ens %>% mutate (traitement_net_reel= traitement_net / ipc_mensuel*112.01,
                               traitement_net_annuel_reel= traitement_net_annuel / ipc_annuel*112.01, 
                               traitement_net_moyen_reel= traitement_net_moyen / ipc_annuel*112.01)

#------------Création du fichier de synthèse---------------------------------------------------------------
tr_brut63_24 <- sal_ens %>% select(mois, ipc_mensuel, ipc_annuel, pt_indice_euros, statut, grade, echelon, indice, zone_ir, annee, 
                   starts_with("traitement_brut"))
remun_brute63_24 <- sal_ens %>% select(mois, ipc_mensuel, ipc_annuel, pt_indice_euros, statut, grade, echelon, indice, zone_ir, annee, 
                                       starts_with("remun_brute"))
tr_net63_24 <- sal_ens %>% select(mois, ipc_mensuel, ipc_annuel, pt_indice_euros, statut, grade, echelon, indice, zone_ir, annee, 
                                  starts_with("traitement_net"))

write_csv(tr_brut63_24, "tr_brut_63_24.csv")
write_csv(remun_brute63_24, "remun_brute_63_24.csv")
write_csv(tr_net63_24, "tr_net_63_24.csv")

#Exemple graphiques---------------
sal_ens %>% filter(annee>1990, (grade=="cn" | grade=="hc" & echelon %in% c("1", "hea1", "hea2", "hea3")), zone_ir=="ir_zone1") %>%
ggplot() + geom_line(aes(x=mois, y=traitement_net_reel, color=statut, linetype=grade)) + facet_grid(grade~echelon)

sal_ens %>% filter(annee>1990, zone_ir=="ir_zone1") %>%
  ggplot() + geom_line(aes(x=mois, y=traitement_net_reel, color=statut, linetype=grade)) + facet_grid(grade~echelon)

