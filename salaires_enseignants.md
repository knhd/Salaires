L’évolution des salaires enseignants
================
Kevin Hédé

## Préparation des données

Ce travail sur l’évolution des salaires enseignants s’appuie sur un
laborieux travail de collecte et de synthèse de l’évolution des
différents éléments qui interviennent dans la rémunération statutaire
des enseignant·e·s du second degré. Pour réaliser ce travail nous avons
donc retracé l’évolution des grilles indiciaires des professeurs
certifiés et agrégés depuis 1963, ainsi que l’évolution de la valeur du
point d’indice et des différements prélèvements sociaux. Les données
brutes qui vont servir de base aux calculs des salaires sont disponibles
dans le fichier “indices_1963-2022.csv” Dans ce fichier, chaque ligne
correspond à un mois et les indices de rémunération correspondant à
chaque echelon et chaque grade sont en colonne.

Ces données ont ensuite été réorganisées (*tidy data*) pour faciliter le
traitement statistique et produire des graphiques

``` r
sal_ens60 <- read_csv("indices_1963-2022.csv")
sal_ens60 <- sal_ens60 %>% 
  pivot_longer(cols = starts_with("indice"), names_to=c("statut","grade","echelon"), names_prefix = "indice_", names_sep="_", values_to="indice")
sal_ens60 <- sal_ens60 %>%
  pivot_longer(cols=starts_with("ir_zone"), names_to=c("zone_ir"), values_to="ir")
sal_ens60$mois <- ymd(sal_ens60$mois)
sal_ens60$annee <- year(sal_ens60$mois)
sal_ens60$echelon <- sal_ens60$echelon %>%
  fct_relevel("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "hea1","hea2", "hea3", "heb1", "heb2", "heb3")
```

## Calcul des salaires pour chaque echelon et chaque grade

A partir de ces données réorganisées, on peut calculer le traitement
brut des enseignants à chaque échelon de chaque grade, puis leur
rémunératon nette.

### Calcul du traitement brut

Le traitement brut annuel des enseignant·e·s, comme de l’ensemble des
fonctionnaires, est égal à l’indice nouveau majoré (instauré au
01/12/1962) associé à leur statut (corps, grade et échelon) multiplié
par la valeur du point d’indice (ou plus exactement la valeur du
traitement brut afférent à l’indice 100 divisée par 100). En divisant ce
traitement brut annuel par 12, on obtient la rémunération brute
mensuelle des enseignant·e·s. La rémunération brute des enseignant·e·s
et des fonctionnaires connaît aussi une dimension indemnité avec
l’indemnité de résidence, qui existe à un niveau résiduel aujourd’hui
(entre 0% et 3% du traitement brut selon la zone de résidence de
l’agent), mais a représenté un élement important de la rémunération
jusque dans les années 70. Le traitement brut avec intégration de
l’indemnité de résidence est donc disponible dans la variable
traitement_brut_ir

``` r
sal_ens60 <- sal_ens60 %>% mutate(traitement_brut = indice * pt_indice_euros/12)
sal_ens60 <- sal_ens60 %>% mutate(traitement_brut_ir = traitement_brut*(1+ir))
```

A partir du traitement brut mensuel perçu chaque mois, on peut alors
simuler le traitement brut annuel pour chaque échelon et chaque grade,
puis le traitement brut mensuel moyen perçu chaque année depuis 1963 par
les enseignant·e·s selon leur corps, leur grade et leur echelon.

``` r
sal_ens60 <- sal_ens60 %>% group_by(annee, statut, grade, echelon, zone_ir) %>% mutate(traitement_brut_annuel = sum(traitement_brut, na.rm=TRUE))
sal_ens60 <- sal_ens60 %>% group_by(annee, statut, grade, echelon, zone_ir) %>% mutate (traitement_brut_ir_annuel = sum(traitement_brut_ir, na.rm=TRUE))
sal_ens60 <- sal_ens60 %>% group_by(annee, statut, grade, echelon, zone_ir) %>% mutate (traitement_brut_moyen = mean(traitement_brut, na.rm=TRUE))
sal_ens60 <- sal_ens60 %>% group_by(annee, statut, grade, echelon, zone_ir) %>% mutate (traitement_brut_ir_moyen = mean(traitement_brut_ir, na.rm=TRUE))
```

Pour réaliser des comparaisons dans le temps, on peut alors calculer le
traitement brut réel (en € constant 2021) à l’aide de la série longue de
l’indice des prix à la consommation de l’INSEE, accessible ici.

``` r
sal_ens60 <- sal_ens60 %>% mutate (traitement_brut_reel= traitement_brut / ipc_mensuel*106.45)
sal_ens60 <- sal_ens60 %>% mutate (traitement_brut_ir_reel= traitement_brut_ir / ipc_mensuel*106.45)
sal_ens60 <- sal_ens60 %>% mutate (traitement_brut_annuel_reel= traitement_brut_annuel / ipc_annuel*106.45)
sal_ens60 <- sal_ens60 %>% mutate (traitement_brut_ir_annuel_reel= traitement_brut_ir_annuel / ipc_annuel*106.45)
sal_ens60 <- sal_ens60 %>% mutate (traitement_brut_moyen_reel= traitement_brut_moyen / ipc_annuel*106.45)
sal_ens60 <- sal_ens60 %>% mutate (traitement_brut_ir_moyen_reel= traitement_brut_ir_moyen / ipc_annuel*106.45)
```

On dispose donc de 12 variables de traitement brut (avec ou sans IR,
mensuel, annuel, mensuel moyen, nominal ou réel)

### Calcul de la rémunération nette statutaire des enseignant·e·s

Le calcul de la rémunération nette statutaire est plus complexe car elle
fait intervenir les primes et indemnités collectives perçues par les
enseignant·e·s mais aussi les prélèvement sociaux réalisés sur le
traitement brut. Si certains prélèvements sont assez faciles à modéliser
(retenue pour pension civile, cotisation maladie plafonnée/déplafonnée),
plusieurs dispotifs instaurés dans les années sont plus complexes. Afin
de pouvoir calculer au plus près la rémunération nette statutaire il a
donc fallu au préalable calculer et/ou corriger quatre variables **La
contribution exceptionnelle de solidarité (1982-2017)**

``` r
sal_ens60$montant_cs = ((sal_ens60$traitement_brut*(1+sal_ens60$ir) + sal_ens60$isoe_fixe) - 
                        (sal_ens60$traitement_brut*sal_ens60$retenue_pc + sal_ens60$tranf_primes_pts + 
                           (sal_ens60$isoe_fixe + sal_ens60$traitement_brut*sal_ens60$ir - sal_ens60$tranf_primes_pts)*sal_ens60$rafp))*sal_ens60$ces_net
```

**L’indemnité compensatrice de la CSG** Le calcul de l’indmenité
compensatrice de la CSG, accordée à partir du 1er janvier 2018 est
complexe car elle fait intervenir la rémunération perçue l’année
précédente. La modélisation choisie ici permet d’estimer un montant
d’indemnité fictive. On notera que la difficulté de modéliser cette
indemnité fait que le ministère de l’éducaion nationale ne l’a pas
incluse dans son simulateur de rémunération des enseignant·e·s.
<https://www.service-public.fr/particuliers/vosdroits/F34473>

``` r
sal_ens60$indemn_csg <- ((subset(sal_ens60,annee=="2017" & month(mois)=="1")$traitement_brut_ir_moyen*12+
                          subset(sal_ens60,annee=="2017" & month(mois)=="1")$isoe_fixe*12)*0.016702-
                         subset(sal_ens60,annee=="2017" & month(mois)=="1")$montant_cs*12)*sal_ens60$ind_csg/12
```

**L’ISOE** Coté primes, l’ISOE (Indemnité de suivi et d’orientation des
élèves) part fixe, instaurée à partir du 01 septembre 1992, doit être
corrigée pour les stagiaires qui ne perçoivent que la moitié de son
montant.

``` r
sal_ens60 <- sal_ens60 %>% mutate(isoe_fixe = case_when(mois < ymd("1992-09-01") ~ 0, (grade=="cn" & echelon =="1") ~ isoe_fixe/2, TRUE ~ isoe_fixe ))
```

\*\*La prime grenelle\* Surtout, suite au grenelle de l’éducation qui
s’est tenue en octobre 2020, a été instauré, à partir du 01 mai 2021 une
prime grenelle qui varie selon l’échelon et dont le montant à été
revalorisé au 01 février 2022. Il faut donc affecter son montant
différent, selon les mois et selon les echelons.

``` r
sal_ens60 <- sal_ens60 %>% mutate (prime_grenelle = case_when(mois < ymd("2021-05-01") ~ 0,  grade=="hc" ~ 0, grade=="ex" ~ 0,
                                                          mois < ymd("2022-02-01") & echelon=="2" ~ 1400/12, mois > ymd("2022-01-01") & echelon=="2" ~ 2200/12, mois > ymd("2022-01-01") & echelon=="3" ~ 2050/12, mois < ymd("2022-02-01") & echelon=="3" ~ 1250/12, mois < ymd("2022-02-01") & echelon=="4" ~ 900/12, mois > ymd("2022-01-01") & echelon=="4" ~ 1500/12, mois < ymd("2022-02-01") & echelon=="5" ~ 700/12, mois > ymd("2022-01-01") & echelon=="5" ~ 1100/12, mois < ymd("2022-02-01") & echelon=="6" ~ 500/12, mois > ymd("2022-01-01") & echelon=="6" ~ 900/12, mois < ymd("2022-02-01") & echelon=="7" ~ 500/12, mois > ymd("2022-01-01") & echelon=="7" ~ 900/12, mois > ymd("2022-01-01") & echelon=="7" ~ 900/12, mois > ymd("2022-01-01") & echelon=="8" ~ 400/12, mois > ymd("2022-01-01") & echelon=="9" ~ 400/12, TRUE ~ 0))
```
