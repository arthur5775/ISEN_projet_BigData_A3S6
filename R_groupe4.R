###########################################
# Projet Big Data A3 ISEN Nantes Groupe 4 #
###########################################
# Arthur GROSSMANN--LE MAUGUEN            #
# Enzo GUILLARD                           #
# Lucas BERCEGEAY                         #
###########################################








###PARTIE 0
# Importer les librairies nécessaires pour la suite du code
library(dplyr)
library(stringr)
#library(lubridate)#pour faire des opérations avec la date actuelle et en déduire des ages estimés manquants
library(png)#pour ouvir l'image au format png qui nous sert de fond de carte
library(ggplot2)#pour afficher les elements sur la carte
library(grid)#pour créer le raster qui sert de carte de fond au cartes des arbres
library(corrplot)#pour afficher la matrice corrélation/ Pearson
library(rpart)#pour créer un modèle d'arbre de décision
library(rpart.plot)#pour afficherun modèle d'arbre de décision
library(ggcorrplot)#pour afficher la matrice corrélation/ Pearson d"une autre manière
library(sf) # Ajouter la librairie sf pour la conversion des coordonnées









###PARTIE 1: Description et exploration des données:
##Nettoyage des données (valeurs manquantes, valeurs aberrantes et doublons)
# Lire le fichier CSV en entrée
Fichier <- read.csv(file = "Patrimoine_Arboré_(RO).csv")
Tableau <- data.frame(Fichier)

# Remplacer les espaces vides par NA
Tableau <- lapply(Tableau, function(x) gsub("^\\s*$", NA, x))
Tableau <- as.data.frame(Tableau)

# Supprimer les lignes parfaitement identiques
Tableau <- Tableau %>% distinct()

# Supprimer si X, Y sont dupliqués et que le fk_arb_etat est similaire
Tableau <- Tableau %>% filter(!duplicated(cbind(X, Y, fk_arb_etat)))

# Supprimer si X est NA
Tableau <- Tableau %>% filter(!is.na(X))

# Supprimer si Y est NA
Tableau <- Tableau %>% filter(!is.na(Y))

Tableau <- Tableau %>% filter(!is.na(fk_port))
Tableau <- Tableau %>% filter(!is.na(remarquable))
Tableau <- Tableau %>% filter(!is.na(feuillage))
Tableau <- Tableau %>% filter(!is.na(fk_nomtech))
Tableau <- Tableau %>% filter(!is.na(fk_prec_estim))
Tableau <- Tableau %>% filter(!is.na(src_geo))
Tableau <- Tableau %>% filter(!is.na(fk_revetement))
Tableau <- Tableau %>% filter(!is.na(haut_tot))
Tableau <- Tableau %>% filter(!is.na(haut_tronc))
Tableau <- Tableau %>% filter(!is.na(tronc_diam))
Tableau <- Tableau %>% filter(!is.na(age_estim))

#NE RENVOIE PAS DES RESULTATS UTILISABLES: SUPPRIME TROP D'ARBRES (4 000)
# Supprimer les lignes où haut_tot est inférieur ou égal à haut_tronc seulement si les deux colonnes ne sont pas NA
#Tableau <- Tableau %>% filter(haut_tot >= haut_tronc)

# Remplir la colonne src_geo de la valeur "Orthophoto" car toutes les données semblent avoir été obtenues de cette manière
t4 <- Tableau %>% mutate(src_geo = "Orthophoto")

# Modifier les valeurs d'age_estim afin de se débraasser des valeurs aberrantes 
t5 <- t4 %>%mutate(age_estim = as.numeric(age_estim), age_estim = if_else(age_estim > 300, 200, age_estim))

# Retirer les colonnes jugées inutiles
colonnes_a_retirer <- c('id_arbre', 'created_user', 'created_date', 'src_geo', 'Editor', 
                        'EditDate', 'nomlatin', 'CreationDate', 'Creator', 
                        'last_edited_date', 'last_edited_user', 'GlobalID', 'OBJECTID', 
                        'dte_plantation', 'dte_abattage', 'nomfrancais', 'commentaire_environnement')
t6 <- t5 %>% select(-one_of(colonnes_a_retirer))

# Remplacer les valeurs "Adulte" par "adulte" et "Jeune" par "jeune" dans la colonne fk_stadedev pour que l'on puisse utiliser ces données pour la partie IA
t6 <- t6 %>% mutate(fk_stadedev = str_replace(fk_stadedev, "Adulte", "adulte"))
t6 <- t6 %>% mutate(fk_stadedev = str_replace(fk_stadedev, "Jeune", "jeune"))

# Convertir les coordonnées Lambert-93 (EPSG:2154) en latitude et longitude (EPSG:4326)
# Créer un objet sf à partir des coordonnées Lambert-93
points_lambert <- st_as_sf(t6, coords = c("X", "Y"), crs = 2154)
# Transformer les coordonnées en WGS84 (EPSG:4326)
points_wgs84 <- st_transform(points_lambert, crs = 4326)
# Extraire les coordonnées transformées
df_wgs84 <- as.data.frame(st_coordinates(points_wgs84))
# Renommer les colonnes pour être plus explicite
names(df_wgs84) <- c("longitude", "latitude")
# Ajuster les coordonnées en retirant les valeurs spécifiées
df_wgs84$longitude <- df_wgs84$longitude - 17.863636
df_wgs84$latitude <- df_wgs84$latitude - 10.67772
# Combiner les nouvelles coordonnées avec votre data frame original
t6 <- cbind(t6, df_wgs84)

# Compléter les quartiers manquants grâce aux coordonnées 
# Vérifier que les colonnes X et Y sont bien des types numériques
t6$X <- as.numeric(t6$X)
t6$Y <- as.numeric(t6$Y)
# Fonction pour calculer la distance euclidienne
euclidean_distance <- function(x1, y1, x2, y2) {sqrt((x1 - x2)^2 + (y1 - y2)^2)}
# Séparer les données en deux : avec et sans quartier
data_with_quartier <- t6 %>% filter(!is.na(clc_quartier))
data_without_quartier <- t6 %>% filter(is.na(clc_quartier))
# Itérer sur les lignes sans quartier pour trouver le quartier le plus proche
for (i in 1:nrow(data_without_quartier)) {
  x <- data_without_quartier$X[i]
  y <- data_without_quartier$Y[i]
  distances <- euclidean_distance(data_with_quartier$X, data_with_quartier$Y, x, y)
  closest_quartier <- data_with_quartier$clc_quartier[which.min(distances)]
  t6$clc_quartier[is.na(t6$clc_quartier) & t6$X == x & t6$Y == y] <- closest_quartier
}

# Compléter les secteurs manquants grâce aux coordonnées 
# Vérifier que les colonnes X et Y sont bien des types numériques
t6$X <- as.numeric(t6$X)
t6$Y <- as.numeric(t6$Y)
# Fonction pour calculer la distance euclidienne
euclidean_distance <- function(x1, y1, x2, y2) {sqrt((x1 - x2)^2 + (y1 - y2)^2)}
# Séparer les données en deux : avec et sans quartier
data_with_quartier <- t6 %>% filter(!is.na(clc_secteur))
data_without_quartier <- t6 %>% filter(is.na(clc_secteur))
# Itérer sur les lignes sans quartier pour trouver le quartier le plus proche
for (i in 1:nrow(data_without_quartier)) {
  x <- data_without_quartier$X[i]
  y <- data_without_quartier$Y[i]
  distances <- euclidean_distance(data_with_quartier$X, data_with_quartier$Y, x, y)
  closest_quartier <- data_with_quartier$clc_secteur[which.min(distances)]
  t6$clc_secteur[is.na(t6$clc_secteur) & t6$X == x & t6$Y == y] <- closest_quartier
}

# Retirer les lignes où clc_quartier est "Gricourt"
t6 <- t6 %>% filter(!(clc_secteur == "Gricourt"))
t6 <- t6 %>% filter(!(clc_secteur == "Griourt"))


#NE RENVOIE PAS DES RESULTATS UTILISABLES
# Calculer date de plantation et estimation d'age manquants
# Définir la fonction pour calculer la date de plantation
#calculer_date_plantation <- function(age_estim, dte_plantation) {
# Utiliser la date actuelle comme référence
#  date_actuelle <- Sys.Date()
# Calculer la date de plantation en soustrayant l'âge estimé à la date actuelle
#  date_plantation <- ifelse(is.na(dte_plantation), format(date_actuelle - years(age_estim), "%Y/%m/%d %H:%M:%S+00"), dte_plantation)
#  return(date_plantation)
#}
# Appliquer la fonction au DataFrame pour mettre à jour la colonne dte_plantation
#t6 <- t6 %>% 
#  mutate(dte_plantation = calculer_date_plantation(age_estim, dte_plantation))
# Définir la fonction pour calculer l'âge estimé
#calculer_age_estim <- function(dte_plantation, age_estim) {
# Utiliser la date actuelle comme référence
#  date_actuelle <- Sys.Date()
# Calculer l'âge estimé en soustrayant la date de plantation à la date actuelle
#  age_estim <- ifelse(is.na(age_estim), as.integer(as.numeric(difftime(date_actuelle, as.Date(dte_plantation), units = "weeks")) / 52), age_estim)
#  return(age_estim)
#}
# Appliquer la fonction au DataFrame pour mettre à jour la colonne age_estim
#t6 <- t6 %>% 
#  mutate(age_estim = calculer_age_estim(dte_plantation, age_estim))

# Remplacer les NA de la colonne remarquable par non nous jugeons que si cet arbre l'était 
#t6 <- t6 %>% mutate(remarquable = if_else(is.na(remarquable), "Non", remarquable))

# Supprimer les problèmes de différentes notations du nom des secteurs
t6$clc_secteur <- str_to_title(tolower(t6$clc_secteur))

# Remplir les valeurs manquantes de fk_pied avec la valeur de fk_pied du même secteur
t6 <- t6 %>%
  group_by(clc_secteur) %>%
  mutate(fk_pied = ifelse(is.na(fk_pied), first(na.omit(fk_pied)), fk_pied)) %>%
  ungroup()

# Remplir les valeurs manquantes de fk_situation avec la valeur de fk_situation du même secteur
t6 <- t6 %>%
  group_by(clc_secteur) %>%
  mutate(fk_situation = ifelse(is.na(fk_situation), first(fk_situation), fk_situation)) %>%
  ungroup()

# Déduire du type d'environnement le plus présent dans le quartier, l'environement manquant d'une igne 
# Regrouper les données par quartier et compter les occurrences de chaque valeur de villeca
quartier_villeca_count <- t6 %>% 
  group_by(clc_quartier, villeca) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1)  # Sélectionner la valeur de villeca la plus fréquente pour chaque quartier
# Fusionner les données avec la valeur de villeca la plus fréquente pour chaque quartier
t6 <- t6 %>% 
  left_join(quartier_villeca_count, by = "clc_quartier") %>%
  mutate(villeca = coalesce(villeca.y, villeca.x)) %>%  # Utiliser la valeur de villeca la plus fréquente, ou celle existante si elle n'est pas disponible
  select(-starts_with("villeca."))  # Supprimer les colonnes temporaires créées pendant la jointure
# Si la valeur de villeca est encore NA, la remplacer par une valeur par défaut (par exemple, "AUTRE")
t6$villeca <- ifelse(is.na(t6$villeca), "AUTRE", t6$villeca)

#Transforme les valeurs NA du tableau par Inconnue
#t6[is.na(t6)] <- "Inconnue"
t6 <- t6 %>% mutate_if(is.character, ~ ifelse(is.na(.), "Inconnue", .))

##Statistiques descriptives univariées, bivariées
# Calcul des moyennes univariées
moyenne_age_estim <- mean(as.numeric(t4$age_estim), na.rm = TRUE)
moyenne_hauteur_totale <- mean(as.numeric(t4$haut_tot), na.rm = TRUE)
moyenne_hauteur_tronc <- mean(as.numeric(t4$haut_tronc), na.rm = TRUE)
moyenne_diametre_tronc <- mean(as.numeric(t4$tronc_diam), na.rm = TRUE)

# Calcul des médianes et quartiles
median_age_estim <- median(as.numeric(t4$age_estim), na.rm = TRUE)
q1_age_estim <- quantile(as.numeric(t4$age_estim), 0.25, na.rm = TRUE)
q3_age_estim <- quantile(as.numeric(t4$age_estim), 0.75, na.rm = TRUE)
iqr_age_estim <- IQR(as.numeric(t4$age_estim), na.rm = TRUE)
# Affichage des résultats
cat("Moyenne age_estim:", moyenne_age_estim, "\n")
cat("Médiane age_estim:", median_age_estim, "\n")
cat("1er quartile age_estim:", q1_age_estim, "\n")
cat("3e quartile age_estim:", q3_age_estim, "\n")
cat("Écart interquartile age_estim:", iqr_age_estim, "\n")

# Calcul des moyennes bivariées par nomfrancais
moyennes_par_nomfrancais <- t6 %>%
  group_by(fk_nomtech) %>%
  summarise(moyenne_age_estim = mean(as.numeric(age_estim), na.rm = TRUE),
            moyenne_haut_tot = mean(as.numeric(haut_tot), na.rm = TRUE),
            moyenne_haut_tronc = mean(as.numeric(haut_tronc), na.rm = TRUE),
            moyenne_tronc_diam = mean(as.numeric(tronc_diam), na.rm = TRUE))
print(moyennes_par_nomfrancais)

# Calcul des moyennes bivariées par quartier
moyennes_par_quartier <- t6 %>%
  group_by(clc_quartier) %>%
  summarise(moyenne_age_estim = mean(as.numeric(age_estim), na.rm = TRUE),
            moyenne_haut_tot = mean(as.numeric(haut_tot), na.rm = TRUE),
            moyenne_haut_tronc = mean(as.numeric(haut_tronc), na.rm = TRUE),
            moyenne_tronc_diam = mean(as.numeric(tronc_diam), na.rm = TRUE))
print(moyennes_par_quartier)

# Calcul des moyennes bivariées par nomfrancais avec médianes et quartiles
moyennes_par_nomfrancais <- t6 %>%
  group_by(fk_nomtech) %>%
  summarise(moyenne_age_estim = mean(as.numeric(age_estim), na.rm = TRUE),
            median_age_estim = median(as.numeric(age_estim), na.rm = TRUE),
            q1_age_estim = quantile(as.numeric(age_estim), 0.25, na.rm = TRUE),
            q3_age_estim = quantile(as.numeric(age_estim), 0.75, na.rm = TRUE),
            iqr_age_estim = IQR(as.numeric(age_estim), na.rm = TRUE),
            moyenne_haut_tot = mean(as.numeric(haut_tot), na.rm = TRUE),
            moyenne_haut_tronc = mean(as.numeric(haut_tronc), na.rm = TRUE),
            moyenne_tronc_diam = mean(as.numeric(tronc_diam), na.rm = TRUE))
print(moyennes_par_nomfrancais)

# Calcul des moyennes bivariées par quartier avec médianes et quartiles
moyennes_par_quartier <- t6 %>%
  group_by(clc_quartier) %>%
  summarise(moyenne_age_estim = mean(as.numeric(age_estim), na.rm = TRUE),
            median_age_estim = median(as.numeric(age_estim), na.rm = TRUE),
            q1_age_estim = quantile(as.numeric(age_estim), 0.25, na.rm = TRUE),
            q3_age_estim = quantile(as.numeric(age_estim), 0.75, na.rm = TRUE),
            iqr_age_estim = IQR(as.numeric(age_estim), na.rm = TRUE),
            moyenne_haut_tot = mean(as.numeric(haut_tot), na.rm = TRUE),
            moyenne_haut_tronc = mean(as.numeric(haut_tronc), na.rm = TRUE),
            moyenne_tronc_diam = mean(as.numeric(tronc_diam), na.rm = TRUE))
print(moyennes_par_quartier)









###PARTIE 2: Visualisation des données sur des graphiques
# Histogramme de la quantité d'arbres pour tous les secteurs d'un quartier
t6_filtré1 <- t6 %>% 
  filter(clc_quartier == "Quartier du Centre-Ville")
ggplot(t6_filtré1, aes(x = clc_quartier, fill = clc_secteur)) +
  geom_bar(width = 1, color="black") +
  labs(title = "Quantité d'arbres pour tous les secteurs d'un quartier, centre ville",
       x = "Les secteurs d'un quartier",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete() + 
  scale_y_continuous(labels = scales::comma)

# Créer un graphique en camembert pour la répartition des espèces
ggplot(t6_filtré1, aes(x = "", fill = fk_nomtech)) +
  geom_bar(width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Répartition des espèces d'arbres dans le Centre ville",
       fill = "Espèce (nom français)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

t6_filtré2 <- t6 %>% 
  filter(clc_quartier == "Quartier de Neuville")
ggplot(t6_filtré2, aes(x = clc_quartier, fill = clc_secteur)) +
  geom_bar(width = 1, color="black") +
  labs(title = "Quantité d'arbres pour tous les secteurs d'un quartier, Neuville",
       x = "Les secteurs d'un quartier",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete() + 
  scale_y_continuous(labels = scales::comma)

# Créer un graphique en camembert pour la répartition des espèces
ggplot(t6_filtré2, aes(x = "", fill = fk_nomtech)) +
  geom_bar(width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Répartition des espèces d'arbres dans le Quartier de Neuville",
       fill = "Espèce (nom français)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

t6_filtré3 <- t6 %>% 
  filter(clc_quartier == "HARLY")
ggplot(t6_filtré3, aes(x = clc_quartier, fill = clc_secteur)) +
  geom_bar(width = 1, color="black") +
  labs(title = "Quantité d'arbres pour tous les secteurs d'un quartier, Harly",
       x = "Les secteurs d'un quartier",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete() + 
  scale_y_continuous(labels = scales::comma)

# Créer un graphique en camembert pour la répartition des espèces
ggplot(t6_filtré3, aes(x = "", fill = fk_nomtech)) +
  geom_bar(width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Répartition des espèces d'arbres dans le quartier Harly",
       fill = "Espèce (nom français)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

t6_filtré4 <- t6 %>% 
  filter(clc_quartier == "OMISSY")
ggplot(t6_filtré4, aes(x = clc_quartier, fill = clc_secteur)) +
  geom_bar(width = 1, color="black") +
  labs(title = "Quantité d'arbres pour tous les secteurs d'un quartier, Omissy",
       x = "Les secteurs d'un quartier",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete() + 
  scale_y_continuous(labels = scales::comma)

# Créer un graphique en camembert pour la répartition des espèces
ggplot(t6_filtré4, aes(x = "", fill = fk_nomtech)) +
  geom_bar(width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Répartition des espèces d'arbres dans Omissy",
       fill = "Espèce (nom français)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

t6_filtré5 <- t6 %>% 
  filter(clc_quartier == "Quartier du faubourg d'Isle")
ggplot(t6_filtré5, aes(x = clc_quartier, fill = clc_secteur)) +
  geom_bar(width = 1, color="black") +
  labs(title = "Quantité d'arbres pour tous les secteurs d'un quartier, faubourg d'Isle",
       x = "Les secteurs d'un quartier",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete() + 
  scale_y_continuous(labels = scales::comma)

# Créer un graphique en camembert pour la répartition des espèces
ggplot(t6_filtré5, aes(x = "", fill = fk_nomtech)) +
  geom_bar(width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Répartition des espèces d'arbres dans le faubourg d'Isle",
       fill = "Espèce (nom français)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

t6_filtré6 <- t6 %>% 
  filter(clc_quartier == "Quartier du Vermandois")
ggplot(t6_filtré6, aes(x = clc_quartier, fill = clc_secteur)) +
  geom_bar(width = 1, color="black") +
  labs(title = "Quantité d'arbres pour tous les secteurs d'un quartier, Vermandois",
       x = "Les secteurs d'un quartier",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete() + 
  scale_y_continuous(labels = scales::comma)

# Créer un graphique en camembert pour la répartition des espèces
ggplot(t6_filtré6, aes(x = "", fill = fk_nomtech)) +
  geom_bar(width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Répartition des espèces d'arbres dans le Quartier Vermandois",
       fill = "Espèce (nom français)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

t6_filtré7 <- t6 %>% 
  filter(clc_quartier == "Quartier Remicourt")
ggplot(t6_filtré7, aes(x = clc_quartier, fill = clc_secteur)) +
  geom_bar(width = 1, color="black") +
  labs(title = "Quantité d'arbres pour tous les secteurs d'un quartier, Remicourt",
       x = "Les secteurs d'un quartier",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete() + 
  scale_y_continuous(labels = scales::comma)

# Créer un graphique en camembert pour la répartition des espèces
ggplot(t6_filtré7, aes(x = "", fill = fk_nomtech)) +
  geom_bar(width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Répartition des espèces d'arbres dans le Quartier Remicourt",
       fill = "Espèce (nom français)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

t6_filtré8 <- t6 %>% 
  filter(clc_quartier == "Quartier Saint-Jean")
ggplot(t6_filtré8, aes(x = clc_quartier, fill = clc_secteur)) +
  geom_bar(width = 1, color="black") +
  labs(title = "Quantité d'arbres pour tous les secteurs d'un quartier, Saint-Jean",
       x = "Les secteurs d'un quartier",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete() + 
  scale_y_continuous(labels = scales::comma)

# Créer un graphique en camembert pour la répartition des espèces
ggplot(t6_filtré8, aes(x = "", fill = fk_nomtech)) +
  geom_bar(width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Répartition des espèces d'arbres dans le Quartier Saint-Jean",
       fill = "Espèce (nom français)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

t6_filtré9 <- t6 %>% 
  filter(clc_quartier == "Quartier Saint-Martin - Oëstres")
ggplot(t6_filtré9, aes(x = clc_quartier, fill = clc_secteur)) +
  geom_bar(width = 1, color="black") +
  labs(title = "Quantité d'arbres pour tous les secteurs d'un quartier, Saint-Martin - Oëstres",
       x = "Les secteurs d'un quartier",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete() + 
  scale_y_continuous(labels = scales::comma)

# Créer un graphique en camembert pour la répartition des espèces
ggplot(t6_filtré9, aes(x = "", fill = fk_nomtech)) +
  geom_bar(width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Répartition des espèces d'arbres dans le quartier Saint-Martin - Oëstres",
       fill = "Espèce (nom français)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

t6_filtré10 <- t6 %>% 
  filter(clc_quartier == "ROUVROY")
ggplot(t6_filtré10, aes(x = clc_quartier, fill = clc_secteur)) +
  geom_bar(width = 1, color="black") +
  labs(title = "Quantité d'arbres pour tous les secteurs d'un quartier, Rouvroy",
       x = "Les secteurs d'un quartier",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete() + 
  scale_y_continuous(labels = scales::comma)

# Créer un graphique en camembert pour la répartition des espèces
ggplot(t6_filtré10, aes(x = "", fill = fk_nomtech)) +
  geom_bar(width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Répartition des espèces d'arbres dans le quartier de Rouvroy",
       fill = "Espèce (nom français)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

t6_filtré11 <- t6 %>% 
  filter(clc_quartier == "Quartier de l'Europe")
ggplot(t6_filtré11, aes(x = clc_quartier, fill = clc_secteur)) +
  geom_bar(width = 1, color="black") +
  labs(title = "Quantité d'arbres pour tous les secteurs d'un quartier, l'Europe",
       x = "Les secteurs d'un quartier",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete() + 
  scale_y_continuous(labels = scales::comma)

# Créer un graphique en camembert pour la répartition des espèces
ggplot(t6_filtré11, aes(x = "", fill = fk_nomtech)) +
  geom_bar(width = 1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Répartition des espèces d'arbres dans le quartier de l'Europe",
       fill = "Espèce (nom français)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


# Histogramme de la répartition des arbres suivant leur stade de développement
ggplot(t6, aes(x = fk_stadedev)) +
  geom_bar() +
  labs(title = "Répartition des arbres suivant leur stade de développement",
       x = "Stade de développement",
       y = "Nombre d'arbres") +
  theme_minimal()

# Histogramme de la quantité d'arbres en fonction du quartier
ggplot(t6, aes(x = clc_quartier)) +
  geom_bar() +
  labs(title = "Quantité d'arbres en fonction du quartier",
       x = "Quartier",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#RESULTAT ILISIBLE
# Histogramme de la quantité d'arbres en fonction du secteur
#ggplot(t6, aes(x = clc_secteur)) +
#  geom_bar() +
#  labs(title = "Quantité d'arbres en fonction du secteur",
#       x = "Secteur",
#       y = "Nombre d'arbres") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Histogramme de la quantité d'arbres en fonction de la situation
ggplot(t6, aes(x = fk_situation)) +
  geom_bar() +
  labs(title = "Quantité d'arbres en fonction de la situation",
       x = "Situation",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Histogramme de la quantité d'arbres en fonction du quartier et la couleur en fonction de l'état
ggplot(t6, aes(x = clc_quartier, fill = fk_arb_etat)) +
  geom_bar() +
  labs(
    title = "Quantité d'arbres en fonction du quartier",
    x = "Quartier",
    y = "Nombre d'arbres",
    fill = "Statut de l'arbre"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# Histogramme de la quantité d'arbres en fonction du quartier et la couleur en fonction de son stade de developpement
ggplot(t6, aes(x = clc_quartier, fill = fk_stadedev)) +
  geom_bar() +
  labs(
    title = "Quantité d'arbres en fonction du quartier",
    x = "Quartier",
    y = "Nombre d'arbres",
    fill = "Stade de développment de l'arbre"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# Carte qui affiche un point par quartier qui grandit par rapport au nombre d'arbres + 1 point rouge par rabre remarquable
arbres_remarquables <- t6 %>% filter(remarquable == "Oui")
autres_arbres <- t6 %>% filter(remarquable == "Non")
nombre_arbres_par_quartier <- autres_arbres %>%
  group_by(clc_quartier) %>%
  summarise(nombre = n(), .groups = 'drop')
autres_arbres <- autres_arbres %>%
  left_join(nombre_arbres_par_quartier, by = "clc_quartier")
ggplot() +
  geom_point(data = autres_arbres, aes(x = X, y = Y, size = nombre), alpha = 0.5) +
  geom_point(data = arbres_remarquables, aes(x = X, y = Y), color = "red") +
  labs(title = "Répartition des arbres par statut et quartier",
       x = "Coordonnée X",
       y = "Coordonnée Y",
       size = "Nombre d'arbres par quartier",
       color = "Statut de l'arbre") +
  theme_minimal() +
  scale_size(range = c(0.2, 2.0))









###PARTIE 3: Visualisation des données sur une carte
# Lecture de l'image
img <- readPNG("image.png")

#x <- t6$X
#y <- t6$Y
#t6$X <- as.numeric(t6$X)
#t6$Y <- as.numeric(t6$Y)

x <- t6$longitude
y <- t6$latitude
t6$X <- as.numeric(t6$longitude)
t6$Y <- as.numeric(t6$latitude)

# Créer le plot avec l'image en fond qui place tous les arbres sur une carte
plot(x, y, type = "n", xlab = "X", ylab = "Y", main = "Cartes des arbres")
rasterImage(img, min(x, na.rm = TRUE), min(y, na.rm = TRUE), max(x, na.rm = TRUE), max(y, na.rm = TRUE), interpolate = TRUE)
points(x, y, pch = 19)

# Créer le plot avec l'image en fond qui place tous les arbres sur une carte et change la couleur en fonction de leur statut
# Créer un rasterGrob (un graphique raster) de l'image
g <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
# Filtrer les données
arbres_remarquables <- t6 %>% filter(remarquable == "Oui")
autres_arbres <- t6 %>% filter(remarquable == "Non")
# Calculer le nombre d'arbres par secteur
nombre_arbres_par_secteur <- autres_arbres %>%
  group_by(clc_secteur) %>%
  summarise(nombre = n(), .groups = 'drop')
# Ajouter le nombre d'arbres aux autres arbres
autres_arbres <- autres_arbres %>%
  left_join(nombre_arbres_par_secteur, by = "clc_secteur")
# Ajouter une colonne factice pour les arbres remarquables
arbres_remarquables <- arbres_remarquables %>%
  mutate(statut = "Arbre remarquable")
# Créer le graphique
ggplot() +
  # Ajouter l'image de fond
  annotation_custom(g, xmin = min(t6$X, na.rm = TRUE), xmax = max(t6$X, na.rm = TRUE),
                    ymin = min(t6$Y, na.rm = TRUE), ymax = max(t6$Y, na.rm = TRUE)) +
  # Ajouter les points pour les autres arbres
  geom_point(data = autres_arbres, aes(x = X, y = Y, size = nombre), alpha = 0.5, color="blue") +
  # Ajouter les points pour les arbres remarquables avec la colonne factice pour la légende
  geom_point(data = arbres_remarquables, aes(x = X, y = Y, color = statut), size = 3) +
  # Ajouter les labels et la légende
  labs(title = "Répartition des arbres par statut et secteur",
       x = "Longitude",
       y = "Latitude",
       size = "Nombre d'arbres par secteur",
       color = "Statut de l'arbre") +
  theme_minimal() +
  # Définir les tailles des points
  scale_size(range = c(0.2, 2.0)) +
  # Personnaliser la légende pour les points rouges
  scale_color_manual(values = c("Arbre remarquable" = "red")) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )

#carte changeant de couleur si seuil dépassé 
g <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))
arbres_remarquables <- t6 %>% filter(remarquable == "Oui")
autres_arbres <- t6 %>% filter(remarquable == "Non")
nombre_arbres_par_quartier <- autres_arbres %>%
  group_by(clc_quartier) %>%
  summarise(nombre = n(), .groups = 'drop')
autres_arbres <- autres_arbres %>%
  left_join(nombre_arbres_par_quartier, by = "clc_quartier")
autres_arbres <- autres_arbres %>%
  mutate(couleur = case_when(
    nombre < 500 ~ "vert",
    nombre >= 500 & nombre <= 1000 ~ "jaune",
    nombre > 1000 ~ "rouge"
  ))
ggplot() +
  annotation_custom(g, xmin = min(t6$X, na.rm = TRUE), xmax = max(t6$X, na.rm = TRUE),
                    ymin = min(t6$Y, na.rm = TRUE), ymax = max(t6$Y, na.rm = TRUE)) +
  geom_point(data = autres_arbres, aes(x = X, y = Y, size = nombre, color = couleur), alpha = 0.5) +
  geom_point(data = arbres_remarquables, aes(x = X, y = Y), color = "blue") +
  labs(title = "Répartition des arbres par statut et secteur",
       x = "Longitude",
       y = "Latitude",
       size = "Nombre d'arbres par secteur",
       color = "Nombre d'arbres par secteur") +
  theme_minimal() +
  scale_size(range = c(0.2, 2.0)) +
  scale_color_manual(values = c("vert" = "green", "jaune" = "yellow", "rouge" = "red"),
                     labels = c("Moins de 500", "500 à 1000", "Plus de 1000")) +
  coord_fixed(ratio = 1) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  ) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

# Plot des arbres avec des couleurs différentes suivant leur statut, et l'image en fond
ggplot(t6, aes(x = X, y = Y, color = fk_stadedev)) +
  annotation_custom(g, xmin = min(t6$X, na.rm = TRUE), xmax = max(t6$X, na.rm = TRUE),
                    ymin = min(t6$Y, na.rm = TRUE), ymax = max(t6$Y, na.rm = TRUE)) +
  geom_point() +
  labs(title = "Répartition des arbres avec statut",
       x = "Longitude",
       y = "Latitude",
       color = "Statut de dev de l'arbre") +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  ) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)









###PARTIE 4: Etude des corrélations entre variables
# Sélectionner les colonnes numériques pertinentes
numerical_vars <- t6 %>%
  select(age_estim, haut_tot, haut_tronc, tronc_diam) %>%
  mutate(
    age_estim = as.numeric(as.character(age_estim)),
    haut_tot = as.numeric(as.character(haut_tot)),
    haut_tronc = as.numeric(as.character(haut_tronc)),
    tronc_diam = as.numeric(as.character(tronc_diam))
  )

# Calculer la matrice de covariance
cov_matrix <- cov(numerical_vars, use = "complete.obs")
print(cov_matrix)

# Sélectionner et convertir les colonnes numériques pertinentes
numerical_vars <- t6 %>%
  select(age_estim, haut_tot, haut_tronc, tronc_diam) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
# Vérifier la structure des colonnes après la conversion
str(numerical_vars)
# Calculer la matrice de corrélation
correlation_matrix <- cor(numerical_vars, use = "complete.obs")
# Afficher la matrice de corrélation
print(correlation_matrix)

# Calculer corrélation de Pearson pour les variables numériques
# Calculer la matrice de corrélation de Pearson
cor_matrix <- cor(numerical_vars, use = "complete.obs")
print(cor_matrix)
# Visualiser la matrice de corrélation
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.8, tl.col = "black", addCoef.col = "black")

# Visualiser la matrice de corrélation sous forme de heatmap avec ggcorrplot
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "upper", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("red", "white", "blue"), 
           title = "Heatmap des Corrélations entre Variables Numériques",
           ggtheme = theme_minimal())


# Test de Chi-Carré pour les variables qualitatives
# Conversion des variables qualitatives en facteur
categorical_vars <- t6 %>%
  select(fk_arb_etat, fk_stadedev, fk_situation, clc_quartier, clc_secteur) %>%
  mutate(across(everything(), as.factor))
# Effectuer le test de chi-carré
chi_square_results <- lapply(categorical_vars, function(var) {
  chisq.test(table(var, t6$age_estim))
})
# Afficher les résultats
chi_square_results

# Modèle de Régression Linéaire
# Créer un modèle de régression linéaire
lm_model <- lm(age_estim ~ haut_tot + haut_tronc + tronc_diam + fk_arb_etat + fk_stadedev + fk_situation + clc_quartier + clc_secteur, data = t6)
# Résumé du modèle
summary(lm_model)

# Calculer le taux de corrélation entre 2 des 4 variables 
t6 <- t6 %>%
  mutate(age_estim = as.numeric(age_estim),
         haut_tot = as.numeric(haut_tot),
         haut_tronc = as.numeric(haut_tronc),
         tronc_diam = as.numeric(tronc_diam))
# Sélectionner les colonnes numériques pertinentes
numerical_vars <- t6 %>%
  select(age_estim, haut_tot, haut_tronc, tronc_diam)
# Calculer la matrice de corrélation de Pearson
cor_matrix <- cor(numerical_vars, use = "complete.obs")
# Trier les corrélations pour chaque variable
sorted_cor_list <- lapply(colnames(cor_matrix), function(var) {
  sorted_cor <- sort(cor_matrix[var, ])
  return(sorted_cor)
})
# Afficher les résultats
for (i in 1:length(sorted_cor_list)) {
  cat("Corrélations pour", colnames(cor_matrix)[i], ":\n")
  print(sorted_cor_list[[i]])
  cat("\n")
}

# Trouver le lien entre le quartier et l'age de l'arbre
anova_result <- aov(age_estim ~ clc_quartier, data = t6)
summary(anova_result)

t6 <- t6 %>%
  mutate(feuillage = as.factor(feuillage),
         villeca = as.factor(villeca))
# Créer un tableau croisé
cross_table <- table(t6$feuillage, t6$villeca)
# Effectuer un test d'indépendance du chi2
chi2_test <- chisq.test(cross_table)
# Afficher le tableau croisé
print(cross_table)
# Afficher le résultat du test chi2
print(chi2_test)
# Créer un diagramme de mosaïque
mosaicplot(cross_table, main="Mosaic Plot entre Feuillage et Ville", color = TRUE)

t6 <- t6 %>%
  mutate(feuillage = as.factor(fk_revetement),
         villeca = as.factor(villeca))
# Créer un tableau croisé
cross_table <- table(t6$fk_revetement, t6$villeca)
# Effectuer un test d'indépendance du chi2
chi2_test <- chisq.test(cross_table)
# Afficher le tableau croisé
print(cross_table)
# Afficher le résultat du test chi2
print(chi2_test)
# Créer un diagramme de mosaïque
mosaicplot(cross_table, main="Mosaic Plot entre Revetement et Ville", color = TRUE)

# Relation entre l'âge estimé et la hauteur totale des arbres
ggplot(t6, aes(x = age_estim, y = haut_tot)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relation entre l'âge estimé et la hauteur totale des arbres",
       x = "Âge estimé (ans)",
       y = "Hauteur totale (m)") +
  theme_minimal()

# Relation entre la hauteur du tronc et le diamètre du tronc
ggplot(t6, aes(x = haut_tronc, y = tronc_diam)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relation entre la hauteur du tronc et le diamètre du tronc",
       x = "Hauteur du tronc (m)",
       y = "Diamètre du tronc (cm)") +
  theme_minimal()

# Relation entre l'age estimé et le diamètre du tronc
ggplot(t6, aes(x = age_estim, y = tronc_diam)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relation entre l'âge estimé de l'arbre et le diamètre du tronc",
       x = "Age estimé de l'arbre (an)",
       y = "Diamètre du tronc (cm)") +
  theme_minimal()

# Distribution entre l'age estimé et le diamètre du tronc
ggplot(t6, aes(x = as.factor(age_estim), y = tronc_diam)) +
  geom_boxplot() +
  labs(title = "Distribution entre l'âge estimé et le diamètre du tronc",
       x = "Âge estimé des arbres",
       y = "Diamètre du tronc (cm)") +
  theme_minimal()

# Relation entre l'âge estimé et le quartier
ggplot(t6, aes(x = clc_quartier, y = age_estim)) +
  geom_boxplot() +
  labs(title = "Répartition de l'âge estimé par quartier",
       x = "Quartier",
       y = "Âge estimé (ans)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Relation entre le stade de développement et la hauteur totale
ggplot(t6, aes(x = fk_stadedev, y = haut_tot)) +
  geom_boxplot() +
  labs(title = "Répartition de la hauteur totale par stade de développement",
       x = "Stade de développement",
       y = "Hauteur totale (m)") +
  theme_minimal()

# Nombre d'arbres remarquables par quartier
ggplot(t6 %>% filter(remarquable == "Oui"), aes(x = clc_quartier)) +
  geom_bar() +
  labs(title = "Nombre d'arbres remarquables par quartier",
       x = "Quartier",
       y = "Nombre d'arbres remarquables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Relation entre l'état de l'arbre et l'âge estimé
ggplot(t6, aes(x = fk_arb_etat, y = age_estim)) +
  geom_boxplot() +
  labs(title = "Répartition de l'âge estimé par état de l'arbre",
       x = "État de l'arbre",
       y = "Âge estimé (ans)") +
  theme_minimal()









###PARTIE 5: Etude des corrélations entre variables
# Créer un modèle de regression pour prédire l'age d'un arbre
# Construire le modèle de prédiction en utilisant tronc_diam comme prédicteur
modele <- lm(age_estim ~ tronc_diam, data = t6)
# Résumé du modèle
summary(modele)
# Faire des prédictions pour de nouvelles données
nouvelles_donnees <- data.frame(tronc_diam = c(10, 15, 20, 240)) # Exemple de diamètres de tronc
predictions <- predict(modele, nouvelles_donnees)
# Afficher les prédictions
predictions

# Erreur standard des résidus (RSE) :
summary(modele)$sigma
# Coefficient de détermination (R**2)
summary(modele)$r.squared

# Régression logistique
# Matrice de confusion pour prédire les arbres à abattre
t6$etat_binaire <- ifelse(t6$fk_arb_etat %in% c("ABATTU","SUPPRIMÉ","Essouché", "Non essouché"), 1, 0)
#Construction du  modèle de régression logistique
modele_logistique <- glm(etat_binaire ~ haut_tot + haut_tronc + tronc_diam + fk_stadedev, data = t6, family = binomial)
#On considère comme positif les arbres devant être abattus (fk_arb_etat %in% c("ABATTU", "Essouché", "Non essouché"))
# On prédit les probas
prediction <- predict(modele_logistique, type = "response")
#Définition des prédictions binaires
pred <- ifelse(prediction > 0.5, 1, 0)
summary(prediction)
#Construction de la matrice de confusion
conf_matrix <- table(t6$etat_binaire, pred)
print(conf_matrix)

t6$pred <- prediction
# Création du graphique
ggplot(t6, aes(x = pred, y = etat_binaire)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Vrai Label", y = "Prédiction", title = "Matrice de Confusion Normalisée") +
  theme_minimal()  # Style minimal du graphique

# Créer carte qui affiche ou notre modèle a prédit qu'il fallait abattre des arbres 
t6$predicted_useful <- predict(modele_logistique, newdata = t6, type = "response") < 0.5
# Filtrer les arbres que le modèle a prédits comme inutiles (donc à abattre)
arbres_a_abattre <- t6 %>% filter(predicted_useful == FALSE)
img <- readPNG("image.png")
g <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
# Créer la carte des arbres prédits comme inutiles
ggplot() +
  # Ajouter l'image de fond
  annotation_custom(g, xmin = min(t6$X, na.rm = TRUE), xmax = max(t6$X, na.rm = TRUE),
                    ymin = min(t6$Y, na.rm = TRUE), ymax = max(t6$Y, na.rm = TRUE)) +
  # Ajouter les points pour les arbres prédits comme inutiles
  geom_point(data = arbres_a_abattre, aes(x = X, y = Y), color = "red") +
  # Ajouter les labels et la légende
  labs(title = "Répartition des arbres prédits comme inutiles",
       x = "Longitude",
       y = "Latitude",
       color = "Statut de l'arbre") +
  theme_minimal() +
  # Personnaliser la légende pour les points rouges
  scale_color_manual(values = c("Arbre inutile" = "red")) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )


















###PARTIE 6: Export pour l’IA
#Supprimer la colonne pred et etat_binaire ajouté au tableau t6
t6 <- t6 %>% select(-pred, -etat_binaire)
t6 <- subset(t6, select = -c(X, Y, count))

write.csv(t6, "Patrimoine_Arboré_Nettoyé.csv", row.names = FALSE)
f1 <- read.csv(file = "Patrimoine_Arboré_Nettoyé.csv")
tab <- data.frame(f1)
View(tab)