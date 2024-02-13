
# Graph

## FishBase

chemin_dossier1 <- "Desktop/Stage_M2/Données_brute/Orecto/FishBase/"
fichiers1 <- list.files(chemin_dossier1, full.names = TRUE)

# Créer une liste pour stocker les données de chaque fichier
liste_dataframes <- list()
a<-1
for (fichier in fichiers1) {
  # Utiliser la fonction correspondante de readr pour lire le fichier
  dataframe <- read.table(fichier, header = TRUE, sep = ",")
  
  # Extraire les noms des deux premières colonnes
  nom_colonne1 <- colnames(dataframe)[1]
  nom_colonne2 <- colnames(dataframe)[2]
  
  # Modifier tableaux
  dataframe <- dataframe %>%
    mutate(espece = paste0(nom_colonne1, " ", nom_colonne2))%>%
    mutate(source = "FishBase")
  
  
  # Renommer les colonnes
  colnames(dataframe) <- c("longitude", "latitude", "espece","source")
  
  # Stocker le dataframe dans la liste
  liste_dataframes[[basename(fichier)]] <- dataframe
  a<-a+1
}

# Accéder aux données de chaque fichier dans la liste (ici accéder aux données du premier fichier) 
print(liste_dataframes[[1]])
resultat_FishBase <- do.call(bind_rows, liste_dataframes)

## Inaturalist

chemin_dossier2 <- "Desktop/Stage_M2/Données_brute/Orecto/Inaturalist/"
fichiers2 <- list.files(chemin_dossier2, full.names = TRUE)


# Créer une liste pour stocker les données de chaque fichier
liste_dataframes <- list()
a<-1
for (fichier in fichiers2) {
  # Modifier tableaux
  dataframe <- read.table(fichier, header = TRUE, sep = "\t",fill=TRUE)
  dataframe <- dataframe[,c(10,22,23)]
  dataframe <- dataframe[,c("decimalLongitude","decimalLatitude","species")]
  
  # Modifier tableaux
  dataframe <- dataframe %>%
    mutate(source = "Inaturalist")
  
  
  # Renommer les colonnes
  colnames(dataframe) <- c("longitude", "latitude", "espece","source")
  
  # Stocker le dataframe dans la liste
  liste_dataframes[[basename(fichier)]] <- dataframe
  a<-a+1
}

# Accéder aux données de chaque fichier dans la liste (ici accéder aux données du premier fichier) 
print(liste_dataframes[[1]])
resultat_Inaturalist <- do.call(bind_rows, liste_dataframes)


## ASEAN

chemin_dossier3 <- "Desktop/Stage_M2/Données_brute/Orecto/ASEAN Fish Occurrences Literature/"
fichiers3 <- list.files(chemin_dossier3, full.names = TRUE)


# Créer une liste pour stocker les données de chaque fichier
liste_dataframes <- list()
a<-1
for (fichier in fichiers3) {
  # Modifier tableaux
  dataframe <- read.table(fichier, header = TRUE, sep = "\t")
  dataframe <- dataframe[,c(10,22,23)]
  dataframe <- dataframe[,c("decimalLongitude","decimalLatitude","species")]
  
  # Modifier tableaux
  dataframe <- dataframe %>%
    mutate(source = "ASEAN")
  
  
  # Renommer les colonnes
  colnames(dataframe) <- c("longitude", "latitude", "espece","source")
  
  # Stocker le dataframe dans la liste
  liste_dataframes[[basename(fichier)]] <- dataframe
  a<-a+1
}

# Accéder aux données de chaque fichier dans la liste (ici accéder aux données du premier fichier) 
print(liste_dataframes[[1]])
resultat_ASEAN <- do.call(bind_rows, liste_dataframes)

## ANFC

chemin_dossier4 <- "Desktop/Stage_M2/Données_brute/Orecto/Australian National Fish Collection (ANFC)/"
fichiers4 <- list.files(chemin_dossier4, full.names = TRUE)


# Créer une liste pour stocker les données de chaque fichier
liste_dataframes <- list()
a<-1
for (fichier in fichiers4) {
  # Modifier tableaux
  dataframe <- read.table(fichier, header = TRUE, sep = "\t")
  dataframe <- dataframe[,c(10,22,23)]
  dataframe <- dataframe[,c("decimalLongitude","decimalLatitude","species")]
  
  # Modifier tableaux
  dataframe <- dataframe %>%
    mutate(source = "ANFC")
  
  
  # Renommer les colonnes
  colnames(dataframe) <- c("longitude", "latitude", "espece","source")
  
  # Stocker le dataframe dans la liste
  liste_dataframes[[basename(fichier)]] <- dataframe
  a<-a+1
}

# Accéder aux données de chaque fichier dans la liste (ici accéder aux données du premier fichier) 
print(liste_dataframes[[1]])
resultat_ANFC <- do.call(bind_rows, liste_dataframes)

## Taiwan

chemin_dossier5 <- "Desktop/Stage_M2/Données_brute/Orecto/The_Fish_DataBase_of_Taiwan/"
fichiers5 <- list.files(chemin_dossier5, full.names = TRUE)


# Créer une liste pour stocker les données de chaque fichier
liste_dataframes <- list()
a<-1
for (fichier in fichiers5) {
  # Modifier tableaux
  dataframe <- read.table(fichier, header = TRUE, sep = "\t")
  dataframe <- dataframe[,c(10,22,23)]
  dataframe <- dataframe[,c("decimalLongitude","decimalLatitude","species")]
  
  # Modifier tableaux
  dataframe <- dataframe %>%
    mutate(source = "Taiwan")
  
  # Renommer les colonnes
  colnames(dataframe) <- c("longitude", "latitude", "espece","source")
  
  # Stocker le dataframe dans la liste
  liste_dataframes[[basename(fichier)]] <- dataframe
  a<-a+1
}

# Accéder aux données de chaque fichier dans la liste (ici accéder aux données du premier fichier) 
print(liste_dataframes[[1]])
resultat_Taiwan <- do.call(bind_rows, liste_dataframes)


## NMNH

chemin_dossier6 <- "Desktop/Stage_M2/Données_brute/Orecto/NMNH Extant Specimen Records/"
fichiers6 <- list.files(chemin_dossier6, full.names = TRUE)


# Créer une liste pour stocker les données de chaque fichier
liste_dataframes <- list()
a<-1
for (fichier in fichiers6) {
  # Modifier tableaux
  dataframe <- read.table(fichier, header = TRUE, sep = "\t",fill=TRUE)
  dataframe <- dataframe[,c(10,22,23)]
  dataframe <- dataframe[,c("decimalLongitude","decimalLatitude","species")]
  
  # Modifier tableaux
  dataframe <- dataframe %>%
    mutate(source = "NMNH")
  
  # Renommer les colonnes
  colnames(dataframe) <- c("longitude", "latitude", "espece","source")
  
  # Stocker le dataframe dans la liste
  liste_dataframes[[basename(fichier)]] <- dataframe
  a<-a+1
}

# Accéder aux données de chaque fichier dans la liste (ici accéder aux données du premier fichier) 
print(liste_dataframes[[1]])
resultat_NMNH <- do.call(bind_rows, liste_dataframes)

# Jeux de données final
Jeux_données_final <- rbind(resultat_FishBase,resultat_Inaturalist,resultat_ASEAN,resultat_ANFC,resultat_Taiwan,resultat_NMNH)
Jeux_données_final <- na.omit(Jeux_données_final)

# Graphiques pour chaque especes en fonction de la source

# Convertir le dataframe en objet sf
sf_data <- st_as_sf(Jeux_données_final, coords = c("longitude", "latitude"), crs = 4326)

# Télécharger les données du monde pour la carte de fond
countries <- ne_countries(scale = 50, type = "countries", continent = NULL,
                          country = NULL, geounit = NULL, sovereignty = NULL,
                          returnclass = "sf")

# Modifier type de données de certaines colonnes
sf_data$espece <- as.factor(sf_data$espece)
sf_data$source <- as.factor(sf_data$source)

# Position des requins en fonction de la source

sf_data_Ginglymostoma_cirratum <- sf_data %>%
  filter(espece == "Ginglymostoma cirratum")

# Tracer la carte avec des points colorés en fonction de l'espèce
z<- ggplot() +
  geom_sf(data = countries, fill = "808080") +
  geom_sf(data = sf_data_Ginglymostoma_cirratum,aes(color = source,text=geometry) , size = 1) +theme_void()

fig <- ggplotly(z,tooltip = "text") %>% layout(showlegend = FALSE)
fig

library(plotly)