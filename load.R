library(dplyr)
library(toastui)
library(DT)
library(lubridate)
library(anytime)
library(stringr)
library(readxl)

###############
## Publis #####
###############


createpublis <- function(publis) { # create a function with the name my_function
  publis$AllAuteurs <- ''
  publis$nbAuteurs <- 6-rowSums(publis[,1:6]==".")
  for(j in 1:nrow(publis)){
    AllAuteurs <- publis[j,1]
    if(publis$nbAuteurs[j] > 1){
      for(i in 2:publis$nbAuteurs[j]){
        NewAuteur <- publis[j,i]
        if(i<publis$nbAuteurs[j]) AllAuteurs <- paste0(AllAuteurs, ', ', NewAuteur) 
        if(i==publis$nbAuteurs[j]) AllAuteurs <- paste0(AllAuteurs, ' & ', NewAuteur) 
      }
    }
    publis$AllAuteurs[j] <- AllAuteurs
  }
  
  publis$AllAuteurs <- paste0(publis$AllAuteurs, ' (', publis$Année, ')')
  publis$Lien <- paste0("<a href='",publis$Lien,"' target='_blank'>",publis$Title,"</a>")
  publis$Pages <- ifelse(is.na(publis$Pages), '', publis$Pages)
  
  publis$Journal2 <- paste0(publis$Journal,',')
  publis$Journal2 <- ifelse(publis$Etat == 'Soumis', 'Soumis pour publication', publis$Journal2)
  publis$Journal2 <- ifelse(publis$Etat == 'Projet', '[Projet en cours]', publis$Journal2)
  publis$Journal2 <- ifelse(publis$Etat == 'Attente', paste0('Publication prochaine dans ', publis$Journal), publis$Journal2)
  return(publis)
}

#data <- read.csv(file="csv_files/Publications.csv") 
#Publis <- createpublis(data) %>%
#  arrange(-Année)

data <- read_excel("xls_files/Publications.xlsx") 
Publis <- createpublis(data) %>%
  arrange(-Année)

createpublis2 <- function(publis) { # create a function with the name my_function
  publis$AllAuteurs <- ''
  publis$nbAuteurs <- 6-rowSums(publis[,1:6]==".")
  for(j in 1:nrow(publis)){
    AllAuteurs <- publis[j,1]
    if(publis$nbAuteurs[j] > 1){
      for(i in 2:publis$nbAuteurs[j]){
        NewAuteur <- publis[j,i]
        if(i<publis$nbAuteurs[j]) AllAuteurs <- paste0(AllAuteurs, ', ', NewAuteur) 
        if(i==publis$nbAuteurs[j]) AllAuteurs <- paste0(AllAuteurs, ' & ', NewAuteur) 
      }
    }
    publis$AllAuteurs[j] <- AllAuteurs
  }
  
  publis$Lien <- paste0("<a href='",publis$Lien,"' target='_blank'>",publis$Title,"</a>")
  publis$Pages <- ifelse(is.na(publis$Pages), '', publis$Pages)
  
  publis$Journal2 <- paste0(publis$Journal,',')
  publis$Journal2 <- ifelse(publis$Etat == 'Soumis', 'Soumis pour publication', publis$Journal2)
  publis$Journal2 <- ifelse(publis$Etat == 'Projet', '[Projet en cours]', publis$Journal2)
  publis$Journal2 <- ifelse(publis$Etat == 'Attente', paste0('Publication prochaine dans ', publis$Journal), publis$Journal2)
  return(publis)
}

#data <- read.csv(file="csv_files/Vulgarisation.csv") 
#Vulgarisation <- createpublis2(data) %>%
#  arrange(-Année)

data <- read_excel("xls_files/Vulgarisation.xlsx") 
Vulgarisation <- createpublis2(data) %>%
  arrange(-Année)

###################
## Présentations ##
###################

pres <- read_excel("xls_files/Presentations.xlsx") %>%
  filter(is.na(Local)) %>%
  arrange(start) %>%
  mutate(num = row_number()) %>%
  arrange(-num)  

#pres <- read.csv(file="csv_files/Presentations.csv") %>%
#  filter(is.na(Local)) %>%
#  arrange(start) %>%
#  mutate(num = row_number()) %>%
#  arrange(-num)  

pres$Pays2 <- ifelse(is.na(pres$Etat), pres$Pays, paste0(pres$Pays, ' (', pres$Etat, ')'))
pres$Ville2 <- ifelse(pres$Ville=="Virtuel", pres$Ville, paste(pres$Ville, pres$Pays2, sep=', '))

pres$mois <- month(pres$start)
pres$mois2 <- factor(pres$mois, levels = 1:12, 
                     labels = c('janvier', 'février', 'mars', 'avril', 'mai', 'juin', 'juillet', 
                                'août', 'septembre', 'octobre', 'novembre', 'décembre'), ordered = TRUE)

pres$Day <- as.character(day(pres$start)) 
pres$Day <- ifelse(pres$Day == '1', '1er', pres$Day) 
pres$Date <- paste(pres$Day, pres$mois2, year(pres$start))
Presentations <- pres

###########################
## Présentations Locales ##
###########################

pres <- read_excel("xls_files/Presentations.xlsx") %>%
  filter(!is.na(Local)) %>%
  arrange(start) %>%
  mutate(num = row_number()) %>%
  arrange(-num)  

#pres <- read.csv(file="csv_files/Presentations.csv") %>%
#  filter(!is.na(Local)) %>%
#  arrange(start) %>%
#  mutate(num = row_number()) %>%
#  arrange(-num)  

pres$Pays2 <- ifelse(is.na(pres$Etat), pres$Pays, paste0(pres$Pays, ' (', pres$Etat, ')'))
pres$Ville2 <- ifelse(pres$Ville=="Virtuel", pres$Ville, paste(pres$Ville, pres$Pays2, sep=', '))

pres$mois <- month(pres$start)
pres$mois2 <- factor(pres$mois, levels = 1:12, 
                     labels = c('janvier', 'février', 'mars', 'avril', 'mai', 'juin', 'juillet', 
                                'août', 'septembre', 'octobre', 'novembre', 'décembre'), ordered = TRUE)

pres$Day <- as.character(day(pres$start)) 
pres$Day <- ifelse(pres$Day == '1', '1er', pres$Day) 
pres$Date <- paste(pres$Day, pres$mois2, year(pres$start))
PresLocales <- pres

#############################
## Cours et Démonstrations ##
#############################

cd <- read_excel("xls_files/CoursDemos.xlsx") 
#cd <- read.csv(file="csv_files/CoursDemos.csv") 

cd$Fulltitle <- paste(cd$Sigle, cd$Titre, sep=' - ')
cd$Lien <- paste0("<a href='",cd$Lien,"' target='_blank'>",cd$Fulltitle,"</a>")
cd$Fullsession <- paste0(substr(cd$Session, 1, 1), cd$Annee)
  
cd <- cd  %>%
  arrange(Sigle, Annee, desc(Session))
  
bycours <- cd %>%
  group_by(Nom, Tache, Lien) %>%
  summarise(list.cours = str_c(Fullsession, collapse=", ")) %>%
  mutate(list.cours = paste0("(", list.cours, ")"))
  
############
## Autres ##
############

Autres <- read_excel("xls_files/Autres.xlsx") 
#Autres <- read.csv(file="csv_files/Autres.csv") 

##############
## Save all ##
##############

Publis <- data.frame(lapply(Publis, function(x) {
  gsub("J.-P.Boucher", "J.-P. Boucher", x)
  }))

Vulgarisation <- data.frame(lapply(Vulgarisation, function(x) {
  gsub("J.-P.Boucher", "J.-P. Boucher", x)
}))

Presentations <- data.frame(lapply(Presentations, function(x) {
  gsub("J.-P.Boucher", "J.-P. Boucher", x)
}))

PresLocales <- data.frame(lapply(PresLocales, function(x) {
  gsub("J.-P.Boucher", "J.-P. Boucher", x)
}))

bycours <- data.frame(lapply(bycours, function(x) {
  gsub("J.-P.Boucher", "J.-P. Boucher", x)
}))

Autres <- data.frame(lapply(Autres, function(x) {
  gsub("J.-P.Boucher", "J.-P. Boucher", x)
}))

Publis <- data.frame(lapply(Publis, function(x) {
  gsub("A.O.ChuisseuTchuisseu", "A.O.Chuisseu Tchuisseu", x)
}))

Vulgarisation <- data.frame(lapply(Vulgarisation, function(x) {
  gsub("A.O.ChuisseuTchuisseu", "A.O.Chuisseu Tchuisseu", x)
}))

Presentations <- data.frame(lapply(Presentations, function(x) {
  gsub("A.O.ChuisseuTchuisseu", "A.O.Chuisseu Tchuisseu", x)
}))

PresLocales <- data.frame(lapply(PresLocales, function(x) {
  gsub("A.O.ChuisseuTchuisseu", "A.O.Chuisseu Tchuisseu", x)
}))

bycours <- data.frame(lapply(bycours, function(x) {
  gsub("A.O.ChuisseuTchuisseu", "A.O.Chuisseu Tchuisseu", x)
}))

Autres <- data.frame(lapply(Autres, function(x) {
  gsub("A.O.ChuisseuTchuisseu", "A.O.Chuisseu Tchuisseu", x)
}))

####

save(Publis, file='rda_files/Publis.Rda')
save(Vulgarisation, file='rda_files/Vulgarisation.Rda')
save(Presentations, file='rda_files/Presentations.Rda')
save(PresLocales, file='rda_files/Preslocales.Rda')
save(bycours, file='rda_files/bycours.Rda')
save(Autres, file='rda_files/Autres.Rda')




