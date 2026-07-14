library(dplyr)
library(toastui)
library(DT)
library(lubridate)
library(anytime)
library(stringr)

printPublis2 <- function(Publis){
  template <- "  
  - %s, %s, *%s* %s" 
  cat("## Publications
        ")
  for (i in seq(nrow(Publis))) {
    current <- Publis[i, ]
    cat(sprintf(template, current$AllAuteurs, current$Lien, current$Journal2, current$Pages))
  }
}

printPublis <- function(Publis){
  template <- "  
  - %s, %s, *%s* %s   

    *%s*  
    " 
  cat("## Publications 
        ")
  for (i in seq(nrow(Publis))) {
    current <- Publis[i, ]
    cat(sprintf(template, current$AllAuteurs, current$Lien, current$Journal2, current$Pages, current$Abstract))
  }
}


printPres <- function(Presentations){
  template <- 
    "
  - **%s**, *%s*, %s, %s." 
  
  cat("## Présentations scientifiques  
      ")
  for (i in seq(nrow(Presentations))) {
    current <- Presentations[i, ]
    cat(sprintf(template, current$Titre, current$Conference, current$Ville2, current$Date))
  }
}

printVulgarisation <- function(Vulgarisation){
  template <- "  
  - %s, %s, *%s* %s" 
  cat("#### Articles de vulgarisation 
        ")
  for (i in seq(nrow(Vulgarisation))) {
    current <- Vulgarisation[i, ]
    cat(sprintf(template, current$AllAuteurs, current$Lien, current$Journal2, current$Pages, current$Abstract))
  }
}


printPresLocales <- function(PresLocales){
  template <- 
    "
  - **%s**, *%s*, %s, %s." 
  
  cat("#### Présentations locales  
      ")
  for (i in seq(nrow(PresLocales))) {
    current <- PresLocales[i, ]
    cat(sprintf(template, current$Titre, current$Conference, current$Ville2, current$Date))
  }
}

printCours <- function(Cours){
  template <- 
    "
  - **%s** %s" 
  
  cat("#### Enseignement 
      ")
  for (i in seq(nrow(Cours))) {
    current <- Cours[i, ]
    cat(sprintf(template, current$Lien, current$list.cours))
  }
}

printDemo <- function(Demo){
  template <- 
    "
  - **%s** %s" 
  
  cat("#### Démonstrations 
      ")
  for (i in seq(nrow(Demo))) {
    current <- Demo[i, ]
    cat(sprintf(template, current$Lien, current$list.cours))
  }
}

printAutres <- function(Autres){
  template <- 
    "
  - %s" 
  
  cat("#### Autres
      ")
  for (i in seq(nrow(Autres))) {
    current <- Autres[i, ]
    cat(sprintf(template, current$Autre))
  }
}


