data <- read_excel("xls_files/ThesesMemoires.xlsx") 


createTheses <- function(data) { # create a function with the name my_function
  
  data$Direction1 <- data$Directeur1
  data$Direction2 <- paste0(data$Directeur1, ' et ', data$Directeur2)
  data$Direction3 <- paste0(data$Directeur1, ', ', data$Directeur2, ' et ', data$Directeur3)
  data$nbDir <- 3-rowSums(data[,2:4]==".")
  data$Direction <- ifelse(data$nbDir == 1, data$Direction1, ifelse(data$nbDir == 1, data$Direction2, data$Direction3))
  data$Direction <- paste0('Dir.: ', data$Direction)

  data$Etudiant <- paste0(data$Etudiant, ' (', data$Année, ')') 
  data$Lien <- paste0('«' , "<a href='",data$Lien,"' target='_blank'>",data$Title,"</a>", '»')
  
  return(data)
}

Theses <- createTheses(data) %>%
  arrange(-Année)


printTheses <- function(Theses){
  template <- "  
  - %s, %s, %s, %s, %s, %s" 
  cat("## Thèse de doctorat
        ")
  for (i in seq(nrow(Theses))) {
    current <- Theses[i, ]
    cat(sprintf(template, current$Etudiant, current$Lien, current$Direction, current$Lieu, current$Universite, current$Niveau))
  }
}

printTheses(Theses)

###



```{r echo = FALSE, results = "asis",  message=FALSE, warning=FALSE}
library(readxl)
data <- read_excel("../../xls_files/ThesesMemoires.xlsx") 


createTheses <- function(data) { # create a function with the name my_function
  
  data$Direction1 <- data$Directeur1
  data$Direction2 <- paste0(data$Directeur1, ' et ', data$Directeur2)
  data$Direction3 <- paste0(data$Directeur1, ', ', data$Directeur2, ' et ', data$Directeur3)
  data$nbDir <- 3-rowSums(data[,2:4]==".")
  data$Direction <- ifelse(data$nbDir == 1, data$Direction1, ifelse(data$nbDir == 1, data$Direction2, data$Direction3))
  data$Direction <- paste0('Dir.: ', data$Direction)
  
  data$Etudiant <- paste0(data$Etudiant, ' (', data$Année, ')') 
  data$Lien <- paste0('«' , "<a href='",data$Lien,"' target='_blank'>",data$Title,"</a>", '»')
  
  return(data)
}

Theses <- createTheses(data) %>%
  arrange(-Année)


printTheses <- function(Theses){
  template <- "  
  - %s, %s, %s, %s, %s, %s" 
  cat("## Thèse de doctorat
        ")
  for (i in seq(nrow(Theses))) {
    current <- Theses[i, ]
    cat(sprintf(template, current$Etudiant, current$Lien, current$Direction, current$Lieu, current$Universite, current$Niveau))
  }
}

printTheses(Theses)
```

