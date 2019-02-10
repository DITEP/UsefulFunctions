# List of generic functions for sanity check

#######
# quick look at nb of NAs or missing values in selected colnames rom your DATA file: a df or matrix

ColNames<-c("X","Y")

NAs<-sapply(ColNames,function(x){
  table(is.na(DATA[,x])|DATA[,x]=="")
})

print(NAs)

v <- which(basePts=="", arr.ind=TRUE)
base[v]

#PROBLEME : sous forme de fonction, la base n'est pas changée:
NAs <- function (base){
  v <- which(base=="", arr.ind=TRUE)
  base[v] <- NA
  #print(head(base))
}

#===============================================================================================================#
###CODE FOR FINDING DOUBLONS IN DATABASE

doublon = function(df){ #retourne le nom des colonnes existant en double & change nom de la col en double
  c <- colnames(df)
  nomColonne <- gsub(pattern = "\\.[0123456789]","",x = c)
  db <- c()
  col_double <- c()
  nb <- 0
  
  for (i in 1:length(nomColonne)) {
    for (j in 1:length(nomColonne)) {
      if(nomColonne[i] == nomColonne[j] & i!=j & !nomColonne[i] %in% db) {
        db <- c(db,nomColonne[j])
        #nomColonne[j] <- paste(nomColonne[j],".1",sep = "")
      }
    }
  }
  return(paste("La colonne '",db, "' existe en plusieurs exemplaires"))
}


#doublon(basePts)

## CODE FOR CHECKING A COLUMN HAS ONLY BINARY VALUES (yes or no) - except NA and ""

type_is_0_1 <- function (db,nomCol01){
  msg <-""
  for (i in 1:nrow(db) ){
    if(db[i,nomCol01] != 0 & db[i,nomCol01] != 1 & !is.na(db[i,nomCol01]) & db[i,nomCol01] != ""){
      msg <- paste("Erreur ! Ligne :" , i , "& Valeur:",db[i,nomCol01] )
      print(msg)
    }
  }
  if (msg == ""){
    print("tout est ok pour cette colonne")
  } else{print("Cette colonne ne contient pas uniquement des variables binaires.")}
}
type_is_0_1(basePts,'rmh')


## CODE FOR CHECKING A COLUMN HAS ONLY NUMERIC VALUES - except NA and ""

type_is_num<- function (db,nomColNum){
  msg <-""
  for (i in 1:nrow(db) ){
    if(!is.numeric(db[i,nomColNum]) & !(db[i,nomColNum]<0) &  !is.na(db[i,nomColNum]) & db[i,nomColNum] != "" & db[i,nomColNum] != "NA" ){
      msg <- paste("Erreur ! Ligne :" , i+1 , "& Valeur:",db[i,nomColNum] )
      print(msg)
    }
  }
  if (msg == ""){
    print("tout est ok pour cette colonne")
  } else{print("Cette colonne ne contient pas uniquement des variables numériques")}
}

type_is_num(baseDDR,'LogRatioDeletionByCGH')

## CODE FOR CHECKING A COLUMN OF DATES

type_is_date <- function (db, nomColDate){
  msg <-""
  for(i in 1:nrow(db)){
    x <- as.Date(as.character(nomColDate[i]),format="%d/%m/%Y")
    if (x ==TRUE && !is.na(x)){msg <- paste("La valeur ligne", i+1, "n'est pas une date." )}
  }
  if(msg == ""){print("toutes les valeurs sont bien des dates")}else{print(msg)}
  
}

type_is_date(basePts,"Date naissance")


## CODE FOR CHECKING IF ALL THE CATEGORIES ARE WELL WRITTEN

#PART 1: entrer le nom des catégories d'une variable (exemple: F et M pour 'sex')

categories <- function(db, nomCol){
  categorie <- c()
  
  nb_cat <- as.integer(readline("saisissez le nombre de catégories que vous avez : "))
  
  for (i in 1:nb_cat){
    val <- readline(paste("Entrez le nom de votre catégorie", i, "pour la colonne", nomCol,": "))
    categorie <- c(categorie, val)
  }
  return(categorie)
}


#exemple : sex <- categories(basePts,'Sex')

#PART 2: vérifier que toutes les données inscrites appartiennent bien à ce vecteur de catégorie. Si non, alors cela signifie qu'il y a eu une erreur de saisie.

verif_categories<- function (db,nomColaVerifier, cat){
  msg <-""
  for (i in 1:nrow(db) ){
    valeur <- db[i,nomColaVerifier]
    if (!(valeur %in% cat)){
      msg <- "Vous avez une (des) erreur(s) dans la saisie de vos données."
      cat("Ligne ", i+1, ":'",db[i,nomColaVerifier], "'", sep = "","\n")
    }
  }
  if (msg != ""){print(msg)}
}
#exemple: verif_categories(test,'Sex',sex)


