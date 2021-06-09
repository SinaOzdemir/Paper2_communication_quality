#two-way interaction indicator functions:


# read packages -----------------------------------------------------------



packs<- c("tidyverse","Hmisc","rtweet","igraph","ggraph")
existing_packs<-installed.packages()[,1]

package_loader<- function(packs = character(0)){
  existing_packs<- installed.packages()[,1]
  if(all(packs%in%existing_packs)){
    lapply(packs,library, character.only = T)
  }else{
    missing_packages<- packs[-which(packs%in%existing_packs)]
    install.packages(missing_packages)
    lapply(packs,library(character.only = T))
  }
  
}

package_loader(packs = packs)

# helper functions --------------------------------------------------------

make.dir<- function(file.path){
  if(dir.exists(file.path)){
    return(file.path)
  }else{
    dir.create(path = file.path, recursive = T)
    return(file.path)
  }
}


# variable manipulators ---------------------------------------------------


