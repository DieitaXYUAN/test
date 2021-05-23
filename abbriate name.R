
# clear the environment
rm(list=ls())

#clear the console windows
cat('\014')

#--------------------------------Class exercise-------------------------------------
#Create a function counting the specific character in a string
  charInString <- function(string='',char=''){
    which.char <- vector(mode='numeric')
    how.many.char <- 0
    looptimes <- nchar(string)+1-nchar(char)
    endpoint <- nchar(char)-1
      
    for(i in 1:looptimes ){
      
      if(substr(string,i,i+endpoint)==char){
        which.char <- c(which.char,i)
        how.many.char <- how.many.char+1
                                            }
                           }
    return <- list(which.char=which.char,how.many.char=how.many.char)                                            
                                                }

a <-  charInString (string = "Mississ.ippi", char = "s")
a




#--------------------------------Homework 8-------------------------------------
 load('fs.spp.table.RData')
  
# ______________Method 1 transfer the fullname data to the matrix by do.call___________________________ 

  create_abbrev <- function(sci_names,sep=''){
    # separate the fullname, generate the species_name_list(list type)
    spe_name_list <- strsplit(sci_names," ")
    
    #transfer the list to the matrix and cut the first two name
    #Metod matrix (unlist (parameter))
    species_name_dealing <- do.call(rbind, spe_name_list)
    
    species_name_dealing <- species_name_dealing[1:nrow(species_name_dealing),1:2]
    
    #cut the first four words of the names  
    spe_name_sep<-sapply(species_name_dealing, FUN=function(x)substr(x,start=1,stop=4 ))
    
    #abbreviate the names
    fullname_sep <-paste0(toupper(substr(x=spe_name_sep,start=1,stop=1)),
                          substr(x=spe_name_sep,start=2,stop=4))
    
    name_abb <- paste(fullname_sep[1:length(fullname_sep)/2],
                              fullname_sep[length(fullname_sep)/2+1:length(fullname_sep)],sep = sep)
    
    return(matrix(name_abb))
  }

  create_abbrev(sci_names=spp.table$fullname1)
  create_abbrev(sci_names=spp.table$fullname1,sep="_")
  
