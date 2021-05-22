# clear the environment
rm(list=ls())

#clear the console windows
cat('\014')

#set the address for data dealing 
setwd("C:/Users/11864/Desktop/R language/Lastproject")

#cheak the address
getwd()

#load the txt document and save the name of the txt document 
name_sample_txt<- list.files(pattern=".txt")

#name document without the ".txt"
name_sample <- gsub('.txt','',name_sample_txt)

#calculate the no.of the txt documents
no_txt_file <- length(name_sample_txt)

#set the initial condition for data combine
i <- 2

reflection.table<- read.table(file=name_sample_txt[1], 
                              skip=19, 
                              nrows=2701, 
                              col.names=c('wavelength','Reflictivity(%)'), 
                              header = F, sep = '\t',
                              as.is = T)


# read the table 
while(i<no_txt_file+1){
  table_mediun<- read.table(file=name_sample_txt[i], 
                         skip=19, 
                         nrows=2701, 
                         col.names=c('wavelength','Reflictivity(%)'), 
                         header = F, sep = '\t',
                         as.is = T)
  reflection.table <- cbind(reflection.table,table_mediun$Reflictivity...)
  #cbind Function combine the new matrix data to the right new column 
  i <- i+1
}

names(reflection.table) <- c('wavelength',name_sample)
write.csv(reflection.table,file='data_combine2.csv',sep='',row.names = FALSE)

