# clear the environment
rm(list=ls())

#clear the console windows
cat('\014')


#load the txt document and save the name of the txt document 
name_sample_txt<- list.files(pattern=".txt")

#name document without the ".txt"
name_sample <- gsub('.txt','',name_sample_txt)

#calculate the no.of the txt documents
no_sample <- length(name_sample_txt)

#set the initial condition for data combine

#initial the table structre
reflec_data <- matrix(ncol=no_sample+1,nrow = 2701)
colnames(reflec_data) <- c('wavelength(nm)',name_sample)
reflec_data[,1] <- 300:3000

no_peak <- vector(mode='numeric')
peak_posi <- vector(mode='numeric')


# read the table 
for(i in 1:no_sample){
  mediun<- read.table(file=name_sample_txt[i], 
                      skip=19, nrows=2701, 
                      header = F, sep = '\t',as.is = T)
  reflec_data[,i+1] <- rev(mediun[,2])
  rm(mediun)
}

#output the csv document
write.csv(reflec_data,file='data_combine.csv',row.names = FALSE)





#cheek  the peak through the differencial way, the former number minus the latter number
diff_reflec_data_1st <- diff(reflec_data[,2:ncol(reflec_data)])

#cheak the position when the slop value has the positive and negative change

slop_change_AC<- ifelse(diff_reflec_data_1st>=0,1,0)

for (j in 1:no_sample) {
  for(i in 1:2699){
    if(slop_change_AC[i,j]==slop_change_AC[(i+1),j]){
      slop_change_AC[i,j] <- 0
    }
  }
  
}

slop_change_AC <- cbind(300:2999,slop_change_AC)


#calculate the 2nd deviation

diff_reflec_data_2nd <- diff(diff_reflec_data_1st)

#sign the positive 2nd deviation is zero and negative deviation is one 
slop_change_deviation_AC<- ifelse(diff_reflec_data_2nd>=0,0,1)

slop_change_deviation_AC <- cbind(300:2998,slop_change_deviation_AC)

finial_position<- matrix(c(2999,0,0),nrow=1)

slop_change_deviation_AC <- rbind(slop_change_deviation_AC,finial_position)



for (j in 2:(no_sample+1)) {
  
  k <- 1
  last_position<-1 
  
  for(i in 1:2700){
    
    # seek the position of k=1 or 0
    #when k=1, it means the peak occurs.When k=0, the peak is missed.
    if(slop_change_deviation_AC[i,j]==k){
      
      # When k=0, we need to judge whether the  width of the peak is enough broad or not.
      #If the peak is narrow, we need to delete it.
      
      if(k%%2==0 ){
        
        if((i-last_position)<20){
          slop_change_deviation_AC[last_position:(i-1),j] <- 0
        }
        last_position <- 0
        
      }
      else{
        last_position <- i
        last_position
      }
      
      
      k <- 1-k 
      k
    }
    
  }
}


#plot the diagram
for(m in 2:(no_sample+1)){
  
  bb <- slop_change_AC[,m]& slop_change_deviation_AC[,m]
  cc <- reflec_data[bb,1:m,drop = FALSE]
  
  plot(x=reflec_data[,1],
       y=reflec_data[,m],
       ylim = c(0,40),
       xlim=c(300,2500),
       xlab = 'Wavelength(nm)',
       ylab = 'Reflectivity(%)',
       las=1,
       lwd=3,
       type='l',
       col=RColorBrewer::brewer.pal (n = 5, name = 'Paired')[m-1],
  )
  
  points(x=cc[,1],
         y=cc[,m],
         col='red',
         pch=16,
         cex=2
  )
for(g in 1:nrow(cc)){ 
  text(x=cc[g,1],
       y=cc[g,m],
       col='red',
       labels=cc[g,1],
       pos=3
       )
    }

  legend('topleft',
         legend=name_sample[m-1],
         pch=16,
         pt.cex=1.5,
         lwd=0.01,
         col=RColorBrewer::brewer.pal (n = 5, name = 'Paired')[m-1],
         bty='n',
         ncol=2
  )
  
  
  rm(bb)
  rm(cc)
}
