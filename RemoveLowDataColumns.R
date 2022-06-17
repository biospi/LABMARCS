#Remove columns that have too much missing data
#thresh:30 = 16 vars and 589 complete, 
#thresh:25 = 9 vars and 785 complete, 
perc_thresh <- 70 #BE & PH are ~50-50.5 so include them

print(sprintf('Exlcuding variables with more than %d percent missing', perc_thresh))
#use to examine proportions of data missing

name_tmp <- names(fulldata_sv)
  
for (i in 3:dim(fulldata_sv)[2]) {
  
  s <- sum(table(fulldata_sv[,i]))
  na_sum1 <- sum(is.na(fulldata_sv[,i]))
  na_sum2 <- sum(fulldata_sv[,i] == 'NA')
  nt_sum <- sum(fulldata_sv[,i] == 'Test not taken')
  na_perc1 <- na_sum1/s*100
  na_perc2 <- na_sum2/s*100
  nt_perc <- nt_sum/s*100
  
  str <- sprintf('%d, %s, %2.2f, %2.2f, %2.2f', i, 
                 names(fulldata_sv)[i], nt_perc,na_perc1, na_perc2)
  print(str)
  
  #filter out variables above missing threshold
  if ((!is.na(na_perc1) & na_perc1 > perc_thresh) | 
      (!is.na(na_perc2) & na_perc2 > perc_thresh) | 
      (!is.na(nt_perc) & nt_perc > perc_thresh)) {
    name_tmp <- name_tmp[name_tmp != names(fulldata_sv)[i] ]   
    
  }
}

#remove incomplete data as from above
print( sum(complete.cases(fulldata_sv)))
print(dim(fulldata_sv))
  
#some variables have very few examples of a positive remove?
#fulldata <- subset(fulldata, select = -c(viral_coinfection, bc_coinfection,
#                                         resp_coinfection,urine_coinfection))

