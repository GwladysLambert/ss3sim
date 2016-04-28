change_a <- function(dat_list, outfile, df, agerr_mat, write_file=TRUE){
  
  # remember that the default will be ageing error matrix 1 - no ageing error
  
  agecomp <- dat_list$agecomp
  df <- as.data.frame(df)
  
  # replace the number of defined matrices and the matrices in the .dat file
  new_mat <- as.data.frame(agerr_mat)
  names(new_mat) <- c("which_matrix",paste("age",0:c(ncol(new_mat)-2), sep=""))
  dat_list$ageerror <- dat_list$ageerror[,c(1:c(dim(new_mat)[2]-1))]# ageing error matrices must have same dimensions
  dat_list$ageerror <- rbind(dat_list$ageerror,new_mat[,-1])
  
  dat_list$N_ageerror_definitions <- dim(dat_list$ageerror)[1]/2
  
  # Now replace Agerr in agecomp from df data
  
  for (yr in unique(agecomp$Yr)){
    sub <- agecomp[agecomp$Yr==yr,]
    for(seas in unique(sub$Seas)){
      sub1 <- sub[sub$Seas==seas,]
      if (dim(sub1)[1]>0){
        for(flt in unique(sub1$FltSvy)){
          sub2 <- sub1[sub1$FltSvy==flt,]
          if (dim(sub2)[1]>0){
            for(sex in unique(sub2$Gender)){
              agecomp[agecomp$Yr==yr & agecomp$Seas==seas & agecomp$FltSvy==flt & agecomp$Gender==sex,"Ageerr"] <-
                df[df$Yr==yr & df$Seas==seas & df$FltSvy==flt & df$Gender==sex,"Ageerr"]
            }
          }
        }
      }
    }
  }
  
  agecomp -> dat_list$agecomp
  
  # If there are mean length at age they need to be replaced too
  
  if (dat_list$N_MeanSize_at_Age_obs !=0) {
    
    mlacomp <- dat_list$MeanSize_at_Age_obs
    
    for (yr in unique(mlacomp$Yr)){
      sub <- mlacomp[mlacomp$Yr==yr,]
      for(seas in unique(sub$Seas)){
        sub1 <- sub[sub$Seas==seas,]
        if (dim(sub1)[1]>0){
          for(flt in unique(sub1$FltSvy)){
            sub2 <- sub1[sub1$FltSvy==flt,]
            if (dim(sub2)[1]>0){
              for(sex in unique(sub2$Gender)){
                mlacomp[mlacomp$Yr==yr & mlacomp$Seas==seas & mlacomp$FltSvy==flt & mlacomp$Gender==sex,"Ageerr"] <-
                  df[df$Yr==yr & df$Seas==seas & df$FltSvy==flt & df$Gender==sex,"Ageerr"]
              }
            }
          }
        }
      }
    }
    
    mlacomp -> dat_list$MeanSize_at_Age_obs
    
  }
  
  ## Write the modified file
  if(write_file) {
    SS_writedat(datlist = dat_list, outfile = outfile, overwrite = TRUE, verbose = FALSE)
    invisible(dat_list)
  }
}
