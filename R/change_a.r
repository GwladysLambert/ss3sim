change_a <- function(dat_list, outfile, fleets, seas=NULL, gender=NULL, years, agerr_mat, which_agerr, by.fleets=NULL, 
                     by.seas=NULL, by.gender=NULL, by.years=NULL, write_file=TRUE){
  
  # remember that the default will be ageing error matrix 1 - no ageing error
  
  agecomp <- dat_list$agecomp
  
  # replace the number of defined matrices and the matrices in the .dat file
  new_mat <- as.data.frame(do.call(rbind,agerr_mat))
  names(new_mat) <- paste("age",0:c(ncol(new_mat)-1), sep="")
  dat_list$ageerror <- dat_list$ageerror[,c(1:dim(new_mat)[2])]# ageing error matrices must have same dimensions
  dat_list$ageerror <- rbind(dat_list$ageerror,new_mat)

  dat_list$N_ageerror_definitions <- dim(dat_list$ageerror)[1]/2
  
    # argument which_agerr must be a list organised as follow: 
  # one value to use only one matrix for all age data
  # or vector of appropriate length
  
  # if only one matrice used for all year, season, fleet and gender 
  if (length(which_agerr)==1) agecomp$Ageerr <- which_agerr[[1]]

  # if a different ageing error matrix for each fleet
  if (!is.null(by.fleets) & is.null(by.seas) & is.null(by.gender) & is.null(by.years)) { # Length of which_agerr must be equal to length of fleets
    for ( ii in 1:length(fleets)){
      agecomp[agecomp$FltSvy==fleets[ii], "Ageerr"] <- which_agerr[[ii]]
    }
  } 
  
  # if a different ageing error matrix every year
  if (is.null(by.fleets) & is.null(by.seas) & is.null(by.gender) & !is.null(by.years)) { # Length of which_agerr must be equal to length of year
      for ( ii in 1:length(years)){
              agecomp[agecomp$Yr==years[ii], "Ageerr"] <- which_agerr[[ii]]
            }
          }
        
  # if a different ageing error matrix for each season
  if (is.null(by.fleets) & !is.null(by.seas) & is.null(by.gender) & is.null(by.years)) { 
    for ( ii in 1:length(seas)){
      agecomp[agecomp$Seas==seas[ii], "Ageerr"] <- which_agerr[[ii]]
    }
  }
  
  # if a different ageing error matrix for each gender
  if (is.null(by.fleets) & is.null(by.seas) & !is.null(by.gender) & is.null(by.years)) { 
    for ( ii in 1:length(gender)){
      agecomp[agecomp$Gender==gender[ii], "Ageerr"] <- which_agerr[[ii]]
    }
  }
  
  
  ### COMBINATIONS
  
  ## Combination of ageing error per fleet and per year - which_agerr should be organised as fleet 1 first all years, then fleet 2 all years etc
  if (!is.null(by.fleets) & is.null(by.seas) & is.null(by.gender) & !is.null(by.years)) { 
    for (ii in 1:length(fleets)) {
    for ( jj in 1:length(years)){
      agecomp[agecomp$FltSvy==fleets[ii] & agecomp$Yr==years[jj], "Ageerr"] <- which_agerr[[i]][[jj]]
    }
    }
  }
  
  ## Combination of ageing error per season and per year - which_agerr should be organised as season 1 first all years, then season 2 all years etc
  if (is.null(by.fleets) & !is.null(by.seas) & is.null(by.gender) & is.null(by.years)) { 
    for (ii in 1:length(seas)) {
      for ( jj in 1:length(years)){
        agecomp[agecomp$Seas==seas[ii] & agecomp$Yr==years[jj], "Ageerr"] <- which_agerr[[i]][[jj]]
      }
  }
  }
    
  ## Combination of ageing error per gender and per year - which_agerr should be organised as gender 1 first all years, then gender 2 all years etc
  if (is.null(by.fleets) & is.null(by.seas) & !is.null(by.gender) & !is.null(by.years)) { 
    for (ii in 1:length(gender)) {
      for ( jj in 1:length(years)){
        agecomp[agecomp$Gender==gender[ii] & agecomp$Yr==years[jj], "Ageerr"] <- which_agerr[[i]][[jj]]
      }
  }
  }
    
  ## Combination of ageing error per fleet and per season - which_agerr should be organised as fleet 1 first all seasons, then fleet 2 all seasons etc
  if (!is.null(by.fleets) & !is.null(by.seas) & is.null(by.gender) & is.null(by.years)) { # Length of which_agerr must be equal to length of year
    for (ii in 1:length(fleets)){
      for ( jj in 1:length(seas)){
        agecomp[agecomp$FltSvy==fleets[ii] & agecomp$Seas==seas[jj], "Ageerr"] <- which_agerr[[i]][[jj]]
      }
  }
  }
    
  ## Combination of ageing error per fleet and per gender - which_agerr should be organised as fleet 1 first all genders, then fleet 2 all genders etc
  if (!is.null(by.fleets) & is.null(by.seas) & !is.null(by.gender) & is.null(by.years)) { # Length of which_agerr must be equal to length of year
    for (ii in 1:length(fleets)){
      for ( jj in 1:length(gender)){
        agecomp[agecomp$FltSvy==fleets[ii] & agecomp$Gender==gender[jj], "Ageerr"] <- which_agerr[[i]][[jj]]
      }
  }
  }
    
  agecomp -> dat_list$agecomp
  
## Write the modified file
  if(write_file) {
    SS_writedat(datlist = dat_list, outfile = outfile, overwrite = TRUE, verbose = FALSE)
  invisible(dat_list)
  }
}


