#
#' 	
#' 	
#' 	
#  function ods takes in a dataset with all variables	
# return a dataset with only a set of selected variables	
makeODS <- function(data)	
{	
  vars <- c("AM Stress", "AM Hunger", "AM Sleep", "AM Sleep hours", 	
         "AM Weight", "Percent Weight change (from prev week)", "Percent Weight change (from prev day)", 	
         "PM Stress", "EVE Stress", 	
         "Number of Episodes Previous Day", "Episode" )	
  ods <- data[,vars]	
  return(ods)	
}	
#ods = makeODS(data)	
#' 	
#' 	
#' 	
prepareODS <- function(path)	
{	
  data = read_csv(path)	
  ods = makeODS(data)	
  return(ods)	
}	
#' 	
#' 	
#' 	
#' 	
adjustEpisode <- function(syn)	
{	
  numRow <- nrow(syn)	
  for (i in 1:numRow)	
  {	
    if (syn$`Number of Episodes Previous Day`[i] == 0)	
    {	
      syn$`Episode`[i-1] = 0	
    }	
    else	
    {	
      syn$`Episode`[i-1] = 1	
    }	
  }	
  return(syn)	
}	
#' 	
#' 	
#' 	
#' 	
#' 	
#' 	
adjustToFactor <- function(data, var1 = "Episode", var2 = "Number of Episodes Previous Day")	
{	
  # Episode and Numbe of Episode the previous day need to be as factor 	
  data[, var1] <- as.factor(data[, var1])	
  data[, var2] <- as.factor(data[, var2])	
  return(data)	
}	
	
adjustToFactor_Mod <- function(data, vars)	
{	
  n = length(vars)	
  for (i in 1:n)	
  {	
    var = vars[i]	
    if (var !=  "AM Sleep hours" & var != "AM Weight" & var != "Percent Weight change (from prev week)" & var!= "Percent Weight change (from prev day)")	
    data[, var] <- as.factor(data[, var])	
  }	
  return(data)	
}	
	
#' 	
#' 	
#' 	
#' 	
cmp <- function(ods, sds, vars)	
{	
  for (i in 1:length(vars))	
  {	
    cmp = compare(sds, ods, vars[i])	
    print(cmp[1])	
    print(cmp[2])	
  }	
}	
#' 	
#' 	
#' 	
#' 	
#' 	
#' ########################################################################################################	
#' ################  AUTOMATING THE PROCESS OF GENERATING SYNTHETIC DATA  #################################	
#' ########################################################################################################	
#' 	
Generate_Synthetic_Data_For_All_Patients <- function(main.Path, write.Path, M, my.Seed)	
{	
  file.Names <- list.files(path = main.Path, pattern = "*.csv")	
  len <- length(file.Names)	
  for (i in 1:len)	
  {	
    #print(paste(main.Path, file.Names[i], sep='/'))	
    full.Path = paste(main.Path, file.Names[i], sep ='/')	
    assign(x = file.Names[i], value = read_csv(full.Path))	
    buf = as.data.frame(read_csv(full.Path))	
    ods = makeODS(buf)	
    ods = adjustEpisode(ods)	
    sds = syn(data = ods, m = M, seed = my.Seed)	
    df.Out <- data.frame()	
    #print(df.Out)	
    for (j in 1:M)	
    {	
      df = as.data.frame(sds$syn[j])	
      colnames(df) = c("AM Stress", "AM Hunger", "AM Sleep", "AM Sleep hours",	
                     "AM Weight", 	
                     "Percent Weight change (from prev week)", "Percent Weight change (from prev day)",	
                     "PM Stress", "EVE Stress",	
                     "Number of Episodes Previous Day", "Episode")	
      df = adjustEpisode(df) 	
      df = adjustToFactor(df)	
      df.Out = rbind(df.Out, df)	
    }	
    #write.Path <- "/Users/KVTran/Documents/Research/WPI/Slip_Buddy/REU_2017/data/synth"	
    write.Path <- paste(write.Path, file.Names[i], sep = "/")	
    #print(writePath)	
    write.csv(x = df.Out, file = write.Path, row.names = FALSE)	
  }	
}	

#' 	
#' 	
# mainPath <- "/Users/KVTran/Documents/Research/WPI/Slip_Buddy/REU_2017/data/Individual_Patient"
# writePath <- "/Users/KVTran/Documents/Research/WPI/Slip_Buddy/REU_2017/data/synth"
# m <- 20
# mySeed = 657578
# Generate_Synthetic_Data_For_All_Patients(mainPath, writePath, m, mySeed)



