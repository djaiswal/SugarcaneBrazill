get_summary <- function(modelname,scenario){
    resultfile <- paste("/home/a-m/djaiswal/Scripts/ClimateProcessing/ISIMIP_slurm/",modelname,"/",scenario,"/result.RData",sep="")
    load(resultfile)
    lat <- as.vector(unlist(finaloutput[,3]))
    lon <- as.vector(unlist(finaloutput[,4]))
    stemdata <- cbind(as.vector(unlist(finaloutput[,12])),
                      as.vector(unlist(finaloutput[,19])),
                      as.vector(unlist(finaloutput[,26])),
                      as.vector(unlist(finaloutput[,33])),
                      as.vector(unlist(finaloutput[,40])),
                      as.vector(unlist(finaloutput[,47])),
                      as.vector(unlist(finaloutput[,54])),
                      as.vector(unlist(finaloutput[,61])))
    output <- data.frame(lon=lon,lat=lat,HarvestedStem=mean(stemdata),modelname=modelname,scenario=scenario)
    return(output)
}

   flag <- NA   
    for (modelname in c("HadGEM","IPSL","MIROC","NorESM","GFDL")){
      for(scenario in c("rcp2p6","rcp4p5","rcp6p0","rcp8p5")){
        tmp <- get_summary(modelname,scenario)
        if(is.na(flag)){
          summarized_data <- tmp
          flag <-0
        } else {
          summarized_data <- rbind(summarized_data,tmp)
        }
  } # scenario loop ends here
  }  #modelname loop ends here

  write.csv(summarized_data, file="./saopaolo_onepoint_summary.csv")