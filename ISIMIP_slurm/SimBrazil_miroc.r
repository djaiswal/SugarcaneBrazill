scenario <-k

library(BioCro,lib.loc="/home/a-m/djaiswal/R/library")

#Reading Co2 levels for different scenarios
rcp_Co2levels<-read.csv("/home/groups/ebimodeling/co2/rcp.csv")
colnames(rcp_Co2levels) <- c("YEAR","rcp2p6","rcp4p5","rcp6p0","rcp8p5")
#simulation period
startyear<-2041
endyear<-2049
#initial planting material
iRhizome <- 6

#harvest/replanting
harvestduration=7

#multiple utilities functions
source("/home/a-m/djaiswal/Scripts/BioCroSimulations/Brazil/BrazilSoil/Brazil_Regional_BioCro_Functions.R")
load("/home/groups/ebimodeling/deepak_tmp/ISIMIP/ISIMIPIndex.RData")

soils<-read.csv("/home/a-m/djaiswal/Scripts/BioCroSimulations/Brazil/BrazilSoil/processedsoil_isimipindices.csv")

#Range of soil indices
istart=1
iend=dim(soils)[1]

               currentyear=startyear         
               finaloutput<-NULL
               output <- NULL
               check=1
               modelname <- "MIROC"
                 for (isoil in c(4500)){ 
                   
                          soillat<-soils[isoil,]$LAT
                          soillon<-soils[isoil,]$LONG
                          ClimateLatIndex=which.min(abs(soillat-ISIMIPIndex$Latitude))
                          ClimateLongIndex=which.min(abs(soillon-ISIMIPIndex$Longitude))
                          
                          mdep<-soils[isoil,]$SoilDepth*0.01
                          fieldc<-soils[isoil,]$FC
                          wiltp<-soils[isoil,]$WP
                          ID<-soils[isoil,]$ID
                          soilP<-soilParms(wsFun="logistic",phi1=0.0177,phi2=0.83,rfl=0.4,soilLayers=1,soilDepth=mdep,FieldC=fieldc,WiltP=wiltp)

                          ii= soils[isoil,]$ISIMIP_Index_Lon
                          jj= soils[isoil,]$ISIMIP_index_Lat
                          
                                      repeat{
                                      currentyear<-currentyear+1
                                      if(currentyear>endyear){
                                      break}
                                         dat<-NULL
                                         planting_at_given_location <- Get_planting_date_in_Brazil(latitude = soillat, currentyear = currentyear, months_in_growing_period=12)
                                         
				                                 dat <- Get_BioCro_ISIMIP_Weather_Data_v_allRCPs(planting_at_given_location$plantingdate,planting_at_given_location$harvestdate,ii,jj,modelname,scenario)                                                               
					                               pdate<-datetoDMY(planting_at_given_location$plantingdate)
                                         hdate<-datetoDMY(planting_at_given_location$harvestdate)
                                         tmp<-getday1dayn(pdate,hdate)
                                         day1<-tmp$day1
                                         dayn<-tmp$dayn
                                         Co2level=rcp_Co2levels[rcp_Co2levels$YEAR==currentyear,][[scenario]]
                                         res1<- caneGro(dat,lat=soillat, soilControl=soilP,photoControl=list(Catm=Co2level),day1 = day1, dayn = dayn,iRhizome=iRhizome,irtl=1e-2)
                                       stem1<-res1$Stem[length(res1$Stem)]
                                       leaf1<-res1$Leaf[length(res1$Leaf)]
                                       root1<-res1$Root[length(res1$Root)]
                                       rhiz1<-res1$Seedcane[length(res1$Seedcane)]
                                       leaflitter1 <- res1$LeafLittervec[length(res1$LeafLittervec)]
                                       tmpoutput<-as.vector(c(currentyear,stem1, leaf1,root1,rhiz1,leaflitter1))
                                       output<-as.vector(c(output,tmpoutput))
                                       check=IstodayHarvest(startyear,currentyear,harvestduration)
                                       if(check==0){   #checking if this is replanting year
                                          check=1      # if so, reset check
                                          output<-as.vector(c(output,stem1*0.89)) #save the output for this year
                                          iRhizome=6 # IRhizome is 6 Mg/ha resulting due to replanting
                                                  } else { #If this is not replanting year
                                                         iRhizome=stem1*0.11 #iRhizome is 11% of total produce last year
                                                         output<-as.vector(c(output,stem1*0.89)) # we still need to save output because sugarcane is harvested every year
                                                         }    
                               } # ending repeat loop here
                                 Tmin=min(dat$Temp)
                               gridoutput<-as.vector(c(c(ii,jj,soillat,soillon,Tmin),output))
                               finaloutput<-rbind(finaloutput,gridoutput)
                 }    # end isoil index here
resultfile <- paste("/home/a-m/djaiswal/Scripts/ClimateProcessing/ISIMIP_slurm/",modelname,"/",scenario,"/result.RData",sep="")


save(finaloutput,file=resultfile)
