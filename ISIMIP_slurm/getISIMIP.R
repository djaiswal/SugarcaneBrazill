      getISIMIP<-function(lat,lon,year,modelname,scenarioname){
     
	 rcp_Co2levels<-read.csv("/home/groups/ebimodeling/co2/rcp.csv")
         load("/home/groups/ebimodeling/deepak_tmp/ISIMIP/ISIMIPIndex.RData")
          ClimateLatIndex=which.min(abs(lat-ISIMIPIndex$Latitude))
          ClimateLongIndex=which.min(abs(lon-ISIMIPIndex$Longitude))
           ii=ISIMIPIndex$Ilong[ClimateLongIndex]
           jj=ISIMIPIndex$Jlat[ClimateLatIndex]
           climatefile<-paste("/home/groups/ebimodeling/deepak_tmp/ISIMIP/",modelname,"/Processed/",year,formatC(ii,width=3,flag=0),formatC(jj,width=3,flag=0),".RData",sep="")
           Co2level=rcp_Co2levels[rcp_Co2levels$YEAR==year,][[scenario]]
           return(list=(CO2=Co2level, climdata=dat))
         }







