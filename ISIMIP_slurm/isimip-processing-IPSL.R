args <- commandArgs(TRUE)

if(length(args) == 0){
  warning("args is missing")
}else{
  k <- as.numeric(args)
}

# K (array index is now used for scenario name)
k= ifelse( k==1,"rcp2p6",
           ifelse(k==2,"rcp4p5",
                  ifelse(k==3,"rcp6p0",
                         ifelse(k==4,"rcp8p5",
                                100))))
if(k==100) {stop("ERROR")}

source("/home/a-m/djaiswal/Scripts/ClimateProcessing/ISIMIP_slurm//yearinterval_for_isimip.R")
source("/home/a-m/djaiswal/Scripts/ClimateProcessing/ISIMIP_slurm//getFirstIndex_for_isimip.R")
#This soil file contain index that needs to be saved for Brazil
soils<-read.csv("/home/a-m/djaiswal/Scripts/BioCroSimulations/Brazil/BrazilSoil/processedsoil_isimipindices.csv")

library(BioCro,lib.loc="/home/a-m/djaiswal/R/library")
library(ncdf4)
maindirectory<-"/home/groups/ebimodeling/deepak_tmp/isimip_allRCP_2041-2050/"
modelname<-"IPSL"
scenarioname<-k # Now I am passing scenario as argument in script

yearinterval<-yearinterval_for_isimip(2041)

#Reading average temperature
filename<-paste(maindirectory,modelname,"/tas_bced_1960_1999_ipsl-cm5a-lr_",scenarioname,"_",yearinterval[3],".nc", sep="")
tmp0<-nc_open(filename)
tavg<-ncvar_get(tmp0)
nc_close(tmp0)

#Reading maximum temperature
filename<-paste(maindirectory,modelname,"/tasmax_bced_1960_1999_ipsl-cm5a-lr_",scenarioname,"_",yearinterval[3],".nc", sep="")
tmp0<-nc_open(filename)
tmax<-ncvar_get(tmp0)
nc_close(tmp0)

#Reading minimum temperature
filename<-paste(maindirectory,modelname,"/tasmin_bced_1960_1999_ipsl-cm5a-lr_",scenarioname,"_",yearinterval[3],".nc", sep="")
tmp0<-nc_open(filename)
tmin<-ncvar_get(tmp0)
nc_close(tmp0)

#Reading precipitation
filename<-paste(maindirectory,modelname,"/pr_bced_1960_1999_ipsl-cm5a-lr_",scenarioname,"_",yearinterval[3],".nc", sep="")
tmp0<-nc_open(filename)
pr<-ncvar_get(tmp0)
nc_close(tmp0)

#Reading  wind
filename<-paste(maindirectory,modelname,"/wind_bced_1960_1999_ipsl-cm5a-lr_",scenarioname,"_",yearinterval[3],".nc", sep="")
tmp0<-nc_open(filename)
wind<-ncvar_get(tmp0)
nc_close(tmp0)


#Shortwave radiation
filename<-paste(maindirectory,modelname,"/rsds_bced_1960_1999_ipsl-cm5a-lr_",scenarioname,"_",yearinterval[3],".nc", sep="")
tmp0<-nc_open(filename)
sRad<-ncvar_get(tmp0)
nc_close(tmp0)


#Relative humidity
filename<-paste(maindirectory,modelname,"/hurs_ipsl-cm5a-lr_",scenarioname,"_",yearinterval[3],".nc", sep="")
tmp0<-nc_open(filename)
rhs<-ncvar_get(tmp0)
 Lat<-ncvar_get(tmp0,"lat")
 Lon<-ncvar_get(tmp0,"lon")
 nc_close(tmp0)

 for (year in 2041:2050) {
 for (ii in 1:720) # Longitude Index 720
     {
      for (jj in 1:360) # Latitude Index 360
      {
        
        ####################################################
        ## First I will check if I need to save results for this ii and jj index or not
        ## I will create a combined index using ii and jj
        ## and compare if it exists in the soil data
        ## if it does then I do anything beyond this point
        ## otherwise I do not need to process the climate data
        combindex <- paste(formatC(ii,width=3,flag=0),formatC(jj,width=3,flag=0),sep="")
        if(combindex%in%soils$ISIMIP_index_comb)
        {
        #######################################################
         currentlon<-Lon[ii]
         currentlat<-Lat[jj]

         weachyear<-numeric(0)
         weachday<-numeric(0)
         weachhumidity<-numeric(0)
         weachsolar<-numeric(0)
         weachtemp<-numeric(0)
         weachwind<-numeric(0)
         weachprecip<-numeric(0)
         weachtempmax<-numeric(0)
         weachtempmin<-numeric(0)
         # finding correct index for the current year in the 10 yr weather file
             firstindex<-getFirstIndex(year,as.numeric(yearinterval[1]),as.numeric(yearinterval[2]))
             lastindex<-firstindex+365-1
         #populating vectors for weach
             iweachyr<-year
             weachyear<-as.vector(c(weachyear,rep(iweachyr,365)))
             weachday<-as.vector(c(weachday,seq(1,365)))
             weachtemp<-as.vector(c(weachtemp,tavg[ii,jj,firstindex:lastindex]))
             weachtempmax<-as.vector(c(weachtempmax,tmax[ii,jj,firstindex:lastindex]))
             weachtempmin<-as.vector(c(weachtempmin,tmin[ii,jj,firstindex:lastindex]))
             weachhumidity<-as.vector(c(weachhumidity,rhs[ii,jj,firstindex:lastindex]))
             weachsolar<-as.vector(c(weachsolar,sRad[ii,jj,firstindex:lastindex]))
             weachwind<-as.vector(c(weachwind,wind[ii,jj,firstindex:lastindex]))
             weachprecip<-as.vector(c(weachprecip,pr[ii,jj,firstindex:lastindex]))
         
####################################################################################################
 # Currently I am masking land laye based on if there is any na value in the weachtemo
 # I shoudl change it to a better method
      if(!(is.na(weachtemp[1])))
####################################################################################################
      {   
         #Unit conversion
          weachtemp<-weachtemp-273    # kelvin to Centigrade
          weachtempmax<-weachtempmax-273  # kelvin to centigrade
          weachtempmin<-weachtempmin-273  # kelvin to Centigrade
          weachhumidity<- weachhumidity*0.01 # percentage to fraction
          weachsolar<-weachsolar*24*60*60*1e-6 # W/m2 to MJ/m2
          weachwind<-abs(weachwind*3.85) # wind speed from 10m to 2m an dm/2 to miles/h
          weachprecip<-weachprecip*(24*60*60)*(1/1000)*39.37 #conerting kg/m2sec to kg.m2 to inche

         # These are approximations to satisfy weach function requirements
          relativehumidity <-weachhumidity
          relativehumiditymax <-weachhumidity+0.01
          relativehumiditymin <-weachhumidity-0.01
       
 # getting rid of missing/strange values (very small negative) of other troublesome climate data to avoid error in simulation
                           weachsolar<-ifelse(weachsolar>32765,30,weachsolar)
                           weachsolar<-ifelse(weachsolar<0,0,weachsolar)
                           weachtempmax<-ifelse(weachtempmax>32765,30,weachtempmax)
                           weachtempmax<-ifelse(weachtempmax<0,0,weachtempmax)
                           weachtempmin<-ifelse(weachtempmin>32765,30,weachtempmin)
                           weachtempmin<-ifelse(weachtempmin<0,0,weachtempmin)
                           weachtemp<-ifelse(weachtemp>32765,30,weachtemp)
                           weachtemp<-ifelse(weachtemp<0,0,weachtemp)
                           relativehumiditymin<-ifelse(relativehumiditymin>1,0.6,relativehumiditymin) # using 1 here for now, later chekc from NCEP website whats being used for missing value
                           relativehumiditymin<-ifelse(relativehumiditymin<0,0.1,relativehumiditymin)
                           relativehumiditymax<-ifelse(relativehumiditymax>1,0.6,relativehumiditymax) # using 1 now
                           relativehumiditymax<-ifelse(relativehumiditymax<0,0.1,relativehumiditymax)
                           relativehumidity<-ifelse(relativehumidity>1,0.6,relativehumidity) #using 1 now
                           relativehumidity<-ifelse(relativehumidity<0,0.1,relativehumidity)
                           weachwind<-ifelse(weachwind>32765,30,weachwind)
                           weachwind<-ifelse(weachwind<0,2,weachwind)
                           weachprecip<-ifelse(weachprecip>32765,0,weachprecip)
                           weachprecip<-ifelse(weachprecip<0,0,weachprecip)

            #Now we are ready to make dataframe to call weach
              forweach<-data.frame(year=weachyear,day=weachday,solarR=weachsolar,Tmax=weachtempmax,Tmin= weachtempmin,Tavg= weachtemp,RHmax=relativehumiditymax,RHmin=relativehumiditymin,RHavg=relativehumidity,WS= weachwind,precip= weachprecip)
 
              dat<-weachNEW(forweach,lat=currentlat,ts=1, temp.units="Celsius", rh.units="fraction",ws.units="mph",pp.units="in")
              filename=paste("/home/a-m/djaiswal/Scripts/ClimateProcessing/ISIMIP_slurm/",modelname,"/",scenarioname,"/Processed/",year,"/",year,formatC(ii,width=3,flag=0),formatC(jj,width=3,flag=0),".RData",sep="")
              
             save(dat,file=filename)
      } ##Checking with respect to index in soil data file ends here
      } #ii loop ends here     
   } #jj loop end here
 } #is.na testing loop ends here
 } #year loop ends here



         


         







