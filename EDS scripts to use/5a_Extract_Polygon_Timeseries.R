####################################################################
### EDS R scripts to attach timeseries variables to in situ data ###
### Originally developed & conceptualized by T.A.Oliver.         ###
### Revised & Maintained by K.R.Tanaka & T.A.Oliver.             ###
### POC: kisei.tanaka@noaa.gov, thomas.oliver@noaa.gov,          ###
### jessica.perelman@noaa.gov, juliette.verstaen@noaa.gov        ###
####################################################################

# This code is very beta - developed by Tom Oliver (thomas.oliver@noaa.gov) 10-23
# This is a modification of 3b_Extract_Timeseries.R to extract EDS data
# over spatial polygons, within a single specified time interval.
# It runs reasonably quickly, and is currently custom built to pull
# a sf polygon set from the downscaling analysis.
#To generalize it, you just need to enter with a sf polygon dataframe
#(e.g. df line 59) with columns:
#"ClustID", "StartDate", "EndDate", "CenLon", "CenLat" (for centroid position), and a polygon geometry
#we also derive "data unit" i.e. ISLAND, from the ClustID 
# Other than than it follows the same structure as EDS, just using terra and sf packages
# with raster and polygon data instead of points and grids.

rm(list = ls())

dir = paste0(getwd(), "/")

source("scripts/eds_functions.R")
source("scripts/HelperCode/ExpandingExactExtractR.R")
###########################################################################
### read survey data points, assign distinct lat, lon, and time columns ###
###########################################################################
load("M:/Environmental_Data_Summary/InSituPointFiles/BenthicCoverClusterDataset_euc_FINAL.Rdata")
library(sf)
library(exactextractr)
library(terra)

#From CoralTrends:
#BC Clust - Assign Temporal PERIOD under "FEW" and "LOTS" scenarios
BC=VPc_BC_FINAL@data

#Get Period (independent of ZONE)
tD_DAC=dist(BC$Date)
tC_DAC=hclust(tD_DAC)
BC$PERIOD_ID=cutree(tC_DAC,h=120) # 120 day PERIODS
#ID to Mean_Period_Date
Pid2Pd=BC %>%
  dplyr::group_by(PERIOD_ID) %>%
  dplyr::summarize(PERIOD=mean(Date)) %>% 
  ungroup()

#Match Back 
BC$PERIOD=Pid2Pd$PERIOD[match(BC$PERIOD_ID,Pid2Pd$PERIOD_ID)]

#Get ZONE, PERIOD lookup indexed on SITEVISITID
SZP=BC[,c("SITEVISITID","SelectedClustLevel","PERIOD")]
names(SZP)=c("SITEVISITID","ZONE","PERIOD")

ZonesSF=st_as_sf(VPu_BC_FINAL)

Zdates=BC %>% 
  rename(ClustID = SelectedClustLevel) %>% 
  group_by(ClustID) %>%
  reframe(StartDate=min(Date),EndDate=max(Date)) 

df=left_join(ZonesSF,Zdates,by="ClustID")

###########################
### read bounding boxes ###
###########################
Bounding_Boxes = read.csv("data/Bounding_Boxes.csv"); unique(Bounding_Boxes$unit)

#######################################################################
### Build list of target environmental variables                    ###
### See folder names in M:/Environmental Data Summary/DataDownload/ ###
#######################################################################
paramdir = "M:/Environmental_Data_Summary/Data_Download/"
parameters = list.files(path = paramdir, full.names = F); parameters

#########################################
### Read EDS Parameter/Variable Table ###
#########################################
Parameter_Table = read.csv("data/EDSpoly_parameters.csv")
names(Parameter_Table)
parameters=unique(Parameter_Table$Dataset)

#####################################
### Prep For Variable Extractions ###
#####################################

# locate each point in an unit bounding box
PT = points.in.polys(df$CenLons, 
                     df$CenLat, 
                     Bounding_Boxes)

df$DATA_UNIT = matrix(unlist(strsplit(df$ClustID,"_")),ncol = 2,byrow = T)[,1]
df$DATA_UNIT = gsub(" ", "_", df$DATA_UNIT)

# Drop points outside target area and report
print(paste("Dropping", length(which(df$DATA_UNIT == "NONE_ASSIGNED")),
            "points of", nrow(df),
            "entered points, as outside Geographic Scope"))

# List of spatial units
unique_units = sort(unique(df$DATA_UNIT)); unique_units

############################################################
### Extract Time Series Variables for every survey point ###
############################################################

# Normal loop for each parameter
start_time <- Sys.time()
for(parameter_i in 8:length(parameters)){
  
  # parameter_i = 1
  
  # Get Unit_Level_Data Directory for this Param
  param.name = parameters[parameter_i]; param.name
  
  this_param_i = which(Parameter_Table$Dataset == param.name); this_param_i
  
  godir = paste(paramdir, param.name, "/Unit_Level_Data", sep = ""); godir
  
  paramsum = unlist(strsplit(as.vector(Parameter_Table[this_param_i, "Summaries"]), ";")); paramsum
  
  # For each unit
  for(unit_i in 1:length(unique_units)){
    
    # unit_i = 38
    
    unique_units[unit_i]
    
    # retrieve raw netcdf data for each unit 
    ncfile = list.files(godir, pattern = paste0(unique_units[unit_i], "_"), full.names = T)
    
    # if there are no data then move to next unit
    if(length(ncfile) == 0){
      
      print(paste0("Skipping ", unique_units[unit_i], " because there are no data"))
      
      next
      
    }
    
    # Subset to Unit
    df_i = which(df$DATA_UNIT == unique_units[unit_i])
    df_unit=st_make_valid(df[df_i,])
    #df_unit=(df[df_i,])
    
    #Pull island as raster stack
    #ras=stack(ncfile)
    rasT=rast(ncfile)
    rasTimes_all=time(rasT)
    
    #Subset just to start/end times
    unitSD=(min(df_unit$StartDate))
    unitSDi=which.min(abs(rasTimes_all-as.POSIXct(unitSD)))
    unitED=(min(df_unit$EndDate))
    unitEDi=which.min(abs(rasTimes_all-as.POSIXct(unitED)))
    
    rasT=subset(rasT,subset=unitSDi:unitEDi)
    rasTimes=time(rasT)

    EEER=ExpandingExactExtractR(rT=rasT,sfpoly=df_unit)
    TimeByPoly=EEER[[1]]
    BufferedDists=EEER[[2]]
    if(any(BufferedDists>0)){
      print(paste(paste(unique_units[unit_i],": Buffered out",length(which(BufferedDists>0)),"polygon(s):"),
                paste(names(BufferedDists)[which(BufferedDists>0)],"at",BufferedDists[which(BufferedDists>0)],"m",collapse = ", ")))
    }
    # #loop through years - probably could use larger blocks and speed this up, but...
    # rasYears=year(rasTimes)
    # uY=unique(rasYears)
    
    # TimeByPoly=NULL
    # for (i_y in 1:length(uY)){
    #   #i_y=1
    #   layer_i=which(rasYears==uY[i_y])
    #   rasTy=subset(rasT,subset=layer_i)
    #   polyint=exact_extract(x = rasTy,y=df_unit,
    #                         fun='mean',
    #                         progress=F)
    #   TimeByPoly_uY=cbind(data.frame(time=rasTimes[layer_i]),t(polyint))
    #   names(TimeByPoly_uY)=c("time",df_unit$ClustID)
    #   TimeByPoly=rbind(TimeByPoly,TimeByPoly_uY)
    #   if(i_y%%10==1){print(uY[i_y])}    
    # }
    
    
    # Apply Summaries to Start and End Times Summaries
    for(sum_i in 1:length(paramsum)){
      
      # sum_i = 1
      
      paramsum.name = paste0(paramsum[sum_i], "_", param.name); paramsum.name
      
      if(!paramsum.name %in% substr(names(df), 1, nchar(paramsum.name))){
        eval(parse(text = paste0("df$",paramsum.name,"_SPAN=-9991")))
      }
      
      # For each polygon in df_unit
      for(sumpt_i in 1:length(df_i)){
        
        # sumpt_i = 1
        zoneSD=df_unit$StartDate[sumpt_i]
        zoneSDi=which.min(abs(rasTimes-as.POSIXct(zoneSD)))
        zoneED=df_unit$EndDate[sumpt_i]
        zoneEDi=which.min(abs(rasTimes-as.POSIXct(zoneED)))
        ts_SPAN = TimeByPoly[zoneSDi:zoneEDi,df_unit$ClustID[sumpt_i]] 
        t_SPAN = rasTimes[zoneSDi:zoneEDi]
        
        if(paramsum[sum_i] %in% c("mean", "q05", "q95","sd")){
          eval(parse(text = paste0("df$", paramsum.name, "_SPAN[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_SPAN, na.rm = T)")))
        }else{
          eval(parse(text = paste0("df$", paramsum.name, "_SPAN[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_SPAN, na.rm = T, t = t_SPAN)")))
        } # END if
        
      } # END Loop over this unit's points (for 1:length(df_i))
      
      print(paste(unique_units[unit_i], 
                  paramsum.name, "Done.", 
                  unit_i, "of", 
                  length(unique_units), "units. Completed", 
                  sumpt_i, " points..."))
      
      save(df, file = paste0(dir, "outputs/EDSPoly_Timeseries_", Sys.Date(), ".Rdata"))
      #load(file = paste0(dir, "outputs/EDSPoly_Timeseries_", Sys.Date(), ".Rdata"))
    } # END Loop over each summary function  (for 1:length(paramsum))
    
  }
  
} # END Loop over each unit

end_time <- Sys.time()
end_time - start_time
beep()



