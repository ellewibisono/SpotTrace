#Determine Fishing Grounds Based on LatLong, speed 
#Need CSV file generated from LoadIFishFindMeSpot.R
#CVS file name is snappertracker. CSV 

library(ggplot2)
library(sp)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(plyr)
library(dplyr)
library(adehabitatMA)
library(adehabitatHR)
library(adehabitatHS)
library(adehabitatLT)
library(DBI)
library(RPostgreSQL)
library(foreign)
library(Hmisc)
library(doBy)
library(ncdf4)
library(tidyr)
library(raster)
library(colorspace)

df <- read.csv(file = "/Users/ellewibisono/Desktop/Dissertation/Gear Selectivity/Rcode/snappertracker.csv", header=TRUE)

#exploratory plots on distribution of gear types based on daily avg lat long 
#Just curious, pls humour me 
plot1 <- ggplot()+ geom_point(data=df, aes(x=daily_avg_latitude, y=daily_avg_longitude)) #def some geographic differences 
#but unclear if due to bathymetry, societal/cultural trends and biases, economical factor, etc 

#Determine fishing ground 
#Adopted from Ernesto's Get Distance codes 

library(lubridate)
df$datetime<-ymd(df$date_time) #change date time format
#remove uneccessary columns 
df <- df %>% dplyr::select(-oid, -oid.1, -tracker_name, -unix_time, -battery_state, -date_time, -tracker_id.1, -X, -tracker_status)
#make sure data is only from Snapper program 
df <- df %>% filter(program_type== 'Snapper')

### replace -99999 for lat and long with missing values
df <-
  df %>%
  dplyr::rename(boatname = pseudonyms) %>%
  # mutate(Longitude = ifelse(Longitude==-99999,NA,Longitude),
  #        Latitude = ifelse(Latitude==-99999,NA,Latitude)) %>%
  #ignore all observations without latitude and longitude
  dplyr::filter(longitude !=-99999) %>%
  dplyr::filter(latitude !=-99999) %>%
  dplyr::arrange(boatname,datetime) 

### let's try to add diff time, which is important for speed
### this in the original script was done by the adehabitat package
### but I didn't understand what they were using for distance function
df <-
  df%>% 
  ungroup() %>%
  #group by boat name, this makes sure that we compute difftime only when it's between the same boat
  dplyr::group_by(boatname) %>%
  dplyr::mutate(time_diff=difftime(`datetime`,
                                   lag(`datetime`,order_by=`datetime`),
                                   unit="hours"))
# ### this is the burst as defined by the original script, we use it only briefly
df <-
  df %>%
  ungroup() %>%
  #begin of burst is flagged here as either a completely new boat or a week has passed
  mutate(BeginOfBurst = ifelse(is.na(time_diff) | time_diff > 7*24*60*60 ,1,0))  %>%
  mutate(burst = cumsum(BeginOfBurst)) %>%
  dplyr::select(-BeginOfBurst)

# how many boats are tracked?
length(unique(df$boatname)) #417

# how many per port?
per_port<-
  df %>%
  group_by(boatname) %>%
  #grab only the first appearance of each boat
  mutate(pings = row_number() ) %>%
  filter(pings==1) %>%
  dplyr::select(-pings) %>%
  group_by(registration_port) %>%
  summarise(count=n()) 

#how mnay boats per port, depending on the geartype 
geartype_per_port <- df %>% 
  dplyr::select(-boat_id, -tracker_id, -tracker_start_date, -tracker_end_date, -findmespot_id, 
                -model_id, -show_custom_msg,
                -hidden,-message_content) %>%
  group_by(registration_port,fishing_gear) %>%
  mutate(count=n()) %>% 
  distinct(registration_port, fishing_gear, count)
write.csv(geartype_per_port, '/Users/ellewibisono/Desktop/Dissertation/Gear Selectivity/Rcode/registration_port.csv')
#Some registration ports are under different names (i.e. PP Tenau, PP Tenau Kupang, etc.)
#Clean port data 
registration_port_df <- geartype_per_port %>% 
  ungroup() %>% dplyr::distinct(registration_port)%>% 
  arrange(as.character(registration_port)) %>% 
#PP Tenau Kupang <-  PP Tenau; Labuan Sumbawa <- Labuhan Sumbawa, PP. Muaro <-  PP Muaro 
  mutate(registration_port= replace(registration_port, list=("PP Tenau Kupang","Labuan Sumbawa","PP.Muaro"), "PP Tenau","Labuhan Sumbawa", "PP Muaro"))
#plot it, to visualize 
plotgeartype <- ggplot()+ geom_col(data=geartype_per_port, aes(x=fishing_gear, y=count, color=fishing_gear)) + 
  facet_wrap(vars(registration_port)) #not highly informative but in general most ports only have ONE gear type. Max 2 gear types. 

#remove some double pings
df<-
  df %>% ungroup() %>%
  group_by(burst) %>%
  distinct(`datetime`,.keep_all = TRUE) %>%
  ungroup()

#simple helper, computes distance as crows flies
library(geosphere)
simple_distance<-function(lon1,lat1,lon2,lat2){
  elements<-length(lon1)
  distances<-distHaversine(cbind(lon1, lat1), cbind(lon2, lat2))
  return(distances)
}

## let's try to build distances by hand without using new libraries
df<-
  df %>%
  group_by(burst) %>%
  # compute distance
  mutate(distance = simple_distance(longitude,latitude,lag(longitude),lag(latitude))) %>%
  # distance in terms of km!
  mutate(distance= distance/1000) 

#now let's compute speed in km/h
df<-
  df %>%
  mutate(speed = distance/as.numeric(time_diff))

#tag bad observations:
df<-
  df %>%
  group_by(burst) %>%
  mutate(quality = ifelse(
    (speed < 30 | is.na(speed)) & (lag(speed)<30 | is.na(lag(speed))),1,0))

#count crap:
df%>%
  group_by(quality) %>%
  summarise(count=n())

## simply drop all observations without lat and long
df<-
  df %>%
  filter(!is.na(longitude)) %>%
  filter(!is.na(latitude)) 

#load some depth information 
library(raster)
depth <- raster("/Users/ElleWibisono/Desktop/Dissertation/R codes/gebco.tif")
depth<- as.data.frame(rasterToPoints(depth)) %>%
  dplyr::rename(depth=gebco) %>%
  ## notice that depth is -99999 for land, we need to change that
  mutate(depth=ifelse(depth==-99999,10000,depth))

coordinates(depth)<-c("x","y") # turn it spatial
# at start they are just long-lat observations
proj4string(depth) <-CRS("+proj=longlat +datum=WGS84")

# now we need to interpolate (this finds depth at each ping)
library(akima)
#this takes quite a long time!
interpolated_depths<-akima::interpp(x=coordinates(depth)[,1],
                                    y=coordinates(depth)[,2],
                                    z = depth$depth,
                                    xo = df$longitude,
                                    yo = df$latitude,
                                    duplicate="strip"
)
# couple of warning, we are using the old function rather than the new one 

#add that to the original data frame
df$Depth = interpolated_depths$z
write.csv(df,"/Users/ellewibisono/Desktop/Dissertation/Gear Selectivity/Rcode/fishingpings.csv") 
#because interpolation takes a while, save df, so if you just need the complete DF, can just use the csv, instead of running the entire code


#let's count possible fishing spots!
df<-
  df %>%
  mutate(fishing = ifelse(quality==1 & Depth < -50 & Depth > -500 & speed < 5,1,0))

#Also use data on data on CODRS photo to filter the fishing location 
#first_CODRS_picture_date must be >= start of trip and <= end of trip 

## okay, let's try to figure out if we can do trips rather than bursts
# let's list all places that have low depth and where at least one boat spent 20 hours
# these are our candidate ports
ports<-
  df %>%
  ungroup()%>%
  # depth isn't perfect given that the observations are quite coarse
  dplyr::filter(message_type=="STATUS" & time_diff>20 & Depth > -35) %>%
  dplyr::select(latitude,longitude,`Depth`) %>%
  # remove all the ones that are so close I don't need to check their distance to know they are the same port!
  dplyr::mutate(round_lat=round(latitude,digits=3),
                round_long=round(longitude,digits=3)) %>%
  dplyr::group_by(round_lat,round_long) %>%
  dplyr::mutate(row=row_number()) %>%
  dplyr::filter(row==1) %>%
  ungroup() %>%
  dplyr::select(-round_lat,-round_long,-row)

# many of these observations are actually just very close to one another and represent the same port
# so let's purge them

#compute distance matrix for all the "ports"
distances<-
  distm(x=data.matrix(ports %>% dplyr::select(longitude,latitude)),
        fun=distHaversine)
#turn them into km
distances<-distances/1000
#let's keep track of only ports that are at least 10km away from each other
tooclose<-NULL
#very ugly loop, but it gets the job done
for(i in 1:(length(ports$latitude)-1)){
  tooclose<-c(tooclose,
              which(
                distances[i,(i+1):length(ports$latitude)]<10
              )
              +i
  )
}
#lots of repetitions
tooclose<-sort(unique(tooclose))
# remove all the ports that are too close
ports<-
  ports %>%
  filter(!(row_number() %in% tooclose))

#plot the ports on a map
ggplot(as.data.frame(depth))+
  geom_tile(aes(x=x,y=y,fill=depth)) +
  scale_fill_gradient2(midpoint=0,low="blue",high="black",mid="white",guide=FALSE) +
  geom_point(aes(x=longitude,y=latitude,col=`Fishing Gear`),data=
               ports,col="yellow",lwd=2) +
  guides(col = guide_legend(override.aes = list(lwd = 3))) #some ports are wrong 
#Maybe we will just ports from the database and then do a boundary around it 

##find for each lat,long, distance to closest port
distance_from_port<-function(
  lat1,lon1,ports){
  positions<-cbind(lon1, lat1)
  distances<-NULL
  for(i in 1:length(positions[,1]))
  {
    distances<-c(distances,
                 min(distHaversine(positions[i,], 
                                   cbind(ports$longitude, ports$latitude))))
    #print(i)
  }
  return(distances/1000)
}

#now go through all the data and for each ping, figure out how close/far it is to any port
#this takes a very long time!
tripped<-
  df %>%
  ungroup() %>%
  dplyr::mutate(port_distance=distance_from_port(latitude,longitude,ports)) 
tripped<-
  tripped %>%
  #re arrange by boats and by date
  dplyr::arrange(boatname,`datetime`) %>%
  #break whenever you are on land or you switch boat
  dplyr::group_by(boatname) %>%
  #you are "on_land" if you are the first observation, your depth is positive, you are less than 15km from port or you haven't pinged for more than a day
  dplyr::mutate(on_land = Depth>0 | row_number()==1 | port_distance < 15 | time_diff > 25) %>%
  ungroup() %>%
  #associate a separate id given by the last ping that was "on land"
  dplyr::mutate(tripid=cumsum(on_land)) %>%
  #turn that into a trip id
  dplyr::group_by(tripid) %>%
  dplyr::mutate(trip_length=n()) %>%
  #remove trips that have too few pings
  dplyr::filter(trip_length>5)

#now compute for each trip their duration + time spent fishing
trip_summary<-
  tripped %>%
  ungroup() %>%
  dplyr::filter(quality==1) %>%
  #count time spent fishing
  dplyr::mutate(fishing_time=ifelse(fishing==TRUE,time_diff,0)) %>%
  #count time spent travelling for each ping
  dplyr::mutate(travelling_time=ifelse(on_land,0,time_diff)) %>%
  #for each trip
  dplyr::group_by(tripid) %>%
  dplyr::mutate(months=month(ymd_hms(datetime)), years=year(ymd_hms(datetime))) %>%
  #compute all summary statistics
  dplyr::summarise(travelling_time=sum(na.omit(travelling_time)),
                   fishing_time=sum(fishing_time),
                   distance=sum(distance),
                   boatname=first(boatname),
                   fishing_gear=first(fishing_gear),
                   burst=first(burst),
                   depth=mean(Depth),
                   months=first(months), years= first(years), 
                   datetime= first(datetime)
  ) 


# remove trips that have no fishing time
#there are a lot of trips that unfortunately have no fishing in them, let's look
trip_summary %>%
  ungroup() %>%
  summarise(sum(na.omit(fishing_time)==0))
#i get 1418 trips with no fishing, which is sad because that's 30% of the data

# let's focus on the "real" trips
valid<-
  trip_summary %>%
  ungroup() %>%
  filter(fishing_time>0) %>%
  filter(!(is.na(distance) | is.na(travelling_time)))

#group them by year and month
validsummary<-
  valid %>% group_by(years,months,fishing_gear) %>% 
  summarise(obs=n(),
            distance=median(na.omit(distance)),
            duration=median(na.omit(travelling_time)))
#see if there are any correlation between depth and fishing gear from the trip summary 
ggplot(valid, aes(x=fishing_gear, y=depth))+geom_point()
