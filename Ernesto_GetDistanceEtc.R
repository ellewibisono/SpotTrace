## load packages
library(raster)
library(tidyverse)
library(sp)
library(readr)
library(data.table)

#read data, this is just the plain csv from the file
SpotTraceData <- fread('/Users/ElleWibisono/Desktop/Dissertation/R codes/SpotTraceData.txt', sep='\t')
SpotTraceData

library(lubridate)
SpotTraceData$'datetime'<-mdy_hm(SpotTraceData$'Date & Time')

### replace -99999 for lat and long with missing values
SpotTraceData <-
  SpotTraceData %>%
  dplyr::rename(`Boat Name` = `Boat Name (pseudonym)`) %>%
  # mutate(Longitude = ifelse(Longitude==-99999,NA,Longitude),
  #        Latitude = ifelse(Latitude==-99999,NA,Latitude)) %>%
  #ignore all observations without latitude and longitude
  dplyr::filter(Longitude !=-99999) %>%
  dplyr::filter(Latitude !=-99999) %>%
  dplyr::mutate(`datetime` = as.POSIXct(`datetime`)) %>%
  #re arrange
  dplyr::arrange(`Boat Name`,`datetime`) 


### let's try to add diff time, which is important for speed
### this in the original script was done by the adehabitat package
### but I didn't understand what they were using for distance function
SpotTraceData <-
  SpotTraceData %>%
  ungroup() %>%
  #group by boat name, this makes sure that we compute difftime only when it's between the same boat
  dplyr::group_by(`Boat Name`) %>%
  dplyr::mutate(time_diff=difftime(`datetime`,
         lag(`datetime`,order_by=`datetime`),
         unit="hours"))


# ### this is the burst as defined by the original script, we use it only briefly
SpotTraceData <-
  SpotTraceData %>%
  ungroup() %>%
  #begin of burst is flagged here as either a completely new boat or a week has passed
  mutate(BeginOfBurst = ifelse(is.na(time_diff) | time_diff > 7*24*60*60 ,1,0))  %>%
  mutate(burst = cumsum(BeginOfBurst)) %>%
  dplyr::select(-BeginOfBurst)


# I count 1023 begin of burst
# sum(SpotTraceData$BeginOfBurst)

#let's look at the basics of the fleet
# how many boats are tracked?
length(unique(SpotTraceData$`Boat Name`)) #253
# how many per port?
per_port<-
  SpotTraceData %>%
  group_by(`Boat Name`) %>%
  #grab only the first appearance of each boat
  mutate(pings = row_number() ) %>%
  filter(pings==1) %>%
  dplyr::select(-pings) %>%
  group_by(`Registration Port`) %>%
  summarise(count=n()) 

ggplot(per_port)+
  geom_col(aes(`Registration Port`,count)) +
  coord_flip()
  
#remove some double pings
SpotTraceData<-
  SpotTraceData %>% ungroup() %>%
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
SpotTraceData<-
  SpotTraceData %>%
  group_by(burst) %>%
  # compute distance
  mutate(distance = simple_distance(Longitude,Latitude,lag(Longitude),lag(Latitude))) %>%
  # distance in terms of km!
  mutate(distance= distance/1000) 


#now let's compute speed in km/h
SpotTraceData<-
  SpotTraceData %>%
  mutate(speed = distance/as.numeric(time_diff))


# ### turn it into a movement habitat
# trajectory<-
#   as.ltraj(
#     data.frame(
#       "longitude" = SpotTraceData$Longitude,
#       "latitude" = SpotTraceData$Latitude
#     ),
#     date=SpotTraceData$`Date & Time`,
#     id=SpotTraceData$`Boat Name`,
#     burst=as.character(SpotTraceData$burst),
#     infolocs = 
#       SpotTraceData %>%
#       select(-`Boat Name`,-`Date & Time`,-burst)
#   )
# 
# 
# ### Change dfx4 into a dataframe
# trajectory <- ld(trajectory)

### Calculate speed in km/hr
# dfx5$speed <- (60*1.852*dfx5$dist)/(dfx5$dt/(60*60)) 


#tag bad observations:
SpotTraceData<-
  SpotTraceData %>%
  group_by(burst) %>%
  mutate(quality = ifelse(
    (speed < 30 | is.na(speed)) & (lag(speed)<30 | is.na(lag(speed))),1,0))

#count crap:
SpotTraceData%>%
  group_by(quality) %>%
  summarise(count=n())


ggplot(SpotTraceData %>% filter(quality==1)) +
  geom_histogram(aes(speed,fill=`Fishing Gear`)) +
  facet_wrap(~`Fishing Gear`) +
  xlab("Speed (km/hr)")+
  ylab("Frequency") +
  ggtitle("Speed recorded per gear")


## simply drop all observations without lat and long
SpotTraceData<-
  SpotTraceData %>%
  filter(!is.na(Longitude)) %>%
  filter(!is.na(Latitude)) 

### Let's open some depth information; this is from Octpous, at:
### https://octopus.zoo.ox.ac.uk/geoserver/wcs?service=WCS&version=1.0.0 &request=GetCoverage &format=geotiff &coverage=context:depth_none_009_gebco &bbox=105,-15,139,5 &crs=EPSG:4326 &resx=0.09 &resy=0.09
library(raster)
depth <- raster("/Users/ElleWibisono/Desktop/Dissertation/R codes/gebco.tif")
depth<- as.data.frame(rasterToPoints(depth)) %>%
  dplyr::rename(depth=gebco) %>%
## notice that depth is -99999 for land, we need to change that
  mutate(depth=ifelse(depth==-99999,10000,depth))

ggplot(depth)+
  geom_tile(aes(x=x,y=y,fill=depth)) +
  scale_fill_gradient2(midpoint=0,low="blue",high="black",mid="white")

coordinates(depth)<-c("x","y") # turn it spatial
# at start they are just long-lat observations
proj4string(depth) <-CRS("+proj=longlat +datum=WGS84")

# now we need to interpolate (this finds depth at each ping)
library(akima)
#this takes quite a long time!
interpolated_depths<-akima::interpp(x=coordinates(depth)[,1],
                           y=coordinates(depth)[,2],
                           z = depth$depth,
                           xo = SpotTraceData$Longitude,
                           yo = SpotTraceData$Latitude,
                           duplicate="strip"
  )
# couple of warning, we are using the old function rather than the new one 

#add that to the original data frame
SpotTraceData$Depth = interpolated_depths$z

#let's count possible fishing spots!
SpotTraceData<-
  SpotTraceData %>%
  mutate(fishing = ifelse(quality==1 & Depth < -50 & Depth > -500 & speed < 5,1,0))

#plot all the points
ggplot(SpotTraceData%>% filter(fishing==1))+
  geom_tile(aes(x=x,y=y,fill=depth),data=as.data.frame(depth)) +
  scale_fill_gradient2(midpoint=0,low="blue",high="black",mid="white",guide=FALSE) +
  geom_point(aes(x=Longitude,y=Latitude,col=`Fishing Gear`),shape=3,lwd=0.5) +
  guides(col = guide_legend(override.aes = list(lwd = 3))) +
  ggtitle("Fishing Pings")

#plot only 2017 points
ggplot(SpotTraceData %>% filter(fishing==1) %>% filter(year(`Date & Time`)==2017))+
  geom_tile(aes(x=x,y=y,fill=depth),data=as.data.frame(depth)) +
  scale_fill_gradient2(midpoint=0,low="blue",high="black",mid="white",guide=FALSE) +
  geom_point(aes(x=Longitude,y=Latitude,col=`Fishing Gear`),shape=3,lwd=0.5) +
  guides(col = guide_legend(override.aes = list(lwd = 3)))+
  ggtitle("Fishing Pings (2017)")


## look at the guy that ends up in darwin
# near darwin coordinates: -11.968253, 129.256424
# grab the closest observations to darwin that was still "fishing"
darwin<-SpotTraceData %>%
  ungroup() %>%
  mutate(australia_distance = 
           simple_distance(Longitude,Latitude,129.256424,-11.968253)) %>%
  filter(fishing==1) %>%
  mutate(australia_distance = min_rank(australia_distance)) %>%
  filter(australia_distance  == 1) %>%
  select(-`Registration Port`,-`Fishing Gear`,-`Gross Tonnage`)
# it's burst  58, boat 2c65b075c849  
darwin<-SpotTraceData %>% filter(burst==darwin$burst)
# look at that little story
print(darwin %>% select(-`Battery State`,-quality,
                        -`Registration Port`,
                        -`Fishing Gear`,
                        -`Gross Tonnage`,
                        -`Message Type`),n=200)



## okay, let's try to figure out if we can do trips rather than bursts
# let's list all places that have low depth and where at least one boat spent 20 hours
# these are our candidate ports
ports<-
  SpotTraceData %>%
  ungroup()%>%
  # depth isn't perfect given that the observations are quite coarse
   dplyr::filter(`Message Type`=="STATUS" & time_diff>20 & Depth > -35) %>%
   dplyr::select(Latitude,Longitude,`Depth`) %>%
  # remove all the ones that are so close I don't need to check their distance to know they are the same port!
  dplyr::mutate(round_lat=round(Latitude,digits=3),
          round_long=round(Longitude,digits=3)) %>%
  dplyr::group_by(round_lat,round_long) %>%
  dplyr::mutate(row=row_number()) %>%
  dplyr::filter(row==1) %>%
  ungroup() %>%
  dplyr::select(-round_lat,-round_long,-row)

# many of these observations are actually just very close to one another and represent the same port
# so let's purge them

#compute distance matrix for all the "ports"
distances<-
  distm(x=data.matrix(ports %>% dplyr::select(Longitude,Latitude)),
      fun=distHaversine)
#turn them into km
distances<-distances/1000
#let's keep track of only ports that are at least 10km away from each other
tooclose<-NULL
#very ugly loop, but it gets the job done
for(i in 1:(length(ports$Latitude)-1)){
  tooclose<-c(tooclose,
              which(
                distances[i,(i+1):length(ports$Latitude)]<10
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
  geom_point(aes(x=Longitude,y=Latitude,col=`Fishing Gear`),data=
               ports,col="yellow",lwd=2) +
  guides(col = guide_legend(override.aes = list(lwd = 3)))


#find for each lat,long, distance to closest port
distance_from_port<-function(
  lat1,lon1,ports){
  positions<-cbind(lon1, lat1)
  distances<-NULL
  for(i in 1:length(positions[,1]))
  {
    distances<-c(distances,
                 min(distHaversine(positions[i,], 
                                   cbind(ports$Longitude, ports$Latitude))))
    #print(i)
  }
  return(distances/1000)
}

#now go through all the data and for each ping, figure out how close/far it is to any port
#this takes a very long time!
tripped<-
  SpotTraceData %>%
  ungroup() %>%
  dplyr::mutate(port_distance=distance_from_port(Latitude,Longitude,ports)) 
tripped<-
  tripped %>%
  #re arrange by boats and by date
  dplyr::arrange(`Boat Name`,`datetime`) %>%
  #break whenever you are on land or you switch boat
  dplyr::group_by(`Boat Name`) %>%
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
            `Boat Name`=first(`Boat Name`),
            `Fishing Gear`=first(`Fishing Gear`),
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
  valid %>% group_by(years,months,`Fishing Gear`) %>% 
  summarise(obs=n(),
            distance=median(na.omit(distance)),
            duration=median(na.omit(travelling_time)))

###ELLE's STARTS MESSING THINGS UP HERE 
#Open DB connection to get boat name (and will be joined with the pseudoname)
library(RPostgreSQL)
m <- dbDriver("PostgreSQL")
con <- dbConnect(m,host='localhost', port='5433',user="postgres", password="fishy", dbname="ern")
rs <- dbSendQuery(con, "SELECT b.boat_name, b.fishing_gear, b.uuid, b.gt_estimate, b.program_site, b.size_category,
                  f.oid, f.var_a, f.var_b, f.fish_genus, f.fish_species, s.cm,
                  d.landing_date
                  FROM ifish_fish f 
                  INNER JOIN ifish_sizing s on f.oid= s.fish_id 
                  INNER JOIN ifish_deepslope d on s.landing_id = d.oid
                  INNER JOIN ifish_boat b on d.boat_id= b.oid                  ")
df2 <- fetch(rs, n=-1)
dbHasCompleted(rs)

#subset postgres DB for just boat information variable so can left join with the pseudonym 
df3 <- df2 %>% 
  dplyr::select(-oid, -var_a, -var_b, -fish_genus, -fish_species, -cm, -landing_date) %>% 
  dplyr::distinct()

df4 <- df2 %>% 
  dplyr::select(boat_name, oid, var_a, var_b, fish_genus, fish_species, cm, landing_date)

#Join DB with trip_summary table, join by uuid == pseudonym 
trip_summary1 <- valid %>% left_join(df3, by=c('Boat Name'= 'uuid')) %>% 
  dplyr::select(-`Boat Name`, -`Fishing Gear`) 

###!!!!!!!FILTERING ISSUES study the data frame format to get only 
#depth, distance and total weight PER boat PER trip 

fishing_details <- trip_summary1 %>% dplyr::group_by(boat_name) 
fishing_details1 <- trip_summary1 %>% dplyr::group_by(boat_name) %>% 
  dplyr::mutate(avg.distance= mean(distance), avg.fishingT=mean(fishing_time), avg.travelT=mean(travelling_time)) #%>% 
  #dplyr::ungroup(boat_name) %>% 
  #dplyr::summarise(avgdist=first(avg.distance), size= first(size_category))
  #dplyr::distinct(boat_name, avg.distance, avg.travelT, avg.fishingT,
   #               avg.dist.gt, avg.fishingT.gt, avg.travelT.gt,
    #              program_site, gt_estimate, size_category, fishing_gear, depth, total_weight) 
  
#Export data frame fishing_details to CSV file 
write.csv(fishing_details1, file = "/Users/ElleWibisono/Desktop/Dissertation/R codes/Fishing_Details.csv")

#remove massive outlier in 5-29 GT fishing distance 
#noOutlier <- fishing_details %>% ungroup() %>% 
#  filter(size_category=='5-29GT') %>%
#  filter(avg.distance== max(avg.distance, na.rm=TRUE)) #this is the outlier data 
#Now remove that data from the fishing_details DF 
#fishing_details <- fishing_details %>% anti_join(noOutlier)

#Clean data from non demersal fishery things 
fishing_details <- fishing_details %>% 
  dplyr::filter(fishing_gear != 'PoleAndLine_Handline'&
                  fishing_gear != 'Others'&fishing_gear != 'Handline'&
                  program_site!='Tanjung Luar')

#the column size_category from the DB is wrong......... 
fishing_details <- fishing_details%>% dplyr::mutate(size_class= case_when(
  gt_estimate>=0 &  gt_estimate<= 4 |gt_estimate == NA ~"1-4GT",
  gt_estimate<=29 & gt_estimate >4 ~"5-29GT",
  gt_estimate>=30 & gt_estimate<=59~"30-59GT", 
  gt_estimate>=60~"60GT-up")) %>% 
  dplyr::group_by(size_class)

#change the orders of GT values in the DF, so that when plotted, it is small to big 
#levels(fishing_details$size_category) <- c('1-4GT','5-29GT','30-59GT','60GT-UP')
levels(fishing_details$size_class) <- c('1-4GT','5-29GT','30-59GT','60GT-UP')


fishing_details$size_class <- as.factor(fishing_details$size_class)
#fishing_details <- fishing_details[!(is.na(fishing_details$avg.distance)),]

#Try to plot things 
plotdist <- ggplot() +
  geom_point(data=fishing_details, aes(y=avg.distance, x= program_site, color=factor(size_category)))
#To make sure that the boxes in the box plot is in the right order 
fishing_details$size_category <- factor(fishing_details$size_category,
                      levels = c('1-4GT','5-29GT','30-59GT','60GT-UP'),ordered = TRUE)

plotdist2 <- ggplot() + #Boxplot per size_category 
  geom_boxplot(data=fishing_details, aes(x=size_category, y=avg.distance))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line())+
  labs(x="Vessel Size Category (GT)", y="Distance to Fishing Ground (km)")+ 
  scale_x_discrete(labels= c('1-4', '5-29', '30-59', '60+'))

plotdist2b <- ggplot()+
  geom_boxplot(data=fishing_details, aes(x=program_site, y=avg.distance))

plotdist3 <- ggplot()+
  geom_boxplot(data=fishing_details, aes(x=fishing_gear, y=avg.distance))

plotdist4 <- ggplot(data=fishing_details, aes(y=log(distance), x=gt_estimate))+ 
  geom_point()+ 
  geom_smooth(method='lm')
fitplotdist4 <- lm(log(distance)~gt_estimate, data=fishing_details)

plotdepth <- ggplot()+ #baka plot 
  geom_boxplot(data=fishing_details, aes(x=fishing_gear, y=depth)) #SOME ARE ON LAND WTF 

#Function to create half boxplot half scatter plot ala nature article. hot damn 
#load library to run function. 
library(rlang)
library(dplyr)
gg_jitterbox <- function(data_in, factor_col, numeric_col, offset) {
  
  # turn bare args into quosures
  quo_factor <- enquo(factor_col)
  quo_numeric <- enquo(numeric_col)
  
  # do the base R stuff that doesn't play nice with quosures
  # the extra factor() call deals with the factor_col parameter potentially
  # being character type - otherwise finding levels() etc will fail
  # quo_text(quo_factor) just gives back the string of the column name that we
  # put as a bare parameter
  numeric_factor <- as.numeric(factor(data_in[[quo_text(quo_factor)]]))
  ftr_breaks <- seq(length(levels(factor(data_in[[quo_text(quo_factor)]]))))
  ftr_labels <- levels(factor(data_in[[quo_text(quo_factor)]]))
  
  # easiest to do this first bit with dplyr instead of pulling out
  # of the ggplot object, because we then only have to call ggplot() once
  data_in %>%
    
    # another check to make sure we have factors when expected
    dplyr::mutate_if(is.character, as.factor) %>%
    
    # !! unquotes quosures, but only works well with tidyverse
    dplyr::group_by(!!quo_factor) %>%
    dplyr::mutate(d_ymin = min(!!quo_numeric),
           d_ymax = max(!!quo_numeric),
           d_lower = quantile(!!quo_numeric, 0.25),
           d_middle = median(!!quo_numeric),
           d_upper = quantile(!!quo_numeric, 0.75)) %>%
    
    ggplot() +
    
    # aes_() requires quoted formula types, but allows mixing
    # of enquo-ed bare names (e.g. quo_factor) and just
    # normal dplyr-style column names
    geom_boxplot(aes_(x = ~numeric_factor - offset,
                      ymin = ~d_lower,
                      ymax = ~d_upper,
                      lower = ~d_lower,
                      middle = ~d_middle,
                      upper = ~d_upper,
                      width = 2 * offset,
                      fill = quo_factor),
                stat = "identity") +
 
    
    geom_jitter(aes_(x = ~numeric_factor + offset,
                     y = quo_numeric,
                     color = quo_factor, alpha=0.35),
                width = offset - 0.25 * offset,
                height = 0, show.legend = FALSE) +
    
    # bottom vertical segment
    geom_segment(aes(x = numeric_factor,
                     y = d_ymin,
                     xend = numeric_factor,
                     yend = d_lower)) +
    
    # top vertical segment
    geom_segment(aes(x = numeric_factor,
                     y = d_ymax,
                     xend = numeric_factor,
                     yend = d_upper)) +
    
    # top horizontal segment
    geom_segment(aes(x = numeric_factor - offset,
                     y = d_ymax,
                     xend = numeric_factor,
                     yend = d_ymax)) +
    
    # top vertical segment
    geom_segment(aes(x = numeric_factor - offset,
                     y = d_ymin,
                     xend = numeric_factor,
                     yend = d_ymin)) +
    
    # have to manually add in the x scale because we made everything numeric
    # to do the shifting
    scale_x_continuous(breaks = ftr_breaks,
                       #labels = ftr_labels +
                       labels=c('1-4', '5-29', '30-59', '60+'))+

    # this also needs to be added manually because of the change to numeric
    #labs(x = quo_text(quo_factor)) #original code 
    labs(x="Vessel Size Category (GT)", y="Distance to Fishing Ground (km)") #this is elle 
  #bruteforcing the labels 
}

###End of the gg_jitterbox function. hot damn indeed. 
# data_in should be a data frame
# factor_col should be a bare column name (not a string)
#     although it will work if that column is factor or a character type
# numeric_col is the y axis continuous variable
# offset is the width of the boxplots and jittered point cloud

#make sure data is in df format 
fishing_details <- as.data.frame(fishing_details) 
#order the variable so it is not mixed up when plotted 
fishing_details$size_class <- factor(fishing_details$size_class,levels=c('small','medium','large','xlarge'))

#plot the jitterbox
#Create a color palatte for the colors of the points and the plots. 
#These colors will correspond to the plots on the fishing ground map 
library(RColorBrewer)
elle_palette <- brewer.pal(n = 9, "YlOrBr")[4:7]
#remove outlier 
C <- min(A$distance)
fishing_details <- fishing_details %>%dplyr::filter(distance != C)
  
#each data point is a distance based on size_category 
jitterboxdist2 <- gg_jitterbox(fishing_details, size_class, 
                               log(distance), 0.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line())+
  scale_color_manual(values=elle_palette)+
  scale_fill_manual(values = elle_palette)
  

jitterboxdist2b <- gg_jitterbox(fishing_details, size_class, 
                               distance, 0.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line())+ 
  scale_color_discrete(name="Size Category")+
  scale_fill_discrete(guide=FALSE)+
  scale_color_manual(name="Size Category",values=elle_palette)+
  scale_fill_manual(values = elle_palette)

#ANOVA on log transformed distances 
a1 <- aov(log(fishing_details$distance)~fishing_details$size_class)
a1HSD <- TukeyHSD(a1, conf.level=0.95)
library(agricolae)
HSD.test(a1, 'fishing_details$size_class')

#each data point is an avg. distance based on size_category 
jitterboxdist3 <- gg_jitterbox(fishing_details1, size, avgdist, 0.2)

#df per size category 
fishing_details_s <- fishing_details %>% dplyr::filter(size_class=='1-4GT')
fishing_details_m <- fishing_details %>% dplyr::filter(size_class=='5-29GT')
fishing_details_l <- fishing_details %>% dplyr::filter(size_class=='30-59GT')
fishing_details_xl <- fishing_details %>% dplyr::filter(size_class=='60GT-up')

#Check distribution for distance per size category (see normal or not)
histogram(fishing_details_s$distance)
histogram(fishing_details_m$distance)
histogram(fishing_details_l$distance)
histogram(fishing_details_xl$distance)
#all skewed left 

#Try log transforming it 
histogram(log(fishing_details_s$distance))
histogram(log(fishing_details_m$distance))#skew right 
histogram(log(fishing_details_l$distance))#skew right 
histogram(log(fishing_details_xl$distance))
logdist <- log(fishing_details$distance)
fitdist <- lm(logdist~size_class, data= fishing_details)
anova(fitdist, data= fishing_details)

fishing_details$size_class <- as.character(fishing_details$size_class) #change into chr so can use grep to 
#match the pattern of the value 

#later while doing Dunn test, the names of the values in size_category will be problematic. So 
#now will replace all the names with a differnt name 
fishing_details$size_class[grepl("1-4GT",fishing_details$size_class,fixed = TRUE)]<-"small"
fishing_details$size_class[grepl("5-29GT",fishing_details$size_class,fixed = TRUE)]<-"medium"
fishing_details$size_class[grepl("30-59GT",fishing_details$size_class,fixed = TRUE)]<-"large"
fishing_details$size_class[grepl("60GT-up",fishing_details$size_class,fixed = TRUE)]<-"xlarge"
#change as factor so can use KW test 
fishing_details$size_class <- as.factor(fishing_details$size_class)

#bc not normal use Kruskal-Wallis test instead of ANOVA 
kruskal.test(distance ~ size_class, data = fishing_details) 
library(FSA)
DT = dunnTest(distance ~ size_class,
              data=fishing_details,
              method="bh")  
### Compact letter display
PT = DT$res
PT
library(rcompanion)
cldList(P.adj ~ Comparison,
        data = PT,
        threshold = 0.05)

#log transform the data and try again 


#take a look at this one:
one_trip<-
  tripped  %>% 
  filter(tripid==20)

#looks like the boat simply moved from one port over the course of a day
print(tripped  %>% select(-`Gross Tonnage`,-`Registration Port`,
                          -`Battery State`,-`Message Type`,
                          -`quality`,-burst,-`Fishing Gear`,
                          -Latitude,-Longitude,-`Boat Name`) %>%
        mutate(travelling_time=ifelse(Depth<0,time_diff,0)) %>% 
        filter(tripid==20),n=2000)
ggplot(as.data.frame(depth))+
  geom_tile(aes(x=x,y=y,fill=depth)) +
  scale_fill_gradient2(midpoint=0,low="blue",high="black",mid="white",guide=FALSE) +
  geom_segment(aes(x=Longitude,y=Latitude,xend=lead(Longitude),yend=lead(Latitude)),
               data=one_trip,
               col="yellow",lwd=2,
               arrow=arrow(length=unit(0.05,"cm"))) +
  guides(col = guide_legend(override.aes = list(lwd = 3)))





# look at number of trips
ggplot(validsummary) +
  geom_col(aes(x=months,y=obs,fill=`Fishing Gear`)) +
  facet_wrap(~years)+
  ggtitle("# of trips")

#look at average distance out
ggplot(valid %>% group_by(years,`Fishing Gear`) %>% 
         summarise(obs=n(),
                   distance=median(distance),
                   duration=median(travelling_time))) +
  geom_col(aes(x=`Fishing Gear`,y=distance,fill=`Fishing Gear`)) +
  facet_wrap(~years) +
  ggtitle("Median Distance (km)")

duration2<-
  ggplot(valid %>% group_by(years,`Fishing Gear`) %>% 
         summarise(obs=n(),
                   distance=median(distance),
                   duration=median(travelling_time))) +
  geom_col(aes(x=`Fishing Gear`,y=duration/24,
               fill=`Fishing Gear`)) +
  scale_fill_discrete(guide=FALSE)+
  facet_wrap(~years) +
  coord_flip() +
  ggtitle("Median Duration (days)") +
  ylab("Trip length (days)") +
  xlab("Gear")

duration1<-
  ggplot(valid %>% filter(`Fishing Gear` %in% c("Dropline","Longline","Others"))) +
  geom_histogram(aes(travelling_time/24),binwidth = 1) +
  facet_wrap(~`Fishing Gear`) +
  xlab("Trip length (days)")  +
  ylab("Observations") +
  ggtitle("Trip Duration Histogram")
  
library(gridExtra)
grid.arrange(duration1,duration2)

