require(GeoAviR)
require(rgdal)
library(raster)
require(rgdal)
require(plyr)
require(sp)
library(geosphere)
library(ggplot2)
library(gplots)
library(Cairo)
require(maptools)
library(htmltools)
library(RColorBrewer)
library(xtable)
library(classInt) 
library(rgeos)


projection.string <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#==============================================================================================================================================  
# load embedded dataset by name
#==============================================================================================================================================  
dataset.embedded.load <- function(dataset.name='quebec'){  
  data(list=dataset.name)
  return(get(dataset.name))
}


#==============================================================================================================================================  
# filter dataset
#==============================================================================================================================================  
dataset.filter <- function(dataset,  
                           transect.id, 
                           distance.labels = c("A", "B", "C", "D"),
                           distance.midpoints = c(25, 75, 150, 250),
                           distance.field,
                           effort.field,
                           lat.field,
                           long.field,
                           sp.field,
                           date.field,
                           dist2m){
  
  return(distance.filter(dataset, 
                         distance.labels = distance.labels, 
                         distance.midpoints = distance.midpoints, 
                         transect.id = transect.id,
                         distance.field = distance.field,
                         effort.field = effort.field, 
                         lat.field = lat.field, 
                         long.field = long.field,
                         sp.field = sp.field, 
                         date.field = date.field,
                         dist2m = dist2m))        
}

#==============================================================================================================================================  
# 
#==============================================================================================================================================  
dataset.observations.shp <- function(dataset, projection.string, count.field = "Count"){
  
  transect <- data.frame(lat=dataset$LatStart,lon=dataset$LongStart)
  coordinates(transect) <- ~lon + lat
  
  transect<-SpatialPointsDataFrame(transect,data=dataset[,count.field,drop=FALSE])
  proj4string(transect)<-CRS(projection.string)
  
  return (transect)
  
}

#==============================================================================================================================================  
# 
#==============================================================================================================================================  
earth.map.shp <- function(file.with.path, layer="ne_10m_land"){
  readOGR(dsn = file.with.path,layer = layer)
} 

#==============================================================================================================================================  
# 
#==============================================================================================================================================  
dataset.and.earth.map.shp <- function(transect, shp){
  prj <- proj4string(transect)
  shpm<-spTransform(shp,CRS(prj)) 
}



#==============================================================================================================================================
# 
#==============================================================================================================================================  
grid.new <- function(shpm.data, size, latitude1=44, latitude2=52, longitude1=-70, longitude2=-56, projection.string, hexgrid = FALSE){
  
  new.grid <- create.grid(Latitude=c(latitude1,latitude2),
                        Longitude=c(longitude1, longitude2),
                        Grid.size=c(size, size),
                        Clip=FALSE,
                        clip.shape= shpm.data,
                        projection=CRS(projection.string), hexgrid = hexgrid)
  
  new.grid$ID<-paste("parc",new.grid$ID,sep="")
  
  return(new.grid)
}


#==============================================================================================================================================
# 
#==============================================================================================================================================  
grid.final <- function(transect, new.grid){
  new.grid <- new.grid[apply(gIntersects(transect, new.grid ,byid=TRUE),1,any),] 
  return(new.grid)
}


#==============================================================================================================================================
# 
#==============================================================================================================================================  

grid.clip <- function(new.grid, shpm){
  gDifference(new.grid, shpm, byid = T) # note: may take several minutes
  return(new.grid)
} #à inclure comme une option à cocher?


#==============================================================================================================================================
#
#==============================================================================================================================================  
new.grid.with.zones<- function(shapefile.data, transect){
  new.grid <- shapefile.data 
  
  temp<-spTransform(new.grid,CRS("+proj=utm +zone=17 +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))  #project to calculate area with package rgeos
  temp<-gArea(temp,byid=T) 
  new.grid$area <- temp
  prj <- proj4string(transect)
  new.grid<-spTransform(new.grid,CRS(prj))
  
  new.grid$IDs <- row.names(new.grid) #n?cessaire pour over ci-dessous, pour s'Assurer que la variable existe.
  #changes to ensure compatibilities w/grid script
  return(new.grid)
}   

#==============================================================================================================================================
#
#==============================================================================================================================================  
dataset.prepare.with.zones <- function(d, shapefile.data, transect, sample="Date"){
  
#   temp<-spTransform(shapefile.data,CRS("+proj=utm +zone=17 +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))  #project to calculate area with package rgeos
#   temp<-gArea(temp,byid=T) #/(1000*1000)
#   shapefile.data$area<-temp #[match(shapefile.data$id,names(temp))]
#   
#   prj <- proj4string(transect)
#   shapefile.data<-spTransform(shapefile.data,CRS(prj))
#   
#   shapefile.data<-shapefile.data[apply(gIntersects(transect,shapefile.data,byid=TRUE),1,any),] 
#   shapefile.data$IDs <- row.names(shapefile.data)
  
  new.grid <- new.grid.with.zones(shapefile.data, transect)
  
  names(d)[names(d) == "zone"] <- "square"
  names(d)[names(d) == "zone_area"] <- "square_area"
  
  # remplacer overlay par over ... voir dans les exemples officiels comment faire l'appel et l'utilisation des arguments 
  # par exemple: over(as(meuse.grid, "SpatialPoints"), srdf) ou over(meuse.grid, srdf, fn = mean)  ... si on utilise un grid avec un spacialpolygonedataframe
  # http://www.inside-r.org/packages/cran/sp/docs/aggregate.Spatial
  # http://www.inside-r.org/packages/cran/sp/docs/overlay
  #
  # En gros, nous avons: new.grid de type "SpatialPolygonsDataFrame" et transect de type: "SpatialPointsDataFrame" ... mais over ne semble pas avoir une signature avec ces deux params
  # 
  # x<-overlay(shapefile.data,transect)
  #x<-overlay(new.grid,transect)
  #over(new.grid$data, transect$data)
  
  x<-over(transect,new.grid)
  
  d$square<-x$IDs
  d$square_area <- x$area/1000000 #for km2
  d<-d[!is.na(d$square),]
  
  # sample <- "Date" #Select variable WatchID, voir L409+ dans Server.R
  d$SMP_LABEL<-paste(d$square,d[,c(sample)],sep="_")

  temp<-aggregate(WatchLenKm~SMP_LABEL,data=unique(d[,c("SMP_LABEL","WatchID","WatchLenKm")]),sum)
  names(temp)[2]<-"SMP_EFFORT"
  d<-merge(d,temp,sort=FALSE)
  #d<-d[,c("square","square_area","Date","SMP_LABEL","SMP_EFFORT","Distance","Count","Alpha")]
  
  dd<-ddply(d,.(SMP_LABEL),function(i){sum(i$Count,na.rm=TRUE)}) #eliminate duplicate lines for transect without observations
  dd<-dd[dd$V1==0,] #get the label name for transect without observations
  d<-d[(d$SMP_LABEL%in%dd$SMP_LABEL & !duplicated(d$SMP_LABEL)) | (!d$SMP_LABEL%in%dd$SMP_LABEL & !(d$Alpha=="")),] #keep only lines for empty transects or non-empty lines for non-empty transects
  d<-d[order(d$square),]
  
  return(d)
  
} 


#==============================================================================================================================================
# shapefile.data => final grid
#==============================================================================================================================================  
dataset.prepare.with.grid <- function(d, shapefile.data, transect, size, sample="Date"){
  
  ## prj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
#    prjm <- "+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#    test<-spTransform(shapefile.data,CRS(prjm))
#    area <- data.frame(km2=gArea(test,byid=T)/1000000)
#    area$ID <- 1:nrow(area)
#    area$ID <- paste("parc",area$ID,sep="")
#    test <- SpatialPolygonsDataFrame(test,data=area)
#    shapefile.data<-spTransform(test,CRS(proj4string(transect))) 

  #x<-overlay(shapefile.data,transect)
  #x<-overlay(shapefile.data,transect, fun=function(x,y){return(x+y)})
  #x<-overlay(shapefile.data,transect)
  x<- over(transect, shapefile.data)
  
  d$square<-x$ID
  d$square_area<-(size/1000)^2 #in kilometers
  #d$square_area <- x$km2
  d<-d[!is.na(d$square),]
  
  # sample <- "Date" #Select variable WatchID, voir L409+ dans Server.R
  d$SMP_LABEL<-paste(d$square,d[,c(sample)],sep="_")

  temp<-aggregate(WatchLenKm~SMP_LABEL,data=unique(d[,c("SMP_LABEL","WatchID","WatchLenKm")]),sum)
  names(temp)[2]<-"SMP_EFFORT"
  d<-merge(d,temp,sort=FALSE)
  #d<-d[,c("zone","zone_area","Date","SMP_LABEL","SMP_EFFORT","Distance","Count","Alpha")]
  dd<-ddply(d,.(SMP_LABEL),function(i){sum(i$Count,na.rm=TRUE)}) #eliminate duplicate lines for transect without observations
  dd<-dd[dd$V1==0,] #get the label name for transect without observations
  d<-d[(d$SMP_LABEL%in%dd$SMP_LABEL & !duplicated(d$SMP_LABEL)) | (!d$SMP_LABEL%in%dd$SMP_LABEL & !(d$Alpha=="")),] #keep only lines for empty transects or non-empty lines for non-empty transects
  d<-d[order(d$square),]
  
  return(d)
}


#==============================================================================================================================================
# 
#==============================================================================================================================================  
distance.sampling.model.fit <- function(d, 
                                        output.dir=NULL,
                                        pathMCDS= NULL, 
                                        STR_LABEL= "square", 
                                        STR_AREA="square_area", 
                                        lsub= NULL,
                                        stratum= NULL,
                                        detection="All",
                                        empty=NULL,
                                        split=TRUE,
                                        period=NULL,
                                        multiplier = 2,
                                        verbose    = TRUE, 
                                        SMP_LABEL  = "SMP_LABEL",
                                        SMP_EFFORT = "SMP_EFFORT",
                                        breaks = c(0, 50, 100,200, 300), 
                                        SIZE = "Count", 
                                        covariates= NULL,
                                        factor= NULL, 
                                        rare= NULL,
                                        monotone = "Strict",
                                        estimator = NULL,
                                        DISTANCE = "DISTANCE",
                                        distanceAnalysis=TRUE){                 
  m <- NULL
  
  if(distanceAnalysis){
    m<-distance.wrap(d,stratum=stratum,empty=empty,detection=detection,lsub=lsub,split=split,
                     path=output.dir,pathMCDS=pathMCDS,breaks=breaks,STR_LABEL=STR_LABEL,STR_AREA=STR_AREA,
                     SMP_LABEL=SMP_LABEL,SMP_EFFORT=SMP_EFFORT,DISTANCE=DISTANCE,SIZE=SIZE,
                     units = list(Type = "Line", Distance = "Perp", Length_units = "Kilometers", 
                                  Distance_units ="Meters", Area_units = "Square kilometers"),
                     #
                     rare=rare,
                     period=period,
                     estimator=estimator,
                     multiplier=multiplier,
                     covariates=covariates,
                     factor=factor,
                     monotone = monotone,
                     #
                     verbose=FALSE)
  } else {
    
    d$Distance <- NULL #remove distance var from dataset
    m <-  strip.wrap(dataset=d, path=output.dir, pathMCDS=pathMCDS, period=period, multiplier=multiplier, verbose=verbose, SMP_LABEL=SMP_LABEL, 
                     SMP_EFFORT=SMP_EFFORT, breaks=breaks, SIZE=SIZE, empty=empty, STR_LABEL=STR_LABEL, STR_AREA=STR_AREA, stratum=stratum,
                     split=split, detection=detection, lsub=lsub, units = list(Type = "Line", Distance = "Perp", Length_units = "Kilometers", 
                                                                           Distance_units ="Meters", Area_units = "Square kilometers"))
  }
  return(m) 
}


#==============================================================================================================================================
#
#============================================================================================================================================== 
summary <- function(model=model, species.subtitles=c("All species"), file="distance", directory.absolute.path, distanceAnalysis=TRUE){  
  
  all.sp.best <- model
  
  if(distanceAnalysis){
    all.sp.best <- keep.best.model(model)
  }
  
  global.summary(model     =all.sp.best, 
                 species   = species.subtitles, 
                 file      =file, 
                 directory =directory.absolute.path)
}



#==============================================================================================================================================
#
#============================================================================================================================================== 
grid.brks.tags <- function(new.grid, densities, classes=c(0,.5,.75,.95) , nbr.arround=3, stratgrille=TRUE){  
  densities$Estimates <- as.numeric(densities$Estimates)
  #
  print("length(densities) = ")
  print(length(densities))
  
  print("densities = ")
  print(densities)
  
  names(densities)[3] <- "CoefVar"
  densities$CoefVar   <- as.numeric(densities$CoefVar)
  names(densities)[names(densities) == "Stratum"] <- "ID"

  if(stratgrille){
    densities <- densities[order(densities$Estimates),]
    densities.tags.brks <- dentisites.tags.brks.grid(densities = densities, classes = classes, nbr.arround = nbr.arround)
  }
  else{
    densities.tags.brks <- dentisites.tags.brks.zone(densities = densities)
  }
  
  densities          <- densities.tags.brks$densities
  tags               <- densities.tags.brks$tags
  brks               <- densities.tags.brks$brks
  
  if(stratgrille){
    o <- match(new.grid$ID, densities$ID)
  }
  else{
    o <- match(new.grid$IDs, densities$ID)
  }
  
  temp <- densities[o,]
  row.names(temp) <- row.names(new.grid)
  new.grid2 <- spCbind(new.grid,temp)
  new.grid2 <- new.grid2[!is.na(new.grid2$Estimates),]
  #head(new.grid2@data)
  
  return(list(grid = new.grid2, brks = brks, tags = tags))
}


#==============================================================================================================================================
#
#============================================================================================================================================== 
dentisites.tags.brks.grid <- function (densities, classes=c(0,.5,.75,.95), nbr.arround=3) {
  
  p99 <- quantile(densities$Estimates, c(.995)) 
  densities$Estimates <- ifelse(densities$Estimates > p99,p99,densities$Estimates)
  qt <- quantile(densities$Estimates, c(classes))
  qt[length(qt)+1] <- max(densities$Estimates)+0.001
  
  if(is.null(nbr.arround)){
    brks <- round(qt,3)
  }
  else {
    brks <- round(qt,nbr.arround)
  }
  
  tags<-vector()
  
  for (i in 1:length(brks)) {
    if (i ==1 ) {
      densities$class    <- ifelse(densities$Estimates == brks[i], as.character(brks[i]),"-")
      densities$classno  <- ifelse(densities$Estimates == brks[i], as.numeric(i),"-")
      tags[1]<- as.character(brks[i])
    }
    if(i>1) {
      densities$class    <- ifelse(densities$Estimates > brks[i-1] & densities$Estimates <= brks[i], paste(" > ",brks[i-1]," - ",brks[i],sep=""), densities$class)
      densities$classno  <- ifelse(densities$Estimates > brks[i-1] & densities$Estimates <= brks[i], as.numeric(i), densities$classno)
      tags[i]<- paste(" > ",brks[i-1]," - ",brks[i],sep="")
    }
  }
  
  return(list(densities = densities, tags = tags, brks = brks))
}

#==============================================================================================================================================
#
#============================================================================================================================================== 
dentisites.tags.brks.zone <- function (densities) {
  
  brks <- round(seq(0, max(densities$Estimates,na.rm = T)+0.01, length.out = 10), 2)
  tags <- vector()
  
  for (i in 1:length(brks)) {
    if (i ==1 ) {
      densities$class  <- ifelse(densities$Estimates == brks[i], as.character(brks[i]),"-")
      densities$classno  <- ifelse(densities$Estimates == brks[i], as.numeric(i),"-")
      tags[1]<- as.character(brks[i])
      
    }
    if(i>1) {
      densities$class  <- ifelse(densities$Estimates > brks[i-1] & densities$Estimates <= brks[i], paste(" > ",brks[i-1]," - ",brks[i],sep=""), densities$class)
      densities$classno  <- ifelse(densities$Estimates > brks[i-1] & densities$Estimates <= brks[i], as.numeric(i), densities$classno)
      tags[i]<- paste(" > ",brks[i-1]," - ",brks[i],sep="")
    }
  }
  
  return(list(densities= densities, tags = tags, brks = brks))
}


#==============================================================================================================================================
#
#============================================================================================================================================== 
dentisity.map.2 <- function (densities, 
                             new.grid2, 
                             brks, 
                             tags, 
                             shpm, 
                             couleurs=c("green","yellow", "red"),  
                             stratgrille=TRUE, 
                             specie=NULL,
                             titre="Corrected densities",
                             legendtitle = "birds/km2",
                             PDF=TRUE,
                             working.dir, # absolute path 
                             output.file.name,
                             all.species=TRUE
) {
  br.palette <- colorRampPalette(c(couleurs), space = "rgb") 
  nb         <- length(brks)-1 
  br.palette(nb)
  pal <- br.palette(n=nb)
  pal <- c("lightgray",pal)
  
  new.grid2$color <- pal[as.numeric(new.grid2$classno)]
  new.grid2$classno <- as.numeric(new.grid2$classno)
  new.grid2 <- new.grid2[order(new.grid2$classno),]
  classes <- unique(new.grid2$class)
  
  setwd(working.dir)
  
  if (PDF) {
    pdf(file = output.file.name, width = 11, height = 8)
  }else{   
    png(output.file.name,width=960,height=960)
  }
  
  plot(new.grid2,bg="lightblue",border="lightblue",axes=T)
  
  if(stratgrille){
    plot(new.grid2[new.grid2$class==classes[1],],col=pal[1],add=T,axes=T)
  }
  
  for (i in 2:length(classes)){
    plot(new.grid2[new.grid2$class==classes[i],],col=new.grid2@data[new.grid2$class==classes[i],"color"],border=new.grid2@data[new.grid2$class==classes[i],"color"],add=T)
  }
  
  plot(new.grid2,border="darkgray",add=T)
  plot(shpm,add=T,col="darkkhaki",border="darkkhaki",axes=T)
  title(main = titre, cex.main=1.5)
  
  if (!all.species) {
    mtext(specie,cex=1)
  }
  
  legend("bottomright", bty = "n",  legend = tags,fill=pal, title=legendtitle, cex=0.6)
  dev.off()
  
}

#==============================================================================================================================================
#
#============================================================================================================================================== 
densities.data <- function(model, distanceAnalysis=TRUE){  
  
  all.sp.best <- model
  
  if(distanceAnalysis){
    all.sp.best <- keep.best.model(model)
  }
  
  tmp <- all.sp.best$density_estimate$Stratum
  
  #tmp <- model$density_estimate$Stratum
  strates <- tmp
  
  densites <- strates[strates$Parameters == "D",c("Stratum","Estimates","% of var.")]
  
  return(densites)
}
