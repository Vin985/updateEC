library(ECSASconnect)
library(readxl)
library(RODBC)
library(compare)
library(data.table)

rm(list = ls())

#####################
## Utility functions
####################

removeDuplicates <- function(data, errors, observations) {
  dupIds <-
    which(duplicated(data[, -which(names(data) %in% c("id", "id_transect", "hasObs"))]))
  toRemove <-
    lapply(dupIds, function(x, data, errors, observations) {
      tmp <- data[c(x - 1, x),]
      # make sure rows are really different
      tmpcmp <- tmp[, !names(tmp) %in% c("id", "id_transect", "hasObs")] 
      identical <-
        compare(tmpcmp[1, ], tmpcmp[2, ], allowAll = TRUE)$result
      if (identical) {
        ids <- tmp$id
        withObs <- tmp[tmp$hasObs, "id_transect"]
        if (length(withObs) == 0) {
          # No row has any observation. Check if they are orphans
          if (any(ids %in% errors)) {
            toRemove <- ids
          } else {
            # ???????
            toRemove <- ids[1]
          }
        } else {
          toRemove <- ids[!tmp$hasObs]
        }
        if (length(toRemove) == 0) {
          toRemove <- NULL
        }
      } else {
        # print("rows different!")
        toRemove <- NULL
      }
      toRemove
    }, data, errors, observations)
  res <- as.numeric(unlist(toRemove))
  res <- res[!is.null(res)]
  list(rem = res, orphans = errors[!errors %in% res])
} 

renameTransect <- function(watches, mission) {
  transectId <- 0
  foundStart <- FALSE
  foundInit <- FALSE
  data <- copy(watches)
  data$newTransectId <- 0
  data$noEnd <- FALSE
  data$orphan <- FALSE
  
  
  for (i in 1:nrow(data)) {
    # get transect id and site
    id_transect <- data$id_transect[i]
    site <- data$site[i]
    
    isInitDesc <-
      (length(grep("init", site, ignore.case = TRUE)) > 0)
    isStart <-
      (length(grep(
        "deb|déb", site, ignore.case = TRUE
      )) > 0)
    isEnd <-
      (length(grep("fin", site, ignore.case = TRUE)) > 0)
    
    
    # Check if transect id is the same as in the previous iteration
    if (isInitDesc) {
      if (!foundInit) {
        # we havent already found a start, create a new transect
        foundInit <- TRUE
      } else {
        # we already had an initial description
        # the previous transect had no end
        # add a note to the previous line
        data[i - 1, noEnd := TRUE]
      }
      transectId <- id_transect
      data$newTransectId[i] <- transectId
    } else if (isStart) {
      if (foundStart) {
        # We already have a start
        # the previous transect had no end add a note to the previous line
        data[i - 1, noEnd := TRUE]
      }
      if (!foundInit) {
        # only restart the transectId if we have no initial description
        transectId <- id_transect
      }
      foundStart <- TRUE
      data$newTransectId[i] <- transectId
    } else {
      if (!foundStart & !foundInit) {
        # we have no start, orphan transect
        data[i, orphan := TRUE]
        transectId <- 0
      }
      if (isEnd) {
        foundInit <- FALSE
        foundStart <- FALSE
      }
      data$newTransectId[i] <- transectId
    }
  }
  data
}

renameTransects <- function(data) {
  data <- as.data.table(data)
  data[, mission := as.character(mission)]
  setkey(data, mission, id_transect, date_heure, id)
  res <- data[, renameTransect(.SD, .BY), by = mission]
  res
}

checkObservations <- function(dates){
  transectDates <- dates$transectDates
  observationDates <- dates$observationDates
  transectDates$ok <- 1
  transectDates$newStart <- 0
  transectDates$newEnd <- 0
  
  for (i in 1:nrow(observationDates)) {
    observation <- observationDates[i]
    transect <- transectDates[id == observation$id]
    if (!observation$start >= transect$start) {
      transectDates[observation$id, ok := -1]
      transectDates[observation$id, newStart := observation$idStart]
    }
    if (!observation$end <= transect$end) {
      transectDates[observation$id, ok := -1]
      transectDates[observation$id, newEnd := observation$idEnd]
    }
  }
  transectDates
}


##############
## Main code
#############

DATA_DIR <- "C:/dev/data/ECSAS"
DATA_FILE <- file.path(DATA_DIR, "SOMEC.accdb")

##import Access tables
channel <- odbcConnectAccess2007(DATA_FILE)
missions <- sqlFetch(channel , "missions")
transects <- sqlFetch(channel , "transects")#all 2016 data included
observations <- sqlFetch(channel , "observations")
close(channel)


# Get missions where data collection has been made on paper
paperMissions <- missions[missions$Saisie == "papier", "mission"]
pcMissions <- missions[missions$Saisie == "ordi", "mission"]

# Order transects
transects <- transects[order(transects$mission, transects$id_transect), ]
# Check if transects have observations
transects$hasObs <- ifelse(is.na(match(paste0(transects$id_transect, transects$mission), 
                                       paste0(observations$id_transect, observations$mission))), FALSE, TRUE)


###########################
## Transects made on paper
##########################

## Get only transects and observations made on paper
transPaper <- transects[transects$mission %in% paperMissions, c("id", "id_transect", "mission", "date_heure", "hasObs")]
obsPaper <- observations[observations$mission %in% paperMissions,
                          c("id", "id_transect", "mission", "date_heure")]

# get duplicated rows
duplicatesPaper <- removeDuplicates(transPaper, "", obsPaper)

# Data without duplicates
transPaper_nodup <- transPaper[!transPaper$id %in% duplicatesPaper$rem, ] 


###############################
## Transects made on computer
#############################

# Missions with computer
transPC <-
  transects[transects$mission %in% pcMissions, c("id", "id_transect", "site", "mission", "date_heure", "hasObs")]

transPC$mission <- as.character(transPC$mission)
transPC$site <- as.character(transPC$site)

# Remove fully duplicated lines, even by transect id
transPC <- transPC[!duplicated(transPC[, -which(names(transPC) == "id")]), ]

# Get observations
obsPC <- observations[!observations$mission %in% paperMissions,
                       c("id", "id_transect", "mission", "date_heure")]


# Apply the function once to get a list of orphans
res <- renameTransects(transPC)
errors <- res$id[which(res$noEnd | res$orphan)]

# Remove duplicates in the database using the list of orphans previously obtained
duplicatesPC <- removeDuplicates(transPC, errors, obsPC)

# New dataset without duplicates
transPC_nodup <- transPC[!transPC$id %in% duplicatesPC$rem, ] 

# Reclean without duplicates
transUpdate <- renameTransects(transPC_nodup)

# Update transect ids
toUpdate <- transUpdate[newTransectId > 0, .(id, newTransectId)]



###############################
## Check observation transect
#############################

getDates <- function(data, by, type = "transect") {
  data <- unique(data, by = "date_heure")
  range <- data[data$date_heure %in% range(data$date_heure), ]
  res <- unlist(range)
  # Only one observation. Duplicate data to have a max
  if (length(res) == 2 && type == "observation") {
    res <- as.vector(rbind(res, res))
  }
  if (length(res) != 4) browser() 
  names(res) <- c("idStart", "idEnd", "start", "end")
  as.list(res)
}

getBounds <- function(transects, observations) {
  transects <- as.data.table(transects)
  observations <- as.data.table(observations)
  transects <- droplevels(transects)
  observations <- droplevels(observations)
  setkey(transects, date_heure)
  setkey(observations, date_heure)
  # get start and end of transects
  transectDates <- transects[, getDates(.SD, .BY), by = list(mission, id_transect), .SDcols = c("id", "date_heure")]
  observationDates <- observations[, getDates(.SD, .BY, "observation"), by = list(mission, id_transect), .SDcols = c("id", "date_heure")]
  # create unique id
  transectDates[, id := paste(mission, id_transect, sep = "_")]
  observationDates[, id := paste(mission, id_transect, sep = "_")]
  observationDates <- observationDates[id %in% transectDates$id]
  class(observationDates$start) <- c('POSIXt','POSIXct')
  class(observationDates$end) <- c('POSIXt','POSIXct')
  class(transectDates$start) <- c('POSIXt','POSIXct')
  class(transectDates$end) <- c('POSIXt','POSIXct')
  
  setkey(transectDates, id)
  setkey(observations, id)
  list(transectDates = transectDates, observationDates = observationDates) 
}

checkIdTransect <- function(observations, by, dates) {
  transects <- dates[mission == unlist(by)]
  res <- copy(observations)
  res[, noTransect := FALSE]
  res[, newId := -1]
  for (i in 1:nrow(observations)) {
    obs <- observations[i]
    transId <- transects[start <= obs$date_heure & end >= obs$date_heure, id_transect]
    
    if (length(transId) == 0) {
      res[i, noTransect := TRUE]
    } else if (transId != obs$id_transect) {
      res[i, newId := transId]
    }
  }
  res
}


# Convert date to seconds in transects and observations
transDate <- as.data.table(transects)
transDate <-  transDate[mission %in% pcMissions, c("id", "id_transect", "site", "mission", "date_heure")]
transDate <- transDate[!mission %in% "TEL110817"]

obsDate <- as.data.table(observations)
obsDate <- obsDate[!mission %in% paperMissions & !tolower(in_transect) %in% "hors transect",
                   c("id", "id_transect", "mission", "date_heure")]
obsDate <- obsDate[!mission %in% "TEL110817"]


r <- transDate[, .N, by = list(mission, id_transect)]
r[N == 1]
o <- paste0(obsDate$id_transect, obsDate$mission)
t <- paste0(transDate$id_transect, transDate$mission)
obsDate[which(!o %in% t), c("mission", "id_transect")]


dates <- getBounds(transDate, obsDate)
res <- checkObservations(dates)

ko <- res[ok == -1]
# done <- c("COR160601", "GOR140228", "TEL150531", "TEL150802", "TEL140905", "HUD091025")
# ko[newEnd != 0 & !mission %in% done]



obsId <- obsDate[, checkIdTransect(.SD, .BY, dates$transectDates), by = mission]
obsId[, wrongId := (newId > 0)]
obsId[wrongId == TRUE]

wrongTransect <- obsId[wrongId == TRUE]
noTransect <- obsId[noTransect == TRUE]



########################
##
## Update the database
##
#######################

###################
## Rows to remove
##################

# Regroup the ids of non duplicated rows
nodupIds <- c(transPaper_nodup$id, transPC_nodup$id)

# Get the ids of the duplicated rows to remove
toRemoveIds <- data.frame(id = transects[!transects$id %in% nodupIds, "id"])


###################
# Rows to update
#################

# Get the data with no duplicated rows
transects_final <- transects[transects$id %in% nodupIds, ]

# Prepare the data frame with ids to update 
transects_final$newTransectId <- 0
transects_final$newTransectId[transects_final$id %in% toUpdate$id] <- toUpdate$newTransectId
updateTransects <- transects_final[(transects_final$id_transect != transects_final$newTransectId 
                                    & transects_final$newTransectId > 0), c("id", "id_transect", "newTransectId", "mission")]
names(updateTransects) <- c("id", "oldTransectId", "newTransectId", "mission")


####################
## Save in database
###################

OBSERVATION_TABLE <- "observations"
TRANSECTS_TABLE <- "transects"

channel <- odbcConnectAccess2007(DATA_FILE)


# Create temporary table to store transect ids to remove
sqlSave(channel, toRemoveIds, "duplicatedTransect", rownames = FALSE, safer = FALSE)
# Create temporary table to store ids to update
sqlSave(channel, updateTransects, "updateTransect", rownames = FALSE, safer = FALSE)
# Create temporary table to store ids tu update
sqlSave(channel, wrongTransect[, c("id", "id_transect", "newId")], "updateObservationTransect", rownames = FALSE, safer = FALSE)

## Query to remove duplicated transects
removeDuplicatesQuery <- sprintf("DELETE FROM %s AS t WHERE (
                                                    SELECT 1
                                                    FROM duplicatedTransect AS dt
                                                    WHERE t.id = dt.id)", TRANSECTS_TABLE)
sqlQuery(channel, removeDuplicatesQuery)

## Query to update ids in the transect table
updateTransectIdsQuery <- sprintf("UPDATE %s t INNER JOIN updateTransect ut ON t.id = ut.id
                           SET t.id_transect = ut.newTransectId", TRANSECTS_TABLE)
sqlQuery(channel, updateTransectIdsQuery)

## Query to update ids in the observation table
updateObservationTransectIdQuery <-  sprintf("UPDATE %s AS o INNER JOIN updateTransect AS ut 
                           ON (o.id_transect = ut.oldTransectId)
                           AND (o.mission = ut.mission)
                           SET o.id_transect = ut.newTransectId", OBSERVATION_TABLE)
sqlQuery(channel, updateObservationTransectIdQuery)

correctObservationTransectIdQuery <-  sprintf("UPDATE %s AS o INNER JOIN updateObservationTransect AS ut 
                           ON (o.id = ut.id)
                           SET o.id_transect = ut.newId", OBSERVATION_TABLE)
sqlQuery(channel, correctObservationTransectIdQuery)

close(channel)


