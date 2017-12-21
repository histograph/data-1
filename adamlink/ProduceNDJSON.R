library(data.table)
library(jsonlite)
library(wellknown)
#library(devtools)
#library(rwkt)

#############################################################
## FUNCTIONS
#############################################################

disJoinCol <- function(columns, fixedCols, datatable){
  unique(datatable[,c(fixedCols,columns),with=FALSE])
}

putMsg <- function(msg1,msg2){
    writeLines("\n***************************************************")
    writeLines(paste(msg1))
    writeLines(paste(msg2))
    writeLines("***************************************************\n")
}

clearGeoJSON <- function(value){
  
  # remove quoted "
  new_value <- gsub('\\\\"','"',toJSON(value))
  
  # remove quotes for numbers
  new_value <- gsub('"([0-9])',"\\1",new_value)
  new_value <- gsub('([0-9])",([0-9])',"\\1,\\2",new_value)
  new_value <- gsub('([0-9])"\\]',"\\1\\]",new_value)
  
  # remove quoted object
  new_value <- gsub('"geometry":"\\{','"geometry":\\{',new_value)
  
  new_value <- gsub('\\}"\\}','\\}\\}',new_value)
  
  # insert new line
  new_value <- gsub('\\},\\{','\\}\n\\{',new_value)
  
  # remove enclosing brackets
  new_value <- gsub('^\\[','',new_value)
  new_value <- gsub('\\]$','',new_value)
  
  return(new_value)
  
}

#############################################################
## PITS
#############################################################

# Read PITS
pits <- read.csv("./pits.csv",stringsAsFactors = FALSE, na.strings ="",encoding="UTF-8")

if ( sum(duplicated(pits)) > 0 ){
  putMsg("The following id have duplicates:" , paste(pits[which(duplicated(pits)),]$id,sep=""))
}

pits <-unique(data.table(pits,key="id"))

# replace url with namespace
pits$type <- gsub("http://rdf.histograph.io/","hg:",pits$type)

a <- lapply(pits$geometry[!is.na(pits$geometry)],wkt2geojson)
b <- lapply(a,function(b){toJSON(b$geometry)})
pits$geometry[!is.na(pits$geometry)] <- as.character(b)

  
# Handle missing time
startYear <- -33

endYear <- as.integer(format(Sys.time(), "%Y")) + 1000


colvect <- c(4:7,9:12,14:17)
valuevect <- as.integer(rep(rep(c(startYear,endYear),each=2),length(colvect)/4))

for( i in 1:length(colvect)){
  pits[is.na(pits[[colvect[i]]]),][[colvect[i]]] <- valuevect[i]
}

diffTimeRange <- pits[name==altName & 
                        ! ( (earliestBegin == startYear & 
                               latestBegin == startYear &
                               earliestEnd  == endYear &
                               latestEnd == endYear ) |
                              (earliestAltBegin == startYear &
                                 latestAltBegin == startYear & 
                                 earliestAltEnd == endYear & 
                                 latestAltEnd == endYear ) |
                              (earliestBegin == earliestAltBegin &
                                 latestBegin == latestAltBegin & 
                                 earliestEnd == earliestAltEnd & 
                                 latestEnd == latestAltEnd)),]

if ( nrow(diffTimeRange) > 0 )
{
  putMsg("There are pits where the main name variant (same name as the main object) has a different time range:" ,
         paste(diffTimeRange$id,sep=""))
}


# These PITS are the main elements for each graph of sameAs relations when one alt name is the same as the main name
mainPitsChildOverlap <- pits[name==altName & ( (
                                  earliestBegin == earliestAltBegin & 
                                    latestBegin == latestAltBegin & 
                                    earliestEnd == earliestAltEnd & 
                                    latestEnd == latestAltEnd
                                ) |
                                  (
                                    earliestAltBegin == startYear & 
                                    latestAltBegin ==startYear & 
                                    earliestAltEnd == endYear & 
                                    latestAltEnd == endYear
                                  ) |
                                  (
                                    earliestBegin == startYear & 
                                      latestBegin ==startYear & 
                                      earliestEnd == endYear & 
                                      latestEnd == endYear
                                  )
                                ),]

# These PITS are the main elements AND the secondary elements in case no alt name overlap in time with the main name
mainPitsNoChildOverlap <- pits[name==altName & !( (
                                  earliestBegin == earliestAltBegin & 
                                    latestBegin == latestAltBegin & 
                                    earliestEnd == earliestAltEnd & 
                                    latestEnd == latestAltEnd
                                ) |
                                  (
                                    earliestAltBegin == startYear & 
                                      latestAltBegin ==startYear & 
                                      earliestAltEnd == endYear & 
                                      latestAltEnd == endYear
                                  )  |
                                  (
                                    earliestBegin == startYear & 
                                      latestBegin ==startYear & 
                                      earliestEnd == endYear & 
                                      latestEnd == endYear
                                  )
                                ),]


# These PITS will be the secondary elements connected to the main with sameAs relations
altNamesPits <- pits[ name != altName,]

stopifnot (nrow(altNamesPits) + nrow(mainPitsNoChildOverlap) + nrow(mainPitsChildOverlap) - nrow(pits) == 0)


# Select PITS that have compatible geometry ranges
mainPitsChildOverlapWithGeo <- mainPitsChildOverlap[earliestBegin == earliestGeoBegin & 
                                       latestBegin == latestGeoBegin & 
                                       earliestEnd == earliestGeoEnd & 
                                       latestEnd == latestGeoEnd,
                                       c("id","name","type","earliestBegin","latestBegin","earliestEnd","latestEnd","geometry")]


mainPitsNoChildOverlapWithGeo <- mainPitsNoChildOverlap[earliestBegin == earliestGeoBegin & 
                                        latestBegin == latestGeoBegin & 
                                        earliestEnd == earliestGeoEnd & 
                                        latestEnd == latestGeoEnd,
                                        c("id","name","type","earliestBegin","latestBegin","earliestEnd","latestEnd","geometry")]

altNamesPitsNoChildOverlapWithGeo <- mainPitsNoChildOverlap[
                                        earliestAltBegin == earliestGeoBegin & 
                                        latestAltBegin == latestGeoBegin & 
                                        earliestAltEnd == earliestGeoEnd & 
                                        latestAltEnd == latestGeoEnd,
                                        c("id","altName","type","earliestAltBegin","latestAltBegin","earliestAltEnd","latestAltEnd","geometry")]

# Make column names equal
names(altNamesPitsNoChildOverlapWithGeo) <- c("id","name","type","earliestBegin","latestBegin","earliestEnd","latestEnd","geometry")

altNamesPitsWithGeo <- altNamesPits[
                                earliestAltBegin == earliestGeoBegin & 
                                latestAltBegin == latestGeoBegin & 
                                earliestAltEnd == earliestGeoEnd & 
                                latestAltEnd == latestGeoEnd,
                                c("id","altName","type","earliestAltBegin","latestAltBegin","earliestAltEnd","latestAltEnd","geometry")]

# Make column names equal
names(altNamesPitsWithGeo) <- c("id","name","type","earliestBegin","latestBegin","earliestEnd","latestEnd","geometry")

# Select PITS that have incompatible geometry ranges

geoToMainPitsChildOverlap <- mainPitsChildOverlap[
                                !(earliestBegin == earliestGeoBegin & 
                                latestBegin == latestGeoBegin & 
                                earliestEnd == earliestGeoEnd & 
                                latestEnd == latestGeoEnd),
                                c("id","name","type","earliestGeoBegin","latestGeoBegin","earliestGeoEnd","latestGeoEnd","geometry")
                                ]
# Make column names equal
names(geoToMainPitsChildOverlap) <- c("id","name","type","earliestBegin","latestBegin","earliestEnd","latestEnd","geometry")


geoToMainPitsNoChildOverlap <- mainPitsNoChildOverlap[
                                !(earliestBegin == earliestGeoBegin & 
                                latestBegin == latestGeoBegin & 
                                earliestEnd == earliestGeoEnd & 
                                latestEnd == latestGeoEnd),
                                c("id","name","type","earliestGeoBegin","latestGeoBegin","earliestGeoEnd","latestGeoEnd","geometry")
                                ]

# Make column names equal
names(geoToMainPitsNoChildOverlap) <- c("id","name","type","earliestBegin","latestBegin","earliestEnd","latestEnd","geometry")

altNamesPitsNoChildOverlapNoGeo <-  mainPitsNoChildOverlap[
                                !(earliestAltBegin == earliestGeoBegin & 
                                latestAltBegin == latestGeoBegin & 
                                earliestAltEnd == earliestGeoEnd & 
                                latestAltEnd == latestGeoEnd),
                                c("id","altName","type","earliestAltBegin","latestAltBegin","earliestAltEnd","latestAltEnd","geometry")
                                ]

# geometry needs to remove as it is not overlapping
altNamesPitsNoChildOverlapNoGeo$geometry <- NA

# Make column names equal
names(altNamesPitsNoChildOverlapNoGeo) <- c("id","name","type","earliestBegin","latestBegin","earliestEnd","latestEnd","geometry")

altNamesPitsNoGeo <- altNamesPits[
                                !(earliestAltBegin == earliestGeoBegin & 
                                latestAltBegin == latestGeoBegin & 
                                earliestAltEnd == earliestGeoEnd & 
                                latestAltEnd == latestGeoEnd),
                                c("id","altName","type","earliestAltBegin","latestAltBegin","earliestAltEnd","latestAltEnd","geometry")
                                ]

# geometry needs to remove as it is not overlapping
altNamesPitsNoGeo$geometry <- NA

# Make column names equal
names(altNamesPitsNoGeo) <- c("id","name","type","earliestBegin","latestBegin","earliestEnd","latestEnd","geometry")

stopifnot (
             nrow(mainPitsChildOverlapWithGeo) +
             nrow(mainPitsNoChildOverlapWithGeo) +
#             nrow(altNamesPitsNoChildOverlapWithGeo) +
             nrow(altNamesPitsWithGeo) +
             nrow(geoToMainPitsChildOverlap) +
             nrow(geoToMainPitsNoChildOverlap) +
#             nrow(altNamesPitsNoChildOverlapNoGeo) +
             nrow(altNamesPitsNoGeo) - nrow(pits) == 0)


mainPitsChildOverlapWithGeo <- unique(mainPitsChildOverlapWithGeo)
mainPitsNoChildOverlapWithGeo <- unique(mainPitsNoChildOverlapWithGeo)
altNamesPitsNoChildOverlapWithGeo <- unique(altNamesPitsNoChildOverlapWithGeo)
altNamesPitsWithGeo <- unique(altNamesPitsWithGeo)
geoToMainPitsChildOverlap <- unique(geoToMainPitsChildOverlap)
geoToMainPitsNoChildOverlap <- unique(geoToMainPitsNoChildOverlap)
altNamesPitsNoChildOverlapNoGeo <- unique(altNamesPitsNoChildOverlapNoGeo)
altNamesPitsNoGeo <- unique(altNamesPitsNoGeo)

allAltNames <- rbind(altNamesPitsNoChildOverlapWithGeo,altNamesPitsWithGeo,altNamesPitsNoChildOverlapNoGeo,altNamesPitsNoGeo)
allGeometries <- rbind(geoToMainPitsChildOverlap,geoToMainPitsNoChildOverlap)
allMainPits <- rbind(mainPitsChildOverlapWithGeo,mainPitsNoChildOverlapWithGeo)


# Create new ids for Alt names
sameAsAltPits <- data.table(allAltNames[,paste(id,"altName",seq_len(.N),sep="_"),by=id]$V1,
                            allAltNames,key="id", stringsAsFactors = FALSE)

# create hg:sameHgConcept relations 
altNamesRelations <- data.table(from=sameAsAltPits$V1,to=sameAsAltPits$id, type=rep("hg:sameHgConcept",length(sameAsAltPits$id)))

# set new id
sameAsAltPits[,id:=NULL]
colnames(sameAsAltPits)[1] <- "id"
setkey(sameAsAltPits,"id")

# Create new ids for Geometries
sameAsGeoPits <- data.table(allGeometries[,paste(id,"geo",seq_len(.N),sep="_"),by=id]$V1,
                            allGeometries,key="id", stringsAsFactors = FALSE)

# create hg:sameHgConcept relations 
geoRelations <- data.table(from=sameAsGeoPits$V1,to=sameAsGeoPits$id, type=rep("hg:sameHgConcept",length(sameAsGeoPits$id)))

# set new id
sameAsGeoPits[,id:=NULL]
colnames(sameAsGeoPits)[1] <- "id"
setkey(sameAsGeoPits,"id")

allPits <- rbind(allMainPits,sameAsAltPits,sameAsGeoPits)

# remove artificially created years
allPits$earliestBegin[allPits$earliestBegin == startYear] <- NA
allPits$latestBegin[allPits$latestBegin == startYear] <- NA
allPits$earliestEnd[allPits$earliestEnd == endYear] <- NA
allPits$latestEnd[allPits$latestEnd == endYear] <- NA

allSameAsRelations <- rbind(altNamesRelations,geoRelations)



#############################################################
#### RELATIONS
#############################################################
relations <- read.csv("./relations.csv",stringsAsFactors = FALSE, na.strings ="")

relations <-data.table(relations,key="id")

relationNames <- colnames(relations)[3:ncol(relations)]

looseTables <- lapply(relationNames, disJoinCol, c("id","label"),relations)

totalRelations <- rbindlist(looseTables,use.names = TRUE, fill = TRUE)                       


#############################################################
#### GENERATE FILES
#############################################################

cat(clearGeoJSON(allPits),file="adamlinkstraten.pits.ndjson",sep="")

cat(gsub('\\},\\{','\\}\n\\{',toJSON(allSameAsRelations)),file="adamlinkstraten.relations.ndjson",sep="")
