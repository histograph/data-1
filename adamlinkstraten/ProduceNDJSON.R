library(data.table)
library(jsonlite)
library(wellknown)
#library(devtools)
#library(rwkt)

#############################################################
## CONSTANTS
#############################################################

ans <- readline(prompt="Reduce input[y/N] ? ")
if( ans == ''){
  REDUCE <- FALSE
}else{
  REDUCE <- TRUE 
}


TKN_OPEN <- "erewredfsc"
TKN_CLOSE <- "sgdfehyfdg"


TKN_ARR_OPEN <- "UTIYOOYOI"
TKN_ARR_CLOSE <- "SDGFSGFG"
TKN_ARR_SEP <-","

#############################################################
## FUNCTIONS
#############################################################

# disJoinCol <- function(columns, fixedCols, datatable){
#   unique(datatable[,c(fixedCols,columns),with=FALSE])
# }

putMsg <- function(msg1, msg2) {
  writeLines("\n***************************************************")
  writeLines(paste(msg1))
  writeLines(paste(msg2))
  writeLines("***************************************************\n")
}

clearGeoJSON <- function(value) {
  # remove quotes for numbers
  new_value <- gsub('"([0-9])', "\\1", value)
  new_value <- gsub('([0-9])",([0-9])', "\\1,\\2", new_value)
  new_value <- gsub('([0-9])"\\]', "\\1\\]", new_value)
  
  new_value <- gsub('"type":\\["', '"type":"', new_value)
  new_value <- gsub('"\\],"coordinates"', '","coordinates"', new_value)
  
  return(new_value)
}

clearJSON <- function(value) {
  # remove quoted object
  new_value <- toJSON(value)
  new_value <- gsub('"geometry":"\\{', '"geometry":\\{', new_value)
  
  new_value <- gsub('\\}"\\}', '\\}\\}', new_value)
  
  # remove quoted "
  new_value <- gsub('\\\\"', '"', new_value)
  
  # handle integers and ranges in dates
  
  new_value <- gsub(paste("\"",TKN_OPEN,sep=""), "",new_value)
  
  new_value <- gsub(paste(TKN_CLOSE,"\"",sep=""), "",new_value)
  
  new_value <- gsub(paste("\"",TKN_ARR_OPEN,sep=""), "\\[",new_value)
  
  new_value <- gsub(paste(TKN_ARR_CLOSE,"\"",sep=""), "\\]",new_value)
  
  # insert new line
  new_value <- gsub('\\},\\{', '\\}\n\\{', new_value)
  
  # remove enclosing brackets
  new_value <- gsub('^\\[', '', new_value)
  new_value <- gsub('\\]$', '', new_value)
  
  return(new_value)
  
}

canNames <- function(){
  return(c("uri","name","type","earliestBegin","latestBegin","earliestEnd","latestEnd","geometry"))
}

altNames <- function(){
  return(c("uri","altName","type","earliestAltBegin","latestAltBegin","earliestAltEnd","latestAltEnd","geometry"))
}

geoNames <- function(){
  return(c("uri","name","type","earliestGeoBegin","latestGeoBegin","earliestGeoEnd","latestGeoEnd","geometry"))
}

bagIDs <- function(uri){
  return(gsub(pattern = "http://bag.basisregistraties.overheid.nl/bag/id/openbare-ruimte/0([0-9]+)","bag/\\1",uri))
}

#############################################################
## PITS
#############################################################
options(digits = 20)
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# Read PITS
pits <-
  read.csv(
    "./pits.csv",
    stringsAsFactors = FALSE,
    na.strings = "",
    encoding = "UTF-8"
  )

if (sum(duplicated(pits)) > 0) {
  putMsg("The following uri have duplicates:" , paste(pits[which(duplicated(pits)), ]$uri, sep =
                                                       ""))
}

pits <- unique(data.table(pits, key = "uri"))

if (REDUCE){
  pits <- pits[uri %in% unique(pits$uri)[1:50],]
}


# replace url with namespace
pits$type <- gsub("http://rdf.histograph.io/", "hg:", pits$type)

# translate geometry to geoJSON and clean it
geoJsonGeo <-
  lapply(pits$geometry[!is.na(pits$geometry)], wkt2geojson)
jsonGeo <- lapply(geoJsonGeo, function(b) {
  toJSON(b$geometry,digits = NA)
})

pits$geometry[!is.na(pits$geometry)] <- sapply(jsonGeo, clearGeoJSON)

# Handle missing time
startYear <- -33

endYear <- as.integer(format(Sys.time(), "%Y")) + 1000

colvect <- c(4:7, 9:12, 14:17)
valuevect <-
  as.integer(rep(rep(c(startYear, endYear), each = 2), length(colvect) /
                   4))

for (i in 1:length(colvect)) {
  pits[is.na(pits[[colvect[i]]]), ][[colvect[i]]] <- valuevect[i]
}

###################################################################################################
# Check for main name variants that have different time range as the root object
###################################################################################################

diffTimeRange <- pits[name == altName &
                        !(
                          (
                            earliestBegin == startYear &
                              latestBegin == startYear &
                              earliestEnd  == endYear &
                              latestEnd == endYear
                          ) |
                            (
                              earliestAltBegin == startYear &
                                latestAltBegin == startYear &
                                earliestAltEnd == endYear &
                                latestAltEnd == endYear
                            ) |
                            (
                              earliestBegin == earliestAltBegin &
                                latestBegin == latestAltBegin &
                                earliestEnd == earliestAltEnd &
                                latestEnd == latestAltEnd
                            )
                        ), ]

if (nrow(diffTimeRange) > 0)
{
  putMsg(
    "There are pits where the main name variant (same name as the main object) has a different time range:" ,
    paste(diffTimeRange$uri, sep = "")
  )
}

###################################################################################################
# Define groups of pits and sameAs pits
###################################################################################################

# These PITS are the main elements for each graph of sameAs relations when one alt name is the same as the main name
Pits_M_e_A_Tm_e_Ta <- pits[name == altName & (
  (
    earliestBegin == earliestAltBegin &
      latestBegin == latestAltBegin &
      earliestEnd == earliestAltEnd &
      latestEnd == latestAltEnd
  ) |
    (
      earliestAltBegin == startYear &
        latestAltBegin == startYear &
        earliestAltEnd == endYear &
        latestAltEnd == endYear
    ) |
    (
      earliestBegin == startYear &
        latestBegin == startYear &
        earliestEnd == endYear &
        latestEnd == endYear
    )
), ]

# These PITS are the main elements AND the secondary elements because the main alt name
# does not overlap in time with the main name
# the 2nd clause is the negation of the 2nd clause above

Pits_M_e_A_Tm_ne_Ta <- pits[name == altName & !(
  (
    earliestBegin == earliestAltBegin &
      latestBegin == latestAltBegin &
      earliestEnd == earliestAltEnd &
      latestEnd == latestAltEnd
  ) |
    (
      earliestAltBegin == startYear &
        latestAltBegin == startYear &
        earliestAltEnd == endYear &
        latestAltEnd == endYear
    )  |
    (
      earliestBegin == startYear &
        latestBegin == startYear &
        earliestEnd == endYear &
        latestEnd == endYear
    )
), ]


# These PITS will be the secondary elements connected to the main with sameAs relations
sameAsPits_M_ne_A <- pits[name != altName, ]

stopifnot (
  nrow(sameAsPits_M_ne_A) + nrow(Pits_M_e_A_Tm_ne_Ta) + nrow(Pits_M_e_A_Tm_e_Ta) - nrow(pits) == 0
)

onlyAltIds <- unique(sameAsPits_M_ne_A$uri[!(
  (sameAsPits_M_ne_A$uri %in% Pits_M_e_A_Tm_e_Ta$uri) |
    (sameAsPits_M_ne_A$uri %in% Pits_M_e_A_Tm_ne_Ta$uri)
)])

if (length(onlyAltIds) > 0)
{
  putMsg(
    "There are pits where the main name is different from each name variant:" ,
    paste(onlyAltIds, sep = "")
  )
}

###################################################################################################
# Select PITS that have compatible geometry ranges
###################################################################################################

Pits_M_e_A_Tm_e_Ta_Tm_e_Tg <- Pits_M_e_A_Tm_e_Ta[(
  earliestBegin == earliestGeoBegin &
    latestBegin == latestGeoBegin &
    earliestEnd == earliestGeoEnd &
    latestEnd == latestGeoEnd
)
|
  (
    earliestGeoBegin == startYear &
      latestGeoBegin == startYear &
      earliestGeoEnd == endYear &
      latestGeoEnd == endYear
  ),
canNames(),with=FALSE]


Pits_M_e_A_Tm_ne_Ta_Tm_e_Tg <- Pits_M_e_A_Tm_ne_Ta[(
  earliestBegin == earliestGeoBegin &
    latestBegin == latestGeoBegin &
    earliestEnd == earliestGeoEnd &
    latestEnd == latestGeoEnd
)
|
  (
    earliestGeoBegin == startYear &
      latestGeoBegin == startYear &
      earliestGeoEnd == endYear &
      latestGeoEnd == endYear
  ),
canNames(),with=FALSE]

sameAsPits_M_e_A_Tm_ne_Ta_Ta_e_Tg <- Pits_M_e_A_Tm_ne_Ta[(
  earliestAltBegin == earliestGeoBegin &
    latestAltBegin == latestGeoBegin &
    earliestAltEnd == earliestGeoEnd &
    latestAltEnd == latestGeoEnd
)
|
  (
    earliestGeoBegin == startYear &
      latestGeoBegin == startYear &
      earliestGeoEnd == endYear &
      latestGeoEnd == endYear
  ),
altNames(),with=FALSE]

# Make column names equal
names(sameAsPits_M_e_A_Tm_ne_Ta_Ta_e_Tg) <- canNames()

# We do not add the previous ids to pitsToAdd since those ids are for sure already in the main ids.


sameAsPits_M_ne_A_Ta_e_Tg <- sameAsPits_M_ne_A[(
  earliestAltBegin == earliestGeoBegin &
    latestAltBegin == latestGeoBegin &
    earliestAltEnd == earliestGeoEnd &
    latestAltEnd == latestGeoEnd
)
|
  (
    earliestGeoBegin == startYear &
      latestGeoBegin == startYear &
      earliestGeoEnd == endYear &
      latestGeoEnd == endYear
  ),
altNames(),with=FALSE]

# Make column names equal
names(sameAsPits_M_ne_A_Ta_e_Tg) <- canNames()

# add the main pits that will be referred to with sameAs, we will filter them later for duplicates
pitsToAdd <- sameAsPits_M_ne_A[(
  earliestAltBegin == earliestGeoBegin &
    latestAltBegin == latestGeoBegin &
    earliestAltEnd == earliestGeoEnd &
    latestAltEnd == latestGeoEnd
)
|
  (
    earliestGeoBegin == startYear &
      latestGeoBegin == startYear &
      earliestGeoEnd == endYear &
      latestGeoEnd == endYear
  ),
canNames(),with=FALSE]


###################################################################################################
# Select PITS that have incompatible geometry ranges. These will have to be linked to the main PITS
# we also check that the PIT target of the sameAs relation exists and otherwise create them
###################################################################################################

##
## SameAs Pits with geometry
##
sameAsPits_M_e_A_Tm_e_Ta_Tm_ne_Tg <- Pits_M_e_A_Tm_e_Ta[!(
  (
    earliestBegin == earliestGeoBegin &
      latestBegin == latestGeoBegin &
      earliestEnd == earliestGeoEnd &
      latestEnd == latestGeoEnd
  )
  |
    (
      earliestGeoBegin == startYear &
        latestGeoBegin == startYear &
        earliestGeoEnd == endYear &
        latestGeoEnd == endYear
    )
),
geoNames(),with=FALSE]

stopifnot(sum(is.na(sameAsPits_M_e_A_Tm_e_Ta_Tm_ne_Tg$geometry)) == 0)

# Make column names equal
names(sameAsPits_M_e_A_Tm_e_Ta_Tm_ne_Tg) <- canNames()

# add the main pits that will be referred to with sameAs, we will filter them later for duplicates
pitsToAdd <- rbind(pitsToAdd,Pits_M_e_A_Tm_e_Ta[!(
  (
    earliestBegin == earliestGeoBegin &
      latestBegin == latestGeoBegin &
      earliestEnd == earliestGeoEnd &
      latestEnd == latestGeoEnd
  )
  |
    (
      earliestGeoBegin == startYear &
        latestGeoBegin == startYear &
        earliestGeoEnd == endYear &
        latestGeoEnd == endYear
    )
),
canNames(),with=FALSE])


sameAsPits_M_e_A_Tm_ne_Ta_Tm_ne_Tg <- Pits_M_e_A_Tm_ne_Ta[!(
  (
    earliestBegin == earliestGeoBegin &
      latestBegin == latestGeoBegin &
      earliestEnd == earliestGeoEnd &
      latestEnd == latestGeoEnd
  )
  |
    (
      earliestGeoBegin == startYear &
        latestGeoBegin == startYear &
        earliestGeoEnd == endYear &
        latestGeoEnd == endYear
    )
),
geoNames(),with=FALSE]

stopifnot(sum(is.na(sameAsPits_M_e_A_Tm_ne_Ta_Tm_ne_Tg$geometry)) == 0)

# Make column names equal
names(sameAsPits_M_e_A_Tm_ne_Ta_Tm_ne_Tg) <- canNames()

# add the main pits that will be referred to with sameAs, we will filter them later for duplicates
pitsToAdd <- rbind(pitsToAdd, Pits_M_e_A_Tm_ne_Ta[!(
  (
    earliestBegin == earliestGeoBegin &
      latestBegin == latestGeoBegin &
      earliestEnd == earliestGeoEnd &
      latestEnd == latestGeoEnd
  )
  |
    (
      earliestGeoBegin == startYear &
        latestGeoBegin == startYear &
        earliestGeoEnd == endYear &
        latestGeoEnd == endYear
    )
),
canNames(),with=FALSE])

##
## Pits without geometry
##

sameAsPits_M_e_A_Tm_ne_Ta_Ta_ne_Tg <-  Pits_M_e_A_Tm_ne_Ta[!(
  (
    earliestAltBegin == earliestGeoBegin &
      latestAltBegin == latestGeoBegin &
      earliestAltEnd == earliestGeoEnd &
      latestAltEnd == latestGeoEnd
  )
  |
    (
      earliestGeoBegin == startYear &
        latestGeoBegin == startYear &
        earliestGeoEnd == endYear &
        latestGeoEnd == endYear
    )
),
altNames(),with=FALSE]

# geometry needs to be removed as it is not overlapping
sameAsPits_M_e_A_Tm_ne_Ta_Ta_ne_Tg$geometry <- NA

# Make column names equal
names(sameAsPits_M_e_A_Tm_ne_Ta_Ta_ne_Tg) <- canNames()

# We do not add the previous ids to pitsToAdd since those ids are for sure already in the main ids.

sameAsPits_M_ne_A_Ta_ne_Tg <- sameAsPits_M_ne_A[!(
  (
    earliestAltBegin == earliestGeoBegin &
      latestAltBegin == latestGeoBegin &
      earliestAltEnd == earliestGeoEnd &
      latestAltEnd == latestGeoEnd
  )
  |
    (
      earliestGeoBegin == startYear &
        latestGeoBegin == startYear &
        earliestGeoEnd == endYear &
        latestGeoEnd == endYear
    )
),
altNames(),with=FALSE]

# geometry needs to be removed as it is not overlapping
sameAsPits_M_ne_A_Ta_ne_Tg$geometry <- NA

# Make column names equal
names(sameAsPits_M_ne_A_Ta_ne_Tg) <- canNames()

# add the main pits that will be referred to with sameAs, we will filter them later for duplicates
pitsToAdd <- rbind(pitsToAdd,sameAsPits_M_ne_A[!(
  (
    earliestAltBegin == earliestGeoBegin &
      latestAltBegin == latestGeoBegin &
      earliestAltEnd == earliestGeoEnd &
      latestAltEnd == latestGeoEnd
  )
  |
    (
      earliestGeoBegin == startYear &
        latestGeoBegin == startYear &
        earliestGeoEnd == endYear &
        latestGeoEnd == endYear
    )
),
canNames(),with=FALSE])

stopifnot (
  nrow(Pits_M_e_A_Tm_e_Ta_Tm_e_Tg) +
    nrow(sameAsPits_M_e_A_Tm_e_Ta_Tm_ne_Tg) +
    nrow(Pits_M_e_A_Tm_ne_Ta_Tm_e_Tg) +
    nrow(sameAsPits_M_e_A_Tm_ne_Ta_Tm_ne_Tg) +
    nrow(sameAsPits_M_ne_A_Ta_e_Tg) +
    nrow(sameAsPits_M_ne_A_Ta_ne_Tg) - nrow(pits) == 0
)

stopifnot (
  nrow(Pits_M_e_A_Tm_e_Ta_Tm_e_Tg) +
    nrow(sameAsPits_M_e_A_Tm_e_Ta_Tm_ne_Tg) +
    nrow(sameAsPits_M_e_A_Tm_ne_Ta_Ta_e_Tg) +
    nrow(sameAsPits_M_e_A_Tm_ne_Ta_Ta_ne_Tg) +
    nrow(sameAsPits_M_ne_A_Ta_e_Tg) +
    nrow(sameAsPits_M_ne_A_Ta_ne_Tg) - nrow(pits) == 0
)

###################################################################################################
# Make pits unique, group them in main and sameAs and check that all targets for sameAs exist
###################################################################################################

Pits_M_e_A_Tm_e_Ta_Tm_e_Tg <- unique(Pits_M_e_A_Tm_e_Ta_Tm_e_Tg)
Pits_M_e_A_Tm_ne_Ta_Tm_e_Tg <- unique(Pits_M_e_A_Tm_ne_Ta_Tm_e_Tg)
sameAsPits_M_e_A_Tm_ne_Ta_Ta_e_Tg <- unique(sameAsPits_M_e_A_Tm_ne_Ta_Ta_e_Tg)
sameAsPits_M_ne_A_Ta_e_Tg <- unique(sameAsPits_M_ne_A_Ta_e_Tg)
sameAsPits_M_e_A_Tm_e_Ta_Tm_ne_Tg <- unique(sameAsPits_M_e_A_Tm_e_Ta_Tm_ne_Tg)
sameAsPits_M_e_A_Tm_ne_Ta_Tm_ne_Tg <- unique(sameAsPits_M_e_A_Tm_ne_Ta_Tm_ne_Tg)
sameAsPits_M_e_A_Tm_ne_Ta_Ta_ne_Tg <- unique(sameAsPits_M_e_A_Tm_ne_Ta_Ta_ne_Tg)
sameAsPits_M_ne_A_Ta_ne_Tg <- unique(sameAsPits_M_ne_A_Ta_ne_Tg)

# remove geometry since per definition these PITS are selected among the ones with no geometry
pitsToAdd$geometry <- NA

pitsToAdd <- unique(pitsToAdd)


# pits that derive from alternative names
allAltNames <-
  rbind(
    sameAsPits_M_e_A_Tm_ne_Ta_Ta_e_Tg,
    sameAsPits_M_ne_A_Ta_e_Tg,
    sameAsPits_M_e_A_Tm_ne_Ta_Ta_ne_Tg,
    sameAsPits_M_ne_A_Ta_ne_Tg
  )

#keep order consistent
setkey(allAltNames,uri,name)

moreGeos <- unique(allAltNames$uri[duplicated(allAltNames[,-"geometry"])])

if( length(moreGeos) > 0 ){
  putMsg(
    "There are sameAs pits that differ only in geometry, we only consider the first one:" ,
    paste(moreGeos, sep = "")
  )
  allAltNames <- allAltNames[! uri %in% moreGeos,]
}

# pits that derive from geometries not overlapping with the main pit
allGeometries <-
  rbind(sameAsPits_M_e_A_Tm_e_Ta_Tm_ne_Tg,
        sameAsPits_M_e_A_Tm_ne_Ta_Tm_ne_Tg)

#keep order consistent
setkey(allGeometries,uri,name)

moreGeos <- unique(allGeometries$uri[duplicated(allGeometries[,-"geometry"])])

if( length(moreGeos) > 0 ){
  putMsg(
    "There are sameAs pits that differ only in geometry, we only consider the first one:" ,
    paste(moreGeos, sep = "")
  )
  allGeometries <- allGeometries[! uri %in% moreGeos,]
}

# main pits with geometries
allGeoMainPits <-
  rbind(Pits_M_e_A_Tm_e_Ta_Tm_e_Tg, Pits_M_e_A_Tm_ne_Ta_Tm_e_Tg)

moreGeos <- unique(allGeoMainPits$uri[duplicated(allGeoMainPits$uri)])

if( length(moreGeos) >0 ){
  putMsg(
    "There are pits that differ only in geometry, we only consider the first one:" ,
    paste(moreGeos, sep = "")
    )
  allGeoMainPits <- allGeoMainPits[! uri %in% moreGeos,]
}

# remove the ones that are already present
pitsToAdd <- pitsToAdd[! (uri %in% allGeoMainPits$uri),]

# main pits
allMainPits <-  rbind(allGeoMainPits, pitsToAdd)

stopifnot(sum(duplicated(allMainPits$uri)) == 0)

stopifnot(sum(!(allAltNames$uri %in% allMainPits$uri)) == 0)

stopifnot(sum(!(allGeometries$uri %in% allMainPits$uri)) == 0)

# Create new uris for Alt names
sameAsAltPits <-
  data.table(allAltNames[, paste(uri, "altName", seq_len(.N), sep = "_"), by=uri]$V1,
             allAltNames,
             key = "uri",
             stringsAsFactors = FALSE)

# create hg:sameHgConcept relations
altNamesRelations <-
  data.table(
    from = sameAsAltPits$V1,
    to = sameAsAltPits$uri,
    type = rep("hg:sameHgConcept", length(sameAsAltPits$uri))
  )

# set new uri
sameAsAltPits[, uri := NULL]
colnames(sameAsAltPits)[1] <- "uri"
setkey(sameAsAltPits, "uri")

# Create new uris for Geometries
sameAsGeoPits <-
  data.table(allGeometries[, paste(uri, "geo", seq_len(.N), sep = "_"), by=uri]$V1,
             allGeometries,
             key = "uri",
             stringsAsFactors = FALSE)

# create hg:sameHgConcept relations
geoRelations <-
  data.table(
    from = sameAsGeoPits$V1,
    to = sameAsGeoPits$uri,
    type = rep("hg:sameHgConcept", length(sameAsGeoPits$uri))
  )

# set new uri
sameAsGeoPits[, uri := NULL]
colnames(sameAsGeoPits)[1] <- "uri"
setkey(sameAsGeoPits, "uri")

allPits <- rbind(allMainPits, sameAsAltPits, sameAsGeoPits)

stopifnot(sum(duplicated(allPits$uri)) == 0)

# remove artificially created years
allPits$earliestBegin[allPits$earliestBegin == startYear] <- NA
allPits$latestBegin[allPits$latestBegin == startYear] <- NA
allPits$earliestEnd[allPits$earliestEnd == endYear] <- NA
allPits$latestEnd[allPits$latestEnd == endYear] <- NA

# handle validSince and validUntil
allPits <- data.table(allPits,validSince=as.character(rep(NA,nrow(allPits))),
                      validUntil=as.character(rep(NA,nrow(allPits))))

allPits[!(is.na(earliestBegin) | is.na(latestBegin)) & !(earliestBegin==latestBegin),
        validSince:=paste(TKN_ARR_OPEN,earliestBegin,TKN_ARR_SEP,latestBegin,TKN_ARR_CLOSE,sep="")]

allPits[!(is.na(earliestBegin) | is.na(latestBegin)) & (earliestBegin==latestBegin),
        validSince:=paste(TKN_OPEN,earliestBegin,TKN_CLOSE,sep="")]

allPits[!is.na(earliestBegin) & is.na(latestBegin),
        validSince:=paste(TKN_OPEN,earliestBegin,TKN_CLOSE,sep="")]

allPits[is.na(earliestBegin) & !is.na(latestBegin),
        validSince:=paste(TKN_OPEN,latestBegin,TKN_CLOSE,sep="")]

allPits[!(is.na(earliestEnd) | is.na(latestEnd)) & !(earliestEnd==latestEnd),
        validUntil:=paste(TKN_ARR_OPEN,earliestEnd,TKN_ARR_SEP,latestEnd,TKN_ARR_CLOSE,sep="")]

allPits[!(is.na(earliestEnd) | is.na(latestEnd)) & (earliestEnd==latestEnd),
        validSince:=paste(TKN_OPEN,earliestEnd,TKN_CLOSE,sep="")]

allPits[!is.na(earliestEnd) & is.na(latestEnd),
        validSince:=paste(TKN_OPEN,earliestEnd,TKN_CLOSE,sep="")]

allPits[is.na(earliestEnd) & !is.na(latestEnd),
        validSince:=paste(TKN_OPEN,latestEnd,TKN_CLOSE,sep="")]


allPits[, c("earliestBegin","latestBegin","earliestEnd","latestEnd") := NULL]

# order of columns matters because we need to patter-matching clean the generated JSON
setcolorder(allPits,c("uri","name","type","validSince","validUntil","geometry"))

###################################################################################################
#### RELATIONS
###################################################################################################
relations <-
  read.csv("./relations.csv",
           stringsAsFactors = FALSE,
           na.strings = "")

relations <- data.table(relations, key = "uri")

#relations$bag_uri <- sapply(relations$bag_uri,bagIDs,USE.NAMES=FALSE)

# create hg:sameHgConcept relations
indx <- !is.na(relations$bag_uri)
bagsameAs <-
  data.table(
    from = relations$uri[indx],
    to = relations$bag_uri[indx],
    type = rep("hg:sameHgConcept", sum(indx))
  )

indx <- !is.na(relations$wikipedia_uri)
wikipediasameAs <-
  data.table(
    from = relations$uri[indx],
    to = relations$wikipedia_uri[indx],
    type = rep("hg:sameHgConcept", sum(indx))
  )

indx <- !is.na(relations$liesInBag)
bagLiesIn <-
  data.table(
    from = relations$uri[indx],
    to = relations$liesInBag[indx],
    type = rep("hg:liesIn", sum(indx))
  )

indx <- !is.na(relations$liesInGem)
gemLiesIn <-
  data.table(
    from = relations$uri[indx],
    to = relations$liesInGem[indx],
    type = rep("hg:liesIn", sum(indx))
  )

indx <- !is.na(relations$absorbed)
absorbRel <-
  data.table(
    from = relations$uri[indx],
    to = relations$absorbed[indx],
    type = rep("hg:absorbed", sum(indx))
  )

indx <- !is.na(relations$absorbedBy)
absorbByRel <-
  data.table(
    from = relations$uri[indx],
    to = relations$absorbedBy[indx],
    type = rep("hg:absorbedBy", sum(indx))
  )

indx <- !is.na(relations$originated)
originatedRel <-
  data.table(
    from = relations$uri[indx],
    to = relations$originated[indx],
    type = rep("hg:originated", sum(indx))
  )

indx <- !is.na(relations$originatedBy)
originatedByRel <-
  data.table(
    from = relations$uri[indx],
    to = relations$originatedBy[indx],
    type = rep("hg:originatedBy", sum(indx))
  )

indx <- !is.na(relations$hasPart)
hasPartRel <-
  data.table(
    from = relations$uri[indx],
    to = relations$hasPart[indx],
    type = rep("hg:containsHgConcept", sum(indx))
  )

indx <- !is.na(relations$isPartOf)
isPartOfRel <-
  data.table(
    from = relations$uri[indx],
    to = relations$isPartOf[indx],
    type = rep("hg:withinHgConcept", sum(indx))
  )

if (REDUCE){
  allSameAsRelations <- rbind(
    altNamesRelations,
    geoRelations
  )
}else{

  allSameAsRelations <- rbind(
    altNamesRelations,
    geoRelations,
    bagsameAs,
    wikipediasameAs,
    bagLiesIn,
    gemLiesIn,
    absorbRel,
    absorbByRel,
    originatedRel,
    originatedByRel,
    hasPartRel,
    isPartOfRel
  )
}
# remove relation without a target
#allSameAsRelations <- allSameAsRelations[!is.na(to),]
###################################################################################################
#### GENERATE FILES
###################################################################################################

cat(clearJSON(allPits), file = "adamlinkstraten.pits.ndjson", sep = "")

cat(clearJSON(allSameAsRelations),
    file = "adamlinkstraten.relations.ndjson",
    sep = "")
