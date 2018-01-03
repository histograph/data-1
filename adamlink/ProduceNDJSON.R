library(data.table)
library(jsonlite)
library(wellknown)
#library(devtools)
#library(rwkt)

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
  
  return(new_value)
}

clearJSON <- function(value) {
  # remove quoted object
  new_value <- toJSON(value)
  new_value <- gsub('"geometry":"\\{', '"geometry":\\{', new_value)
  
  new_value <- gsub('\\}"\\}', '\\}\\}', new_value)
  
  # remove quoted "
  new_value <- gsub('\\\\"', '"', new_value)
  
  # insert new line
  new_value <- gsub('\\},\\{', '\\}\n\\{', new_value)
  
  # remove enclosing brackets
  new_value <- gsub('^\\[', '', new_value)
  new_value <- gsub('\\]$', '', new_value)
  
  return(new_value)
  
}

canNames <- function(){
  return(c("id","name","type","earliestBegin","latestBegin","earliestEnd","latestEnd","geometry"))
}

altNames <- function(){
  return(c("id","altName","type","earliestAltBegin","latestAltBegin","earliestAltEnd","latestAltEnd","geometry"))
}

geoNames <- function(){
  return(c("id","name","type","earliestGeoBegin","latestGeoBegin","earliestGeoEnd","latestGeoEnd","geometry"))
}

bagIDs <- function(id){
  return(gsub(pattern = "http://bag.basisregistraties.overheid.nl/bag/id/openbare-ruimte/0([0-9]+)","bag/\\1",id))
}

#############################################################
## PITS
#############################################################

# Read PITS
pits <-
  read.csv(
    "./pits.csv",
    stringsAsFactors = FALSE,
    na.strings = "",
    encoding = "UTF-8"
  )

if (sum(duplicated(pits)) > 0) {
  putMsg("The following id have duplicates:" , paste(pits[which(duplicated(pits)), ]$id, sep =
                                                       ""))
}

pits <- unique(data.table(pits, key = "id"))
# pits <- pits[1:100,]

# replace url with namespace
pits$type <- gsub("http://rdf.histograph.io/", "hg:", pits$type)

# translate geometry to geoJSON and clean it
geoJsonGeo <-
  lapply(pits$geometry[!is.na(pits$geometry)], wkt2geojson)
jsonGeo <- lapply(geoJsonGeo, function(b) {
  toJSON(b$geometry)
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
    paste(diffTimeRange$id, sep = "")
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

onlyAltIds <- unique(sameAsPits_M_ne_A$id[!(
  (sameAsPits_M_ne_A$id %in% Pits_M_e_A_Tm_e_Ta$id) |
    (sameAsPits_M_ne_A$id %in% Pits_M_e_A_Tm_ne_Ta$id)
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
altNames(),with=FALSE]

# Make column names equal
names(sameAsPits_M_e_A_Tm_ne_Ta_Ta_e_Tg) <- canNames()

sameAsPits_M_ne_A_Ta_e_Tg <- sameAsPits_M_ne_A[(
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
altNames(),with=FALSE]

# Make column names equal
names(sameAsPits_M_ne_A_Ta_e_Tg) <- canNames()

# add the main pits that will be referred to with sameAs, we will filter them later for duplicates
pitsToAdd <- sameAsPits_M_ne_A[(
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


###################################################################################################
# Select PITS that have incompatible geometry ranges. These will have to be linked to the main PITS
# we also check that the PIT target of the sameAs relation exists and otherwise create them
###################################################################################################

##
## Pits with geometry
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
altNames(),with=FALSE]

# geometry needs to be removed as it is not overlapping
sameAsPits_M_e_A_Tm_ne_Ta_Ta_ne_Tg$geometry <- NA

# Make column names equal
names(sameAsPits_M_e_A_Tm_ne_Ta_Ta_ne_Tg) <- canNames()

sameAsPits_M_ne_A_Ta_ne_Tg <- sameAsPits_M_ne_A[!(
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
altNames(),with=FALSE]

# geometry needs to be removed as it is not overlapping
sameAsPits_M_ne_A_Ta_ne_Tg$geometry <- NA

# Make column names equal
names(sameAsPits_M_ne_A_Ta_ne_Tg) <- canNames()

# add the main pits that will be referred to with sameAs, we will filter them later for duplicates
pitsToAdd <- rbind(pitsToAdd,sameAsPits_M_ne_A[!(
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

moreGeos <- unique(allAltNames$id[duplicated(allAltNames[,-"geometry"])])

if( length(moreGeos) > 0 ){
  putMsg(
    "There are sameAs pits that differ only in geometry:" ,
    paste(moreGeos, sep = "")
  )
  allAltNames <- allAltNames[! id %in% moreGeos,]
}

# pits that derive from geometries not overlapping with the main pit
allGeometries <-
  rbind(sameAsPits_M_e_A_Tm_e_Ta_Tm_ne_Tg,
        sameAsPits_M_e_A_Tm_ne_Ta_Tm_ne_Tg)

moreGeos <- unique(allGeometries$id[duplicated(allGeometries[,-"geometry"])])

if( length(moreGeos) > 0 ){
  putMsg(
    "There are sameAs pits that differ only in geometry:" ,
    paste(moreGeos, sep = "")
  )
  allGeometries <- allGeometries[! id %in% moreGeos,]
}

# main pits with geometries
allGeoMainPits <-
  rbind(Pits_M_e_A_Tm_e_Ta_Tm_e_Tg, Pits_M_e_A_Tm_ne_Ta_Tm_e_Tg)

moreGeos <- unique(allGeoMainPits$id[duplicated(allGeoMainPits$id)])

if( length(moreGeos) >0 ){
  putMsg(
    "There are pits that differ only in geometry:" ,
    paste(moreGeos, sep = "")
    )
  allGeoMainPits <- allGeoMainPits[! id %in% moreGeos,]
}

# remove the ones that are already present
pitsToAdd <- pitsToAdd[! (id %in% allGeoMainPits$id),]

# main pits
allMainPits <-  rbind(allGeoMainPits, pitsToAdd)

stopifnot(sum(duplicated(allMainPits$id)) == 0)

stopifnot(sum(!(allAltNames$id %in% allMainPits$id)) == 0)

stopifnot(sum(!(allGeometries$id %in% allMainPits$id)) == 0)

# Create new ids for Alt names
sameAsAltPits <-
  data.table(allAltNames[, paste(id, "altName", seq_len(.N), sep = "_"), by=id]$V1,
             allAltNames,
             key = "id",
             stringsAsFactors = FALSE)

# create hg:sameHgConcept relations
altNamesRelations <-
  data.table(
    from = sameAsAltPits$V1,
    to = sameAsAltPits$id,
    type = rep("hg:sameHgConcept", length(sameAsAltPits$id))
  )

# set new id
sameAsAltPits[, id := NULL]
colnames(sameAsAltPits)[1] <- "id"
setkey(sameAsAltPits, "id")

# Create new ids for Geometries
sameAsGeoPits <-
  data.table(allGeometries[, paste(id, "geo", seq_len(.N), sep = "_"), by=id]$V1,
             allGeometries,
             key = "id",
             stringsAsFactors = FALSE)

# create hg:sameHgConcept relations
geoRelations <-
  data.table(
    from = sameAsGeoPits$V1,
    to = sameAsGeoPits$id,
    type = rep("hg:sameHgConcept", length(sameAsGeoPits$id))
  )

# set new id
sameAsGeoPits[, id := NULL]
colnames(sameAsGeoPits)[1] <- "id"
setkey(sameAsGeoPits, "id")

allPits <- rbind(allMainPits, sameAsAltPits, sameAsGeoPits)

stopifnot(sum(duplicated(allPits$id)) == 0)

# remove artificially created years
allPits$earliestBegin[allPits$earliestBegin == startYear] <- NA
allPits$latestBegin[allPits$latestBegin == startYear] <- NA
allPits$earliestEnd[allPits$earliestEnd == endYear] <- NA
allPits$latestEnd[allPits$latestEnd == endYear] <- NA


###################################################################################################
#### RELATIONS
###################################################################################################
relations <-
  read.csv("./relations.csv",
           stringsAsFactors = FALSE,
           na.strings = "")

relations <- data.table(relations, key = "id")

relations$bag_uri <- sapply(relations$bag_uri,bagIDs,USE.NAMES=FALSE)

# create hg:sameHgConcept relations
indx <- !is.na(relations$bag_uri)
bagsameAs <-
  data.table(
    from = relations$id[indx],
    to = relations$bag_uri[indx],
    type = rep("hg:sameHgConcept", sum(indx))
  )

indx <- !is.na(relations$wikipedia_uri)
wikipediasameAs <-
  data.table(
    from = relations$id[indx],
    to = relations$wikipedia_uri[indx],
    type = rep("hg:sameHgConcept", sum(indx))
  )

indx <- !is.na(relations$liesInBag)
bagLiesIn <-
  data.table(
    from = relations$id[indx],
    to = relations$liesInBag[indx],
    type = rep("hg:liesIn", sum(indx))
  )

indx <- !is.na(relations$liesInGem)
gemLiesIn <-
  data.table(
    from = relations$id[indx],
    to = relations$liesInGem[indx],
    type = rep("hg:liesIn", sum(indx))
  )

indx <- !is.na(relations$liesInGem)
absorbRel <-
  data.table(
    from = relations$id[indx],
    to = relations$absorbed[indx],
    type = rep("hg:absorbed", sum(indx))
  )

indx <- !is.na(relations$liesInGem)
absorbByRel <-
  data.table(
    from = relations$id[indx],
    to = relations$absorbedBy[indx],
    type = rep("hg:absorbedBy", sum(indx))
  )

indx <- !is.na(relations$originated)
originatedRel <-
  data.table(
    from = relations$id[indx],
    to = relations$originated[indx],
    type = rep("hg:originated", sum(indx))
  )

indx <- !is.na(relations$originatedBy)
originatedByRel <-
  data.table(
    from = relations$id[indx],
    to = relations$originatedBy[indx],
    type = rep("hg:originatedBy", sum(indx))
  )

indx <- !is.na(relations$hasPart)
hasPartRel <-
  data.table(
    from = relations$id[indx],
    to = relations$hasPart[indx],
    type = rep("hg:containsHgConcept", sum(indx))
  )

indx <- !is.na(relations$isPartOf)
isPartOfRel <-
  data.table(
    from = relations$id[indx],
    to = relations$isPartOf[indx],
    type = rep("hg:withinHgConcept", sum(indx))
  )

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
###################################################################################################
#### GENERATE FILES
###################################################################################################

cat(clearJSON(allPits), file = "adamlinkstraten.pits.ndjson", sep = "")

cat(clearJSON(allSameAsRelations),
    file = "adamlinkstraten.relations.ndjson",
    sep = "")
