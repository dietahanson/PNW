###############################################################################
## Code to get PNW intertidal data from raw to usable for input
## into buildweb.r to build a network
###############################################################################

setwd("~/Documents/PNW/network_data/intertidal")
library(data.table)
library(dplyr)


# get all required tables

reefs = read.csv(file = "spencer_raw/bcoindex_Reef.csv")
sects = read.csv(file = "spencer_raw/bcoindex_Tran.csv")
groups = read.csv(file = "spencer_raw/bcoindex_TSNreleases.csv",
                  stringsAsFactors = F)
tran = read.csv(file = "spencer_raw/bcoraw_BeltTran.csv")
look = read.csv(file = "spencer_raw/bcoraw_SppSearch.csv")
quad = read.csv(file = "spencer_raw/bcoraw_VertQuad.csv")
species = read.csv(file = "spencer_raw/bcoindex_Species.csv")


###############################################################################
## We want to get the names of all the species found in either the search, 
## transects, or quadrats
###############################################################################

# first get the transect data

tranm = merge(tran, sects[,c("TranNum", "ReefNum")],
              by = "TranNum",
              all.x = T)[,c("SpeciesNum", "ReefNum")]

# now the search data

lookm = look[,c("SpeciesNum", "ReefNum")]

# and quad data

quadm = merge(quad, sects[,c("TranNum", "ReefNum")],
              by = "TranNum",
              all.x = T)[,c("SpeciesNum", "ReefNum")]


# now row bind all the data together, but make sure column names match first

if ((colnames(tranm) == colnames(lookm)) &&  
    (colnames(lookm) == colnames(quadm))) {
  alldat = rbind(tranm, lookm, quadm)} else {
    print("column names don't match")
  }

# merge in the region information (linked to reef number)

alldatm = merge(alldat, reefs[,c("ReefNum", "RegionName")],
                by = "ReefNum", all.x = T)

# merge in the species information (linked to SpeciesNum)

alldatmall = as.data.table(merge(alldatm, groups[,c("SpeciesNum",
                                      "SpecOtterRegionName",
                                      "SpecOtterRegionTSN")], 
                   by = "SpeciesNum", all.x = T))

# change column names to match the code that we will use later to make the web

setnames(alldatmall, "RegionName" , "AreaCode")
setnames(alldatmall, "SpecOtterRegionName" , "TaxaName")
setnames(alldatmall, "SpecOtterRegionTSN" , "TaxaTSN")

# get a list of species which are missing a TaxaName and a TaxTSN

missing = unique(alldatmall[(alldatmall$TaxaName=="") & 
                              (alldatmall$TaxaTSN==""),]$SpeciesNum)
missing = species[match(missing, species$SpeciesNum),]

# output the list of species

#write.csv(alldatmall, file = "taxonlist.csv", row.names = F)


################################################################################
## Now get abundance data from quadrats, which we will use to make our known 
## outcomes for the loop analysis
################################################################################

# first add reef numbers, because we want to summarize by reef, not transect
quad.data = merge(quad, sects[,c("TranNum", "ReefNum")],
                  by = "TranNum",
                  all.x = T)


quad.data = data.table(quad.data)


# summed species by reef
quad_agg = as.data.frame(quad.data[, list(specsum = sum(VertQuadAbun)),
                              by = list(ReefNum, SpeciesNum)])



# get number of quadrats per reef

quad.data$uniquequad = paste(quad.data$TranNum, quad.data$QuadNum)
numquad = as.data.frame(quad.data[, list(quad.count = 
                                           length(unique(uniquequad))),
                              by = ReefNum])

# add number of quadrats to species sums

quad_agg = merge(quad_agg, numquad, by = "ReefNum", all.x = T)

# get number of individuals/% cover per 0.25m^2 by dividing total by number of 
# quads * 0.25m^2 (which gives #/% per m^2), then multiply by 0.25m^2

quad_agg$specden = (quad_agg$specsum/(quad_agg$quad.count*0.25))*0.25


# we need to make zeros for cases where a species was searched for but not
# found in a quadrat

reefspec = expand.grid(ReefNum=unique(quad_agg$ReefNum),  # every species/reef
                       SpeciesNum=unique(quad_agg$SpeciesNum))

quadtotal = merge(quad_agg, reefspec, by=c("ReefNum", "SpeciesNum"),
                  all.x = T, all.y = T)

quadtotal[is.na(quadtotal$specden),"specden"] <- 0

# add region info
quadall = merge(quadtotal, reefs[,c("ReefNum", "RegionName")],
                by = "ReefNum",
                all.x = T)

# plot differences between regions for each species, using reefs as samples
plot(quadall[quadall$SpeciesNum==100,]$RegionName,
     quadall[quadall$SpeciesNum==100,]$specden)

# run t test on each species to look for sig diff between regions

sp = unique(quadall$SpeciesNum) 

reg = levels(quadall$RegionName)

quadall$pval = 0

for (w in 1:length(sp)) {  # loop over species
  
  spnum = sp[w]
  
  test = t.test(quadall[quadall$SpeciesNum==spnum,]$specden ~    
                  quadall[quadall$SpeciesNum==spnum,]$RegionName)
  
  pval = test$p.value  
  
  quadall[quadall$SpeciesNum==spnum,]$pval = pval 
  
}

sigspec = quadall[(quadall$pval<0.05) &
                    (!is.na(quadall$pval)),]

sigspec = merge(sigspec, species[,c("SpeciesNum", "SpeciesName")],
                by = "SpeciesNum", all.x = T, all.y = F)


################################################################################
## Now get abundance data from belt transects, which we will use to make our 
## known outcomes for the loop analysis
################################################################################

tran.data = data.table(tran)

# get total distance of transect
tran.data = as.data.frame(tran.data[, list(mindist = min(BeltDistStart),
                                           maxdist = max(BeltDistStop),
                                           totaldist = max(BeltDistStop)-
                                             min(BeltDistStart),
                                           beltnum = length(unique(
                                             BeltDistStart))),
                                   by = list(TranNum, BeltRep)])

tran.data$area = (tran.data$beltnum*5)*0.5




