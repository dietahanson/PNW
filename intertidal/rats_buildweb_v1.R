
###
###     This script takes the output from rats_buildverout.r and an site-wide list (.csv file)
###          and creates a foodweb out of them
###       Forked from kodiak_buildweb_v7.r
###         which was created by combining the sanak comboNI_buildweb_v10.r and bottom half of julie taxonomyNbuildweb_v7.r scripts
###

###
### version 1:   forked from kodiak_buildweb_v7.r
###  20140901
###


###
###      LIBRARIES & FUNCTIONS
###

library(DBI)
library(RMySQL)
#library(network)                # for network plots
#library(sna)                    # for network plots

op <- par(no.readonly=TRUE)
su <- summary
#options(width=Sys.getenv("COLUMNS"))

## date
dddd <- gsub("-", "", as.character(Sys.Date()))


###
###    IMPORT
###

###  Taxon List
setwd("~/Documents/PNW/network_data/intertidal/")
# qry.inhier <- "SELECT S.SpeciesName, S.SpecTSN, S.DigAfName AS TaxaName, S.DigAfTSN AS TaxaTSN, S.SpecSSPGroup AS TaxaGroup
#qry.inhier <- "SELECT S.SpeciesName, S.SpecTSN, S.WebName AS TaxaName, S.WebTSN AS TaxaTSN, S.SpecSSPGroup AS TaxaGroup, T.AreaCode
               # FROM raoindex_Species S, raoindex_Site T, raoraw_SppSearch D
               # WHERE S.SpeciesNum = D.SpeciesNum
               # AND T.SiteNum = D.SiteNum"
#                AND T.AreaCode = 'LARS'"
# channel <- odbcConnect("rats_otters_local")
inhier.org <- read.csv("taxonlist.csv", stringsAsFactors = F)
inhier.org$AreaCode = as.factor(inhier.org$AreaCode)
 
#odbcClose(channel)


###  Foodweb Data
### ### Run Sanak.Pkg.A.DoITISLoopsFilters.r to create, or import from a local copy
# setwd("~/backups/tempbackups/")
#setwd("K:/sanak/sanaknet/snetreleases/jen_release_200805/jr0805_temp/")
ver.out <- read.csv("ver.out_rats_20140901_v1.csv")



###
###   ASSUMED TAXA
###

## Assumed species                                                            ## !!!!!!!!!!!!!!!
## ## for kodaik webs
inhier.ass <- as.data.frame(rbind(c("Bacillariophyta","2286","plankton"),
                                  c("Biofilm Complex","san272","biofilm"),
                                  c("Ciliophora","46211","plankton"),
                                  c("Cyanophycota","601","bluegreen"),
                                  c("Bacteria","202421","bacteria"),
                                  c("Detritus","san266","detritus"),
                                  c("Pyrrophycophyta","9873","plankton"),
                                  c("Euphausiidae","95500","plankton"),
                                  c("Prasinophyta","9512","plankton"),
                                  c("Sarcomastigophora","43781","plankton"),
                                  c("Foraminiferida","44030","plankton"),
                                  c("Prymnesiophyceae","2135","plankton"),
                                  c("Radiolaria","46088","plankton"),
                                  c("Ostracoda","84195","plankton"),
                                  c("Copepoda","85257","plankton"),
                                  c("Nematoda","59490","plankton"),
#                                   c("Homo sapiens","180092","mammal"),
                                  c("Enhydra lutris","180547","mammal"),       ## !!!!!!!!!!!!!!!
                                  c("Hemilepidotus hemilepidotus","180547","fish"),       ## !!!!!!!!!!!!!!!
                                  c("Hexagrammos decagrammus","180547","fish"),       ## !!!!!!!!!!!!!!!
                                  c("Sebastes melanops","180547","fish")       ## !!!!!!!!!!!!!!!
                           ))



## ## and what about appendicularian/larvaceans?   nah, they're covered by tunicata
## ## sep calanoid and harpacticoid cops?
## ## pteropods?, cladocerans?
## ## haptophytes?


dimnames(inhier.ass)[[2]] <- c("TaxaName","TaxaTSN","TaxaType")

inhier.ass$TaxaType = NULL

###
###   USER-DEFINED NODES
###

## Construct the inhier table (of taxa in each web)
inhier.lst <- list()
for (area in levels(inhier.org$AreaCode)) {
  print(paste("AREA =", area))

  ## Cleanup
  ## ## column names
  inhiertmp <- inhier.org[inhier.org$AreaCode==area,c("TaxaName", "TaxaTSN")]
  #dimnames(inhiertmp)[[2]] <- c("TaxaName","TaxaTSN","TaxaType") # I don't have Type

  ## ## Some taxa are in multiple webs (ns, int, bird) or multiple times in one web
  inhiertmp <- unique(inhiertmp)

  ## ## Remove the "drop" taxa from the web
  inhiertmp <- inhiertmp[inhiertmp$TaxaTSN != "drop", ]

  ## ## Strip trailing spaces from TaxaNames
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  inhiertmp$TaxaName <- trim(inhiertmp$TaxaName)

  ## ## add areas to output list
  inhier.lst[[area]] <- inhiertmp
}

## QA/QC of inhier
for (area in names(inhier.lst)) {
  inhier <- inhier.lst[[area]]
  print(paste("AREA =",area))

  print("## ## CHECK that everyone has a TSN")
  print(subset(inhier, TaxaTSN=="" | is.na(TaxaTSN)))
  ## ## want to know which taxa are failing this check?
  # print(subset(inhier.org, (TaxaTSN=="" | is.na(TaxaTSN)) & AreaCode==area))

  ## ## Confirm there aren't any synonyms in the web taxon list (inhier)
  ## ## ## a problem because synonyms won't exist in ver.out (they'll have been converted to current name/tsn)
  ## ## ## Import (unfortunately)
  ## ## ## ## ITIS
  qry.itis <- "SELECT tsn, tsn_accepted FROM synonym_links"
  channel = dbConnect(dbDriver("MySQL"), user = "dieta", password = "greenchile",
                   dbname="itis", host="sterna.dyndns.org")
  # channel <- odbcConnect("itis_local")
     it.synos <- dbGetQuery(channel,qry.itis)
     dbDisconnect(channel)
  # odbcClose(channel)
  # ## ## ## ## SanakNet
   qry.snet <- "SELECT tsn, tsn_accepted FROM sn_synonym_links"
   channel = dbConnect(dbDriver("MySQL"), user = "dieta", password = "greenchile",
                          dbname="sanaknet", host="sterna.dyndns.org")
  # channel <- odbcConnect("sanaknet_local")
  sn.synos <- dbGetQuery(channel,qry.snet)
  dbDisconnect(channel)
  # odbcClose(channel)
  # ## ## ## ## Combine itis and snet synonyms
   synos <- rbind(it.synos, sn.synos)

  print("## ## ## CHECK HERE for taxa names that need updated")
  print("## ## ## ## IF YOU SEE ROWS above, then STOP and update these taxa to their current name/tsn")
  print(inhier[which(inhier$TaxaTSN %in% synos$tsn), ])

  print("## ## CHECK for records with the SAME TSN but different names or groups")
  print(inhier[(duplicated(inhier)), ])
  # inhier[inhier$TaxaTSN=="1447", ]

  print("## ## CHECK for species IN INHIER list BUT NOT IN VER.OUT web")
  print("## ## ## for some unknown reason, this doesn't work: inhier$TaxaName[which(!inhier$TaxaTSN %in% unique(c(ver.out$PredTSN_parent,ver.out$PreyTSN_parent)))]")
  print(as.character(inhier$TaxaName[which(!inhier$TaxaTSN %in% unique(ver.out$PredTSN_parent) & !inhier$TaxaTSN %in% unique(ver.out$PreyTSN_parent))]))

  print("## ## FYI: These taxa don't have predators in ver.out (not final exported food web):")
  print(as.character(unique(inhier$TaxaName[which(!inhier$TaxaTSN %in% ver.out$PreyTSN_parent)])))
  print("## ## FYI: These taxa don't have prey in ver.out (not final exported food web):")
  print(as.character(unique(inhier$TaxaName[which(!inhier$TaxaTSN %in% ver.out$PredTSN_parent)])))
}


###
###   PARE PREDPREY TO CUSTOM NODES 
###        (CREATE VER.TRIM)
###

## function to build vertrim for specified pred & prey
## ## for prey, accepts either "all" or a vector of TSNs to keep
pare2vertrim <- function(pred.touse,prey.touse) {
  ## subset ver.out to just keep only pred/prey
  ## ## predators
  predkeep <- which(ver.out$PredTSN_parent %in% pred.touse)
  ## ## prey
  ## ## ## accepts TSNs or "all", which are rows where TSN = parentTSN
  if (unique(length(prey.touse)==1 & prey.touse=="all")) {
    preykeep <- which(ver.out$PreyTSN == ver.out$PreyTSN_parent)
  } else {
    preykeep <- which(ver.out$PreyTSN_parent %in% prey.touse)
  }
  ## pick ver.out rows with pred and prey both in the list of keepers
  forvetr <- ver.out[predkeep[which(predkeep %in% preykeep)], ]

  ## We want the TSN_parent columns, not the original pred/prey TSNs
  forvetr$PredTSN <- NULL
  forvetr$PreyTSN <- NULL
  dimnames(forvetr)[[2]][which(dimnames(forvetr)[[2]] == "PredTSN_parent")] <- "PredTSN"
  dimnames(forvetr)[[2]][which(dimnames(forvetr)[[2]] == "PreyTSN_parent")] <- "PreyTSN"
  forvetr <- forvetr[ ,c("PredPreyNum","PredTSN","PreyTSN","PredStageNum","PreyStageNum")]
  return(forvetr)
}

## vertrim with PARED pred & PARED prey & ASSUMED taxa
ver.trim.lst <- list()
for (area in names(inhier.lst)) {
  inhier <- inhier.lst[[area]]
  print(paste("AREA =",area))

  pred.touse <- c(as.character(inhier$TaxaTSN),as.character(inhier.ass$TaxaTSN))
  prey.touse <- c(as.character(inhier$TaxaTSN),as.character(inhier.ass$TaxaTSN))
  ver.trim.lst[[area]] <- pare2vertrim(pred.touse,prey.touse)
}

## vertrim with PARED pred & ALL prey
# pred.touse <- inhier$TaxaTSN[inhier$TaxaType=="predator"]
# ver.trim.lst.allprey <- pare2vertrim(pred.touse,"all")

## Add ITIS taxon names to the inhier table
## ## inhier tsn needs to be a character value for merge (with san tsns)
# inhier$TaxaTSN <- as.character(inhier$TaxaTSN)
## ## merge
# inhier.out <- merge(inhier, spnams[ ,c("tsn","fullname")], by.x="TaxaTSN", by.y="tsn", all.x=T, all.y=F)


###
###    MORE QA/QC
###

## CONFIRM count of taxa in web is < count of taxa in incoming taxon list RETURNS TRUE
## No, this isn't likely true b/c inhier.lst lacks the assumed spp
# for (x in 1:length(ver.trim.lst)) {
#   print(length(unique(ver.trim.lst[[x]]$PredTSN,ver.trim.lst[[x]]$PreyTSN)) <= length(unique(inhier.lst[[x]]$TaxaTSN)))
# }



###
###    THE REST
###

## vt3 (aka to.analyse.out) contains all PredPreyNum for a given Pred and Prey
## ## so you can refer back to the data behind each link
## ## for later web analyses, you probably want the unique() of toanalyse.out Pred and Prey
## ## for trophic levels (below), cheddar is smart enough to extract unique nodes and links from vt3
inhier2 <- list()
vt2 <- list()
vt3 <- list()
for (area in names(inhier.lst)) {
  ## ## include the assumed species
  inhier2[[area]] <- unique(rbind(inhier.lst[[area]] , inhier.ass))

  ## ## Stick on the ID key
  inhier2[[area]]$TaxaNum <- 1:nrow(inhier2[[area]])

  ## Attach pred/prey names and nums
  vt2[[area]] <- merge(ver.trim.lst[[area]], inhier2[[area]][ ,c("TaxaName","TaxaTSN","TaxaNum")], by.x="PredTSN", by.y="TaxaTSN", all.x=T, all.y=F)
  dimnames(vt2[[area]])[[2]][(ncol(vt2[[area]])-1):ncol(vt2[[area]])] <- c("PredName","PredNum")
  ## Attach prey Names
  vt3[[area]] <- merge(vt2[[area]], inhier2[[area]][ ,c("TaxaName","TaxaTSN","TaxaNum")], by.x="PreyTSN", by.y="TaxaTSN", all.x=T, all.y=F)
  dimnames(vt3[[area]])[[2]][(ncol(vt3[[area]])-1):ncol(vt3[[area]])] <- c("PreyName","PreyNum")
}


###
###    MORE QA/QC
###

for (area in names(vt3)) {
  print(paste("AREA=",area))

  print("## These taxa lack prey in the final web:")
  vtnopy <- unique(vt3[[area]]$PreyNum[which(!(vt3[[area]]$PreyName %in% vt3[[area]]$PredName))])
  print(vtnopyih <- inhier2[[area]][inhier2[[area]]$TaxaNum %in% vtnopy,"TaxaName"])

  print("## These taxa lack preds in the final web:")
  vtnopd <- unique(vt3[[area]]$PredNum[which(!(vt3[[area]]$PredName %in% vt3[[area]]$PreyName))])
  print(vtnopdih <- inhier2[[area]][inhier2$TaxaNum %in% vtnopd,"TaxaName"])

  ## These consumers lack prey in the final web (should be zero!):
  # vtnopyih[vtnopyih$TaxaPhylum %in% conphy, ]

  ## Skipping 2nd QA/QC check for producers w/ prey, because no possible change from prev check above
}




###
###     EXPORT
###

## unlisted dataframe of inhier2 (with area codes)
inhier2mat <- do.call(rbind.data.frame, inhier2)
## ## add a vector of area codes
inhier2areas <- vector()
for (area in names(inhier2))
  inhier2areas <- c(inhier2areas , rep(area, nrow(inhier2[[area]])))
inhier2mat$AreaCode <- inhier2areas

## unlisted dataframe of vt3 (with area codes)
vt3mat <- do.call(rbind.data.frame, vt3)
## ## add a vector of area codes
vt3areas <- vector()
for (area in names(vt3))
  vt3areas <- c(vt3areas , rep(area, nrow(vt3[[area]])))
vt3mat$AreaCode <- vt3areas

#setwd(oudir)
## Write the species index
write.csv(inhier2mat, paste("vi_speciesindex_",dddd,".csv", sep=''), row.names=F)
## Write the combined web+names
write.csv(vt3mat, paste("vi_toanalyse.out_",dddd,".csv", sep=''), row.names=F)

## Write web-specific files
for (area in names(vt3)) {
  ## Write the .web file
  write.table(unique(vt3[[area]][ ,c("PredNum","PreyNum")]), paste("vi_web_",area,"_",dddd,".web", sep=''), quote=F, row.names=F, col.names=F)

  ## Write a version of toanalyse.out for gephi graphing
  ## ## gephi accepts dup rows, but causes it to automat create edge weights in data and plots, so use unique() here.
  # write.table(unique(vt3[ ,c("PredName","PreyName")]), paste("rats_toanalyse.out_",dddd,"_forgephi.csv", sep=''), sep=",", row.names=F, col.names=F)
  library(rgexf)

  nods <- inhier2[[area]][ ,c("TaxaNum","TaxaName")]
  edgs <- unique(vt3[[area]][ ,c("PredNum","PreyNum")])

  write.gexf(nodes=nods,
            edges=edgs,
            nodesAtt=inhier2[[area]]["TaxaTSN"],
  #            nodesVizAtt=list(position=gepos),
            defaultedgetype="directed",
            output=paste("vi_toanalyse.out_",area,"_",dddd,"_forgephi.gexf", sep=''))
}



###
###   THIS IS A HANDY BREAKING POINT
###     you can stop here and pick up again by reimporting the vt3mat and inhier2mat of all webs
###

# vt3mat <- read.csv("rats_toanalyse.out_20131002.csv")
# inhier2mat <- read.csv("rats_speciesindex_20131002.csv")



###
###   GENERATE NICHE WEBS
###

## source code to generate niche model webs (for info see foodweb_readme.txt file)
source("/home/woodsp/Desktop/kodiak/koddata/kodiakweb/foodweb/pkg/foodweb/R/GenerateWebs.r")
## source code required for GenerateWebs.r
source("/home/woodsp/Desktop/kodiak/koddata/kodiakweb/foodweb/pkg/foodweb/R/IntervalStats.R")

## Try to generate 500(?) niche webs per empirical web
numberofwebstocreate <- 10
niwe <- list()
for (area in unique(vt3mat$AreaCode)) {
  for (i in 1:numberofwebstocreate) {
    tmpS <- length(unique(c(vt3mat$PredNum[vt3mat$AreaCode==area] , vt3mat$PreyNum[vt3mat$AreaCode==area])))
    tmpL <- nrow(unique(vt3mat[vt3mat$AreaCode==area,c("PredNum","PreyNum")]))
    niwe[[area]][[i]] <- generate.niche(S=tmpS, L=tmpL, Ctol=0.05, toliter=100)
  }
  names(niwe[[area]]) <- 1:length(niwe[[area]])
}
names(niwe) <- unique(vt3mat$AreaCode)



###
###    WEB PROPERTIES
###

library(cheddar)

## code of owen petchey's foodweb stats functions (to calculate prop basal, genSD, Maxsim, etc)
source("/home/woodsp/Desktop/kodiak/koddata/kodiakweb/kodiak_buildweb_owenpetcheyfxns_v1.r")

## Fxn to calculate a suite of web properties
## ## accepts a LIST of webs
## ## each web in the list must be a BINARY matrix with 0/1s and nrow/ncol = S
webprops <- function(webs,area) {
 slc <- list();  bit <- list();  her <- vector();  omn <- vector();  mxs <- vector();  trl <- vector()
 for (i in names(webs)) {
    print(paste("processing web",i))

## write web to csv to debug memory issue in TrohpicLevels fxn
write.csv(webs[[i]], paste("debuggingSWTL/webii_",area,i,".csv", sep=''))

    ## S, L, and C
    slc[[i]] <- c(nrow(webs[[i]]), sum(webs[[i]]),  sum(webs[[i]])/nrow(webs[[i]])^2)
    ## %bottom, int, top, unconnected, cannibals
    bit[[i]] <- Bottom.Intermediate.Top(webs[[i]], proportion=T)
    ## %herbivores
    her[i] <- PropHerbs(webs[[i]])
    ## %omnivores
    ## ## sometimes this leads to NA values, when GetTL() can't solve diagonal (line ~7 of GetTL())
    ## ## i don't know why, but owen uses try(), so that must be expected.
    ## ## NAs removed from ave/sd calcs below
    omn[i] <- Fraction.omnivores(webs[[i]])
    ## maximum similarity
    mxs[i] <- Maxsim(webs[[i]])
    ## mean SWTL
    ## ## make a cheddar community
    nod <- data.frame(paste("x",rownames(webs[[i]]), sep=''));  dimnames(nod)[[2]] <- "node"
##
## !!!!! IS THE ABOVE CORRECT?!  ROWS EAT COLUMNS????  CHECK THIS!!!  !!!!!!!
##
    edg <- data.frame(webMatrixToLong(webs[[i]]));  dimnames(edg)[[2]] <- c("consumer","resource")
    edg$consumer <- paste("x",edg$consumer, sep='');  edg$resource <- paste("x",edg$resource, sep='')
    cweb <- RemoveIsolatedNodes(Community(nod, properties=list(title=area), trophic.links=edg))
    ## ## doit
    trl[i] <- mean(ShortWeightedTrophicLevel(cweb, include.isolated=F), na.rm=T)
  }
  tmpslc <- matrix(unlist(slc), ncol=3, byrow=T)
  tmpbit <- t(data.frame(sapply(bit, "[", "Proportions")))

  pout <- data.frame(cbind(tmpslc , tmpbit , her , omn , mxs , trl))
  dimnames(pout) <- list(names(webs) , c("s","l","c","pbot","pint","ptop","punc","pcan","pher","pomn","maxsim","swtl"))
  pout
}

## calculate structural properties of each NICHE web
noutmes <- data.frame();  noutsds <- data.frame();  noutars <- vector()
for (area in names(niwe)) {
  print(paste("processing", area))
  tmppout <- webprops(niwe[[area]],area)  # temp passing area name for debugging

  noutmes <- rbind(noutmes , apply(tmppout, 2, mean, na.rm=T))
  noutsds <- rbind(noutsds , apply(tmppout, 2, sd, na.rm=T))
  noutars <- c(noutars , area)
}

noutmes$AreaCode <- noutars;  noutsds$AreaCode <- noutars
dimnames(noutmes)[[2]] <- c(dimnames(tmppout)[[2]] , "AreaCode")
dimnames(noutsds)[[2]] <- c(dimnames(tmppout)[[2]] , "AreaCode")

# write.csv(noutmes, paste("kodiak_noutmes_",dddd,".csv", sep=''))
# write.csv(noutsds, paste("kodiak_noutsds_",dddd,".csv", sep=''))



## calculate structural properties of each EMPIRICAL web
## ## turn vt3matrix back into a list
## ## and with webs into wide (matrix) versions
vt3 <- list()
for (area in unique(vt3mat$AreaCode)) {
  print(paste("reshaping",area))
  ww <- unique(vt3mat[vt3mat$AreaCode==area,c("PredNum","PreyNum")])
  wws <- unique(c(ww$PredNum,ww$PreyNum))
  web.matrix <- matrix(data=0, nrow=length(wws), ncol=length(wws))
  dimnames(web.matrix) <- list(wws, wws)
  for (i in 1:nrow(ww)) {
    web.matrix[which(dimnames(web.matrix)[[1]]==ww$PredNum[i]),which(dimnames(web.matrix)[[2]]==ww$PreyNum[i])] <- 1
  }
  vt3[[area]] <- web.matrix
}
## ## (supposedly i should be using reshape, but can't get it to work...)  whatever, my loops are fast.
# reshape(ww[ ,c("PredNum","PreyNum","AreaCode")], v.names="PreyNum", idvar="PredNum", timevar="AreaCode", direction="wide")
## ## then run list through the magic webprops fxn
eoutmes <- data.frame();  eoutsds <- data.frame();  eoutars <- vector()
print(paste("processing all areas"))
eoutall <- webprops(vt3,"empiricals")  # temp passing area name for debugging
eoutall$AreaCode <- rownames(eoutall)

# write.csv(eoutall, paste("kodiak_eoutall_",dddd,".csv", sep=''))




###
###  Let's make some graphs!!
###

## plot niche vs empirical
# pdf(paste("kodiak_buildweb_v6_nicheXempirical_",dddd,".pdf", sep=''), width=11, height=8.5)
par(mfrow=c(3,3))
for (i in c(4:9,11:12)) {
  emes <- eoutall[ ,i]
  nmes <- noutmes[ ,i]
  ybot <- nmes - (2 * noutsds[ ,i])
  ytop <- nmes + (2 * noutsds[ ,i])
  ylim <- range(c(ybot,ytop,emes), na.rm=T)
  plot(as.factor(noutmes$AreaCode), emes, ylim=ylim, main=colnames(eoutall[i]))
  segments(1:6, ybot, 1:6, ytop)
  points(1:6, nmes, pch=1)
}
par(op)
# dev.off()


## plot niche error vs empirical
## ## i THINK rich's CIerr is number of 2sds btwn the emp and niche estimates (see rich and neo J Ecol paper)
## ## such that a CIerr < 1 is within the bounds of 2sds
## ## first few lines are same code as prev plot
cietab <- data.frame()
# pdf(paste("kodiak_buildweb_v6_nicheXempirical_CIerr_",dddd,".pdf", sep=''), width=11, height=8.5)
par(mfrow=c(3,3))
for (i in c(4:6,9,11:12)) {
  emes <- eoutall[ ,i]
  nmes <- noutmes[ ,i]
  ybot <- nmes - (2 * noutsds[ ,i])
  ytop <- nmes + (2 * noutsds[ ,i])

  cie <- rep(0, 6)
  ## distance between empirical and niche means
  cie[which(emes < nmes)] <- nmes[which(emes < nmes)] - emes[which(emes < nmes)]
  cie[which(emes > nmes)] <- emes[which(emes > nmes)] - nmes[which(emes > nmes)]
#   cie[which(emes < ybot)] <- -abs(ybot[which(emes < ybot)] - emes[which(emes < ybot)])
#   cie[which(emes > ytop)] <- emes[which(emes > ytop)] - ytop[which(emes > ytop)]
  ## normalized by 2sds
  cie <- cie/(2*noutsds[ ,i])
  ## distinguish +/- errors
  cie[which(emes < nmes)] <- -abs(cie[which(emes < nmes)])
  ## plotit
  ylim <- rep(max(abs(cie), na.rm=T), 2) * c(-1,1)
  plot(as.factor(noutmes$AreaCode), cie, ylim=ylim, main=colnames(eoutall[i]))
  abline(h=0)
  abline(h=-1, col="grey")
  abline(h=+1, col="grey")
  cietab <- rbind(cietab, cie)
}
cietab <- t(cietab)
dimnames(cietab)[[2]] <- colnames(eoutall[c(4:6,9,11:12)])
par(op)
# dev.off()


## Create modern otter abundance table
## ## my method was to first run otter_boat_surveys_USFW_v1.r
## ## which creates transect average SEOT abund over time as junk.csv 
## ## then open junk.csv in QGIS and select all transect points within 10km of the area
## ## using the select by radius tool
## ## then manually average however many transects, by entering and manually creating this table...
## ## AFOG mean(c(22,27.5,82,12.5,59,157.5)) = 60
## ## ANTO mean(c(16,2.5,17,3,5)) = 9
## ## CHIN mean(c(89.5,5,3.5,3)) = 25    # many of the same transects as WOME
## ## LARS mean(c(9,1,3,2,2,18)) = 6
## ## UGAN mean(c(10,10,2.5)) = 8
## ## WOME mean(c(89.5,5,3.5,1,3,3,6)) = 16
modottab <- data.frame(cbind(names(vt3) , c(60,9,25,6,8,16)), stringsAsFactors=F)
dimnames(modottab)[[2]] <- c("AreaCode","ottabu")
modottab$ottabu <- as.numeric(modottab$ottabu)


## Plot emp props vs Motter abund
# pdf(paste("kodiak_buildweb_v6_empiricalprops_Xotterabund",dddd,".pdf", sep=''), width=11, height=8.5)
par(mfrow=c(2,3))
for (i in c(4:6,9,11:12)) {
  plot(modottab$ottabu, eoutall[ ,i], main=colnames(eoutall)[i])
#   plot(modottab$ottabu, log1p(eoutall[ ,i]), main=colnames(eoutall)[i])
  print(paste("property",colnames(eoutall)[i]))
  print(su(lm(eoutall[ ,i] ~ modottab$ottabu)))
#   print(paste("LOG1P property",colnames(eoutall)[i]))
#   print(su(lm(log1p(eoutall[ ,i]) ~ modottab$ottabu)))
}
par(op)
# dev.off()


## Plot CIerr vs Motter abund
# pdf(paste("kodiak_buildweb_v6_nicheXempirical_CIerr_Xotterabund",dddd,".pdf", sep=''), width=11, height=8.5)
par(mfrow=c(3,4))
for (i in 1:ncol(cietab)) {
  plot(modottab$ottabu, cietab[ ,i], main=colnames(cietab)[i])
#   plot(modottab$ottabu, log1p(cietab[ ,i]), main=colnames(cietab)[i])
  print(paste("property",colnames(cietab)[i]))
  print(su(lm(cietab[ ,i] ~ modottab$ottabu)))
#   print(paste("LOG1P property",colnames(cietab)[i]))
#   print(su(lm(log1p(cietab[ ,i]) ~ modottab$ottabu)))
}
par(op)
# dev.off()






###
###   Trophic positions of all taxa in empirical webs for comparison to isotopes
###

## Trophic Levels
## ## Which trophic measure should we calculate?  and which to compare to isotopes?
## ## ## Look at WilliamsMartinez04AmNat.pdf!  (an earlier version of which is SFI WORKING PAPER 2002-10-056)
## ## ## they describe each measure of trophic position and compare it to a flow-based measure that includes diet fraction
## ## ## importantly, the 7 diff measures span the range of shortest to longest possible TL (W&M 2004 pg 460)
## ## ## they say "The bracket between shortest and prey-averaged TL may accurately predict isotopic composition"
## ## ## but i don't totally understand why they suggest prey-averaged TL as the upper bound.  Need to read the paper better.
# library(cheddar)
tl <- list();  gepos <- list()
for (area in names(vt3)) {
  print(paste("AREA=",area))
  ## make a cheddar community
  chnods <- inhier2[[area]][ ,c("TaxaNum","TaxaName")]
  dimnames(chnods)[[2]][2] <- "node"
  chedgs <- unique(vt3[[area]][ ,c("PredName","PreyName")])
  dimnames(chedgs)[[2]] <- c("consumer","resource")
  ## ## include remove isolated nodes
  ## ## ## ie, taxa in the inhier, that didn't make it into the final web (because not connected to any other nodes)
  ## ## ## alternatively, could create chnods of just inhier$TaxaName %in% c(vt3$PredName,vt3$PreyName)
  chweb <- RemoveIsolatedNodes(Community(chnods, properties=list(title=area), trophic.links=chedgs))

  ## calculate trophic positions
  ## ## BEWARE this fxn has memory problems!!
  ## ## on 20130701, a kodiak afognak web took ~10 mins and over 4Gb of mem
  ## ## then, a smaller web for dig afognak used all 8Gb of my mem and crashed X.  WTF!
  ## ## other webs, like Larsen Bay are fine (12% memory; runs in ~10 sec)
  ## ## better save first!  and watch top!
  # swtl <- ShortWeightedTrophicLevel(chweb)
  tl[[area]] <- TrophicLevels(chweb)

  print("## ## QA/QC: check swtl data sorted correctly")
  print(unique(chnods$node == rownames(tl[[area]])))

  ## vector of gephi plot positions: x is random uniform, y is SWTL, z is 1
  gepos[[area]] <- cbind(runif(length(tl[[area]][ ,2]), 0, 1000), tl[[area]][ ,2]*100, rep(1, length(tl[[area]][ ,2])))
}




## Write the trophic levels
write.csv(tl[[area]], paste("kodiak_toanalyse.out_",area,"_",dddd,"_trophiclevels.csv", sep=''), row.names=T)




###
###   ISOTOPE vs TROPHIC POSITION
###

## Assemble isotope data
## ## isotope values from ISU
iso.res1 <- read.csv("/home/woodsp/Desktop/kodiak/isotopes/isotope_results/ReportFinney Misarti Kodiak intertidal 06-19-2013_v2_SAWreformat.csv", na.strings="")
iso.res2 <- read.csv("/home/woodsp/Desktop/kodiak/isotopes/isotope_results/Report Nicole Misarti Kodiak Intertidal 09-2013_v2_SAWreformat.csv", na.strings="")
iso.res3 <- read.csv("/home/woodsp/Desktop/kodiak/isotopes/isotope_results/ReportNicole Kodiak 01-22-2014_SAWreformat.csv", na.strings="")
iso.res4 <- read.csv("/home/woodsp/Desktop/kodiak/isotopes/isotope_results/ReportMisarti Kodiak muscle and plant tissue 02-13-2014_SAWreformat.csv", na.strings="")
iso.res <- rbind(iso.res1,iso.res2,iso.res3,iso.res4)
## ## sample metadata catalogs
## ## ## 2012 samples: includes fish
iso.key.org1 <- read.csv("/home/woodsp/Desktop/kodiak/isotopes/catalog_intertidal/Kodiak Intertidal Collections spread sheet 2.18.2014_SAWreformat.csv", na.strings="")
## ## ## 2013 samples
iso.key.org2 <- read.csv("/home/woodsp/Desktop/kodiak/isotopes/catalog_intertidal/Kodiak Intertidal Collections 2013 1.6.2014 NEEDS WORK_SAWreformat.csv", na.strings="")
## ## ## 2013 fish
iso.key.org3 <- read.csv("/home/woodsp/Desktop/kodiak/isotopes/catalog_fish/Kodiak Intertidal Collections-Fish2013_2014-02-18_SAWreformat.csv", na.strings="")
## ## combine and cleanup
iso.key.org <- rbind(iso.key.org1[ ,c("SampleID","SpeciesName","Region")], iso.key.org2[ ,c("SampleID","SpeciesName","Region")], iso.key.org3[ ,c("SampleID","SpeciesName","Region")])
iso <- merge(iso.key.org, iso.res, by="SampleID", all.x=F, all.y=T)

## ## ## !!!!! WHAT IS UP WITH THESE ?? !!!!
# iso <- subset(iso, Region!="NONE")

## ## ## species names
iso$SpeciesName <- as.character(iso$Species)
iso$SpeciesName[iso$SpeciesName=="Alaria marginata"] <- "Alaria"
iso$SpeciesName[iso$SpeciesName=="Cancer oregonensis"] <- "Cancer"
iso$SpeciesName[iso$SpeciesName=="Cancer oregonesis"] <- "Cancer"
iso$SpeciesName[iso$SpeciesName=="Fucus gardneri"] <- "Fucus"
iso$SpeciesName[iso$SpeciesName=="Littorina sitkana"] <- "Littorina"
iso$SpeciesName[iso$SpeciesName=="Mytilus californianus"] <- "Mytilus"
iso$SpeciesName[iso$SpeciesName=="Pugettia sp."] <- "Pugettia"
iso$SpeciesName[iso$SpeciesName=="Saxidomus sp"] <- "Saxidomus gigantea"
iso$SpeciesName[iso$SpeciesName=="Strongylocenotus droebachiensus"] <- "Strongylocentrotus droebachiensis"
iso$SpeciesName[iso$SpeciesName=="Strongylocentrotus purpuratus"] <- "Strongylocentrotus droebachiensis"
iso$SpeciesName[iso$SpeciesName=="Tectura scutum"] <- "Lottia scutum"
iso$SpeciesName[iso$SpeciesName=="Sebastes sp."] <- "Sebastes melanops"
iso$SpeciesName[iso$SpeciesName=="Sebastes ciliatus"] <- "Sebastes melanops"

## ## ## export a table for NM
# write.csv(iso, "iso_20140218.csv", row.names=F)

## ## statistics
d15Nmes <- aggregate(list(d15Nme=iso$d15N), list(SpeciesName=iso$SpeciesName, Region=iso$Region), FUN=mean, na.rm=TRUE)
d15Nsds <- aggregate(list(d15Nsd=iso$d15N), list(SpeciesName=iso$SpeciesName, Region=iso$Region), FUN=sd, na.rm=TRUE)
d15Nmes$regxspp <- paste(d15Nmes$Region,d15Nmes$SpeciesName,sep='x')
d15Nsds$regxspp <- paste(d15Nsds$Region,d15Nsds$SpeciesName,sep='x')
d15N <- merge(d15Nmes, d15Nsds[ ,c("regxspp","d15Nsd")], by="regxspp", all=T)

## ## ## nicole's modern otters
## ## ## ## thesis samples
iso.el1 <- read.csv("/home/woodsp/Desktop/kodiak_isotope_explore/Mean by species tissue correction_enhydra_sawformat.csv")
## ## ## ## another bigger newer batch of samples from UAM
iso.el2 <- read.csv("/home/woodsp/Desktop/kodiak_isotope_explore/UAM otters 2012_saw.csv")
## ## ## ## combine the datasets
## ## ## ## ## there two otters that are in both datasets:
inboth <- iso.el1$GUID[iso.el1$GUID %in% iso.el2$GUID]
## ## ## ## ## NM says to keep the new run, and toss the old run (see explanation in UAM otters 2012_README.txt)
iso.el1 <- iso.el1[-(iso.el1$GUID %in% inboth), ]
## ## ## ## ## do it
elncdat <- rbind(iso.el1[ ,c("GUID","d15N","d13C")] , iso.el2[ ,c("GUID","d15N","d13C")])
## ## ## ## add metadata
elncmet <- read.csv("/home/woodsp/Desktop/kodiak_isotope_explore/UAM_seaotters_fullcatalog_20130726.csv")
# gsub("DMNS:Mamm:", "", as.character(elncmet$GUID))
elncmet$GUID <- sapply(strsplit(as.character(elncmet$GUID), ":"), "[[", 3)
elncmer <- merge(elncdat, elncmet[ ,c("GUID","SPEC_LOCALITY","DEC_LAT","DEC_LONG")], by="GUID", all.x=T)
## ## ## ## ## USE KODIAK OTTERS ONLY
elnc <- subset(elncmer, DEC_LONG > -155.17 & DEC_LONG < -151.57 & DEC_LAT > 56.179 & DEC_LAT < 58.728)
## ## ## ## Add Enhydra to the modern isotope table
d15N <- rbind(d15N, c("ALLSxEnhydra lutris", "Enhydra lutris", NA, mean(elnc$d15N), sd(elnc$d15N)))
d15N$d15Nme <- as.numeric(d15N$d15Nme);  d15N$d15Nsd <- as.numeric(d15N$d15Nsd)


## ## trophic positions
## ## ## many diff ways to measure TL
# pdf(paste("kodiak_buildweb_v7_tlVSisotopes_wfish_",dddd,".pdf", sep=''), width=22, height=30)
par(mfrow=c(6,5))
impd <- "20130930"   # dddd
for (area in c("AFOG","ANTO","CHIN","LARS","UGAN","WOME")) {  # unique(d15N$Region)) {
#   tl <- read.csv(paste("/home/woodsp/Desktop/kodiak_isotope_explore/kodiak_toanalyse.out_",area,"_",impd,"_trophiclevels.csv", sep=''))
  tl <- read.csv(paste("/home/woodsp/Desktop/kodiak_isotope_explore/exploration2outputs/kodiak_toanalyse.out_",area,"_",impd,"_trophiclevels.csv", sep=''))
  tps <- as.data.frame(tl)   ## if not running above, import the ...toanalyse.out_dddd_trophiclevels.csv file first
  # tps$SpeciesName <- rownames(tl)
  dimnames(tps)[[2]][1] <- "SpeciesName"

  ## ## merge em
  tpsiso <- merge(tps, d15N[d15N$Region==area | d15N$SpeciesName=="Enhydra lutris", ], by="SpeciesName", all=T)
 
  ## ## plot em
  for (i in 3:7) {
    x <- tpsiso[ ,i]
    plot(d15Nme ~ x, tpsiso, ylim=c(5,18), xlab=colnames(tpsiso)[i], main=paste(area, "- all sites"))  # xlim=c(.5,5), 
    segments(x, tpsiso$d15Nme-tpsiso$d15Nsd, x, tpsiso$d15Nme+tpsiso$d15Nsd)
    abline(tilm <- lm(d15Nme ~ x, tpsiso))
    anova(tilm)
    text(x, tpsiso$d15Nme, as.character(tpsiso$SpeciesName), cex=.8)

    ## pearson linear correlation
#     print(paste(area , rcorr(matrix(c(tpsiso$d15Nme, x), ncol=2), type="pearson")$r[1,2]))
  }
}

par(op)
# dev.off()


## ## ## SWTL only  
## BUT I'M NOT SO SURE WE WANT TO USE THIS ONE FOR ANALYSES
## BECAUSE IT DOESN'T VARY FROM SITE TO SITE
## BUT ON THE OTHER HAND, SOME OF THE OTHER METRICS DON'T VARY MUCH ACROSS SPP
## SEE THE VARIATION PLOT CREATED BELOW
## ALTHOUGH PEARSON CORRELATION (SNIPET ABOVE) SHOW SWTL IS THE BEST CORRELATED METRIC W/ d15N ACROSS SITES
# pdf(paste("kodiak_buildweb_v7_swtlVSisotopes_",dddd,".pdf", sep=''), width=22, height=16)
par(mfrow=c(2,3))
impd <- "20130930"   # dddd
for (area in c("AFOG","ANTO","CHIN","LARS","UGAN","WOME")) {  # unique(d15N$Region)) {
#   tl <- read.csv(paste("/home/woodsp/Desktop/kodiak_isotope_explore/kodiak_toanalyse.out_",area,"_",impd,"_trophiclevels.csv", sep=''))
  tl <- read.csv(paste("/home/woodsp/Desktop/kodiak_isotope_explore/exploration2outputs/kodiak_toanalyse.out_",area,"_",impd,"_trophiclevels.csv", sep=''))
  tps <- as.data.frame(tl)   ## if not running above, import the ...toanalyse.out_dddd_trophiclevels.csv file first
  # tps$SpeciesName <- rownames(tl)
  dimnames(tps)[[2]][1] <- "SpeciesName"

  ## ## merge em
  tpsiso <- merge(tps, d15N[d15N$Region==area | d15N$SpeciesName=="Enhydra lutris", ], by="SpeciesName", all=T) 
#   print(area)
#   print(tpsiso[tpsiso$SpeciesName=="Telmessus cheiragonus", ])

  ## ## plot em
  i <- 3
  x <- tpsiso[ ,i]
  plot(d15Nme ~ x, tpsiso, ylim=c(4,18), xlim=c(.5,5), xlab=colnames(tpsiso)[i], main=paste(area, "- all sites"))
  segments(x, tpsiso$d15Nme-tpsiso$d15Nsd, x, tpsiso$d15Nme+tpsiso$d15Nsd)
  abline(tilm <- lm(d15Nme ~ x, tpsiso))
  anova(tilm)
  text(x, tpsiso$d15Nme, as.character(tpsiso$SpeciesName), cex=.8)
}

par(op)
# dev.off()





## Plot variation in diff trophic level metrics calculated from foodwebs
# pdf("kodiak_buildweb_v7_variation_in_diff_foodweb_TL_metrics.pdf", width=11, height=8.5)
tl <- data.frame()
for (area in c("AFOG","ANTO","CHIN","LARS","UGAN","WOME")) {  # unique(d15N$Region)) {
  tl <- rbind(tl, cbind(area, read.csv(paste("/home/woodsp/Desktop/kodiak_isotope_explore/exploration2outputs/kodiak_toanalyse.out_",area,"_",impd,"_trophiclevels.csv", sep=''))))
}
tltop <- tl[tl$X %in% d15N$SpeciesName, ]
par(mfrow=c(2,3))
for (i in 3:8) {
  tly <- tltop[ ,i]
  boxplot(tly ~ abbreviate(as.character(tltop$X), 3), main=colnames(tltop)[i])
}
# dev.off()










ee <- read.csv("kodiak_toanalyse.out_CHIN_20130930.csv")
write.csv(unique(ee[ ,c("PredNum","PreyNum")]), "~/vbshare/junk.csv", row.names=F)









as.matrix(dcast(edgs, PredNum ~ PreyNum, value.var = "1", fill=0))[,2:3]


GetTL <- function(web){
    
    ## takes predation matrix with consumers in columns
    ## identify the columns with basal species
    tweb <- t(web)

    ## make the rows add to one
    rs <- rowSums(tweb)
    for(i in 1:length(tweb[,1]))
        tweb[i,tweb[i,]==1] = 1/rs[i]

    nb.TL <- try(solve(diag(length(tweb[,1])) - tweb), T)

    if(class(nb.TL)=="try-error")
        nbTL <- rep(NA, length(tweb[,1]))

    if(class(nb.TL)!="try-error")
        nbTL <- rowSums(nb.TL)

    nbTL
    
}
    












