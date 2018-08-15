################################################################################
################################################################################
## Subtidal data wrangling
################################################################################
################################################################################

setwd("~/Documents/PNW/subtidal/")
library(data.table)

################################################################################
## First, quadrat data
################################################################################

quadraw = read.csv("subtidal_quadrat_raw.csv", stringsAsFactors = F)

# remove Clayquot data
quadraw = quadraw[!quadraw$region=="Clayquot Sound",]

# how are species measured? number or percent cover?
coversp = unique(quadraw[!is.na(quadraw$percent_cov),]$species)

numsp = unique(quadraw[!is.na(quadraw$number),]$species)

coversp[coversp %in% numsp]

numsp[numsp %in% coversp]


# get total number of quads per site
quaddt = data.table(quadraw)

quaddt = as.data.frame(quaddt[,list(sitequadtotal = length(unique(id))),
                                    by = list(site)])

# aggregate species by quad first

quadagg = data.table(quadraw)

quadagg = as.data.frame(quadagg[, list(quadsum = sum(number)),
                                   by = list(id, species)])

# add site data

quadagg = merge(quadagg, unique(quadraw[,c("id", "site")]),
                by = "id", all.x = T, all.y = F)

# now aggregate species by site

siteagg = data.table(quadagg)

siteagg = as.data.frame(siteagg[, list(sitesum = sum(quadsum)),
                                by = list(site, species)])

# add total number of quadrats to calculate density
siteagg = merge(siteagg, quaddt, by = "site", all.x = T)

siteagg$density = siteagg$sitesum/siteagg$sitequadtotal

# we need to make zeros for cases where a species was searched for but not
# found in a quadrat
sitespec = expand.grid(site=unique(siteagg$site),  # every species/reef
                       species=unique(siteagg$species))

quadtotal = merge(siteagg, sitespec, by=c("site", "species"),
                  all.x = T, all.y = T)

quadtotal[is.na(quadtotal$density),"density"] <- 0

# add in region info
quadtotal = merge(quadtotal, unique(quadraw[,c("site", "region")]),
                by = "site", all.x = T)

# do t tests to look for differences between regions
sp = unique(quadtotal$species) 

reg = levels(quadtotal$region)

quadtotal$pval = 0

for (w in 1:length(sp)) {  # loop over species
  
  spnum = sp[w]
  
  test = t.test(quadtotal[quadtotal$species==spnum,]$density ~    
                  quadtotal[quadtotal$species==spnum,]$region)
  
  pval = test$p.value  
  
  quadtotal[quadtotal$species==spnum,]$pval = pval 
  
}

sigspec = quadtotal[(quadtotal$pval<0.05) &
                    (!is.na(quadtotal$pval)),]





################################################################################
## Next, transect data
################################################################################

tranraw = read.csv("subtidal_transect_raw.csv", stringsAsFactors = F)

# remove Clayquot data
tranraw = tranraw[!tranraw$region=="Clayquot Sound",]
