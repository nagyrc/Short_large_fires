#unused code

#######################################################
#from script #7 (shortyseasons_lev3_github)
####################################
#large fire data exploration

#what is max fire size of large fires?
summary(keep$FIRE_SIZE_ha)
#max=225895 ha
#use this as max for x axis and determining bin width

#look at histogram of fire sizes across all ecns
ggplot(data=keep, aes(keep$FIRE_SIZE_ha)) + 
  geom_histogram(binwidth=1000) 
#many relatively small 'large' fires


#######################

#seasonal correlations of large fires with fires of all sizes for east and west (by region)
#number of fires by Julian day of year
#fires of all causes
#not reported in the manuscript

#large fires only
r3 <- summaryBy(OBJECTID~DISCOVERY1+region, data=outputrr, FUN=nobs)
head(r3)

#fires of all sizes
subzrr<-left_join(subz,region,by="NA_L3CODE")
head(subzrr)

r4 <- summaryBy(OBJECTID~DISCOVERY1+region, data=subzrr, FUN=nobs)
head(r4)

#join the two dataframes
lll<-merge(r3,r4, by=c("DISCOVERY1","region"))
head(lll)
lll

names(lll)[1] <- "doy"
names(lll)[3]<-"large"
names(lll)[4]<-"all"

tempw<-subset(lll, lll$region == "west")
cor(tempw$large,tempw$all)
#r=0.978
summary(lm(large~all, data=tempw))
#p<2e-16

tempe<-subset(lll, lll$region == "east")
cor(tempe$large,tempe$all)
#r=0.988
summary(lm(large~all, data=tempe))
#p<2e-16


##############################
#####
#seasonal correlations at the ecoregion scale
head(output)

r3 <- summaryBy(OBJECTID~DISCOVERY1+NA_L3CODE, data=output, FUN=nobs)
head(r3)

r4 <- summaryBy(OBJECTID~DISCOVERY1+NA_L3CODE, data=subz, FUN=nobs)
head(r4)

lll<-merge(r3,r4, by=c("DISCOVERY1","NA_L3CODE"))
head(lll)


names(lll)[1] <- "doy"
names(lll)[3]<-"large"
names(lll)[4]<-"all"


cor(lll$large,lll$all)
#r=0.937
summary(lm(large~all, data=lll))
#p<2e-16


tt3<-unique(lll$NA_L3CODE)
tt3

outpute<-NULL
for (i in tt3) {
  subbye<-lll[lll$NA_L3CODE==i,]
  corre<-cor(subbye$large,subbye$all)
  outpute<-rbind(outpute,corre)
}

cop<-outpute

head(cop)

df<-as.data.frame(cop)

names(df)[1] <- "corr"

tt3v<-as.vector(tt3)
df$NA_L3CODE<-tt3v

row.names(df)<-NULL


head(df)

write.table(df, "C:/Users/rnagy/Dropbox/ecoregions/derived/seasonal_corr.csv", sep=",", row.names=FALSE)







#######################################################
#####
#not used in manuscript; below is for fires of all sizes, not just the largest fires
str(slim)
head(slim)

vec1<-unique(slim$NA_L3CODE)
vec2<-unique(slim$NA_L3NAME)

vec1

#############################
#BY ECOREGION


#############
#histograms with fire frequency by day of year, all fires
eco.legend <- data.frame(ec = unique(slim$NA_L3CODE), ed = unique(slim$NA_L3NAME))
head(eco.legend)
str(eco.legend)

#change the format of the legend in a not so elegant fashion
eco.legend$ec2<-as.character(eco.legend$ec)
eco.legend$ec3<-gsub("\\.", "", (eco.legend$ec2))
eco.legend$ecn<-as.numeric(eco.legend$ec3)
eco.legend <- subset(eco.legend, eco.legend$ecn != 000 & is.na(eco.legend$ecn) == FALSE)
eco.legend <- eco.legend[order(eco.legend$ecn),]

head(eco.legend)
unique(eco.legend$ecn)
###################

head(slim)

r2 <- summaryBy(OBJECTID~DISCOVERY1+ig+ecn, data=slim, FUN=length)
r2 <- subset(r2, r2$ecn != 000)
r2 <- subset(r2, is.na(r2$ecn) == FALSE)


head(r2)
unique(r2$ecn)
table(r2$DISCOVERY1)

#plots of number of fires (human and lightning) by Julian day per ecoregion
tt<-as.numeric(unique(r2$ecn))

for (i in tt) {
  
  wide <- reshape(r2[r2$ecn == i,], v.names="OBJECTID.length", idvar="ig", timevar="DISCOVERY1", direction="wide") #change number here 
  wide[is.na(wide)] <- 0
  
  head(wide)
  
  w <- wide[,3:length(wide)]
  
  colnames(w) <- c(substr(colnames(w), 17,19))
  rownames(w)<-c ("human","lightning")
  
  wm<-as.matrix(w)
  
  head(wm)
  
  blank<-matrix(0,2,366)
  rownames(blank)<-c("human","lightning")
  colnames(blank)<-c(1:366)
  
  head(blank)
  
  h <- match(rownames(wm), rownames(blank))
  j <- match(colnames(wm), colnames(blank))
  
  blank[h,j] <- wm
  
  head(blank)
  
  eco.desc <- eco.legend[eco.legend$ecn == i, c("ed")] #change number here
  
  barplot(blank, beside=T, horiz=F, legend=T, col=c("red", "blue"), 
          main = eco.desc, xlab = "Day of year in Julian Day", ylab = "Number of Fires", 
          border=c("red","blue"), xlim=c(1,1300))
}

########
#example ecoregion plots
tt2<-c(6215,1112)  
for (i in tt2) {
  
  wide <- reshape(r2[r2$ecn == i,], v.names="oid.nobs", idvar="ig", timevar="ddoy", direction="wide") #change number here 
  wide[is.na(wide)] <- 0
  
  w <- wide[,3:length(wide)]
  colnames(wide) <- c("ig", "ecn", substr(colnames(w), 10, 12))
  
  r2m <- as.matrix(wide[,3:length(wide)])
  rownames(r2m) <- wide[,1]
  
  head(r2m)
  
  eco.desc <- eco.legend[eco.legend$ecn == i, c("ed")] #change number here
  
  barplot(r2m, beside=T, horiz=F, legend=T, col=c("red", "blue"), 
          main = eco.desc, xlab = "Day of year in Julian Day", ylab = "Number of Fires", 
          border=c("red","blue"), cex.names=1.25, cex.axis=1.25, cex.lab=1.25)
}

#######
#END (unless needed on same scale or log scale, then change ec to ecn in below)


#######################################################

#plots by indv. ecoregions of fire size binned in categories for human vs. lightning fires

###
#to make a key of ecoregion names and numbers
tt7<-unique(subz[c("ecn", "NA_L3NAME")])

#write table to interpret the ecn numbers
write.table(tt7, "/Users/rana7082/Desktop/ecn_Lev3_key.csv", sep=",", row.names=FALSE)
###

###
#to make plots
tt3<-unique(subz$ecn)

hp <- list()

for (i in tt3) {
  subzz<-subz[subz$ecn==i,]
  
  hp[[i]] <- ggplot(subzz, aes(x=ha)) + 
    geom_histogram(binwidth=100) + 
    facet_wrap(~ig)
  
}

#print one example
hp[821]


#############################

#summary stats of number of fires, mean fire size, median fire size, total burned area for each ecoregion
r3ccc <- summaryBy(ha~ecn, data=subz, FUN=c(length,mean,sd,median,sum))
r3ccc$ha.se<-r3ccc$ha.sd/sqrt(r3ccc$ha.length)

#make into a dataframe
r3cccb<-as.data.frame(r3ccc)
head(r3cccb)

#order dataframe by mean fire size (ha)
r3d<-r3cccb[order(r3cccb$ha.mean),]

#output table
write.table(r3d, "/Users/rana7082/Dropbox/ecoregions/derived/fireha_ecn.csv", sep=",", row.names=FALSE)

#order dataframe by sum of burned area (ha)
r3e<-r3cccb[order(r3cccb$ha.sum),]

#output table
write.table(r3e, "/Users/rana7082/Dropbox/ecoregions/derived/firehasum_ecn.csv", sep=",", row.names=FALSE)


#add in code to dataframe for easy joining in Arc
tt9<-unique(subz[c("ecn", "NA_L3CODE")])
hhh<-left_join(r3d,tt9,by="ecn")

#output table
write.table(hhh, "/Users/rana7082/Dropbox/ecoregions/derived/fireha_ecn.csv", sep=",", row.names=FALSE)

#################################
#summary stats of number of fires, mean fire size, median fire size, total burned area by ecoregion and ignition source
r4 <- summaryBy(ha~ig+ecn, data=subz, FUN=c(length,mean,sd,median,sum))
r4$ha.se<-r4$ha.sd/sqrt(r4$ha.length)

#make into dataframe
r4b<-as.data.frame(r4)

#output table
write.table(r4b, "/Users/rana7082/Desktop/fireha_hvsl_ecn.csv", sep=",", row.names=FALSE)

#order by mean fire size
r5<-r4b[order(r4b$ha.mean),]

#order by sum of burned area
r5bb<-r4b[order(r4b$ha.sum),]

#transpose table into wide format
r5wide<-r5[,c("ig","ecn","ha.mean")]
r5wide<-spread(r5wide,ig,ha.mean)
head(r5wide)

#scatterplot of mean fire size of human vs. lightning by ecoregion
plot(r5wide$human,r5wide$lightning)
#within an ecoregion, as the human fires get bigger, lightning fires tend to get bigger


r5widebb<-r5bb[,c("ig","ecn","ha.sum")]
r5widebb<-spread(r5widebb,ig,ha.sum)
head(r5widebb)

#scatterplot of sum of burned area of human vs. lightning by ecoregion
plot(r5widebb$human,r5widebb$lightning)

#convert to dataframe
r5widebbdf<-as.data.frame(r5widebb)

#output table
write.table(r5widebbdf, "/Users/rana7082/Desktop/firehasum_hvsl_ecn_wide.csv", sep=",", row.names=FALSE)

#######
#number of human vs. lightning caused fires by ecoregion of fires of all sizes

r6 <- summaryBy(OBJECTID~ig+ecn, data=subz, FUN=length)

#transform to wide
r6wide<-spread(r6,ig,OBJECTID.length)

#calculate the total number of fires (all ignitions)
r6wide$tot<-r6wide$human+r6wide$lightning

#calculate the percent of human started fires
r6wide$perh<-r6wide$human/r6wide$tot*100

#order by decreasing percent human fires
r7<-r6wide[order(r6wide$perh),]
#range across ecn's is from 16-99% human

#output table
write.table(r7, "/Users/rana7082/Dropbox/ecoregions/derived/perh_ecn_Short_update.csv", sep=",", row.names=FALSE)


#######################################################