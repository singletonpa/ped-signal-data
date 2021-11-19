########################################
# Project:  UDOT-18.602 Traffic signal pedestrians
# Authors:  Prasanna Humagain (prasanna.hmg@usu.edu), Patrick Singleton (patrick.singleton@usu.edu)
# File:     cluster_ts.R
# Date:     2021 Spring, Summer, Fall
# About:    Temporal patterns and spatial characteristics using pedestrian push button data

########################################
# Initial loading

# Open R script by double-clicking, with data files in same directory

# Load packages
library(TSclust)
library(NbClust)
library(fpc)
library(ggplot2)
library(dplyr)
library(magrittr)
library(mlogit)
library(tidyr)

# Load data
# signal data
load("signal.Rdata") # as dat
# spatial & built environment data
load("be_dat.Rdata")
# average monthly data
load("mo_dat.Rdata")
# climatic division data
load("signals_cl.Rdata")
# date, time, and hour data
load("datime.Rdata")

#########################################
# Time-series clustering for hourly/weekday patterns

# Calculate hour to week expansion factors
dat[] <- apply(dat[], 2,function(x){x/sum(x)})

# Calculate distance matrix (CORT)
dat <- t(dat) # transpose
dis_cort <- diss(dat, "CORT")
dat <- t(dat) # transpose

# Perform k-means clustering
# with 1-7 clusters
set.seed(1234)
# keu_1 <- kmeans(dis_cort, 1, nstart=50)
# keu_2 <- kmeans(dis_cort, 2, nstart=50)
# keu_3 <- kmeans(dis_cort, 3, nstart=50)
# keu_4 <- kmeans(dis_cort, 4, nstart=50)
keu_5 <- kmeans(dis_cort, 5, nstart=50)
# keu_6 <- kmeans(dis_cort, 6, nstart=50)
# keu_7 <- kmeans(dis_cort, 7, nstart=50)

# Calculate goodness-of-fit measures
# v1 <- cluster.stats(dis_cort, keu_2$cluster, silhouette=T, wgap=T, sepindex=T); v1
# v2 <- cluster.stats(dis_cort, keu_3$cluster, silhouette=T, wgap=T, sepindex=T); v2
# v3 <- cluster.stats(dis_cort, keu_4$cluster, silhouette=T, wgap=T, sepindex=T); v3
v4 <- cluster.stats(dis_cort, keu_5$cluster, silhouette=T, wgap=T, sepindex=T); v4
# v5 <- cluster.stats(dis_cort, keu_6$cluster, silhouette=T, wgap=T, sepindex=T); v5
# v6 <- cluster.stats(dis_cort, keu_7$cluster, silhouette=T, wgap=T, sepindex=T); v6
# calinhara(dis_cort, keu_2$cluster, cn=2)
# calinhara(dis_cort, keu_3$cluster, cn=3)
# calinhara(dis_cort, keu_4$cluster, cn=4)
calinhara(dis_cort, keu_5$cluster, cn=5)
# calinhara(dis_cort, keu_6$cluster, cn=6)
# calinhara(dis_cort, keu_7$cluster, cn=7)

# Select k
# number of clusters chosen = 5, based on various fit statistics

# Calculate mean expansion factor for each cluster
clus_1 <- dat[,which(keu_5$cluster==1)]
clus_2 <- dat[,which(keu_5$cluster==2)]
clus_3 <- dat[,which(keu_5$cluster==3)]
clus_4 <- dat[,which(keu_5$cluster==4)]
clus_5 <- dat[,which(keu_5$cluster==5)]

# Convert means into data.frame
hr_wk <- data.frame(rowMeans(clus_1[,1:ncol(clus_1)]),
                    rowMeans(clus_2[,1:ncol(clus_2)]),
                    rowMeans(clus_3[,1:ncol(clus_3)]),
                    rowMeans(clus_4[,1:ncol(clus_4)]),
                    rowMeans(clus_5[,1:ncol(clus_5)]))
hr_wk <- cbind(datime[,1:3], hr_wk) # add daytime, weekday, and hour
names(hr_wk)<- c("DAYTIME","WEEKDAY","HOUR","Clus_2","Clus_5","Clus_4","Clus_1","Clus_3")

# Plot hourly means
# example below is for Cluster 1
# repeat for other clusters
# add day, hour, week for plotting
clus_1 <- cbind(datime[,1:3], clus_1)
# calculate statistics
clus_1$mean <- rowMeans(clus_1[,4:ncol(clus_1)])
clus_1$median <- apply(clus_1[,4:ncol(clus_1)], 1, median)
clus_1$sd <- apply(clus_1[,4:ncol(clus_1)], 1, sd)
clus_1$p90 <- apply(clus_1[,4:ncol(clus_1)], 1, function(x) quantile(x, probs=.90))
clus_1$p10 <- apply(clus_1[,4:ncol(clus_1)], 1, function(x) quantile(x, probs=.10))
clus_1$p25 <- apply(clus_1[,4:ncol(clus_1)], 1, function(x) quantile(x, probs=.25))
clus_1$p75 <- apply(clus_1[,4:ncol(clus_1)], 1, function(x) quantile(x, probs=.75))
clus_1$max <- apply(clus_1[,4:ncol(clus_1)], 1, max)
clus_1$min <- apply(clus_1[,4:ncol(clus_1)], 1, min)
# plot and save as TIFF
# tiff("C-1.tiff", width = 4.25, height = 2.75, units = 'in', res = 300)
mar=c(5,4,4,15)
p <- ggplot(clus_1, aes(x=DAYTIME))+
  geom_line(aes(y=mean,color="mean", linetype="mean",size="mean"))+
  geom_line(aes(y=median,color="median", linetype="median", size="median"))+
  geom_line(aes(y=p10, color= "10th and 90th percentile", linetype="10th and 90th percentile",size="10th and 90th percentile"))+ 
  geom_line(aes(y=p90, color= "10th and 90th percentile", linetype="10th and 90th percentile",size="10th and 90th percentile"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(color="black"))
p <- p + scale_x_datetime(date_labels = "%a",date_breaks = "1 day")+ 
  theme(axis.title.x = element_blank())+ labs(y="Proportion of weekly total" )+
  scale_color_manual("", values =c ("mean"="red","median"="black","10th and 90th percentile"="black"))+
  scale_size_manual("",values=c(0.5,1,1.2))+ scale_linetype_manual("",values=c(2,1,1))+
  theme(legend.position ="bottom", text=element_text(size=12, family="Times New Roman"),
        plot.title = element_text(hjust=0.5),legend.key = element_blank())
p
# dev.off()
rm(v4, p)

#########################################
# Time-series clustering for monthly patterns

# Prepare data for clustering
mo_dat <- mo_dat[,colSums(is.na(mo_dat))==0] # remove signals with NA
temp <- mo_dat %>% select_if(~any(. > 3)) # select signals with expansion factor >3
mo_dat <- mo_dat[,(names(mo_dat) %in% names(temp)==F)] # remove those in temp

# Calculate distance matrix (CORT)
mo_cort <- diss(mo_dat[,-1], "CORT" )

# Perform k-means clustering
# with 1-7 clusters
set.seed(1234)
# kmo_1 <- kmeans(mo_cort, 1, nstart=50)
# kmo_2 <- kmeans(mo_cort, 2, nstart=50)
kmo_3 <- kmeans(mo_cort, 3, nstart=50)
# kmo_4 <- kmeans(mo_cort, 4, nstart=50)
# kmo_5 <- kmeans(mo_cort, 5, nstart=50)
# kmo_6 <- kmeans(mo_cort, 6, nstart=50)
# kmo_7 <- kmeans(mo_cort, 7, nstart=50)

# Calculate goodness-of-fit measures
# v1 <- cluster.stats(mo_cort, kmo_2$cluster, silhouette=T, wgap=T, sepindex=T); v1
v2 <- cluster.stats(mo_cort, kmo_3$cluster, silhouette=T, wgap=T, sepindex=T); v2
# v3 <- cluster.stats(mo_cort, kmo_4$cluster, silhouette=T, wgap=T, sepindex=T); v3
# v4 <- cluster.stats(mo_cort, kmo_5$cluster, silhouette=T, wgap=T, sepindex=T); v4
# v5 <- cluster.stats(mo_cort, kmo_6$cluster, silhouette=T, wgap=T, sepindex=T); v5
# v6 <- cluster.stats(mo_cort, kmo_7$cluster, silhouette=T, wgap=T, sepindex=T); v6
# calinhara(mo_cort, kmo_2$cluster, cn=2)
calinhara(mo_cort, kmo_3$cluster, cn=3)
# calinhara(mo_cort, kmo_4$cluster, cn=4)
# calinhara(mo_cort, kmo_5$cluster, cn=5)
# calinhara(mo_cort, kmo_6$cluster, cn=6)
# calinhara(mo_cort, kmo_7$cluster, cn=7)

# Select k
# number of clusters chosen = 3, based on various fit statistics

# Calculate mean expansion factor for each cluster
clus_mo <- mo_dat[,-1] # remove first column
p_1 <- rowMeans(clus_mo[,which(kmo_3$cluster==1)])
p_2 <- rowMeans(clus_mo[,which(kmo_3$cluster==2)])
p_3 <- rowMeans(clus_mo[,which(kmo_3$cluster==3)])

# Convert means into data.frame
mo_ex<- data.frame(mo_dat[,1],p_1,p_2,p_3) # add 1-12 as months
names(mo_ex)<-c("month","clus1","clus2","clus3")
mo_ex$mo <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# Plot monthly expansion factor means#
mo_ex$mo <- factor(mo_ex$mo, levels = month.abb) # change month into factors
# plot and save as tiff
# tiff("month.tiff", width = 6.5, height = 3, units = 'in', res = 300)
mar=c(5,4,4,15)
p<- ggplot( data = mo_ex,aes(x=mo) )+ylim(0,1.5)+
  geom_line(aes(y=clus1,color="Cluster A",group=1),size=1.5)+
  geom_line(aes(y=clus3,color="Cluster B",group=3),size=1.5)+ 
  geom_line(aes(y=clus2,color="Cluster C",group=2),size=1.5)+
  labs(colour=element_blank())+
  labs(y="Proportion of yearly average")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(color="black"),
        axis.title.x = element_blank(),
        legend.position =c(0.5,0.1), legend.direction ="horizontal",text=element_text(size=12, family="Times New Roman"),
        plot.title = element_text(hjust=0.5),legend.key = element_blank())+
  scale_color_manual("", values =c ("Cluster A"="black","Cluster B"="red",
                                    "Cluster C"="grey"))+
  scale_x_discrete(name="mo")
p
#dev.off()
rm(v2, p)

#######################################
# Multinomial logit modeling to determine
# spatial factors affecting patterns

# Multinomial logit model for hourly/weekly patterns
# create data.frame of signals and clusters
temp <- data.frame(names(keu_5$cluster), keu_5$cluster) # dataframe combining signal and clusters
names(temp) <- c("SIGNAL","Hr_clus")
temp$SIGNAL <- as.integer(gsub("[a-zA-z ]","",temp$SIGNAL)) # remove "sig" from signal ID
# merge with be_dat
be_dat <- merge(be_dat, temp, by="SIGNAL")
# adjust cluster #s
be_dat$Hr_clus <- as.factor(be_dat$Hr_clus) # convert to factor
levels(be_dat$Hr_clus) <- c("2","5","4","1","3") # renumber based on paper #s
be_dat$Hr_clus <- relevel(be_dat$Hr_clus, ref = "1") # set Cluster 1 as reference
# create as mlogit.data
mdat_be <- mlogit.data(be_dat, varying = NULL, choice="Hr_clus", shape="wide")
# estimate model
mod_1 <- mlogit(Hr_clus ~ 1 | per_res_qtmi+per_com_qtmi+per_ind_qtmi+
                 park_sqmi_qtmi + per4wy_qtmi +stopden_qtmi+avgveh_qtmi+hhsize_qtmi +schools_qtmi+
                 popden_000_qtmi+empden_000_qtmi+income_000_qtmi +intden_qtmi+ worship_qtmi,data=mdat_be)
# model results
summary(mod_1)
# cleanup
rm(temp)

# Multinomial logit model for monthly patterns
# create data.frame of signals and clusters
temp <- data.frame(names(kmo_3$cluster), kmo_3$cluster) # dataframe combining signal and clusters
names(temp) <- c("SIGNAL","Mo_clus")
temp$SIGNAL <- as.integer(gsub("[a-zA-z ]","",temp$SIGNAL))  # remove "sig" from signal ID
# merge with be_dat and signals_cl
be_dat<- merge(be_dat, temp, by="SIGNAL")
be_dat <- merge(be_dat, signals_cl, by.x = "SIGNAL",by.y="Signal_ID")
# adjust climatic divisions
be_dat$Cl_division <- factor(be_dat$Cl_division)
be_dat$Cl_division <- relevel(be_dat$Cl_division, ref = "northcentral")
levels(be_dat$Cl_division) <- c("NC","Dixie","NM","O","O","O")
be_dat<- fastDummies::dummy_cols(be_dat, select_columns = "Cl_division") # convert to dummies
# adjust cluster #s
be_dat$Mo_clus <- factor(be_dat$Mo_clus)
levels(be_dat$Mo_clus) <- c("A", "C", "B") # renumber based on paper #s
be_dat$Mo_clus <- relevel(be_dat$Mo_clus, ref = "A") # set Cluster A as reference
# create as mlogit.data
mdat_be <- mlogit.data(be_dat, varying = NULL, choice="Mo_clus", shape="wide")
# estimate model
mod_me <- mlogit(Mo_clus~ 1 | Cl_division_Dixie+Cl_division_NM+
                 per_res_qtmi+per_com_qtmi+per_ind_qtmi+
                 park_sqmi_qtmi + per4wy_qtmi +stopden_qtmi+avgveh_qtmi+hhsize_qtmi +schools_qtmi+
                 popden_000_qtmi+empden_000_qtmi+income_000_qtmi +intden_qtmi+ worship_qtmi,data=mdat_be)
# model results
summary(mod_me)
# cleanup
rm(temp)

#######################################
# Expansion/adjustment factor accuracy

# Calculate hourly expansion accuracy for hourly/weekday patterns
hrwk_err <- data.frame(clus_1, clus_2, clus_3, clus_4, clus_5)
hrwk_err[,(4:281)] <- apply(hrwk_err[,(4:281)], 2, function(x){abs((x/hr_wk$Clus_1)-1)*100})
hrwk_err[,(282:339)] <- apply(hrwk_err[,(282:339)], 2, function(x){abs((x/hr_wk$Clus_2)-1)*100})
hrwk_err[,(340:527)] <- apply(hrwk_err[,(340:527)], 2, function(x){abs((x/hr_wk$Clus_3)-1)*100})
hrwk_err[,(528:1398)] <- apply(hrwk_err[,(528:1398)], 2, function(x){abs((x/hr_wk$Clus_4)-1)*100})
hrwk_err[,(1399:1700)] <- apply(hrwk_err[,(1399:1700)], 2, function(x){abs((x/hr_wk$Clus_5)-1)*100})
each_er <- data.frame(rowMeans(hrwk_err[,(4:281)]),
                      rowMeans(hrwk_err[,(282:339)]),
                      rowMeans(hrwk_err[,(340:527)]),
                      rowMeans(hrwk_err[,(528:1398)]),
                      rowMeans(hrwk_err[,(1399:1700)]))
# plot in Excel

# Calculate adjustment factor accuracy for monthly clusters
mo_1 <- clus_mo[,which(kmo_3$cluster==1)]
mo_2 <- clus_mo[,which(kmo_3$cluster==2)]
mo_3 <- clus_mo[,which(kmo_3$cluster==3)]
mo_1[1:ncol(mo_1)]<- apply(mo_1[1:ncol(mo_1)], 2, function(x){abs((x/p_1)-1)*100})
mo_2[1:ncol(mo_2)]<- apply(mo_2[1:ncol(mo_2)], 2, function(x){abs((x/p_2)-1)*100})
mo_3[1:ncol(mo_3)]<- apply(mo_3[1:ncol(mo_3)], 2, function(x){abs((x/p_3)-1)*100})
mo_er <- data.frame(mo_dat$MONTH,
                    rowMeans(mo_1[1:ncol(mo_1)]),
                    rowMeans(mo_2[1:ncol(mo_2)]),       
                    rowMeans(mo_3[1:ncol(mo_3)]))
names(mo_er)<-c("MONTH","p_1","p_2","p_3")
mo_er$mo<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
mo_er$mo <- factor(mo_ex$mo, levels = month.abb)
# plot in Excel

########################################
# END
########################################