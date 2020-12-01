########################################
# Project:  UDOT-18.602 Traffic signal pedestrians
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     cluster_ts.R
# Date:     2020 Summer
# About:    Estimate models (factoring) predicting pedestrian volumes from signal data
########################################

########################################
# Notes

# Library
# install.packages("TSclust") # needed if running for the first time
library("TSclust")

########################################
# Load and filter data

# Load averaged hourly/weekday data
sigtab <- readRDS("sigtab.rds")
names(sigtab)

# Get selected PAMs
pam1 <- sigtab$PAM1
pam1[,c("TIME1", "TIME2", "TDIFF")] <- NULL
pam2 <- sigtab$PAM2
pam2[,c("TIME1", "TIME2", "TDIFF")] <- NULL
pam6 <- sigtab$PAM6
pam6[,c("TIME1", "TIME2", "TDIFF")] <- NULL

# Remove NA or Inf or zeros
pam1 <- pam1[,colSums(is.na(pam1))==0]
pam2 <- pam2[,colSums(is.na(pam2))==0]
pam6 <- pam6[,colSums(is.na(pam6))==0]
pam1 <- pam1[,colSums(is.infinite(as.matrix(pam1)))==0]
pam2 <- pam2[,colSums(is.infinite(as.matrix(pam2)))==0]
pam6 <- pam6[,colSums(is.infinite(as.matrix(pam6)))==0]
pam1 <- pam1[,colSums(pam1!=0)>0]
pam2 <- pam2[,colSums(pam2!=0)>0]
pam6 <- pam6[,colSums(pam6!=0)>0]

# Remove others
pam2 <- pam2[,names(pam2) %in% names(pam1)]
pam6 <- pam6[,names(pam6) %in% names(pam1)]
pam2[,c("1040","1041")] <- NULL
pam6[,c("1040","1041")] <- NULL
rm(pam1)

########################################
# Cluster

# Calculate distance matrix
dpam2 <- diss(pam2, "EUCL")
dpam6 <- diss(pam6, "CORT")

# Cluster: k-means on PAM2
set.seed(1234)
cpam2 <- kmeans(dpam2, 3, nstart = 50);
gpam2 <- cpam2$cluster
table(gpam2)

# Cluster: hierarchical on PAM6
cpam6 <- hclust(dpam6, method="ward.D2")
gpam6 <- cutree(cpam6, k=3);
table(gpam6)

########################################
# Typology

# Inspect
table(gpam2, gpam6)

# Assign typologies
temp <- data.frame(SIGNAL=as.integer(names(gpam2)), TYPOLOGY=NA)
temp$TYPOLOGY[gpam2==2] <- 1L
temp$TYPOLOGY[gpam2==3 & gpam6==1] <- 2L
temp$TYPOLOGY[gpam2==3 & gpam6==2] <- 3L
temp$TYPOLOGY[gpam2==3 & gpam6==3] <- 4L
temp$TYPOLOGY[gpam2==1 & gpam6==1] <- 5L
temp$TYPOLOGY[gpam2==1 & gpam6==2] <- 6L
temp$TYPOLOGY[gpam2==1 & gpam6==3] <- 7L
table(temp$TYPOLOGY)

########################################
# Finish

# Save
write.csv(temp, "typologies.csv", row.names=F)

# Cleanup
rm(pam2, cpam2, dpam2, gpam2)
rm(pam6, cpam6, dpam6, gpam6)
rm(temp)
rm(sigtab)
gc()

########################################
# END
########################################