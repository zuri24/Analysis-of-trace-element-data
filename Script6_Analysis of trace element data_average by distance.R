### TRace elements results####:
# Clean memory:
rm(list = ls('all.names' = TRUE)) # Remove all objects
graphics.off() # Remove all graphics
gc() # Clear cache

last_update <- "08_03_2023"
#### LOAD PACKAGES: ####

pkgs<- c('ggplot2','RNetCDF','ncdf4','SDMTools', 'data.table',
         'svMisc','abind','plyr','maptools','maps',
         'dplyr','RColorBrewer','raster','grid','scales',
         'ggsn','viridis', 'ff','rgdal', 'invgamma', 'xlsx', 'openxlsx', 'berryFunctions', 'zoo', 'roll')
lapply(pkgs, require, character.only = TRUE)

#### 1-Load Data####
load("Trace elements_data.RData") #This data base has mean median ppb and umm for all the samples

#### 2-Convert list to df - 85 individuals####
df_trace_elem <- do.call(rbind, df_trac_elem[4:88])

#df_trace_elem <- droplevels(df_trace_elem[which(df_trace_elem$dist_um <= 150),]) ###Only core region 

### 3-Calculate median and sd of each element considering all data####
## df_Sr88 mean by dist_um###
mean <- aggregate(mean_Sr88_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Sr88_umm ~ dist_um, df_trace_elem , sd)
#Rename columns
df_Sr88 <- data.frame(mean, sd$mean_Sr88_umm)
colnames(df_Sr88) <- c("dist_um", "mean", "sd") #
df_Sr88$baseline <- "all_data" #core analysis 
df_Sr88$element <- "Sr:Ca" #core analysis 
rm(mean, sd)

## df_Li7###
mean <- aggregate(mean_Li7_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Li7_umm ~ dist_um, df_trace_elem , sd)
#Rename columns
df_Li7 <- data.frame(mean, sd$mean_Li7_umm)
colnames(df_Li7) <- c("dist_um", "mean", "sd")
df_Li7$baseline <- "all_data" #core analysis 
df_Li7$element <- "Li:Ca" #core analysis 
rm(mean, sd)

## Mg 24###
mean <- aggregate(mean_Mg24_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Mg24_umm ~ dist_um, df_trace_elem , sd)
#Rename columns
df_Mg24<- data.frame(mean, sd$mean_Mg24_umm)
colnames(df_Mg24) <- c("dist_um", "mean", "sd")
df_Mg24$baseline <- "all_data" #core analysis 
df_Mg24$element <- "Mg:Ca" #core analysis 
rm(mean, sd)

## Mn 55###
mean <- aggregate(mean_Mn55_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Mn55_umm ~ dist_um, df_trace_elem , sd)
#Rename columns
df_Mn55<- data.frame(mean, sd$mean_Mn55_umm)
colnames(df_Mn55) <- c("dist_um", "mean", "sd")
df_Mn55$baseline <- "all_data" #core analysis 
df_Mn55$element <- "Mn:Ca" #core analysis 
rm(mean, sd)

## Zn 66###
mean <- aggregate(mean_Zn66_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Zn66_umm ~ dist_um, df_trace_elem , sd)
#Rename columns
df_Zn66 <- data.frame(mean, sd$mean_Zn66_umm)
colnames(df_Zn66) <- c("dist_um", "mean", "sd")
df_Zn66$baseline <- "all_data" #core analysis 
df_Zn66$element <- "Zn:Ca" #core analysis 

rm(mean, sd)

## Ba 138###
mean <- aggregate(mean_Ba138_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Ba138_umm ~ dist_um, df_trace_elem , sd)
#Rename columns
df_Ba138 <- data.frame(mean, sd$mean_Ba138_umm)
colnames(df_Ba138) <- c("dist_um", "mean", "sd")
df_Ba138$baseline <- "all_data" #core analysis 
df_Ba138$element <- "Ba:Ca" #core analysis 
rm(mean, sd)


## Co 59 ###
# mean <- aggregate(mean_Co59_umm ~ dist_um, df_trace_elem , mean)
# sd <- aggregate(mean_Co59_umm ~ dist_um, df_trace_elem , sd)

#Rename columns
# df_Co59 <- data.frame(mean, sd$mean_Co59_umm)
# colnames(df_Co59) <- c("dist_um", "mean", "sd")

## Cu 63 ###
# mean <- aggregate(mean_Cu63_umm ~ dist_um, df_trace_elem , mean)
# sd <- aggregate(mean_Cu63_umm ~ dist_um, df_trace_elem , sd)

#Rename columns
# df_Cu63 <- data.frame(mean, sd$mean_Cu63_umm)
# colnames(df_Cu63) <- c("dist_um", "mean", "sd")

###Put all bases together core edge ####
#Change the name, I estimate the profiles using the firt 150 umm (core) and all the transect (core to edge)
#df_allsamples_core_edge
#df_allsamples_core

df_allsamples_core_edge <- do.call(rbind, 
                        list(df_Ba138, 
                             df_Mg24, 
                             df_Mn55, 
                             df_Zn66, 
                             df_Sr88,
                             df_Li7 ))

####Save DF ####
save(df_allsamples_core_edge,
     file = "df_Trace elements_allsamples_core_edge_profiles_Oct2023.RData")




### 4. Subset data to specific baselines and core region#### can be perform separate analysis
####2012####


list_2012 <- (df_trac_elem[c(17,18,22,23,
                             31,33,37,
                             39,40,42,43,
                             55,59,60,67)])

df_2012 <- do.call(rbind, list_2012)

#df_2012 <- droplevels(df_2012[which(df_2012$dist_um <= 150),]) ###Only core region 

#Sr###
mean <- aggregate(mean_Sr88_umm ~ dist_um, df_2012, mean)
sd <- aggregate(mean_Sr88_umm ~ dist_um, df_2012, sd)

#Rename columns
df_Sr88_2012 <-data.frame(mean, sd$mean_Sr88_umm)
colnames(df_Sr88_2012) <- c("dist_um", "mean", "sd")
df_Sr88_2012$baseline <- 2012 #core analysis 
df_Sr88_2012$element <- "Sr:Ca" #core analysis 

rm(mean, sd)

#Ba 138###
mean <- aggregate(mean_Ba138_umm ~ dist_um, df_2012, mean)
sd <- aggregate(mean_Ba138_umm ~ dist_um, df_2012, sd)

#Rename columns
df_Ba138_2012 <- data.frame(mean, sd$mean_Ba138_umm)
colnames(df_Ba138_2012) <- c("dist_um", "mean", "sd")
df_Ba138_2012$baseline <- 2012 #core analysis 
df_Ba138_2012$element <- "Ba:Ca" #core analysis 
rm(mean, sd)

#Li 7###
# mean <- aggregate(mean_Li7_umm ~ dist_um, df_2012, mean)
# sd <- aggregate(mean_Li7_umm ~ dist_um, df_2012, sd)
# 
# #Rename columns
# df_Li7_2012 <- data.frame(mean, sd$mean_Li7_umm)
# colnames(df_Li7_2012) <- c("dist_um", "mean", "sd")

#Mg 24###
mean <- aggregate(mean_Mg24_umm ~ dist_um, df_2012, mean)
sd <- aggregate(mean_Mg24_umm ~ dist_um, df_2012, sd)

#Rename columns
df_Mg24_2012 <- data.frame(mean, sd$mean_Mg24_umm)
colnames(df_Mg24_2012) <- c("dist_um", "mean", "sd")

df_Mg24_2012$baseline <- 2012 #core analysis 
df_Mg24_2012$element <- "Mg:Ca" #core analysis 
rm(mean, sd)

#Zn 66###
mean <- aggregate(mean_Zn66_umm ~ dist_um, df_2012, mean)
sd <- aggregate(mean_Zn66_umm ~ dist_um, df_2012, sd)

#Rename columns
df_Zn66_2012 <- data.frame(mean, sd$mean_Zn66_umm)
colnames(df_Zn66_2012) <- c("dist_um", "mean", "sd")
df_Zn66_2012$baseline <- 2012 #core analysis
df_Zn66_2012$element <- "Zn:Ca" #core analysis 
rm(mean, sd)

#Mn 55###
mean <- aggregate(mean_Mn55_umm ~ dist_um, df_2012, mean)
sd <- aggregate(mean_Mn55_umm ~ dist_um, df_2012, sd)

#Rename columns
df_Mn55_2012 <- data.frame(mean, sd$mean_Mn55_umm)
colnames(df_Mn55_2012) <- c("dist_um", "mean", "sd")
df_Mn55_2012$baseline <- 2012 #core analysis 
df_Mn55_2012$element <- "Mn:Ca" #core analysis 
rm(mean, sd)

#Co59###
# mean <- aggregate(mean_Co59_umm ~ dist_um, df_2012, mean)
# sd <- aggregate(mean_Co59_umm ~ dist_um, df_2012, sd)
# 
# #Rename columns
# df_Co59_2012 <- data.frame(mean, sd$mean_Co59_umm)
# colnames(df_Co59_2012) <- c("dist_um", "mean", "sd")
# 
# #Cu63###
# mean <- aggregate(mean_Cu63_umm ~ dist_um, df_2012, mean)
# sd <- aggregate(mean_Cu63_umm ~ dist_um, df_2012, sd)
# 
# #Rename columns
# df_Cu63_2012 <- data.frame(mean, sd$mean_Cu63_umm)
# colnames(df_Cu63_2012) <- c("dist_um", "mean", "sd")

###Put all bases together 2012 baseline ####

df_2012_core_edge <- do.call(rbind, 
                        list(df_Ba138_2012, 
                        df_Mg24_2012, 
                        df_Mn55_2012, 
                        df_Zn66_2012, 
                        df_Sr88_2012))




#2013####

list_2013 <- (df_trac_elem[c(4,5,6,7,
                             12,13,14,24,
                             25,28,34,38,46,63)])

df_2013 <- do.call(rbind, list_2013)
# df_2013 <- na.omit(df_2013)
#df_2013 <- droplevels(df_2013[which(df_2013$dist_um <= 150),]) ###Only core region 



#Sr88 ###
mean <- aggregate(mean_Sr88_umm ~ dist_um, df_2013, mean)
sd <- aggregate(mean_Sr88_umm ~ dist_um, df_2013, sd)

#Rename columns
df_Sr88_2013 <-data.frame(mean, sd$mean_Sr88_umm)
colnames(df_Sr88_2013) <- c("dist_um", "mean", "sd")
df_Sr88_2013$baseline <- 2013 #core analysis 
df_Sr88_2013$element <- "Sr:Ca" #core analysis 
rm(mean, sd)

#Ba 138###
mean <- aggregate(mean_Ba138_umm ~ dist_um, df_2013, mean)
sd <- aggregate(mean_Ba138_umm ~ dist_um, df_2013, sd)

#Rename columns
df_Ba138_2013 <-data.frame(mean, sd$mean_Ba138_umm)
colnames(df_Ba138_2013) <- c("dist_um", "mean", "sd")
df_Ba138_2013$baseline <- 2013 #core analysis 
df_Ba138_2013$element <- "Ba:Ca" #core analysis 
rm(mean, sd)

#Li 7###
# mean <- aggregate(mean_Li7_umm ~ dist_um, df_2013, mean)
# sd <- aggregate(mean_Li7_umm ~ dist_um, df_2013, sd)
# 
# #Rename columns
# df_Li7_2013 <- data.frame(mean, sd$mean_Li7_umm)
# colnames(df_Li7_2013) <- c("dist_um", "mean", "sd")

#Mg 24###
mean <- aggregate(mean_Mg24_umm ~ dist_um, df_2013, mean)
sd <- aggregate(mean_Mg24_umm ~ dist_um, df_2013, sd)

#Rename columns
df_Mg24_2013 <- data.frame(mean, sd$mean_Mg24_umm)
colnames(df_Mg24_2013) <- c("dist_um", "mean", "sd")
df_Mg24_2013$baseline <- 2013 #core analysis 
df_Mg24_2013$element <- "Mg:Ca" #core analysis 
rm(mean, sd)

#Zn 66###
mean <- aggregate(mean_Zn66_umm ~ dist_um, df_2013, mean)
sd <- aggregate(mean_Zn66_umm ~ dist_um, df_2013, sd)

#Rename columns
df_Zn66_2013 <- data.frame(mean, sd$mean_Zn66_umm)
colnames(df_Zn66_2013) <- c("dist_um", "mean", "sd")
df_Zn66_2013$baseline <- 2013 #core analysis
df_Zn66_2013$element <- "Zn:Ca" #core analysis 
rm(mean, sd)

#Mn 55###
mean <- aggregate(mean_Mn55_umm ~ dist_um, df_2013, mean)
sd <- aggregate(mean_Mn55_umm ~ dist_um, df_2013, sd)

#Rename columns
df_Mn55_2013 <- data.frame(mean, sd$mean_Mn55_umm)
colnames(df_Mn55_2013) <- c("dist_um", "mean", "sd")
df_Mn55_2013$baseline <- 2013 #core analysis 
df_Mn55_2013$element <- "Mn:Ca" #core analysis 
rm(mean, sd)

#Co59###
# mean <- aggregate(mean_Co59_umm ~ dist_um, df_2013, mean)
# sd <- aggregate(mean_Co59_umm ~ dist_um, df_2013, sd)
# 
# #Rename columns
# df_Co59_2013 <- data.frame(mean, sd$mean_Co59_umm)
# colnames(df_Co59_2013) <- c("dist_um", "mean", "sd")
# 
# #Cu63###
# mean <- aggregate(mean_Cu63_umm ~ dist_um, df_2013, mean)
# sd <- aggregate(mean_Cu63_umm ~ dist_um, df_2013, sd)
# 
# #Rename columns
# df_Cu63_2013 <- data.frame(mean, sd$mean_Cu63_umm)
# colnames(df_Cu63_2013) <- c("dist_um", "mean", "sd")

###Put all bases together 2013 baseline ####

df_2013_core_edge <- do.call(rbind, 
                        list(df_Ba138_2013, 
                             df_Mg24_2013, 
                             df_Mn55_2013, 
                             df_Zn66_2013, 
                             df_Sr88_2013))


#2014####
list_2014 <- (df_trac_elem[c(9,11,15,16,
                             30,54,62,68)])

df_2014 <- do.call(rbind, list_2014)

#df_2014 <- droplevels(df_2014[which(df_2014$dist_um <= 150),]) ###Only core region 


#Sr ###
mean <- aggregate(mean_Sr88_umm ~ dist_um, df_2014, mean)
sd <- aggregate(mean_Sr88_umm ~ dist_um, df_2014, sd)

#Rename columns
df_Sr88_2014 <-data.frame(mean, sd$mean_Sr88_umm)
colnames(df_Sr88_2014) <- c("dist_um", "mean", "sd")
df_Sr88_2014$baseline <- 2014 #core analysis 
df_Sr88_2014$element <- "Sr:Ca" #core analysis 


rm(mean, sd)
#Ba 138###
mean <- aggregate(mean_Ba138_umm ~ dist_um, df_2014, mean)
sd <- aggregate(mean_Ba138_umm ~ dist_um, df_2014, sd)

#Rename columns
df_Ba138_2014 <-data.frame(mean, sd$mean_Ba138_umm)
colnames(df_Ba138_2014) <- c("dist_um", "mean", "sd")
df_Ba138_2014$baseline <- 2014 #core analysis 
df_Ba138_2014$element <- "Ba:Ca" #core analysis 

rm(mean, sd)
##Li 7###
# mean <- aggregate(mean_Li7_umm ~ dist_um, df_2014, mean)
# sd <- aggregate(mean_Li7_umm ~ dist_um, df_2014, sd)
# 
# #Rename columns
# df_Li7_2014 <- data.frame(mean, sd$mean_Li7_umm)
# colnames(df_Li7_2014) <- c("dist_um", "mean", "sd")

#Mg 24###
mean <- aggregate(mean_Mg24_umm ~ dist_um, df_2014, mean)
sd <- aggregate(mean_Mg24_umm ~ dist_um, df_2014, sd)

#Rename columns
df_Mg24_2014 <- data.frame(mean, sd$mean_Mg24_umm)
colnames(df_Mg24_2014) <- c("dist_um", "mean", "sd")
df_Mg24_2014$baseline <- 2014 #core analysis 
df_Mg24_2014$element <- "Mg:Ca" #core analysis 

rm(mean, sd)
#Zn 66###
mean <- aggregate(mean_Zn66_umm ~ dist_um, df_2014, mean)
sd <- aggregate(mean_Zn66_umm ~ dist_um, df_2014, sd)

#Rename columns
df_Zn66_2014 <- data.frame(mean, sd$mean_Zn66_umm)
colnames(df_Zn66_2014) <- c("dist_um", "mean", "sd")
df_Zn66_2014$baseline <- 2014 #core analysis
df_Zn66_2014$element <- "Zn:Ca" #core analysis 

rm(mean, sd)

#Mn 55###
mean <- aggregate(mean_Mn55_umm ~ dist_um, df_2014, mean)
sd <- aggregate(mean_Mn55_umm ~ dist_um, df_2014, sd)

#Rename columns
df_Mn55_2014 <- data.frame(mean, sd$mean_Mn55_umm)
colnames(df_Mn55_2014) <- c("dist_um", "mean", "sd")
df_Mn55_2014$baseline <- 2014 #core analysis 
df_Mn55_2014$element <- "Mn:Ca" #core analysis 

rm(mean, sd)
#Co59###
# mean <- aggregate(mean_Co59_umm ~ dist_um, df_2014, mean)
# sd <- aggregate(mean_Co59_umm ~ dist_um, df_2014, sd)
# 
# #Rename columns
# df_Co59_2014 <- data.frame(mean, sd$mean_Co59_umm)
# colnames(df_Co59_2014) <- c("dist_um", "mean", "sd")
# 
# #Cu63###
# mean <- aggregate(mean_Cu63_umm ~ dist_um, df_2014, mean)
# sd <- aggregate(mean_Cu63_umm ~ dist_um, df_2014, sd)
# 
# #Rename columns
# df_Cu63_2014 <- data.frame(mean, sd$mean_Cu63_umm)
# colnames(df_Cu63_2014) <- c("dist_um", "mean", "sd")


####Put all bases together 2014 baseline ####
df_2014_core_edge <- do.call(rbind, 
                        list(df_Ba138_2014, 
                             df_Mg24_2014, 
                             df_Mn55_2014, 
                             df_Zn66_2014, 
                             df_Sr88_2014))






#2015-16 baseline <4####

# list_2015 <- (df_trac_elem[c(10, 87)])  ###Change the number 10 to other list 10 is here
# df_2015 <- do.call(rbind, list_2015)
# #df_2015 <- droplevels(df_2015[which(df_2015$dist_um <= 150),]) ###Only core region 
# 
# 
# #Sr ###
# mean <- aggregate(mean_Sr88_umm ~ dist_um, df_2015, mean)
# sd <- aggregate(mean_Sr88_umm ~ dist_um, df_2015, sd)
# 
# #Rename columns
# df_Sr88_2015 <-data.frame(mean, sd$mean_Sr88_umm)
# colnames(df_Sr88_2015) <- c("dist_um", "mean", "sd")
# df_Sr88_2015$baseline <- 2015 #core analysis 
# df_Sr88_2015$element <- "Sr:Ca" #core analysis 
# 
# 
# rm(mean, sd)
# #Ba 138###
# mean <- aggregate(mean_Ba138_umm ~ dist_um, df_2015, mean)
# sd <- aggregate(mean_Ba138_umm ~ dist_um, df_2015, sd)
# 
# #Rename columns
# df_Ba138_2015 <-data.frame(mean, sd$mean_Ba138_umm)
# colnames(df_Ba138_2015) <- c("dist_um", "mean", "sd")
# df_Ba138_2015$baseline <- 2015 #core analysis 
# df_Ba138_2015$element <- "Ba:Ca" #core analysis 
# 
# rm(mean, sd)
# ##Li 7###
# # mean <- aggregate(mean_Li7_umm ~ dist_um, df_2015, mean)
# # sd <- aggregate(mean_Li7_umm ~ dist_um, df_2015, sd)
# # 
# # #Rename columns
# # df_Li7_2015 <- data.frame(mean, sd$mean_Li7_umm)
# # colnames(df_Li7_2015) <- c("dist_um", "mean", "sd")
# 
# #Mg 24###
# mean <- aggregate(mean_Mg24_umm ~ dist_um, df_2015, mean)
# sd <- aggregate(mean_Mg24_umm ~ dist_um, df_2015, sd)
# 
# #Rename columns
# df_Mg24_2015 <- data.frame(mean, sd$mean_Mg24_umm)
# colnames(df_Mg24_2015) <- c("dist_um", "mean", "sd")
# df_Mg24_2015$baseline <- 2015 #core analysis 
# df_Mg24_2015$element <- "Mg:Ca" #core analysis 
# 
# rm(mean, sd)
# #Zn 66###
# mean <- aggregate(mean_Zn66_umm ~ dist_um, df_2015, mean)
# sd <- aggregate(mean_Zn66_umm ~ dist_um, df_2015, sd)
# 
# #Rename columns
# df_Zn66_2015 <- data.frame(mean, sd$mean_Zn66_umm)
# colnames(df_Zn66_2015) <- c("dist_um", "mean", "sd")
# df_Zn66_2015$baseline <- 2015 #core analysis
# df_Zn66_2015$element <- "Zn:Ca" #core analysis 
# 
# rm(mean, sd)
# #Mn 55###
# mean <- aggregate(mean_Mn55_umm ~ dist_um, df_2015, mean)
# sd <- aggregate(mean_Mn55_umm ~ dist_um, df_2015, sd)
# 
# #Rename columns
# df_Mn55_2015 <- data.frame(mean, sd$mean_Mn55_umm)
# colnames(df_Mn55_2015) <- c("dist_um", "mean", "sd")
# df_Mn55_2015$baseline <- 2015 #core analysis 
# df_Mn55_2015$element <- "Mn:Ca" #core analysis 
# 
# rm(mean, sd)
# #Co59###
# # mean <- aggregate(mean_Co59_umm ~ dist_um, df_2015, mean)
# # sd <- aggregate(mean_Co59_umm ~ dist_um, df_2015, sd)
# # 
# # #Rename columns
# # df_Co59_2015 <- data.frame(mean, sd$mean_Co59_umm)
# # colnames(df_Co59_2015) <- c("dist_um", "mean", "sd")
# # 
# # #Cu63###
# # mean <- aggregate(mean_Cu63_umm ~ dist_um, df_2015, mean)
# # sd <- aggregate(mean_Cu63_umm ~ dist_um, df_2015, sd)
# # 
# # #Rename columns
# # df_Cu63_2015 <- data.frame(mean, sd$mean_Cu63_umm)
# # colnames(df_Cu63_2015) <- c("dist_um", "mean", "sd")
# 
# 
# ####Put all bases together 2015 baseline ####
# df_2015_core_edge <- do.call(rbind, 
#                         list(df_Ba138_2015, 
#                              df_Mg24_2015, 
#                              df_Mn55_2015, 
#                              df_Zn66_2015, 
#                              df_Sr88_2015))
# 
# 
# 

#no baseline >7####
# list_older <- (df_trac_elem[c(8,19,20,21,26,
#                               10,
#                              27,29,32,35,36,
#                              41,44,45,47,48,
#                              49,50,51,52,53,
#                              56,57,58,61,64,
#                              65,66,69,70,71,
#                              72,73,74,75,76,
#                                 78,79,80,81, #remove  77
#                              82,83,84,85,86,
#                              88)])
# 


##Two consider only two baselines Here I included the data that I had for 2012 
list_older <- (df_trac_elem[c(8,19,20,21,26,
                              10,             ###Aqui puse la muestra A9 porque eliminamos la linea base 2015-2016
                              27,29,32,35,36,
                              41,44,45,47,48,
                              49,50,51,52,53,
                              56,57,58,61,64,
                              65,66,69,70,71,
                              72,73,74,75,76,
                              77,78,79,80,81,  #remove i77, I did not understand why???so I put again in the analysis
                              82,83,84,85,86,
                              88,
                              17,18,22,23, #2012
                              31,33,37,
                              39,40,42,43,
                              55,59,60,67
)])




 df_older <- do.call(rbind, list_older)
 
 #df_older <- droplevels(df_older[which(df_older$dist_um <= 150),]) ###Only core region 

 
 
 #Sr ###
 mean <- aggregate(mean_Sr88_umm ~ dist_um, df_older, mean)
 sd <- aggregate(mean_Sr88_umm ~ dist_um, df_older, sd)
 
 #Rename columns
 df_Sr88_older <-data.frame(mean, sd$mean_Sr88_umm)
 colnames(df_Sr88_older) <- c("dist_um", "mean", "sd")
 df_Sr88_older$baseline <- 9999 #core analysis 
 df_Sr88_older$element <- "Sr:Ca" #core analysis 
 
 
 rm(mean, sd)
 #Ba 138###
 mean <- aggregate(mean_Ba138_umm ~ dist_um, df_older, mean)
 sd <- aggregate(mean_Ba138_umm ~ dist_um, df_older, sd)
 
 #Rename columns
 df_Ba138_older <-data.frame(mean, sd$mean_Ba138_umm)
 colnames(df_Ba138_older) <- c("dist_um", "mean", "sd")
 df_Ba138_older$baseline <- 9999 #core analysis 
 df_Ba138_older$element <- "Ba:Ca" #core analysis 
 
 rm(mean, sd)
 ##Li 7###
 # mean <- aggregate(mean_Li7_umm ~ dist_um, df_older, mean)
 # sd <- aggregate(mean_Li7_umm ~ dist_um, df_older, sd)
 # 
 # #Rename columns
 # df_Li7_older <- data.frame(mean, sd$mean_Li7_umm)
 # colnames(df_Li7_older) <- c("dist_um", "mean", "sd")
 
 #Mg 24###
 mean <- aggregate(mean_Mg24_umm ~ dist_um, df_older, mean)
 sd <- aggregate(mean_Mg24_umm ~ dist_um, df_older, sd)
 
 #Rename columns
 df_Mg24_older <- data.frame(mean, sd$mean_Mg24_umm)
 colnames(df_Mg24_older) <- c("dist_um", "mean", "sd")
 df_Mg24_older$baseline <- 9999 #core analysis 
 df_Mg24_older$element <- "Mg:Ca" #core analysis 
 
 rm(mean, sd)
 #Zn 66###
 mean <- aggregate(mean_Zn66_umm ~ dist_um, df_older, mean)
 sd <- aggregate(mean_Zn66_umm ~ dist_um, df_older, sd)
 
 #Rename columns
 df_Zn66_older <- data.frame(mean, sd$mean_Zn66_umm)
 colnames(df_Zn66_older) <- c("dist_um", "mean", "sd")
 df_Zn66_older$baseline <- 9999 #core analysis
 df_Zn66_older$element <- "Zn:Ca" #core analysis 
 
 rm(mean, sd)
 #Mn 55###
 mean <- aggregate(mean_Mn55_umm ~ dist_um, df_older, mean)
 sd <- aggregate(mean_Mn55_umm ~ dist_um, df_older, sd)
 
 #Rename columns
 df_Mn55_older <- data.frame(mean, sd$mean_Mn55_umm)
 colnames(df_Mn55_older) <- c("dist_um", "mean", "sd")
 df_Mn55_older$baseline <- 9999 #core analysis 
 df_Mn55_older$element <- "Mn:Ca" #core analysis 
 
 rm(mean, sd)
 #Co59###
 # mean <- aggregate(mean_Co59_umm ~ dist_um, df_older, mean)
 # sd <- aggregate(mean_Co59_umm ~ dist_um, df_older, sd)
 # 
 # #Rename columns
 # df_Co59_older <- data.frame(mean, sd$mean_Co59_umm)
 # colnames(df_Co59_older) <- c("dist_um", "mean", "sd")
 # 
 # #Cu63###
 # mean <- aggregate(mean_Cu63_umm ~ dist_um, df_older, mean)
 # sd <- aggregate(mean_Cu63_umm ~ dist_um, df_older, sd)
 # 
 # #Rename columns
 # df_Cu63_older <- data.frame(mean, sd$mean_Cu63_umm)
 # colnames(df_Cu63_older) <- c("dist_um", "mean", "sd")
 
 
####Put all bases together older baseline ####
 df_older_core_edge <- do.call(rbind, 
                          list(df_Ba138_older, 
                               df_Mg24_older, 
                               df_Mn55_older, 
                               df_Zn66_older, 
                               df_Sr88_older))
 
 

####Put all the baselines together ####
 df_baselines_core_edge <- do.call(rbind, 
                                   list(#df_2012_core_edge, 
                                        df_2013_core_edge, 
                                        df_2014_core_edge,
                                       # df_2015_core_edge,
                                        df_older_core_edge))
 
 
 save(df_baselines_core_edge,
      file = "df_Trace elements_allsamples_Baselines_core_edge_profiles_Apr2024.RData")
 
 
 

#### 5-GGPLOT for baselines####
load("df_Trace elements_allsamples_Baselines_core_edge_profiles_Oct2023.RData")
 
 df_baselines_core_edge <- droplevels(df_baselines_core_edge[which(df_baselines_core_edge$element=='Ba:Ca'),]) #
 
 df_baselines_core_edge <- droplevels(df_baselines_core_edge [which(df_baselines_core_edge$dist_um < 3700),])
 
 df_baselines_core_edge$baseline <- as.factor(df_baselines_core_edge$baseline) 

xy <- ggplot() + 
   geom_line(data = df_baselines_core_edge,
           aes(x= dist_um,
               y=mean,
               colour= baseline),
           size = 1.5) +
   
   geom_ribbon(data = df_baselines_core_edge , #change!!!!
               aes(x= dist_um,
                   ymax = mean+sd,
                   ymin = mean-sd,
                   fill= baseline),
               alpha= 0.2)+
   theme_bw()+
  ylab("Ba:Ca (mmol mol-1)") + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.1,8)) +   #Ba
  scale_x_continuous(name='Distance (μm)', expand = c(0,0), 
                      limits = c(0,3500), breaks=seq(0,3500,500), 
                      labels = c("0", "500","1000", "1500",  "2000", "2500", "3000", "3500"))+
   theme_bw()+
  scale_colour_discrete(name = "Baseline", labels = c("2012", "2013", "2014", "2015-16", "NA"))+
  scale_fill_discrete(name = "Baseline", labels = c("2012", "2013", "2014", "2015-16", "NA"))+
   theme(plot.title = element_text(hjust = 0.5,size=8),
         axis.title.x = element_text(size=16),
         axis.title.y = element_text(size=16),
         axis.text=element_text(size=16),
         plot.margin=unit(c(0.3, 0.8 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 
 
 x11()
 xy
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
xy <- ggplot() + 
   
# 
  # geom_line(data = df_Ba138_2012,
  #           aes(x= dist_um,
  #               y=mean),
  #           colour = "blue",
  #           size = 1) +
  # 
  # geom_ribbon(data = df_Ba138_2012, #change!!!!
  #             aes(x= dist_um,
  #                 ymax = mean+sd,
  #                 ymin = mean-sd),
  #             fill= "blue",
  #             alpha= 0.2)+
# 
  geom_line(data = df_Ba138_2013,
            aes(x= dist_um,
                y=mean),
            colour = "green",
            size = 1) +

  geom_ribbon(data = df_Ba138_2013, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= 'green',
              alpha= 0.2)+
  #
   geom_line(data = df_Ba138_2014,
             aes(x= dist_um,
                 y=mean),
             colour = "darkorange",
             size = 1) +

   geom_ribbon(data = df_Ba138_2014, #change!!!!
               aes(x= dist_um,
                   ymax = mean+sd,
                   ymin = mean-sd),
               fill= "darkorange",
               alpha= 0.2)+
  #

   geom_line(data = df_Ba138_2015,
             aes(x= dist_um,
                 y=mean),
             colour = "blue",
             size = 1) +
   #
   geom_ribbon(data = df_Ba138_2015, #change!!!!
               aes(x= dist_um,
                   ymax = mean+sd,
                   ymin = mean-sd),
               fill= "blue",
               alpha= 0.1)+

   # geom_line(data = df_Ba138_older,
   #           aes(x= dist_um,
   #               y=mean),
   #           colour = "darkred",
   #           size = 1) +
   # 
   # geom_ribbon(data = df_Ba138_older, #change!!!!
   #             aes(x= dist_um,
   #                 ymax = mean+sd,
   #                 ymin = mean-sd),
   #             fill= "darkred",
   #             alpha= 0.1)+

 ylab("Ba:Ca (mmol mol-1)") + 
   
   scale_y_continuous(expand = c(0,0), limits = c(-0.1,8)) +   #Ba

   scale_x_continuous(name='Distance (μm)', expand = c(0,0), limits = c(0,3500), breaks=seq(0,3500,500), labels = c("0", "500","1000", "1500",  "2000", "2500", "3000", "3500"))+
   theme_bw()+
  scale_fill_discrete(name = "Baseline", labels = c("2012", "2013", "2014", "2015-16", "NA"))+
   theme(plot.title = element_text(hjust = 0.5,size=8),
         axis.title.x = element_text(size=16),
         axis.title.y = element_text(size=16),
         axis.text=element_text(size=16),
         plot.margin=unit(c(0.3, 0.8 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 
 
x11()
xy













### Save Layout and file: ####
tiff(filename="Profile core to edge Ba_baselines.tiff",
     width=20,height=16,units="cm",
     bg="white",
     res=600,
     compression = c("lzw"))
print(xy)
dev.off()




#### 7-Core analysis per element ####
#We need to extract only the first 150 um of each otolith and make a plot
rm(mean, sd)

core_df <- droplevels(df_trace_elem[which(df_trace_elem$dist_um <= 150),])

## df_Sr88 mean by dist_um####
mean <- aggregate(mean_Sr88_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Sr88_umm ~ dist_um, df_trace_elem , sd)


#Rename columns
df_Sr88 <- data.frame(mean, sd$mean_Sr88_umm)
colnames(df_Sr88) <- c("dist_um", "mean", "sd") #
df_Sr88$baseline <- "all_data" #core analysis 
df_Sr88$element <- "Sr:Ca" #core analysis 

rm(mean, sd)



## df_Li7####
mean <- aggregate(mean_Li7_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Li7_umm ~ dist_um, df_trace_elem , sd)


#Rename columns
df_Li7 <- data.frame(mean, sd$mean_Li7_umm)
colnames(df_Li7) <- c("dist_um", "mean", "sd")
df_Li7$baseline <- "all_data" #core analysis 
df_Li7$element <- "Li:Ca" #core analysis 


rm(mean, sd)
## Mg 24####
mean <- aggregate(mean_Mg24_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Mg24_umm ~ dist_um, df_trace_elem , sd)

#Rename columns
df_Mg24<- data.frame(mean, sd$mean_Mg24_umm)
colnames(df_Mg24) <- c("dist_um", "mean", "sd")
df_Mg24$baseline <- "all_data" #core analysis 
df_Mg24$element <- "Mg:Ca" #core analysis 

rm(mean, sd)

## Mn 55####
mean <- aggregate(mean_Mn55_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Mn55_umm ~ dist_um, df_trace_elem , sd)

#Rename columns
df_Mn55<- data.frame(mean, sd$mean_Mn55_umm)
colnames(df_Mn55) <- c("dist_um", "mean", "sd")
df_Mn55$baseline <- "all_data" #core analysis 
df_Mn55$element <- "Mn:Ca" #core analysis 

rm(mean, sd)

## Zn 66####
mean <- aggregate(mean_Zn66_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Zn66_umm ~ dist_um, df_trace_elem , sd)

#Rename columns
df_Zn66 <- data.frame(mean, sd$mean_Zn66_umm)
colnames(df_Zn66) <- c("dist_um", "mean", "sd")
df_Zn66$baseline <- "all_data" #core analysis 
df_Zn66$element <- "Zn:Ca" #core analysis 

rm(mean, sd)
## Ba 138####
mean <- aggregate(mean_Ba138_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Ba138_umm ~ dist_um, df_trace_elem , sd)

#Rename columns
df_Ba138 <- data.frame(mean, sd$mean_Ba138_umm)
colnames(df_Ba138) <- c("dist_um", "mean", "sd")
df_Ba138$baseline <- "all_data" #core analysis 
df_Ba138$element <- "Ba:Ca" #core analysis 

## Co 59 ####
mean <- aggregate(mean_Co59_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Co59_umm ~ dist_um, df_trace_elem , sd)

#Rename columns
df_Co59 <- data.frame(mean, sd$mean_Co59_umm)
colnames(df_Co59) <- c("dist_um", "mean", "sd")

## Cu 63 ####
mean <- aggregate(mean_Cu63_umm ~ dist_um, df_trace_elem , mean)
sd <- aggregate(mean_Cu63_umm ~ dist_um, df_trace_elem , sd)

#Rename columns
df_Cu63 <- data.frame(mean, sd$mean_Cu63_umm)
colnames(df_Cu63) <- c("dist_um", "mean", "sd")

###Put all bases together core edge ####
df_allsamples_core <- do.call(rbind, 
                                   list(df_Ba138, 
                                        df_Mg24, 
                                        df_Mn55, 
                                        df_Zn66, 
                                        df_Sr88))









#### 8-GPLOT CORE Analysis 150 um: Dist vs element:Ca#####
####Sr 88####
xy <- ggplot() + 
  
  geom_line(data = df_core_Sr88,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_core_Sr88, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Sr:Ca (??mol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(1500,3000)) +
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(name='Distance (??m)', expand = c(0,0), 
                     limits = c(0,175), breaks=seq(0,175,50), 
                     labels = c("0", "50","100", "150"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text=element_text(size=14),
        plot.margin=unit(c(0.2, 0.6 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 


x11()
xy


###Li 7####

xy <- ggplot() + 
  
  geom_line(data = df_core_Li7,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_core_Li7, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Li:Ca (mmol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(-60,60)) +
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(name='Distance (??m)', expand = c(0,0), 
                     limits = c(0,200), breaks=seq(0,200,50), 
                     labels = c("0", "50","100", "150", "200"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text=element_text(size=12),
        plot.margin=unit(c(0.2, 0.6 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 


x11()
xy


###Mg 24####


xy <- ggplot() + 
  
  geom_line(data = df_core_Mg24,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_core_Mg24, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Mg:Ca (??mol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(0,1000)) +
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(name='Distance (??m)', expand = c(0,0), 
                     limits = c(0,300), breaks=seq(0,300,50), 
                     labels = c("0", "50","100", "150", "200", "250", "300"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text=element_text(size=12),
        plot.margin=unit(c(0.2, 0.6 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 


x11()
xy

###Mn 55####

xy <- ggplot() + 
  
  geom_line(data = df_core_Mn55,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_core_Mn55, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Mn:Ca (??mol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(-5,18)) +
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(name='Distance (??m)', expand = c(0,0), 
                     limits = c(0,300), breaks=seq(0,300,50), 
                     labels = c("0", "50","100", "150", "200", "250", "300"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text=element_text(size=12),
        plot.margin=unit(c(0.2, 0.6 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 


x11()
xy



###Zn 66####


xy <- ggplot() + 
  
  geom_line(data = df_core_Zn66,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_core_Zn66, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Zn:Ca (??mol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(-2,25)) +
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(name='Distance (??m)', expand = c(0,0), 
                     limits = c(0,300), breaks=seq(0,300,50), 
                     labels = c("0", "50","100", "150", "200", "250", "300"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text=element_text(size=12),
        plot.margin=unit(c(0.2, 0.6 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 


x11()
xy



###Ba 138####



xy <- ggplot() + 
  
  geom_line(data = df_core_Ba138,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_core_Ba138, #
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Ba:Ca (??mol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(0,4)) +
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(name='Distance (??m)', expand = c(0,0), 
                     limits = c(0,300), breaks=seq(0,300,50), 
                     labels = c("0", "50","100", "150", "200", "250", "300"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text=element_text(size=12),
        plot.margin=unit(c(0.2, 0.6 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 


x11()
xy

###Co 59####

df_Co59 <- droplevels(df_Co59[which(df_Co59$mean < 0.009),])


xy <- ggplot() + 
  
  geom_line(data = df_core_Co59,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_core_Co59, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Co:Ca (mmol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(-0.001,0.01)) +
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(name='Distance (??m)', expand = c(0,0), limits = c(0,4000), breaks=seq(0,4000,500), labels = c("0", "500","1000", "1500",  "2000", "2500", "3000", "3500", "4000"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text=element_text(size=12),
        plot.margin=unit(c(0.2, 0.6 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 


x11()
xy


###Cu 63####

df_Cu63 <- droplevels(df_Cu63[which(df_Cu63$mean < 0.02),])


xy <- ggplot() + 
  
  geom_line(data = df_core_Cu63,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_core_Cu63, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Cu:Ca (mmol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(-0.009,0.02)) +
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(name='Distance (??m)', expand = c(0,0), limits = c(0,4000), breaks=seq(0,4000,500), labels = c("0", "500","1000", "1500",  "2000", "2500", "3000", "3500", "4000"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text=element_text(size=12),
        plot.margin=unit(c(0.2, 0.6 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 


x11()
xy













#### 9-Core analysis per region (Central and southern GoM) and element, Subset specific samples ####
#We need to extract only the first 150 um of each otolith and make a plot

list_southern <- (df_trac_elem[c(4:53,72:88)])
df_southern <- do.call(rbind, list_southern)
df_southern <- droplevels(df_southern[which(df_southern$dist_um <= 150),])


list_central <- (df_trac_elem[c(54:71)]) #Otoliths collected in Feb-March 2019 
df_central <- do.call(rbind, list_central)
df_central <- droplevels(df_central[which(df_central$dist_um <= 150),])


## df_Sr88####
#southern
mean <- aggregate(mean_Sr88_umm ~ dist_um, df_southern , mean)
sd <- aggregate(mean_Sr88_umm ~ dist_um, df_southern , sd)


mean <- aggregate(mean_Sr88_umm ~ ID, df_southern , mean)
sd <- aggregate(mean_Sr88_umm ~ ID, df_southern , sd)


#Rename columns
df_southern_Sr88 <- data.frame(mean, sd$mean_Sr88_umm)
colnames(df_southern_Sr88) <- c("ID", "mean", "sd")
df_southern_Sr88$baseline <- "Southern_GoM" #core analysis 
df_southern_Sr88$element <- "Sr:Ca" #core analysis 

rm(mean, sd)



##Central
mean <- aggregate(mean_Sr88_umm ~ dist_um, df_central , mean)
sd <- aggregate(mean_Sr88_umm ~ dist_um, df_central , sd)

mean <- aggregate(mean_Sr88_umm ~ ID, df_central , mean)
sd <- aggregate(mean_Sr88_umm ~ ID, df_central, sd)


#Rename columns
df_central_Sr88 <- data.frame(mean, sd$mean_Sr88_umm)
colnames(df_central_Sr88) <- c("ID", "mean", "sd")
df_central_Sr88$baseline <- "Central_GoM" #core analysis 
df_central_Sr88$element <- "Sr:Ca" #core analysis 

rm(mean, sd)


## df_Mg 24####
mean <- aggregate(mean_Mg24_umm ~ dist_um, df_southern , mean)
sd <- aggregate(mean_Mg24_umm ~ dist_um, df_southern , sd)

mean <- aggregate(mean_Mg24_umm ~ ID, df_southern , mean)
sd <- aggregate(mean_Mg24_umm ~ ID, df_southern , sd)


#Rename columns
df_southern_Mg24 <- data.frame(mean, sd$mean_Mg24_umm)
colnames(df_southern_Mg24) <- c("ID", "mean", "sd")
df_southern_Mg24$baseline <- "Southern_GoM" #core analysis 
df_southern_Mg24$element <- "Mg:Ca" #core analysis 

rm(mean, sd)

##Central
mean <- aggregate(mean_Mg24_umm ~ dist_um, df_central , mean)
sd <- aggregate(mean_Mg24_umm ~ dist_um, df_central , sd)

mean <- aggregate(mean_Mg24_umm ~ ID, df_central , mean)
sd <- aggregate(mean_Mg24_umm ~ ID, df_central , sd)


#Rename columns
df_central_Mg24 <- data.frame(mean, sd$mean_Mg24_umm)
colnames(df_central_Mg24) <- c("ID", "mean", "sd")
df_central_Mg24$baseline <- "Central_GoM" #core analysis 
df_central_Mg24$element <- "Mg:Ca" #core analysis 

rm(mean, sd)




## df_Mn55####
#G59 have outliers???
mean <- aggregate(mean_Mn55_umm ~ dist_um, df_southern , mean)
sd <- aggregate(mean_Mn55_umm ~ dist_um, df_southern , sd)

mean <- aggregate(mean_Mn55_umm ~ ID, df_southern , mean)
sd <- aggregate(mean_Mn55_umm ~ ID, df_southern , sd)



#Rename columns
df_southern_Mn55 <- data.frame(mean, sd$mean_Mn55_umm)
colnames(df_southern_Mn55) <- c("ID", "mean", "sd")
df_southern_Mn55$baseline <- "Southern_GoM" #core analysis 
df_southern_Mn55$element <- "Mn:Ca" #core analysis 

rm(mean, sd)



##Central
mean <- aggregate(mean_Mn55_umm ~ dist_um, df_central , mean)
sd <- aggregate(mean_Mn55_umm ~ dist_um, df_central , sd)

mean <- aggregate(mean_Mn55_umm ~ ID, df_central , mean)
sd <- aggregate(mean_Mn55_umm ~ ID, df_central , sd)


#Rename columns
df_central_Mn55 <- data.frame(mean, sd$mean_Mn55_umm)
colnames(df_central_Mn55) <- c("ID", "mean", "sd")
df_central_Mn55$baseline <- "Central_GoM" #core analysis 
df_central_Mn55$element <- "Mn:Ca" #core analysis 


## df_Zn66####

mean <- aggregate(mean_Zn66_umm ~ dist_um, df_southern , mean)
sd <- aggregate(mean_Zn66_umm ~ dist_um, df_southern , sd)

mean <- aggregate(mean_Zn66_umm ~ ID, df_southern , mean)
sd <- aggregate(mean_Zn66_umm ~ ID, df_southern , sd)


#Rename columns
df_southern_Zn66 <- data.frame(mean, sd$mean_Zn66_umm)
colnames(df_southern_Zn66) <- c("ID", "mean", "sd")
df_southern_Zn66$baseline <- "Southern_GoM" #core analysis 
df_southern_Zn66$element <- "Zn:Ca" #core analysis 

rm(mean, sd)




##Central
## df_Ba138####
mean <- aggregate(mean_Ba138_umm ~ dist_um, df_southern , mean)
sd <- aggregate(mean_Ba138_umm ~ dist_um, df_southern , sd)

mean <- aggregate(mean_Ba138_umm ~ ID, df_southern , mean)
sd <- aggregate(mean_Ba138_umm ~ ID, df_southern , sd)


#Rename columns
df_southern_Ba138 <- data.frame(mean, sd$mean_Ba138_umm)
colnames(df_southern_Ba138) <- c("ID", "mean", "sd")
df_southern_Ba138$baseline <- "Southern_GoM" #core analysis 
df_southern_Ba138$element <- "Ba:Ca" #core analysis 

rm(mean, sd)


##Central
mean <- aggregate(mean_Ba138_umm ~ dist_um, df_central , mean)
sd <- aggregate(mean_Ba138_umm ~ dist_um, df_central , sd)

mean <- aggregate(mean_Ba138_umm ~ ID, df_central , mean)
sd <- aggregate(mean_Ba138_umm ~ ID, df_central , sd)


#Rename columns
df_central_Ba138 <- data.frame(mean, sd$mean_Ba138_umm)
colnames(df_central_Ba138) <- c("ID", "mean", "sd")
df_central_Ba138$baseline <- "Central_GoM" #core analysis 
df_central_Ba138$element <- "Ba:Ca" #core analysis 

###Put df together####

df_core_region <- do.call(rbind, 
                        list(df_central_Ba138, 
                             df_central_Zn66,
                             df_central_Mn55,
                             df_central_Mg24,
                             df_central_Sr88,
                             df_southern_Ba138,
                             df_southern_Zn66, 
                             df_southern_Mn55,
                             df_southern_Mg24,
                             df_southern_Sr88))




save(df_core_region,
     file = "df_core_region_southern_and_central_GoM.RData")


xy <- ggplot() + 

  geom_line(data = df_southern_Ba138,
          aes(x= dist_um,
              y=mean),
          colour = "blue",
          size = 1) +
  
  geom_ribbon(data = df_southern_Ba138, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "blue",
              alpha= 0.2)+
  # 
  geom_line(data = df_central_Ba138,
            aes(x= dist_um,
                y=mean),
            colour = "darkorange",
            size = 1) +
  
  geom_ribbon(data = df_central_Ba138, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= 'darkorange',
              alpha= 0.2)+


ylab("Ba:Ca (??mol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(-1,7.5)) +
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(name='Distance (??m)', expand = c(0,0),
                     limits = c(0,200), breaks=seq(0,200,50), 
                     labels = c("0", "50","100", "150", "200"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text=element_text(size=12),
        plot.margin=unit(c(0.2, 0.6 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 


x11()
xy


### Save Layout and file: ####
tiff(filename="Core signal by region Sr_Ca_zoom 150um.tiff",
     width=20,height=16,units="cm",
     bg="white",
     res=600,
     compression = c("lzw"))
print(xy)
dev.off()



####Box plot ####
par(mfrow= c (2,4), cex= 1)

boxplot(df_trace_elem$mean_Li7_umm,  ylab = "Li:Ca (mmol mol-1)")
boxplot(df_trace_elem$mean_Mg24_umm,  ylab = "Mg:Ca (mmol mol-1)")
boxplot(df_trace_elem$mean_Mn55_umm,  ylab = "Mn:Ca (mmol mol-1)")
boxplot(df_trace_elem$mean_Co59_umm,  ylab = "Co:Ca (mmol mol-1)")
boxplot(df_trace_elem$mean_Cu63_umm,  ylab = "Cu:Ca (mmol mol-1)")
boxplot(df_trace_elem$mean_Zn66_umm,  ylab = "Zn:Ca (mmol mol-1)")
boxplot(df_trace_elem$mean_Sr88_umm,  ylab = "Sr:Ca (mmol mol-1)")
boxplot(df_trace_elem$mean_Ba138_umm,  ylab = "Ba:Ca (mmol mol-1)")


#### Change df , p.e. 4 correspond to A1   ####
i=40
col_names <- colnames(df_trac_elem[[i]])
classes <- sapply(df_trac_elem[[i]],class)

for(h in 36:ncol(df_trac_elem[[i]])) ####Change i
{
  dev.new()
  hist(df_trac_elem[[i]][,h],100,
     main = paste0("Histogram of ", col_names[h]), xlab= col_names[h]) # subset with [] not $
}




### X Y plots ####

for(h in 36:ncol(df_trac_elem[[i]])) ####Change i
{

dev.new()

plot(df_trac_elem[[i]]$dist_um, df_trac_elem[[i]][,h], ylab= col_names[h])

}

i= 4
h= 46

raw <- ggplot() + 
  
  geom_line(data = df_trac_elem[[i]],
            aes(x= dist_um,
                y=df_trac_elem[[i]][,h]),
            colour = "gray27",
            size = 1)+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text=element_text(size=12),
        plot.margin=unit(c(0.2, 0.6 , 0.5, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 




tiff(filename="rawBa138.tiff",
     width=20,height=16,units="cm",
     bg="white",
     res=600,
     compression = c("lzw"))
print(raw)
dev.off()






