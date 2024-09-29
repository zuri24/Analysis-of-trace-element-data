### Trace elements results####:
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
the_theme<-theme(title=element_text(size=16),axis.title=element_text(size=16),axis.text=element_text(size=16))


# Load data ####

#YFT01 <- read.csv("YFT01.csv", header = FALSE, sep = ',')
# # specifying the path name
# path <- "C:/Users/Lab/Desktop/Trace elements/Ramirez-Herzka YFT LA-ICP-MS results.xlsx"
# 
# # getting data from sheets
# sheets <- openxlsx::getSheetNames(path)
# df_trac_elem <- lapply(sheets, openxlsx::read.xlsx, xlsxFile=path)
# 
# # assigning names to data frame
# names(df_trac_elem) <- sheets
# 
# save(df_trac_elem,
#       file = "Trace elements results.RData")
# 

load("Trace elements database_raw.RData") #the same database that Miller sent us but in RData format

# printing the data
#print (df_trac_elem)

for (i in 4:length(df_trac_elem)) {
  
#i=4


#head df 
names(df_trac_elem[[i]]) <- as.matrix(df_trac_elem[[i]][1, ])  
df_trac_elem[[i]] <- df_trac_elem[[i]][c(-1), ]
df_trac_elem[[i]] <- df_trac_elem[[i]][, c(-1)]



df_trac_elem[[i]][,] <- lapply(df_trac_elem[[i]], function(x) type.convert(as.numeric(x), as.is = TRUE)) #convert data to numeric values

df_trac_elem[[i]]$dist_um <- NA # add new column
df_trac_elem[[i]]$dist_um[1] <- 0 # first row with zero value


#Distance in micras - core to edge

# for (j in 2:length(df_trac_elem[[i]]$`Elapsed Time`)) {

for (j in seq(2, length(df_trac_elem[[i]]$`Elapsed Time`))) {
  
  #j=2
  
  df_trac_elem[[i]]$dist_um[j] <- (df_trac_elem[[i]]$dist_um[j-1]) + (0.4032*5) 

  
}


ID <- names(df_trac_elem)
df_trac_elem[[i]]$ID <- NA # add new column
df_trac_elem[[i]]$ID <- rep(ID[i],nrow(df_trac_elem[[i]]))



#New raws
df_trac_elem[[i]] <- insertRows(df_trac_elem[[i]],r=c(1,2,3), new= NA)
df_trac_elem[[i]] [nrow(df_trac_elem[[i]]) + 3,] <- list(NA)


####Conversion to ppb ####
df_trac_elem[[i]]$Li7_ppm <- df_trac_elem[[i]]$Li7_ppm*1000
df_trac_elem[[i]]$Mg24_ppm <- df_trac_elem[[i]]$Mg24_ppm*1000
df_trac_elem[[i]]$Mg25_ppm <- df_trac_elem[[i]]$Mg25_ppm*1000
df_trac_elem[[i]]$Ca44_ppm <- df_trac_elem[[i]]$Ca44_ppm*1000
df_trac_elem[[i]]$Mn55_ppm <- df_trac_elem[[i]]$Mn55_ppm*1000
df_trac_elem[[i]]$Co59_ppm <- df_trac_elem[[i]]$Co59_ppm*1000
df_trac_elem[[i]]$Cu63_ppm <- df_trac_elem[[i]]$Cu63_ppm*1000
df_trac_elem[[i]]$Zn66_ppm <- df_trac_elem[[i]]$Zn66_ppm*1000
df_trac_elem[[i]]$Sr88_ppm <- df_trac_elem[[i]]$Sr88_ppm*1000
df_trac_elem[[i]]$Ba137_ppm <- df_trac_elem[[i]]$Ba137_ppm*1000
df_trac_elem[[i]]$Ba138_ppm <- df_trac_elem[[i]]$Ba138_ppm*1000

#Conversion trace element concentrations E pp to element:Ca ratios (umol mol-1)
#element:Ca =  Element_ppm/1000 (Molar mass 0.38/44)-1

#Molar mass

Li <- (6.941*0.38/44)^-1
Mg <- (24.305*0.38/44)^-1
Ca <- (40.078*0.38/44)^-1
Mn <- (54.938*0.38/44)^-1
Co <- (58.933*0.38/44)^-1
Cu <- (63.546*0.38/44)^-1
Zn <- (65.38*0.38/44)^-1
Sr <- (87.62*0.38/44)^-1
Ba <- (137.33*0.38/44)^-1


###First part of the equation

df_trac_elem[[i]]$Li7_umm <- (df_trac_elem[[i]]$Li7_ppm/1000)*Li
df_trac_elem[[i]]$Mg24_umm <- (df_trac_elem[[i]]$Mg24_ppm/1000)*Mg
df_trac_elem[[i]]$Mg25_umm <- (df_trac_elem[[i]]$Mg25_ppm/1000)*Mg
df_trac_elem[[i]]$Ca44_umm <- (df_trac_elem[[i]]$Ca44_ppm/1000)*Ca
df_trac_elem[[i]]$Mn55_umm <- (df_trac_elem[[i]]$Mn55_ppm/1000)*Mn
df_trac_elem[[i]]$Co59_umm <- (df_trac_elem[[i]]$Co59_ppm/1000)*Co
df_trac_elem[[i]]$Cu63_umm <- (df_trac_elem[[i]]$Cu63_ppm/1000)*Cu
df_trac_elem[[i]]$Zn66_umm <- (df_trac_elem[[i]]$Zn66_ppm/1000)*Zn
df_trac_elem[[i]]$Sr88_umm <- (df_trac_elem[[i]]$Sr88_ppm/1000)*Sr
df_trac_elem[[i]]$Ba137_umm <- (df_trac_elem[[i]]$Ba137_ppm/1000)*Ba
df_trac_elem[[i]]$Ba138_umm <- (df_trac_elem[[i]]$Ba138_ppm/1000)*Ba

####7-point moving average ####

### Movil median
median_vec <- list()

for(k in 15:ncol(df_trac_elem[[i]])) {
  
  #k=14 
       median <- rollapply(df_trac_elem[[i]][ , k], 7, median, na.rm= TRUE)
   
    median_vec [[k]] <- median
  
  
}


med_all <- do.call(cbind,median_vec)

#Data frame
col_nams <- c("Li7_umm",   "Mg24_umm",  "Mg25_umm", "Ca44_umm",  "Mn55_umm",  "Co59_umm",  "Cu63_umm", 
              "Zn66_umm",  "Sr88_umm",  "Ba137_umm", "Ba138_umm")

df_med_movil <- data.frame(
  med_all
  
)

colnames(df_med_movil) <- paste0('med_', col_nams)

df_med_movil <- insertRows(df_med_movil,r=c(1,2,3), new= NA)
df_med_movil [nrow(df_med_movil) + 3,] <- list(NA)


####Movil average #####

mean_vec <- list()

for(l in 1:ncol(df_med_movil)) {
  
  #l=1
    
    mean <- rollapply(df_med_movil[ , l], 7, mean, na.rm= TRUE)
    
    mean_vec [[l]] <- mean
    
  
}

mean_all <- do.call(cbind,mean_vec)

#Data frame

df_mean_movil <- data.frame(
  mean_all
  
)

colnames(df_mean_movil) <- paste0('mean_', col_nams)

df_mean_movil <- insertRows(df_mean_movil,r=c(1,2,3), new= NA)
df_mean_movil [nrow(df_mean_movil) + 3,] <- list(NA)


df_trac_elem[[i]] <- cbind(df_trac_elem[[i]],
              df_med_movil,
              df_mean_movil)

}


#print (head(df_trac_elem[[4]]))

 save(df_trac_elem,
      file = "Trace elements_data.RData") #database with median and mean movil I used this df to perfom analysis and 
 # create figures


####Figures ####

load("Trace elements_data.RData")
 
##Histograms

 
 #### Change df , p.e. 4 correspond to A1 
i=17
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








x11()
plot(YFT01$dist_um, YFT01$mean_Ba138_umm)
x11()
plot(YFT01$dist_um, YFT01$med_Ba138_umm)


###Comparison

#write.csv(YFT01, 'Zuri_results.csv')
zuri <- read.csv("Zuri_results.csv", header = TRUE, sep = ',')
Nate <- read.csv("Nate_results.csv", header = TRUE, sep = ',')
Nate [nrow(Nate ) + 3,] <- list(NA)

identical(YFT01, Nate)


summary(zuri)
summary(Nate)


###NOtes


###First part of the equation
#YFT01$Mg24_umm2 <- (YFT01$Mg24_ppm/1000)*Mg2
#YFT01$Mg24_umm3 <- (YFT01$Mg24_ppm)/(1000*0.2099068)


# h <- list()
# col_names <- colnames(YFT01)
# for (i in 14:ncol(YFT01)) {
#  # h=14
#   
#   histograma <- hist(YFT01[,i],100,
#   main = paste0("Histogram of ", col_names[i]))
#   h[[i]] <- histograma
# }


# 
# for (i in 14:ncol(YFT01)) {
#   #h=18
#   
#   histograma <- ggplot(YFT01, aes(x=YFT01[,i])) + 
#     geom_histogram(color="black", fill="white", bins=100) +
#     theme_classic()+ 
#     the_theme +
#     labs(title= col_names[i],
#          x= col_names[i], y="Frequency")
#   
#   h[[i]] <- histograma
# }
# 
# print(h[[18]])
# 


####7-point moving average ####

# median <- sapply(c("left"),
#                  function(x)zoo::rollmedian(YFT01$Li7_ppm_raw,7,align = x, na= TRUE))

#median <- rollapplyr(YFT01$Li7_ppm_raw, 7, median, na.rm= TRUE, by = 1, partial= TRUE, fill= NA)

#YFT01$Li7_ppm_raw_med <- median


