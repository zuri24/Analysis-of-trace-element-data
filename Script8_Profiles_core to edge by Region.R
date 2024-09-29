### TRace elements results####:
# Clean memory:
rm(list = ls('all.names' = TRUE)) # Remove all objects
graphics.off() # Remove all graphics
gc() # Clear cache
#This df was created in script 6

#### 5-GGPLOT by Regions of capture####

load("df_coretoedge_southern_and_central_GoM_Profiles by region.RData")

#Ba
df_edge_core_region <- droplevels(df_edge_core_region[which(df_edge_core_region$element=='Ba:Ca'),]) #
df_edge_core_region$baseline <- as.factor(df_edge_core_region$baseline) 



xy <- ggplot() + 
  geom_line(data = df_edge_core_region,
            aes(x= dist_um,
                y=mean,
                colour= baseline),
            linewidth = 1) +
  geom_ribbon(data = df_edge_core_region , #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd,
                  fill= baseline),
              alpha= 0.2)+
  theme_bw()+
  ylab("Ba:Ca (mmol mol-1)") + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.4,8)) +   #Ba
  scale_x_continuous(name='Distance (μm)', expand = c(0,0), 
                     limits = c(0,3700), breaks=seq(0,3700,500), 
                     labels = c("0", "500","1000", "1500",  "2000", "2500", "3000", "3500"))+
  theme_bw()+
  scale_colour_discrete(name = "Region of capture", labels = c("Central GoM", "Southern GoM"))+
  scale_fill_discrete(name = "Region of capture", labels = c("Central GoM", "Southern GoM"))+
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text=element_text(size=14),
        plot.margin=unit(c(0.3, 0.8 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 

x11()
xy



#Sr
df_edge_core_region <- droplevels(df_edge_core_region[which(df_edge_core_region$element=='Sr:Ca'),]) #

df_edge_core_region$baseline <- as.factor(df_edge_core_region$baseline) 

xy <- ggplot() + 
  geom_line(data = df_edge_core_region,
            aes(x= dist_um,
                y=mean,
                colour= baseline),
            size = 1) +
  
  geom_ribbon(data = df_edge_core_region , #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd,
                  fill= baseline),
              alpha= 0.2)+
  theme_bw()+
  ylab("Sr:Ca (mmol mol-1)") + 
  scale_y_continuous(expand = c(0,0), limits = c(1500,4100)) +   #Ba
  scale_x_continuous(name='Distance (μm)', expand = c(0,0), 
                     limits = c(0,3700), breaks=seq(0,3700,500), 
                     labels = c("0", "500","1000", "1500",  "2000", "2500", "3000", "3500"))+
  theme_bw()+
  scale_colour_discrete(name = "Region of capture", labels = c("Central GoM", "Southern GoM"))+
  scale_fill_discrete(name = "Region of capture", labels = c("Central GoM", "Southern GoM"))+
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text=element_text(size=14),
        plot.margin=unit(c(0.3, 0.8 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 

x11()
xy

#Mg
df_edge_core_region <- droplevels(df_edge_core_region[which(df_edge_core_region$element=='Mg:Ca'),]) #


df_edge_core_region$baseline <- as.factor(df_edge_core_region$baseline) 

xy <- ggplot() + 
  geom_line(data = df_edge_core_region,
            aes(x= dist_um,
                y=mean,
                colour= baseline),
            size = 1) +
  
  geom_ribbon(data = df_edge_core_region , #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd,
                  fill= baseline),
              alpha= 0.2)+
  theme_bw()+
  ylab("Mg:Ca (mmol mol-1)") + 
  scale_y_continuous(expand = c(0,0), limits = c(-150,900)) +   #Ba
  scale_x_continuous(name='Distance (μm)', expand = c(0,0), 
                     limits = c(0,3700), breaks=seq(0,3700,500), 
                     labels = c("0", "500","1000", "1500",  "2000", "2500", "3000", "3500"))+
  
  theme_bw()+
  scale_colour_discrete(name = "Region of capture", labels = c("Central GoM", "Southern GoM"))+
  scale_fill_discrete(name = "Region of capture", labels = c("Central GoM", "Southern GoM"))+

  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text=element_text(size=14),
        plot.margin=unit(c(0.3, 0.8 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 

x11()
xy


#Mn

df_edge_core_region <- droplevels(df_edge_core_region[which(df_edge_core_region$element=='Mn:Ca'),]) #

df_edge_core_region$baseline <- as.factor(df_edge_core_region$baseline) 

xy <- ggplot() + 
  geom_line(data = df_edge_core_region,
            aes(x= dist_um,
                y=mean,
                colour= baseline),
            size = 1) +
  
  geom_ribbon(data = df_edge_core_region , #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd,
                  fill= baseline),
              alpha= 0.2)+
  theme_bw()+
  ylab("Mn:Ca (mmol mol-1)") + 
  scale_y_continuous(expand = c(0,0), limits = c(-5,20)) +   #Ba
  scale_x_continuous(name='Distance (μm)', expand = c(0,0), 
                     limits = c(0,3700), breaks=seq(0,3700,500), 
                     labels = c("0", "500","1000", "1500",  "2000", "2500", "3000", "3500"))+
  theme_bw()+
  # scale_colour_discrete(name = "", labels = c("2012", "2013", "2014", "Non-B"))+
  # scale_fill_discrete(name = "", labels = c("2012", "2013", "2014", "Non-B"))+
  # scale_colour_manual(name = "", labels = c("2012", "2013", "2014", "Non-B"), values=c( "gold2","#69b3a2","tomato3", "gray59"))+
  #scale_fill_manual(name = "", labels = c("2012", "2013", "2014",  "Non-B"), values=c( "gold2","#69b3a2","tomato3", "gray59"))+
  scale_colour_discrete(name = "Region of capture", labels = c("Central GoM", "Southern GoM"))+
  scale_fill_discrete(name = "Region of capture", labels = c("Central GoM", "Southern GoM"))+
  
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text=element_text(size=14),
        plot.margin=unit(c(0.3, 0.8 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 

x11()
xy

### Save Layout and file: ####
tiff(filename="Profile core to edge_by region_Mn.tiff",
     width=26,height=14,units="cm",
     bg="white",
     res=600,
     compression = c("lzw"))
print(xy)
dev.off()



