

#### 6-GPLOT considering all data: Dist vs element:Ca#####
####Sr 88####


xy <- ggplot() + 
  
  geom_line(data = df_Sr88,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_Sr88, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Sr:Ca (mmol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(0,4000)) +
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(name='Distance (??m)', expand = c(0,0), 
                     limits = c(0,4000), breaks=seq(0,4000,500), 
                     labels = c("0", "500","1000", "1500",  "2000", "2500", "3000", "3500", "4000"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=8),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text=element_text(size=12),
        plot.margin=unit(c(0.2, 0.6 , 0.1, 0.4),units="cm")) #(top, right, bottom, and left distance from the device borders))) 


x11()
xy


###Li 7####

df_Li7 <- droplevels(df_Li7 [which(df_Li7 $mean > -0.88),])

xy <- ggplot() + 
  
  geom_line(data = df_Li7,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_Li7, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Li:Ca (mmol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(-0.3,0.12)) +
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


###Mg 24####

df_Mg24 <- droplevels(df_Mg24 [which(df_Mg24 $mean > 0),])

xy <- ggplot() + 
  
  geom_line(data = df_Mg24,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_Mg24, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Mg:Ca (mmol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(0,3)) +
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

###Mn 55####

df_Mn55 <- droplevels(df_Mn55 [which(df_Mn55 $mean > 0),])
df_Mn55 <- droplevels(df_Mn55 [which(df_Mn55 $dist_um < 3500),])


xy <- ggplot() + 
  
  geom_line(data = df_Mn55,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_Mn55, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Mn:Ca (mmol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(0,0.020)) +
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



###Zn 66####

df_Zn66 <- droplevels(df_Zn66 [which(df_Zn66$mean < 0.03),])


xy <- ggplot() + 
  
  geom_line(data = df_Zn66,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_Zn66, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Zn:Ca (mmol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(0,0.03)) +
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



###Ba 138####

df_Ba138 <- droplevels(df_Ba138[which(df_Ba138$mean < 0.009),])


xy <- ggplot() + 
  
  geom_line(data = df_Ba138,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_Ba138, #change!!!!
              aes(x= dist_um,
                  ymax = mean+sd,
                  ymin = mean-sd),
              fill= "gray27",
              alpha= 0.2)+
  
  ylab("Ba:Ca (mmol mol-1)") + 
  
  scale_y_continuous(expand = c(0,0), limits = c(0,0.009)) +
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

###Co 59####

df_Co59 <- droplevels(df_Co59[which(df_Co59$mean < 0.009),])


xy <- ggplot() + 
  
  geom_line(data = df_Co59,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_Co59, #change!!!!
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
  
  geom_line(data = df_Cu63,
            aes(x= dist_um,
                y=mean),
            colour = "gray27",
            size = 1) +
  
  geom_ribbon(data = df_Cu63, #change!!!!
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


