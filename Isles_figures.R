library(tidyr)
library(dplyr)
library(ggplot2)
library(AICcmodavg)
library(gridExtra)
library(grid)
library(lme4)
library(lmtest)
library(expss)

aru=read.csv("ISLES_breeding.csv") 
#add vegetation data 
veg=read.csv("veg_AM.csv")
aru=merge(aru, veg, by= c("Point", "Year"))

#breeding index: load data, select breeding spp index, add column for total of bb per row)
pc=read.csv("PointCounts_breeding.csv") 
veg=read.csv("veg_AM.csv")
pc=merge(pc, veg, by= c("Point", "Year")) %>%
  mutate(Full=rowSums(.[7:31]))  %>%
  mutate(Index=MAWR+SESP+CLRA+WILL+OROR+RWBL+LEBI+GRHE+BCNH+YCNH)
#separate out Whiskey from Raccoon (Indexed)
w=pc %>%
  filter(Island.x=="Whiskey")
r=pc %>%
  filter(Island.x=="Raccoon")

#figure 3
#plot of dominant vegetation cover by habitat type, Whiskey 
(wveg_boxplot0 <- ggplot(aru, aes(LOCATION, plant_av)) + geom_boxplot(aes(fill = LOCATION)) +
    theme_bw() +
    scale_fill_manual(values = c("White", "White")) +               
    scale_colour_manual(values = c("White", "White")) +            
    ylab("% Plant Cover\n") +                             
    xlab("")  +
    theme(axis.text.x = element_text(size = 25, color="black", family="serif"), 
          axis.text.y = element_text(size = 25, color="black", family="serif"),
          axis.title.x = element_text(size = 25, face = "plain", color="black", family="serif"),             
          axis.title.y = element_text(size = 25, face = "plain", color="black", family="serif"),             
          panel.grid.major.x = element_blank(),                           
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               
          legend.position = "none")) 

(wveg_boxplot1 <- ggplot(aru, aes(LOCATION,bare_av)) + geom_boxplot(aes(fill = LOCATION)) +
    theme_bw() +
    scale_fill_manual(values = c("White", "White")) +               
    scale_colour_manual(values = c("White", "White")) +     
    ylab("% Bare Ground\n") +                             
    xlab("")  +
    theme(axis.text.x = element_text(size = 25, color="black", family="serif"), 
          axis.text.y = element_text(size = 25, color="black", family="serif"),
          axis.title.x = element_text(size = 25, face = "plain", color="black", family="serif"),             
          axis.title.y = element_text(size = 25, face = "plain", color="black", family="serif"),             
          panel.grid.major.x = element_blank(),                           
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               
          legend.position = "none")) 

(wveg_boxplot2 <- ggplot(aru, aes(LOCATION, water_av)) + geom_boxplot(aes(fill = LOCATION)) +
    theme_bw() +
    scale_fill_manual(values = c("White", "White")) +               
    scale_colour_manual(values = c("White", "White")) +        
    ylab("% Standing Water\n") +                             
    xlab("")  +
    theme(axis.text.x = element_text(size = 25, color="black", family="serif"), 
          axis.text.y = element_text(size = 25, color="black", family="serif"),
          axis.title.x = element_text(size = 25, face = "plain", color="black", family="serif"),             
          axis.title.y = element_text(size = 25, face = "plain", color="black", family="serif"),             
          panel.grid.major.x = element_blank(),                           
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               
          legend.position = "none")) 

#plot of dominant vegetation cover by habitat type, Raccoon
#note: Raccoon data from PC file 
(rveg_boxplot0 <- ggplot(r, aes(LOCATION, plant_av)) + geom_boxplot(aes(fill = LOCATION)) +
    theme_bw() +
    scale_fill_manual(values = c("White", "White")) +               
    scale_colour_manual(values = c("White", "White")) +           
    ylab("% Plant Cover\n") +                             
    xlab("")  +
    theme(axis.text.x = element_text(size = 25, color="black", family="serif"), 
          axis.text.y = element_text(size = 25, color="black", family="serif"),
          axis.title.x = element_text(size = 25, color="black", family="serif"),             
          axis.title.y = element_text(size = 25, color="black", family="serif"),             
          panel.grid.major.x = element_blank(),                           
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               
          legend.position = "none")) 

(rveg_boxplot1 <- ggplot(r, aes(LOCATION, bare_av)) + geom_boxplot(aes(fill = LOCATION)) +
    theme_bw() +
    scale_fill_manual(values = c("White", "White")) +               
    scale_colour_manual(values = c("White", "White")) +           
    ylab("% Bare Ground\n") +                             
    xlab("")  +
    theme(axis.text.x = element_text(size = 25, color="black", family="serif"), 
          axis.text.y = element_text(size = 25, color="black", family="serif"),
          axis.title.x = element_text(size = 25, color="black", family="serif"),             
          axis.title.y = element_text(size = 25, color="black", family="serif"),             
          panel.grid.major.x = element_blank(),                           
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               
          legend.position = "none"))  

(rveg_boxplot2 <- ggplot(r, aes(LOCATION, water_av)) + geom_boxplot(aes(fill = LOCATION)) +
    theme_bw() +
    scale_fill_manual(values = c("White", "White")) +               
    scale_colour_manual(values = c("White", "White")) +           
    ylab("% Standing Water\n") +                             
    xlab("")  +
    theme(axis.text.x = element_text(size = 25, color="black", family="serif"), 
          axis.text.y = element_text(size = 25, color="black", family="serif"),
          axis.title.x = element_text(size = 25, color="black", family="serif"),             
          axis.title.y = element_text(size = 25, color="black", family="serif"),             
          panel.grid.major.x = element_blank(),                           
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               
          legend.position = "none"))   

#arrange plots in a row
a=grid.arrange(wveg_boxplot0, wveg_boxplot1, wveg_boxplot2 , ncol = 3)
b=grid.arrange(rveg_boxplot0, rveg_boxplot1, rveg_boxplot2, ncol = 3)

#with island header
whiskey=grid.arrange(wveg_boxplot0, wveg_boxplot1, wveg_boxplot2 , ncol = 3)
raccoon=grid.arrange(rveg_boxplot0, rveg_boxplot1, rveg_boxplot2, ncol = 3)
##########################################################################################################################
#figure 3
#plot of dominant vegetation cover by habitat type to show strong correlation, Whiskey 
(wveg_boxplot0 <- ggplot(aru, aes(LOCATION, Grassav)) + geom_boxplot(aes(fill = LOCATION)) +
    theme_bw() +
    scale_fill_manual(values = c("White", "White")) +               
    scale_colour_manual(values = c("White", "White")) +            
    ylab("% Spartina cover\n") +                             
    xlab("")  +
   theme(axis.text.x = element_text(size = 25, color="black", family="serif"), 
         axis.text.y = element_text(size = 25, color="black", family="serif"),
         axis.title.x = element_text(size = 25, color="black", family="serif"),             
         axis.title.y = element_text(size = 25, color="black", family="serif"),             
         panel.grid.major.x = element_blank(),                           
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_blank(),  
         plot.margin = unit(c(1,1,1,1), units = , "cm"),               
         legend.position = "none"))   

(wveg_boxplot1 <- ggplot(aru, aes(LOCATION, Bmav)) + geom_boxplot(aes(fill = LOCATION)) +
    theme_bw() +
    scale_fill_manual(values = c("White", "White")) +               
    scale_colour_manual(values = c("White", "White")) +     
    ylab("% Avicennia Cover\n") +                             
    xlab("")  +
    theme(axis.text.x = element_text(size = 25, color="black", family="serif"), 
          axis.text.y = element_text(size = 25, color="black", family="serif"),
          axis.title.x = element_text(size = 25, color="black", family="serif"),             
          axis.title.y = element_text(size = 25, color="black", family="serif"),             
          panel.grid.major.x = element_blank(),                           
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               
          legend.position = "none"))   

#plot of dominant vegetation cover by habitat type to show strong correlation, Raccoon
#note: Raccoon data from PC file 
(veg_boxplot0 <- ggplot(r, aes(LOCATION, Grassav)) + geom_boxplot(aes(fill = LOCATION)) +
    theme_bw() +
    scale_fill_manual(values = c("White", "White")) +               
    scale_colour_manual(values = c("White", "White")) +           
    ylab("% Spartina cover\n") +                             
    xlab("")  +
    theme(axis.text.x = element_text(size = 25, color="black", family="serif"), 
          axis.text.y = element_text(size = 25, color="black", family="serif"),
          axis.title.x = element_text(size = 25, color="black", family="serif"),             
          axis.title.y = element_text(size = 25, color="black", family="serif"),             
          panel.grid.major.x = element_blank(),                           
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               
          legend.position = "none"))   

(veg_boxplot1 <- ggplot(r, aes(LOCATION, Bmav)) + geom_boxplot(aes(fill = LOCATION)) +
    theme_bw() +
    scale_fill_manual(values = c("White", "White")) +               
    scale_colour_manual(values = c("White", "White")) +           
    ylab("% Avicennia Cover\n") +                             
    xlab("")  +
    theme(axis.text.x = element_text(size = 25, color="black", family="serif"), 
          axis.text.y = element_text(size = 25, color="black", family="serif"),
          axis.title.x = element_text(size = 25, color="black", family="serif"),             
          axis.title.y = element_text(size = 25, color="black", family="serif"),             
          panel.grid.major.x = element_blank(),                           
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               
          legend.position = "none"))   

#arrange plots in a row
a=grid.arrange(wveg_boxplot0, wveg_boxplot1, ncol = 2)
b=grid.arrange(veg_boxplot0, veg_boxplot1, ncol = 2)

#without heading
a=grid.arrange(wveg_boxplot0, wveg_boxplot1, ncol = 2)
b=grid.arrange(veg_boxplot0, veg_boxplot1, ncol = 2)

##########################################################################################################################
#figure: presentation 
#species richness by location, island
##########################################################################################################################
#POINT COUNTS
#whiskey
(w <- ggplot(w, aes(LOCATION, Average)) + geom_boxplot(aes(fill = LOCATION)) +
   theme_bw() +
   scale_fill_manual(values = c("White", "White")) +               
   scale_colour_manual(values = c("White", "White")) +            
   ylab("Mean Species Presence\n") +                             
   xlab("")  +
   theme(axis.text.x = element_text(size = 25, color="black", family="serif"), 
         axis.text.y = element_text(size = 25, color="black", family="serif"),
         axis.title.x = element_text(size = 25, color="black", family="serif"),             
         axis.title.y = element_text(size = 25, color="black", family="serif"),             
         panel.grid.major.x = element_blank(),                           
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_blank(),  
         plot.margin = unit(c(1,1,1,1), units = , "cm"),               
         legend.position = "none"))   
#raccoon
(r <- ggplot(r, aes(LOCATION, Average)) + geom_boxplot(aes(fill = LOCATION)) +
   theme_bw() +
   scale_fill_manual(values = c("White", "White")) +               
   scale_colour_manual(values = c("White", "White")) +            
   ylab("") +                             
   xlab("")  +
   theme(axis.text.x = element_text(size = 25, color="black", family="serif"), 
         axis.text.y = element_text(size = 25, color="black", family="serif"),
         axis.title.x = element_text(size = 25, color="black", family="serif"),             
         axis.title.y = element_text(size = 25, color="black", family="serif"),             
         panel.grid.major.x = element_blank(),                           
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_blank(),  
         plot.margin = unit(c(1,1,1,1), units = , "cm"),               
         legend.position = "none"))   
#arrange plots in a row
a=grid.arrange(w, r, ncol = 2)

#ARU: Whiskey
(aru <- ggplot(aru, aes(LOCATION, Average)) + geom_boxplot(aes(fill = LOCATION)) +
    theme_bw() +
    scale_fill_manual(values = c("White", "White")) +               
    scale_colour_manual(values = c("White", "White")) +            
    ylab("Mean Species Presence\n") +                             
    xlab("")  +
    theme(axis.text.x = element_text(size = 25, color="black", family="serif"), 
          axis.text.y = element_text(size = 25, color="black", family="serif"),
          axis.title.x = element_text(size = 25, color="black", family="serif"),             
          axis.title.y = element_text(size = 25, color="black", family="serif"),             
          panel.grid.major.x = element_blank(),                           
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               
          legend.position = "none"))   

