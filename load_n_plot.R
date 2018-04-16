# This script was written by Matthew Morriss April 13, 2018 for the express purpose of visualizing African
#American members of congress

library(tidyverse)
library(ggplot2)

library(ggalt)
library(scales)
library(gridExtra)
library(forcats)
options(bitmapType="cairo")
#################################### Load Data ####################################
setwd('/Users/matthew/Documents/GitHub/African_American_Congress_Senators')

AMCS <- read.csv('Data.csv')

AMCS[AMCS==""] <- NA
AMCS <- AMCS[complete.cases(AMCS),]

#create important dates
importantDate <- (c(1870, 1960, 1964, 1965,2017,2019))
#################################### grouped by congress person, colored by party ####################################
# AMCS <- filter(AMCS, AMCS$start <= 1970)
library(forcats)
clr <- "gray42"
y = 1:nrow(AMCS)

ggplot(AMCS, aes(x = start, xend = End, 
                 y = fct_reorder(Name,start, .desc = TRUE),
                 group = Name,
                 color = Party))+
  geom_dumbbell(size = 0.8,
                size_xend = 0,
                size_x = 0,
                dot_guide_size=0)+
  theme_classic()+
  # scale_fill_manual(values=c("#F8766D", "#00BA38"))+
  theme(
    plot.background = element_rect(fill = clr),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.background = element_rect(fill = clr),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"),
    
    axis.title.x = element_text(colour = "white"),
    axis.text.x = element_text(color="white"),
    plot.title = element_text(color = "white"),
    
    legend.title = element_blank(),
    legend.background = element_rect(fill = clr),
    legend.text = element_text(size = 13))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  
  scale_x_continuous(limits = c(1868, 2019))+
  scale_x_continuous(position = "top")+
  scale_x_continuous(limits = c(1868, 2019))+
  scale_color_manual(values = c("Republican" = "red","Independent" =  "green","Democrat" = "blue"))+
  labs(x = "Year", y = "African American Congress Person")+
  labs(title = "Presence of African American Congress persons (Senate and House)")+
  scale_x_continuous(position = "top")+
  geom_vline(xintercept = importantDate, linetype = "dashed") 
    # scale_y_reverse()
# ggsave("African American Congress, by party.eps", device=cairo_ps)
#################################### grouped by congress person, colored by Body ####################################
# AMCS <- filter(AMCS, AMCS$start <= 1970)

clr <- "gray42"
y = 1:nrow(AMCS)
ggplot(AMCS, aes(x = start, xend = End, 
                 y = fct_reorder(Name,start, .desc = TRUE),
                 group = Name,
                 color = Body))+
  geom_dumbbell(size = 0.8,
                size_xend = 0,
                size_x = 0,
                dot_guide_size=0)+
  
  
  theme_classic()+
  
  theme(
    plot.background = element_rect(fill = clr),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.background = element_rect(fill = clr),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"),
    
    axis.title.x = element_text(colour = "white"),
    axis.text.x = element_text(color="white"),
    plot.title = element_text(color = "white"),
    
    legend.title = element_blank(),
    legend.background = element_rect(fill = clr),
    legend.text = element_text(size = 13))+
    guides(color = guide_legend(override.aes = list(size = 5)))+
  
    scale_x_continuous(limits = c(1868, 2019))+
    scale_x_continuous(position = "top")+
 

  labs(x = "Year", y = "African American Congress Person")+
  labs(title = "Presence of African American Congress persons (Senate and House)")+
  
  geom_vline(xintercept = importantDate, linetype = "dashed") 
 
ggsave("African American Congress, by body.eps", device=cairo_ps)

# Who has the longest terms?
cnt <- data.frame((table(AMCS$Name)))

