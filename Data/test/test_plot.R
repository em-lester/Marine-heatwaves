###############################################
## make test plot of summary figure for paper
###############################################

# Clear memory ----
rm(list=ls())

# Install and load packages ----
#install.packages("GISTools") # see how we go without this..
library(tidyverse)
library(ggplot2)
library(jpeg)
library(grid)
library(png)
require(MetBrewer)
library(patchwork)

## Set your working directory ----
working.dir <-  "~/Repositories/Marine heatwaves"

# Set directories----

data.dir<-paste(working.dir,"Data",sep="/")
plots.dir=paste(working.dir,"Plots",sep="/")
tidy.dir<-paste(data.dir,"ridy",sep="/")
raw.dir<-paste(data.dir,"raw",sep="/")
test.dir<-paste(data.dir,"test",sep="/")

# Read in data 
setwd(test.dir)
dir()

dat <- read.csv("test_data.csv")%>%
  mutate_at(vars(heatwave.event, species.category, species.detailed), list(as.factor)) %>%
  glimpse()

# convert to long format

df_long <- pivot_longer(dat, cols = c(impact, mechanism, persistance), names_to = "metric", values_to = "publications")
head(df_long)

# make a plot for habitat ----

dat.habitat <- df_long %>%
  filter(species.category == "habitat")%>%
  glimpse()

# Set a theme ----

Theme1 <-
  theme( # use theme_get() to see available options
    # legend.background = element_rect(fill="white"),
    #legend.background = element_blank(),
    #legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size = 18), # Adjusting legend text size
    legend.title = element_text(size = 19,hjust = 0, vjust = 0.8), # Adjusting legend title size
    legend.position = "bottom",
    legend.key.height = unit(1, "cm"), # Increasing legend key height for better visibility
    text=element_text(size=18),
    strip.text.y = element_text(size = 20,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=22),
    axis.title.y=element_text(vjust=0.6, angle=90, size=22),
    axis.text.x=element_text(size=20, angle=90, hjust = 0.5, vjust = 0.5),
    axis.text.y=element_text(size=20),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    plot.title.position = "plot",
    strip.background = element_blank())

Theme2 <- 
  theme( # use theme_get() to see available options
    # legend.background = element_rect(fill="white"),
    #legend.background = element_blank(),
    #legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size = 18), # Adjusting legend text size
    legend.title = element_text(size = 19,hjust = 0, vjust = 0.8), # Adjusting legend title size
    legend.position = "bottom",
    legend.key.height = unit(1, "cm"), # Increasing legend key height for better visibility
    text=element_text(size=18),
    strip.text.y = element_text(size = 20,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=22),
    axis.title.y=element_text(vjust=0.6, angle=90, size=22),
    axis.text.x=element_text(size=20, angle=90, hjust = 0.5, vjust = 0.5),
    axis.text.y=element_text(size=20),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    plot.title.position = "plot",
    strip.text = element_blank() # Remove facet labels
  )

habitat.plot <- ggplot(dat.habitat, aes(x = metric, y = species.detailed)) + facet_grid(~heatwave.event)+
  geom_tile(aes(fill = publications), color = "#E9E6CB") +
  scale_fill_gradientn(colors = c("#F1DAC4", "#474973"), limits = c(0, 15), na.value = "#C2C1BC") + 
  ylab("")+
  xlab("")+
  labs(fill = "No. publications") + # Changing legend title
  ggtitle("Habitat")+ # Adding title to the plot
  #geom_text(aes(label = publications), color = "black", size = 3) +
  theme_classic()+
  Theme1
habitat.plot 

# Fisheries stock plot ----

dat.fisheries <- df_long %>%
  filter(species.category == "fisheries")%>%
  glimpse()

fisheries.plot <- ggplot(dat.fisheries, aes(x = metric, y = species.detailed)) + facet_grid(~heatwave.event)+
  geom_tile(aes(fill = publications), color = "#E9E6CB") +
  scale_fill_gradientn(colors = c("#F1DAC4", "#474973"), limits = c(0, 15), na.value = "#C2C1BC") + 
  ylab("")+
  xlab("")+
  labs(fill = "No. publications") + # Changing legend title
  ggtitle("Fisheries stock")+ # Adding title to the plot
  #geom_text(aes(label = publications), color = "black", size = 3) +
  theme_classic()+
  Theme2

fisheries.plot

# Non targetstock plot ----

dat.non.target <- df_long %>%
  filter(species.category == "non-target species")%>%
  glimpse()

non.target.plot <- ggplot(dat.non.target, aes(x = metric, y = species.detailed)) + facet_grid(~heatwave.event)+
  geom_tile(aes(fill = publications), color = "#F1DAC4") +
  scale_fill_gradientn(colors = c("#F1DAC4", "#474973"), limits = c(0, 15), na.value = "#C2C1BC") + 
  ylab("")+
  xlab("")+
  labs(fill = "No. publications") + # Changing legend title
  ggtitle("Non-target species")+ # Adding title to the plot
  #geom_text(aes(label = publications), color = "black", size = 3) +
  theme_classic()+
  Theme2

non.target.plot 


# combine using patchwork

combined_plot <- habitat.plot/fisheries.plot/non.target.plot + plot_layout(guides = 'collect')
combined_plot

# save ----

setwd(plots.dir)

ggsave("test_summary_plot.tiff", combined_plot, dpi=300, height=12, width=12)

