Animations
================
Michele Wiseman
June 7th, 2021

Good resources for maps and animation
* https://luisdva.github.io/rstats/GIS-with-R/
* https://m-clark.github.io/posts/2020-03-23-covid/
* https://cengel.github.io/R-spatial/mapping.html
* https://blog.mastermaps.com/
* http://thematicmapping.org/downloads/world_borders.php
* https://www.r-graph-gallery.com/choropleth-map-in-r.html
* https://github.com/adamgibbons/oregon-choropleth
* https://blogs.oregonstate.edu/developer/2017/06/26/building-geometries-new-data-locations-api/
* https://www.r-bloggers.com/2013/01/maps-in-r-plotting-data-points-on-a-map/
* https://gganimate.com/articles/gganimate.html


# Example 1: Animating data from iNaturalist

Techniques learned/used:
* How to use ```rinat``` package
* Tidying data
* Animating geom_point/geom_size data with gganimate
* Manual scaling
* Adding multiple scales
* Creating a wrapping function

``` r
#load required packages 
library(ggspatial) 
library(lwgeom)
library(sf)
library(tidyverse)
library(rgeos)
library(ggmap)
library(rinat)
library(gganimate)
library(rgdal)
library(maps)
library(geojsonio)

# Make a dataframe for the state maps
states <- map_data("state")

# Make a dataframe for the west coast (using one of the map packages above)
west_coast <- states %>%
  filter(region %in% c("california", "oregon", "washington"))

# Use rinat package to download a inat dataframe
# I have chosen filters for taxon_id (Psilocybe spp.), place_id (west coast), quality (research), geo = TRUE (only if geo coordinates are available).
# Package documentation here: 

psilocybe_inat <- get_inat_obs(
  taxon_id = 54026,
  place_id = 65360,
  quality = "research",
  geo = TRUE,
  maxresults = 10000,
  meta = FALSE
)

# Filter for accuracy and open coordinates
psilocybe_inat_accurate <- psilocybe_inat %>% 
  filter(public_positional_accuracy < 1000) %>% 
  filter(coordinates_obscured == "false")

# Loading colorblind safe palette into a vector
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")


# Changing columns for year month and day so I can animate by month
psilocybe_inat_accurate_animation <- psilocybe_inat_accurate %>%
  separate(datetime, sep="-", into = c("year", "month", "day"))

# Writing a function so I can automatically wrap text (otherwise it will get cut off)

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

#Renaming months from numbers to characters
psilocybe_inat_accurate_animation$month[psilocybe_inat_accurate_animation$month=="01"] <- "January"
psilocybe_inat_accurate_animation$month[psilocybe_inat_accurate_animation$month=="02"] <- "February"
psilocybe_inat_accurate_animation$month[psilocybe_inat_accurate_animation$month=="03"] <- "March"
psilocybe_inat_accurate_animation$month[psilocybe_inat_accurate_animation$month=="04"] <- "April"
psilocybe_inat_accurate_animation$month[psilocybe_inat_accurate_animation$month=="05"] <- "May"
psilocybe_inat_accurate_animation$month[psilocybe_inat_accurate_animation$month=="06"] <- "June"
psilocybe_inat_accurate_animation$month[psilocybe_inat_accurate_animation$month=="07"] <- "July"
psilocybe_inat_accurate_animation$month[psilocybe_inat_accurate_animation$month=="08"] <- "August"
psilocybe_inat_accurate_animation$month[psilocybe_inat_accurate_animation$month=="09"] <- "September"
psilocybe_inat_accurate_animation$month[psilocybe_inat_accurate_animation$month=="10"] <- "October"
psilocybe_inat_accurate_animation$month[psilocybe_inat_accurate_animation$month=="11"] <- "November"
psilocybe_inat_accurate_animation$month[psilocybe_inat_accurate_animation$month=="12"] <- "December"

# Re-ordering months so they go from January -> December
psilocybe_inat_accurate_animation$month <- factor(psilocybe_inat_accurate_animation$month, levels = month.name)

# Creating vectors for my labels, this tidies things and allows me to use my wrapper function. 
title <- "The diversity and abundance of Psilocybe spp. is greatest in November and December in the western US."
caption <- "Figure 1. Each point represents n number of observations from the respective species. Data was retrieved from research grade iNaturalist observations. Some species (ex. Ps. cyanescens) are missing due to obscured datapoints."

# Here's the final animation and plot code
my.animation<-ggplot(data = west_coast) + 
  geom_polygon(aes(x = long,                # geom_polygon is a polygon of the western states; this is the first layer
                   y = lat,
                   group = group),
               fill = "white",
               color = "darkgray") + 
  coord_quickmap()+
  geom_count(data = psilocybe_inat_accurate_animation,     # geom_count is a special form of geom_point that counts overlapping points and adjusts size accordingly
             mapping = aes(                                 #second layer geom
               x = longitude,
               y = latitude,
               fill = scientific_name),                     # any attributes within the aesthetics paranthesis will automatically have a legend
             color = "black",                               # color of shape outline
             shape= 21,                                     # shape 21 allows for fill and stroke
             alpha= 0.7,                                    # alpha = transparency. Higher = more transparent. 
             stroke = 1) +                                  # stroke = outine size
  scale_size_area(
    breaks= c(1,2,3,4),
    max_size = 4) +
  scale_size(
    range = c(2,7),                         #the size range for my circles. 1 was too small, so I started with 2. 
    breaks = c(1,2,3,4)) +                  #ggplot was automatically assinging 0.5 points in my scale; you can't have half an observation. This changes that. 
  scale_fill_manual(
    name = "Species",                                       # This names my "fill"-based legend
    values = safe_colorblind_palette) +                     # Loading colorblind safe colors
  theme_bw() +                                              # themes can be found here: https://ggplot2.tidyverse.org/reference/ggtheme.html
  guides(fill = 
           guide_legend(
             override.aes = list(size=5))) +                # this manually scales the circles in my key legend so they're bigger
  theme(                                                    # Customization of my theme. Learn more here: https://rpubs.com/mclaire19/ggplot2-custom-themes
    plot.background= element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(face= "italic", hjust = 0),
    legend.text = element_text(face= "italic", size = 11),
    legend.title = element_text(face= "bold", size = 12),
    title = ggtext::element_markdown(hjust = 0.5, size=12),
    axis.title.x = element_text(margin=margin(5,0,5,0)),
    axis.title.y = element_text(margin=margin(0,5,0,0))) +
      labs(title= (wrapper(title, width = 60)),
       subtitle = 'Month: {closest_state}',
       x = 'Longitude',
       y = 'Latitude',
       caption = wrapper(caption, width =75)) +
  transition_states(                                       # Gganimation starts here
    month,                                                 # animating by month
    transition_length = 2,                                 # how long each transition takes
    state_length = 3                                       # how long each frame stays
  ) +
  enter_fade() +                                           # the next three lines are stylistic choices for animation movement, entrance, and exit
  exit_shrink() +
  ease_aes('sine-in-out')                                  


# I need a two part animation process to ensure the plot will size correctly.
animate(my.animation, height = 500, width = 450)
anim_save("final_animation.gif", animation = last_animation())
```

Here is my final product:

![](https://raw.githubusercontent.com/mswiseman/R-examples/main/_plots/final_animation.gif)

  # Example 2: Animating a basic bar chart
  
* More practice with dpylr, gganimate
* Minor polishing
  
```r
#load required packages... some of these may not be necessary...I didn't triple check.
#remember, if you need to install any packages, the command is: install.packages('packagenamehere')
library(magrittr)
library(rvest)
library(readxl)
library(gridExtra) 
library(tidyverse)  
library(stringr)
library(hrbrthemes)
library(scales)
library(readr)
library(gganimate)
library(magick)
library(janitor)
library(maps)

#load freely available data
covid<- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", na = ".")

#it's a huge file, so I trimmed it down
covid_simplified <- subset(covid, select=c(iso_code, date, total_cases, continent, new_cases, reproduction_rate))

#I'm just interested in North America for this example, so I use dplyr to filter North America by ISO code. 
covid_north_america <- covid_simplified %>%
  filter(iso_code %in% north_america)

#Make a North America ISO code vector
north_america <- c("USA", "MEX", "CAN")

#Checking the structure and dataframe for any errors
str(covid_simplified)
View(covid_simplified)

#Trimming it down because large animations are memory and space intensive
covid_north_america_short <- covid_north_america %>%
  filter(date > "2020-06-17" & date <"2020-06-30")

```

## Now for the animation

```r

#The animation. Admittedly, this isn't super polished, but you can get an idea of polishing from the above example.

a <- ggplot(covid_north_america_short,               #here's the data frame
            aes(x = iso_code,                        #countries on x axis
                y = total_cases,                     #cases on y axis
                fill= iso_code)) +
  geom_bar(stat = "identity") +                      #we're making a bar chart
  labs(title = "North American COVID-19 cases from June 17th to June 30th, 2020",
      subtitle = "Date: {closest_state}") +
  ylab("Total COVID-19 Cases")+
  xlab("North American Country")+
  geom_text(aes(label = total_cases, 
                y = round(total_cases, digits=0)),   #rounding integers above bars
            position = position_dodge(0.9), 
            vjust = -1) +
  theme_classic() +
  theme(
    legend.title = element_blank()
  )+
  transition_states(states = date,                   #animating by date
                    transition_length = 1,           #shorter transition
                    state_length = 3) +              #longer time at each frame
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
  
```

Final bar chart product:


![](https://raw.githubusercontent.com/mswiseman/R-examples/main/_plots/final_animation2.gif)

