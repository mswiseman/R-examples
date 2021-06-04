R mapping examples for Gent Lab
================
Michele Wiseman
June 4th, 2021

## Mapping charts

### Example 1: COVID

The first example uses: \* dplyr filtering, selecting, and na. removal
\* labeling of title, caption, legend \* manual color scaling (package:
scales) \* log transformation \* usmap packages

``` r
#https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
#https://covidcountydata.org/data/download  - this is the data source
```

``` r
#load required packages
library(tidyverse)
library(usmap)
library(scales)
library(readr)
library(lubridate)
library(readxl)
library(ggmap)

#load data (downloaded from here: https://covidcountydata.org/data/download)
#covid2 <- read_csv("~/Downloads/deaths_2021-04-09T03:00.csv")   #be sure to change to your directory

#the other file didn't seem complete, lets try this one.
covid3 <- read_csv("~/Downloads/all_current_date_2021-04-09T03:00 2.csv")

#look at structure
str(covid3)

#lets simplify the file
covid_county_latest <- subset(covid3, select=c(county_fips, dt, county_name, deaths_total))

#check to make sure it worked - worked fine!!
view(covid_county_latest)

#figure out column names to make sure they're compatible with usmaps
colnames(covid_county_latest)

#rename county_fips to make compatible with usmap (only allows "fips)
covid_county_latest$fips <- covid_county_latest$county_fips


#Looks like I'm missing county data for some places (like New York), but dataset seems pretty complete. Good enough for the exercise. 

#this isn't very informative because there are such large differences, so let's log transform. 
map1 <- plot_usmap(regions = "counties", data = covid_county_latest, values = "deaths_total") +
  scale_fill_gradient("Total Deaths", low="blue", high="red") +
  theme(legend.position = "right") +
    labs(title = "COVID cumulative deaths per county (April 8th, 2021)", 
         caption = "Counties shaded in grey have either zero documented cases or are missing from the dataset.")

```

![](https://raw.githubusercontent.com/mswiseman/R-examples/main/_plots/plot3.png)<!-- -->

``` r
#Here's the log transformation. 
map2 <- plot_usmap(regions = "counties", data = covid_county_latest, values = "deaths_total") +
  scale_fill_gradient("Log Transform \nof Total Deaths", trans = "log10", low="blue", high="red") +
  theme(legend.position = "right") +
    labs(title = "Log transformed COVID cumulative deaths per county (April 8th, 2021)", 
         caption = "Counties shaded in grey have either zero documented cases or are missing from the dataset.")

map2
```

![](maps_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
#set color palette
myColors <- c("#0066FFFF", "#00CCFFFF", "#00FFCCFF", "#00FF66FF", "#00FF00FF", "#66FF00FF", "#CCFF00FF", "#FFCC00FF", "#FF6600FF", "#FF0000FF")

#Here's the log transformation with custom colors (tried to make differentiation more clear).
map3 <- plot_usmap(regions = "counties", data = covid_county_latest, values = "deaths_total") +
  scale_fill_gradientn("Log Transform \nof Total Deaths", trans = "log10", colors = myColors)+
  theme(legend.position = "right") +
  labs(title = "Log transformed COVID cumulative deaths per county (April 8th, 2021)", 
       caption = "Counties shaded in grey have either zero documented cases or are missing from the dataset.")
```

[](https://raw.githubusercontent.com/mswiseman/R-examples/main/_plots/plot3.png)

### Example 2: Powdery mildew spatial data

The second example uses: \* dplyr filtering, selecting, and na. removal
\* labeling of title, caption, legend \* manual color scaling (package:
scales) \* log transformation \* usmap packages

``` r
#importing and tidying data
area_wide <- read_excel("~/Downloads/2019-2020 Area Wide Powdery Mildew Database_CR_10_13_2020.xlsx")
#view(area_wide)
area_wide_cond <- area_wide[c(1:18)]
area_wide_cond <- area_wide[-c(661:667), ]
area_wide_sort <- area_wide_cond
area_wide_sort$Month <- factor(area_wide_sort$Month, levels = c("April", "May", "June", "July"))
#view(area_wide_sort)

#willamette valley bounding box
wv <- c(left = -123.52, bottom = 44.7, right = -121.6, top = 45.38) #get_stamenmap(wv, maptype = "terrain") %>% ggmap()
flagshoot <- area_wide_sort %>%
  filter(`Flag shoots`>0)

# rough remaking Cameron's graph
cam1<- qmplot(Centroid_Long, Centroid_Lat, data = flagshoot, area_wide_sort, maptype = "terrain-background", geom= "blank") +
geom_point(data = area_wide_sort, aes(Centroid_Long, Centroid_Lat),)

cam2<- cam1 + geom_point(aes(size='Flag shoots'), pch= 21, fill = "hot pink")+
  theme(
    legend.position = "bottom",
    legend.title= element_blank())

#pull out flag shoot data
flag_shoots <- area_wide_sort %>%
  filter(`Flag shoots`>0)
#view(flag_shoots)

#Here's my attempt at making it better
plot4 <- qmplot(
  Centroid_Long,
  Centroid_Lat,
  data = area_wide_sort,
  geom = "blank",
  maptype = "terrain", 
  darken = .1, 
  legend = "bottomright") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA)+
  scale_fill_gradient2("Mildew\nIncidence", low = "blue", mid = "yellow", high = "white", midpoint = 50, space = "Lab", limits=c(0, 100))+
  facet_wrap(~ Month) +
  geom_point(data= flag_shoots, aes(x= Centroid_Long, y= Centroid_Lat, size= 1), shape = 21, fill="hot pink", alpha=0.6) +
  labs(title= "Powdery mildew incidence is most severe on hop in June and July in the Willamette Valley",
       subtitle = "Disease incidence and are severity are correlated with temperature and proximity to inoculum sources",
       caption = "Figure 1. Farms that have overwintering powdery mildew or small buffer zones between hop yards are most at risk for powdery mildew outbreaks.",
       size = "Flagshoot") +
  theme(panel.spacing  = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1), 
        legend.background = element_rect(fill=alpha('white', 0.7)), 
        legend.text.align = 0,
        legend.text = element_text(size=10),
        plot.caption = element_text(face = "italic", size = 8, hjust = 0), 
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face="bold", size=12))

#need to specify the scale so you don't cut the title off
ggsave("plot4.png",
        width = 12,
       height = 12,
       units = "in",
       dpi = 300)
```

[](https://raw.githubusercontent.com/mswiseman/R-examples/main/_plots/plot4.png)

### Example 3: Mapping basketball coordinate data to a custom polygon.

This third example is incredibly complicated, but hopefully serves to
demonstrate that you can do almost anything with R… if you’re willing to
write the code.

``` r
# load our data
shots_sum <- readr::read_csv(url("https://raw.githubusercontent.com/cwickham/basketball-shots/master/shots_sum.csv"))

# subset it to remove outlier
new <- subset(shots_sum, num_shots < 3000)
```

``` r
# this code was taken from here: https://egallic.fr/en/drawing-a-basketball-court-with-r/
circle_fun <- function(center=c(0,0), diameter=1, npoints=500, start=0, end=2){
  tt <- seq(start*pi, end*pi, length.out=npoints)
  data.frame(
    x = center[1] + diameter / 2 * cos(tt),
    y = center[2] + diameter / 2 * sin(tt)
  )
}

# Gives y coordinates of the opposite side
rev_y <- function(y) 94-y

# From x and y coordinates for a line (represented by a polygon here),
# a number of group and a short description
# creates a data.frame for this line
# in order to use it with ggplot2.
new_coords <- function(x, y, group, descri){
  new_coords_df <- data.frame(x = x, y = y)
  new_coords_df$group <- group
  new_coords_df$side <- 1
  group <- group + 1
  
  # The same thing for the opposite side
  new_coords_df2 <- data.frame(x = x, y = rev_y(y))
  new_coords_df2$group <- group
  new_coords_df2$side <- 2
  group <<- group + 1
  
  # On reunit les donnees
  new_coords_df <- rbind(new_coords_df, new_coords_df2)
  new_coords_df$descri <- descri
  
  return(new_coords_df)
}
```

``` r
# Restricted area
cercle_np_out <- circle_fun(center = c(25,5+3/12), diameter = (4+1/6)*2)
cercle_np_in <- circle_fun(center = c(25,5+3/12), diameter = 4*2)

# Three point
cercle_3pts_out <- circle_fun(center = c(25,5+3/12), diameter = (23+9/12)*2)
cercle_3pts_in <- circle_fun(center = c(25,5+3/12), diameter = (23+7/12)*2)

# Hoop
cercle_ce <- circle_fun(center = c(25,5+3/12), diameter = 1.5)

# Free Throws
cercle_lf_out <- circle_fun(center = c(25,19), diameter = 6*2)
cercle_lf_in <- circle_fun(center = c(25,19), diameter = (6-1/6)*2)

# Center Circle
cercle_mil_out <- circle_fun(center = c(25,47), diameter = 6*2)
cercle_mil_in <- circle_fun(center = c(25,47), diameter = (6-1/6)*2)

# Small Center Circle
cercle_mil_petit_out <- circle_fun(center = c(25,47), diameter = 2*2)
cercle_mil_petit_in <- circle_fun(center = c(25,47), diameter = (2-1/6)*2)
```

``` r
group <- 1
court <- new_coords(c(0-1/6,0-1/6,50 + 1/6,50 + 1/6), c(0 - 1/6,0,0,0 - 1/6), group = group, descri = "ligne de fond")
court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,0,0), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne gauche"))
court <- rbind(court, new_coords(x = c(50,50,50+1/6,50+1/6), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne droite"))
court <- rbind(court, new_coords(x = c(0,0,3,3), y = c(28,28+1/6,28+1/6,28), group = group, descri = "marque entraineur gauche"))
court <- rbind(court, new_coords(x = c(47,47,50,50), y = c(28,28+1/6,28+1/6,28), group = group, descri = "marque entraineur droite"))
court <- rbind(court, new_coords(x = c(3,3,3+1/6,3+1/6), y = c(0,14,14,0), group = group, descri = "3pts bas gauche"))
court <- rbind(court, new_coords(x = c(47-1/6,47-1/6,47,47), y = c(0,14,14,0), group = group, descri = "3pts bas droit"))
court <- rbind(court, new_coords(x = c(17,17,17+1/6,17+1/6), y = c(0,19,19,0), group = group, descri = "LF bas gauche"))
court <- rbind(court, new_coords(x = c(33-1/6,33-1/6,33,33), y = c(0,19,19,0), group = group, descri = "LF bas droit"))
court <- rbind(court, new_coords(x = c(17,17,33,33), y = c(19-1/6,19,19,19-1/6), group = group, descri = "LF tireur"))
court <- rbind(court, new_coords(x = c(14-1/6,14-1/6,14,14), y = c(0,1/2,1/2,0), group = group, descri = "marque fond gauche"))
court <- rbind(court, new_coords(x = c(36,36,36+1/6,36+1/6), y = c(0,1/2,1/2,0), group = group, descri = "marque fond droit"))
court <- rbind(court, new_coords(x = c(19,19,19+1/6,19+1/6), y = c(0,19,19,0), group = group, descri = "LF gauche interieur"))
court <- rbind(court, new_coords(x = c(31-1/6,31-1/6,31,31), y = c(0,19,19,0), group = group, descri = "LF droite interieur"))
court <- rbind(court, new_coords(x = c(22, 22, 28, 28), y = c(4-1/6,4,4,4-1/6), group = group, descri = "planche"))
court <- rbind(court, new_coords(x = c(cercle_3pts_out[31:220,"x"], rev(cercle_3pts_in[31:220,"x"])),
                                y = c(cercle_3pts_out[31:220,"y"], rev(cercle_3pts_in[31:220,"y"])), group = group, descri = "cercle 3pts"))
court <- rbind(court, new_coords(x = c(cercle_np_out[1:250,"x"], rev(cercle_np_in[1:250,"x"])),
                                y = c(cercle_np_out[1:250,"y"], rev(cercle_np_in[1:250,"y"])), group = group, descri = "cercle non passage en force"))
court <- rbind(court, new_coords(x = c(20+1/6,20+1/6,20+8/12,20+8/12), y = c(13,13+1/6,13+1/6,13), group = group, descri = "marque bas gauche cercle LF"))
court <- rbind(court, new_coords(x = c(30-8/12,30-8/12,30-1/6,30-1/6), y = c(13,13+1/6,13+1/6,13), group = group, descri = "marque bas droite cercle LF"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[1:250,"x"], rev(cercle_lf_in[1:250,"x"])),
                                y = c(cercle_lf_out[1:250,"y"], rev(cercle_lf_in[1:250,"y"])), group = group, descri = "cercle LF haut"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[250:269,"x"], rev(cercle_lf_in[250:269,"x"])),
                                y = c(cercle_lf_out[250:269,"y"], rev(cercle_lf_in[250:269,"y"])), group = group, descri = "cercle LF partie 1"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[288:308,"x"], rev(cercle_lf_in[288:308,"x"])),
                                y = c(cercle_lf_out[288:308,"y"], rev(cercle_lf_in[288:308,"y"])), group = group, descri = "cercle LF partie 2"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[327:346,"x"], rev(cercle_lf_in[327:346,"x"])),
                                y = c(cercle_lf_out[327:346,"y"], rev(cercle_lf_in[327:346,"y"])), group = group, descri = "cercle LF partie 3"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[365:385,"x"], rev(cercle_lf_in[365:385,"x"])),
                                y = c(cercle_lf_out[365:385,"y"], rev(cercle_lf_in[365:385,"y"])), group = group, descri = "cercle LF partie 4"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[404:423,"x"], rev(cercle_lf_in[404:423,"x"])),
                                y = c(cercle_lf_out[404:423,"y"], rev(cercle_lf_in[404:423,"y"])), group = group, descri = "cercle LF partie 5"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[442:462,"x"], rev(cercle_lf_in[442:462,"x"])),
                                y = c(cercle_lf_out[442:462,"y"], rev(cercle_lf_in[442:462,"y"])), group = group, descri = "cercle LF partie 6"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[481:500,"x"], rev(cercle_lf_in[481:500,"x"])),
                                y = c(cercle_lf_out[481:500,"y"], rev(cercle_lf_in[481:500,"y"])), group = group, descri = "cercle LF partie 7"))
court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(7,7+1/6,7+1/6,7), group = group, descri = "marque 1 LF gauche"))
court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(8+1/6,8+1/3,8+1/3,8+1/6), group = group, descri = "marque 2 LF gauche"))
court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(11+1/3,11.5,11.5,11+1/3), group = group, descri = "marque 3 LF gauche"))
court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(14.5,14.5+1/6,14.5+1/6,14.5), group = group, descri = "marque 4 LF gauche"))
court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(7,7+1/6,7+1/6,7), group = group, descri = "marque 1 LF droite"))
court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(8+1/6,8+1/3,8+1/3,8+1/6), group = group, descri = "marque 2 LF droite"))
court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(11+1/3,11.5,11.5,11+1/3), group = group, descri = "marque 3 LF droite"))
court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(14.5,14.5+1/6,14.5+1/6,14.5), group = group, descri = "marque 4 LF droite"))
court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,50+1/6,50+1/6), y = c(94/2-1/12,94/2, 94/2, 94/2-1/12), group = group, descri = "ligne mediane"))
court <- rbind(court, new_coords(x = c(cercle_mil_out[250:500,"x"], rev(cercle_mil_in[250:500,"x"])),
                                y = c(cercle_mil_out[250:500,"y"], rev(cercle_mil_in[250:500,"y"])), group = group, descri = "cercle milieu grand"))
court <- rbind(court, new_coords(x = c(cercle_mil_petit_out[250:500,"x"], rev(cercle_mil_petit_in[250:500,"x"])),
                                y = c(cercle_mil_petit_out[250:500,"y"], rev(cercle_mil_petit_in[250:500,"y"])), group = group, descri = "cercle milieu petit"))
court <- rbind(court, new_coords(x = cercle_ce[,"x"], y = cercle_ce[,"y"], group = group, descri = "anneau"))
```

Finally, for the actual graph.

``` r
shots <- ggplot(new, aes(x, y)) +
  geom_point(aes(color = total_points, size=num_shots)) +
  geom_polygon(data = court, aes(x = x, y = y, group = group), col = "white") +
  coord_equal() +
  ylim(-2,96) +
  xlim(-5,55) +
  scale_x_continuous(breaks = c(0, 12.5, 25, 37.5, 50)) +
  scale_y_continuous(breaks = c(0, 23.5, 47, 70.5, 94)) +
  scale_colour_distiller("Points", palette = "RdYlGn") +
  coord_equal() +
  scale_size("Attempts", range = c(0.05, 5)) +
  labs(title = "Miami Heat bring heat under the net")+
  theme_classic(18) +
  theme(axis.ticks= element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "burlywood1",
                                colour = "burlywood2",
                                size = 0.5, linetype = "solid"))
ggsave("plot5.png",
        width = 12,
       height = 12,
       units = "in",
       dpi = 300)
```

[](https://raw.githubusercontent.com/mswiseman/R-examples/main/_plots/plot5.png)
