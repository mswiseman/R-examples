Plotting points in R
================
Michele Wiseman
August 24th, 2021

### Example 1: percent change (amino acids, oils, thiols)

The first example uses: 

-   colorblind friendly palette
-   manual scaling of the y-axis
-   theme edits to axis fonts, facet labels, axis ticks
-   `coord_flip()` to swap x and y axes

``` r
#if you need to install any packages use install.packages("packnamehere")
library(gridExtra)        
library(stringr)
library(scales)
library(readr)             #only if reading in a csv
library(lme4)
library(reshape2)
library(ggpubr)
library(readxl)            #only if reading in an excel file
library(tidyverse)
```
#### Loading and tidying the data

The starting data set for this graph is in wide form; we need to use [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html) to convert the data to long form (R's preferred format).

``` r 

tidy_oils <- read_excel("~/Desktop/tidy_oils.xlsx")
#View(tidy_oils)    

```

As we can see, the data is currently in wide format. We need it in long format for ggplot to read it correctly. 

![wideformat](wideformat.png)

``` r 
#Converting to long form and removing any "na" cells. 
melt_tidy_oils <- melt(tidy_oils, na.rm = TRUE, id.vars = c("ID","Nitrogen","Sulfur","Replicate","Year","Treatment"))

```
Alas, long format!
![longformat](longformat.png)

#### Data transformations
Now we need to try to figure out the effect the different treatments had on each variable. If there were a known standard, we would simply compare our treatment values to the known values, but there are no standards for the treatments we're examining. Alas, we're going to creat a point of reference by taking the grand mean of each variable (in this case, each oil compound), and then finding the average difference from the grand mean for each treatment. 

``` r 
#Computing grand mean for each variable (separating by year)
oil_1 <- melt_tidy_oils %>%  
  group_by(Year, variable) %>%         
  mutate(Grand_mean_compound = mean(value))

#Difference for each data point from the grand mean. 
oil_2 <- oil_1%>%  
  group_by(variable) %>% 
  mutate(Diff2=(((value-Grand_mean_compound)/Grand_mean_compound)*100))
#View(oil_2)

#Treatment mean for a given compound (separates years)
oil_3 <- oil_2  %>%  
  group_by(Year, Treatment, variable) %>% 
  mutate(Treatment_mean_diff = mean(Diff2))

#Treatment sd for a given compound (separates years)
oil_4 <- oil_3  %>%  
group_by(Year, Treatment, variable) %>%
  mutate(Treatment_sd = sd(Diff2))

oil_4 <- oil_4 %>%
  rename(Compound = variable)

oil_4 <- oil_4 %>%
  rename(Value = value)

View(oil_4)
```

#### Making the plot
``` r
p <- ggplot(data = oil_4)
gp <-p + geom_point(aes(x = reorder(Compound),                   # orders labels into reverse order (not sure why, will fix with scale_x_discrete)
                     y = Treatment_mean_diff,
                     color=as.factor(Treatment),                 # R thinks this is continuous; fix it by calling it a factor.
                     shape= as.factor(Treatment)),
                 size=2) + 
  scale_x_discrete(limits=rev) +                                  # makes labels alpha numerical
  coord_flip() +
  facet_wrap(~Year) +                                             # separates data from each year
  labs(shape = "Treatments", title = "Oils") +                    # custom labels with labs()
    geom_hline(yintercept = 0,                                    # 0 represents the grand mean of the compound
             linetype = "solid",
             size = 0.2) + 
  scale_color_manual(name = "Treatments",                         # scale_x_manual allows for us to customize scale
                      labels = c("Low Nitrogen, Low Sulfur",      # these are custom labels for treatments
                                 "High Nitrogen, Low Sulfur",
                                 "Low Nitrogen, High Sulfur",
                                 "High Nitrogen, High Sulfur"),
                      values = c("#009E73",                       # these are custom colorblind-friendly shape colors
                                 "#CC79A7",
                                 "#009E73",
                                 "#CC79A7")) +   
  scale_shape_manual(name = "Treatments",
                     labels = c("Low Nitrogen, Low Sulfur",
                                "High Nitrogen, Low Sulfur",
                                "Low Nitrogen, High Sulfur",
                                "High Nitrogen, High Sulfur"),
                     values = c(19, 19, 17, 17)) +                # 19 = fillable circle; 17 = fillable triangle
  ylim(-45, 25) +                                                 # manually defining axis scale
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 8),                         # change size of font for y axis text
    axis.text.x = element_text(size = 12),                        # change font for x axis text
    axis.ticks.x = element_blank(),                               # remove tick marks
    panel.grid.major = element_blank(),                           # remove grid
    plot.title = element_text(hjust = 0.5, size=12),              # center title and change size
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),                               # remove y axis title
    strip.text = element_text(face = "bold", size = 12))          # change facet title details

gp
```

![nxsplot1](nxsplot1.png)

### Example 2: combining point graphs
The second example builds on the first and adds: 

-   combining legends
-   using and customizing `ggarrange` and `annotate_figure`
-   theme edits to axis fonts, facet labels, axis ticks
-   `coord_flip()` to swap x and y axes

Let's add info on the thiols to the oil data and then use ggarrange to combine the graphs. Most of the changes to the code are explained in the previous example, so reference back in case you're confused. 

``` r
p2 <- ggplot(data = thiols)
gp2<- p2 + geom_point(aes(x = fct_rev(as.factor(Compound_abb)),
                     y = Treatment_mean_diff,
                     color=as.factor(Treatment),
                     shape= as.factor(Treatment)),
                 size=2) + 
  coord_flip() +
  facet_wrap(~Year) +
  ylab("Percent difference") +
  labs(shape = "Treatments", title = "Thiols and Thiol Precursors") +
  geom_hline(yintercept = 0, 
             linetype = "solid",
             size = 0.2) +
  scale_color_manual(name = "Treatments",
                      labels = c("Low Nitrogen, Low Sulfur",
                                 "High Nitrogen, Low Sulfur",
                                 "Low Nitrogen, High Sulfur",
                                 "High Nitrogen, High Sulfur"),
                      values = c("#009E73",
                                 "#CC79A7",
                                 "#009E73",
                                 "#CC79A7")) +   
  scale_shape_manual(name = "Treatments",
                     labels = c("Low Nitrogen, Low Sulfur",
                                "High Nitrogen, Low Sulfur",
                                "Low Nitrogen, High Sulfur",
                                "High Nitrogen, High Sulfur"),
                     values = c(19, 19, 17, 17)) +
  ylim(-30, 30) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 10.5),
    axis.text.x = element_text(size = 12),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5, size=12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.text = element_text(face = "bold", size = 12))

```

Finally, let's combine our first and second graphs using ggarrange. 

``` r 
#ggarrange combines multiple plots into one. We'll use it to create two columns and a shared legend. 

p1 <-ggarrange(gp, gp2,         # first gp1 then gp2 
  ncol = 2,                     # two columns
  nrow = 1,                     # one row
  common.legend = TRUE,         # shared legend
  legend = "right")

#annotate_figure works with ggarrange to create custom annotations. We'll use it to create a shared x-axis label. 
p2 <-annotate_figure(
  p1, 
  bottom = text_grob(
    "Percent change",           # shared x-axis title
    color = "black",            # color of text
    x = 0.5,                    # 0.5 = absolute middle
    size = 12)                  # size of font
)
```

![nxsplot2](nxsplot2.png)
