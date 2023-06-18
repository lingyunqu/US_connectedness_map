# File:    Heatmap_zbetweenness1_v1
# Data:    zbetweenness1.csv
# Date:    2021-10-05

# CHECK AND SET WORK DIRECTORY 
getwd()
setwd("C:/Users/Slim/Desktop/Research Assistant/Heat map")

# LOAD PACKAGES
library(dplyr)
library(choroplethr)
library(choroplethrMaps)

# CHECK LOAD PACKAGES
(.packages())

# READ IN THE ZCONNECTEDNESS DATA
betweenness <- read.csv("zbetweenness1.csv")

# UNDERSTAND THE DATA (ASK PROF: z-score?)
summary(betweenness)
hist(betweenness$zbetweenness) 




####### IDENTIFYING THE TOP 10 COUNTY ######### ASK
betweenness$zbetweenness_ln <- abs(betweenness$zbetweenness)
betweenness_abs <- abs(betweenness)

top_n(closeness_abs, 10) 
tail(sort(closeness_abs$zcloseness),10) 
# 60010, 60020, 60050, 69100, 69110, 72021, 72033, 72045, 72047, 72101
bottom_n(closeness,10)
head(sort(closeness$zcloseness),10)





# HIGHLIGHTING THE FOCAL COUNTY: Santa Clara 06085
highlight_focal_county = function(county_fips)
{
  library(choroplethrMaps)
  data(county.map, package="choroplethrMaps", envir=environment())
  df = county.map[county.map$region %in% county_fips, ]
  geom_polygon(data=df, aes(long, lat, group = group), color = "red", fill = NA, size = 1)
}

# HIGHLIGHTING THE COUNTY (TOP 10)
highlight_county = function(county_fips)
{
  library(choroplethrMaps)
  data(county.map, package="choroplethrMaps", envir=environment())
  df = county.map[county.map$region %in% county_fips, ]
  geom_polygon(data=df, aes(long, lat, group = group), color = "yellow", fill = NA, size = 1)
}

library(ggplot2) # for coord_map(), which adds a Mercator projection

names(betweenness)[names(betweenness) == 'zbetweenness']<- 'value'
names(betweenness)[names(betweenness) == 'fips']<- 'region'

county_choropleth(betweenness, 
                  title      = "Santa Clara",
                  legend     = "zbetweenness",
                  num_colors = 5) 


# USE IF NECESSARY
+highlight_focal_county(06085)+highlight_county(25017) +highlight_county(04013)+highlight_county(06059)+highlight_county(06001)




# CLEAN UP 

# Clear packages
invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)),
                 detach,
                 character.only = TRUE, unload = TRUE))

# Clear plots
dev.off()  

# Clear console
cat("\014")