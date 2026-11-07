setwd("C:/Users/ROSSET/Documents/Docs/Polimi/Stats/Project")

library(data.table)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(tidyr)
library(dplyr)
library(glue)

df <- fread("datasets/SOCATv2026_NorthAtlantic.tsv", skip = 2300) # data is after 2300 lines
df <- df %>% drop_na("latitude [dec.deg.N]", "longitude [dec.deg.E]")
df <- df %>%
  mutate(`longitude [dec.deg.E]` = ifelse(`longitude [dec.deg.E]` > 180, 
                                          `longitude [dec.deg.E]` - 360, 
                                          `longitude [dec.deg.E]`))
threshold <- 1000000
df2 <- df[1:threshold,]

head(df[,0:5])

# To get the minimal size of the picture
max(df[,11])
min(df[,11])
max(df[,12])
min(df[,12])

# Load map
world <- ne_countries(scale = "medium", returnclass = "sf")

df_spatial <- st_as_sf(df2, coords = c("longitude [dec.deg.E]", "latitude [dec.deg.N]"), crs = 4326)

# Picture
x11()
ggplot(data = world) +
  geom_sf(fill = "#e8e6e1", color = "#b5b5b5") +  # Continents
  geom_sf(data = df_spatial, color = "darkred", size = 2, alpha = 0.7) + 
  coord_sf(xlim = c(-100, 10), ylim = c(10, 75), expand = FALSE) + # Atlantic
  theme_minimal() +
  labs(
    title = glue("SOCAT North Atlantic {threshold}."),
    x = "Longitude", y = "Latitude"
  )
