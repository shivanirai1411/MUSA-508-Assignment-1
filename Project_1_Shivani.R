#imports
library(rjson)
library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
options(scipen=999)
options(tigris_class = "sf")

#data import 
sfo <- read.csv('final_sfo_lines.csv')
#crime <- fromJSON('crimeOSFO.json')

# ---- Load Styling options -----

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

# Load hexadecimal color palette

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")


census_api_key("aea3dee2d96acb5101e94f3dcfa1b575f73d093a", overwrite = TRUE)

#-------Data Wrangling for County Data-------------

# ---- Year 2009 tracts -----

# We run our year 2000 code using 2009 ACS (and ACS variables from our 2017 list)
# Notice this returns "long" data - let's examine it

tracts09 <-  
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B19013_001E",
                                             "B25058_001E","B01001_006E","B01001_007E",
                                             "B01001_008E","B01001_009E","B01001_010E",
                                             "B01001_011E","B01001_012E","B01001_013E",
                                             "B01001_014E","B01001_015E","B01001_016E",
                                             "B01001_030E","B01001_031E","B01001_032E",
                                             "B01001_033E","B01001_034E","B01001_035E",
                                             "B01001_036E","B01001_037E","B01001_038E",
                                             "B01001_039E","B01001_040E","B12001_004E",
                                             "B12001_013E","B02001_003E","B02001_004E",
                                             "B02001_005E","B02001_006E","B03001_003E"), 
          year=2009, state=06, county=c(001,075,081,013,085), geometry=T) %>% 
  st_transform('ESRI:102241')

# We create a new data frame consisting only of population

totalPop09 <-
  tracts09 %>%
  filter(variable == "B25026_001")

tracts09 <- 
  tracts09 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B25026_001, 
         Whites = B02001_002,
         Blacks = B02001_003,
         AmInd = B02001_004,
         Asian = B02001_005,
         Hawaiian = B02001_006,
         Hispanic = B03001_003,
         MedHHInc = B19013_001, 
         MedRent = B25058_001,
         WorkingClassM1517 = B01001_006,
         WorkingClassM1819 = B01001_007,
         WorkingClassM20 = B01001_008,
         WorkingClassM21 = B01001_009,
         WorkingClassM2224 = B01001_010,
         WorkingClassM2529 = B01001_011,
         WorkingClassM3034 = B01001_012,
         WorkingClassM3539 = B01001_013,
         WorkingClassM4044 = B01001_014,
         WorkingClassM4549 = B01001_015,
         WorkingClassM5054 = B01001_016,
         WorkingClassF1517 = B01001_030,
         WorkingClassF1819 = B01001_031,
         WorkingClassF20 = B01001_032,
         WorkingClassF21 = B01001_033,
         WorkingClassF2224 = B01001_034,
         WorkingClassF2529 = B01001_035,
         WorkingClassF3034 = B01001_036,
         WorkingClassF3539 = B01001_037,
         WorkingClassF4044 = B01001_038,
         WorkingClassF4549 = B01001_039,
         WorkingClassF5054 = B01001_040,
         MaleMarried = B12001_004,
         FemaleMarried = B12001_013
         )

tracts2009 <- 
  tracts09 %>%
  mutate(pctWorking = ifelse(TotalPop > 0, ((WorkingClassM1517 + WorkingClassM1819 + WorkingClassM20 
                                             + WorkingClassM21 + WorkingClassM2224 + WorkingClassM2529 
                                             + WorkingClassM3034 + WorkingClassM3539 + WorkingClassM4044 
                                             + WorkingClassM4549 + WorkingClassM5054 + WorkingClassF1517 
                                             + WorkingClassF1819 + WorkingClassF20 + WorkingClassF21 
                                             + WorkingClassF2224 + WorkingClassF2529 + WorkingClassF3034 
                                             + WorkingClassF3539 + WorkingClassF4044 + WorkingClassF4549 
                                             + WorkingClassF5054)/TotalPop), 0),
         pctMarried = ifelse(TotalPop > 0, ((FemaleMarried + MaleMarried) / TotalPop), 0),
         pctWhite = ifelse(TotalPop > 0, Whites / TotalPop, 0),
         pctOtherRace = ifelse(TotalPop > 0, ((Blacks + AmInd +Asian
                                               + Hawaiian + Hispanic) / TotalPop), 0),
         year = "2009") %>%
  dplyr::select(-Whites,-FemaleMarried,-MaleMarried,-Blacks,-AmInd,-Asian,-Hawaiian,-Hispanic,
                -WorkingClassM1517,-WorkingClassM1819,-WorkingClassM20,-WorkingClassM21,-WorkingClassM2224,-WorkingClassM2529, 
                -WorkingClassM3034,-WorkingClassM3539,-WorkingClassM4044,-WorkingClassM4549,-WorkingClassM5054,-WorkingClassF1517, 
                -WorkingClassF1819,-WorkingClassF20,-WorkingClassF21,-WorkingClassF2224,-WorkingClassF2529,-WorkingClassF3034,
                -WorkingClassF3539,-WorkingClassF4044,-WorkingClassF4549,-WorkingClassF5054)

tracts2017 <-  
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B19013_001E",
                                             "B25058_001E","B01001_006E","B01001_007E",
                                             "B01001_008E","B01001_009E","B01001_010E",
                                             "B01001_011E","B01001_012E","B01001_013E",
                                             "B01001_014E","B01001_015E","B01001_016E",
                                             "B01001_030E","B01001_031E","B01001_032E",
                                             "B01001_033E","B01001_034E","B01001_035E",
                                             "B01001_036E","B01001_037E","B01001_038E",
                                             "B01001_039E","B01001_040E","B12001_004E",
                                             "B12001_013E","B02001_003E","B02001_004E",
                                             "B02001_005E","B02001_006E","B03001_003E"), 
          year=2017, state=06, county=c(001,075,081,013,085), geometry=T) %>% 
  st_transform('ESRI:102241') %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B25026_001, 
         Whites = B02001_002,
         Blacks = B02001_003,
         AmInd = B02001_004,
         Asian = B02001_005,
         Hawaiian = B02001_006,
         Hispanic = B03001_003,
         MedHHInc = B19013_001, 
         MedRent = B25058_001,
         WorkingClassM1517 = B01001_006,
         WorkingClassM1819 = B01001_007,
         WorkingClassM20 = B01001_008,
         WorkingClassM21 = B01001_009,
         WorkingClassM2224 = B01001_010,
         WorkingClassM2529 = B01001_011,
         WorkingClassM3034 = B01001_012,
         WorkingClassM3539 = B01001_013,
         WorkingClassM4044 = B01001_014,
         WorkingClassM4549 = B01001_015,
         WorkingClassM5054 = B01001_016,
         WorkingClassF1517 = B01001_030,
         WorkingClassF1819 = B01001_031,
         WorkingClassF20 = B01001_032,
         WorkingClassF21 = B01001_033,
         WorkingClassF2224 = B01001_034,
         WorkingClassF2529 = B01001_035,
         WorkingClassF3034 = B01001_036,
         WorkingClassF3539 = B01001_037,
         WorkingClassF4044 = B01001_038,
         WorkingClassF4549 = B01001_039,
         WorkingClassF5054 = B01001_040,
         MaleMarried = B12001_004,
         FemaleMarried = B12001_013
  ) %>%
  mutate(pctWorking = ifelse(TotalPop > 0, ((WorkingClassM1517 + WorkingClassM1819 + WorkingClassM20 
                                             + WorkingClassM21 + WorkingClassM2224 + WorkingClassM2529 
                                             + WorkingClassM3034 + WorkingClassM3539 + WorkingClassM4044 
                                             + WorkingClassM4549 + WorkingClassM5054 + WorkingClassF1517 
                                             + WorkingClassF1819 + WorkingClassF20 + WorkingClassF21 
                                             + WorkingClassF2224 + WorkingClassF2529 + WorkingClassF3034 
                                             + WorkingClassF3539 + WorkingClassF4044 + WorkingClassF4549 
                                             + WorkingClassF5054)/TotalPop), 0),
         pctMarried = ifelse(TotalPop > 0, ((FemaleMarried + MaleMarried) / TotalPop), 0),
         pctWhite = ifelse(TotalPop > 0, Whites / TotalPop, 0),
         pctOtherRace = ifelse(TotalPop > 0, ((Blacks + AmInd +Asian
                                               + Hawaiian + Hispanic) / TotalPop), 0),
         year = "2017") %>%
  dplyr::select(-Whites,-FemaleMarried,-MaleMarried,-Blacks,-AmInd,-Asian,-Hawaiian,-Hispanic,
                -WorkingClassM1517,-WorkingClassM1819,-WorkingClassM20,-WorkingClassM21,-WorkingClassM2224,-WorkingClassM2529, 
                -WorkingClassM3034,-WorkingClassM3539,-WorkingClassM4044,-WorkingClassM4549,-WorkingClassM5054,-WorkingClassF1517, 
                -WorkingClassF1819,-WorkingClassF20,-WorkingClassF21,-WorkingClassF2224,-WorkingClassF2529,-WorkingClassF3034,
                -WorkingClassF3539,-WorkingClassF4044,-WorkingClassF4549,-WorkingClassF5054)

# --- Combining 09 and 17 data ----

finalTract <- rbind(tracts2009,tracts2017)

#---------------------------------------------

x <- vector(mode='list', length = 105)
y <- vector(mode='list', length = 105)
library(rlist)
spli <- strsplit(sfo$Location, ",")
for (val in spli){
  print(val)
  print(val[[1]])
  print(val[[2]])
  x <- append(x, val[[1]])
  y <- append(y, val[[2]])
  
}
sfo_new <- 
  sfo %>%
  mutate(X = x[106:210],
         Y = y[106:210]) %>%
  dplyr::select(-Abbreviation, -Location, -Description)
crs_tract09 <- st_crs(tracts2009)
sfo_spatial <- st_as_sf(sfo_new, coords = c("X","Y"), crs = 4326, agr = "constant")
sfo_spatial <- sfo_spatial %>% st_transform(st_crs(tracts2009))

ggplot() + 
  geom_sf(data=st_union(tracts2009)) +
  geom_sf(data=sfo_spatial, 
          aes(colour = Line_colour), 
          show.legend = "point", size= 2) +
  labs(title="Bart Stops", 
       subtitle="Bay Area, CA", 
       caption="Figure 2.5") +
  mapTheme()

# Create buffers (in feet - note the CRS) around Septa stops -
# Both a buffer for each stop, and a union of the buffers...
# and bind these objects together

bartBuffers <- 
  rbind(
    st_buffer(sfo_spatial, 2640) %>%
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(sfo_spatial, 2640)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

# Let's examine both buffers by making a small multiple
# "facet_wrap" plot showing each

ggplot() +
  geom_sf(data=bartBuffers) +
  geom_sf(data=sfo_spatial, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "Figure 2.6") +
  mapTheme()

# Create an sf object with ONLY the unioned buffer
buffer <- filter(bartBuffers, Legend=="Unioned Buffer")

# Clip the 2009 tracts ... by seeing which tracts intersect (st_intersection)
# with the buffer and clipping out only those areas
clip <- 
  st_intersection(buffer, tracts2009) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip")
A <- 
  ggplot() +
  geom_sf(data = clip) +
  theme(plot.title = element_text(size=22))

# Do a spatial selection to see which tracts touch the buffer
selection <- 
  tracts2009[buffer,] %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Spatial Selection")
B <- 
  ggplot() +
  geom_sf(data = selection) +
  theme(plot.title = element_text(size=22))

# Do a centroid-in-polygon join to see which tracts have their centroid in the buffer
# Note the st_centroid call creating centroids for each feature
selectCentroids <-
  st_centroid(tracts2009)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts2009, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")
C <- 
  ggplot() +
  geom_sf(data = selectCentroids) +
  theme(plot.title = element_text(size=22))
# ---- Indicator Maps ----

# We do our centroid joins as above, and then do a "disjoin" to get the ones that *don't*
# join, and add them all together.
# Do this operation and then examine it.
# What represents the joins/doesn't join dichotomy?
# Note that this contains a correct 2009-2017 inflation calculation

finalTract.group <- 
  rbind(
    st_centroid(finalTract)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(finalTract) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(finalTract)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(finalTract) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.14, MedRent)) 

#table 
finalTract.Summary <- 
  st_drop_geometry(finalTract.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Working = mean(pctWorking, na.rm = T),
            Percent_Married = mean(pctMarried, na.rm = T),
            Percent_MedIn = mean(MedHHInc, na.rm = T))

kable(finalTract.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.2")

#plot
finalTract.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", nrow=2) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")


