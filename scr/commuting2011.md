



This report was automatically generated with the R package **knitr**
(version 1.30).


```r
---
title: "Commuting between OA, 2011"
output:
  html_document:
    toc: true
    toc_float: true
knit: (function(inputFile, encoding) {
    rmarkdown::render(inputFile, encoding = encoding, output_dir = "../outputs/")
  })
---
```

```
## Error: <text>:12:0: unexpected end of input
## 10: ---
## 11: 
##    ^
```

```r
library(igraph)
library(knitr)
# library(corrplot)
# library(corrgram)
library(rgdal)
library(tidyverse)
#library(spdplyr)
# library(geojsonio)
library(stplanr)
library(ggplot2)
#library(leaflet)
#library(SpatialPosition)
library(stargazer)
library(rprojroot)
library(cleangeo)
library(tmap)

# This is the project path
path <- find_rstudio_root_file()
```

## Commuting flows


```r
path.data <- paste0(path, "/data/wf02ew_oa_v1.csv" )

commuting <- read_csv(path.data, 
                      col_names = F) %>% 
  rename(o = X1,
         d = X2,
         weight = X3) %>%              # commuting flows
  filter(substr(d, 1, 1) == "E" |      # Keep England, 
           substr(o, 1, 1) == "E") %>% # drop Wales, Scotland, NI, and 
                                       # non-geographical OD codes 
                                       # OD0000001 = Mainly work at or from home
                                       # OD0000002 = Offshore installation
                                       # OD0000003 = No fixed place
                                       # OD0000004 = Outside UK
  glimpse()
```

```
## 
## ── Column specification ───────────────────────────────────────────────────────────────────
## cols(
##   X1 = col_character(),
##   X2 = col_character(),
##   X3 = col_double()
## )
```

```
## Rows: 15,575,954
## Columns: 3
## $ o      <chr> "E00000001", "E00000001", "E00000001", "E00000001", "E00000001…
## $ d      <chr> "E33025329", "E33027441", "E33028825", "E33028827", "E33028853…
## $ weight <dbl> 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 2, 1,…
```

The below is with to exclude the intra-OA flows. 
**NOT TO RUN FOR NOW*


```r
commuting.intra <- commuting %>%
  dplyr::filter(`Area of usual residence` == `Area of workplace`)
#commuting <- commuting %>%
#  dplyr::filter(`Area of usual residence` != `Area of workplace`)
```

Given the size of the network I am using the `fast and greedy` community detection algorithm. 
The optimal number of communities is $11$.
The problem is that when I try to force $communities$ = $constituencies$ the communities after the $11th$ have only $1$ $OA$.
Therefore, I present the optimal solution here.



```r
net <-graph_from_data_frame(commuting, directed = TRUE, vertices = NULL)

# I need to look on the sensitivity of the below
# net.un <- as.undirected(net,
#                         mode=c("mutual"),
#                         edge.attr.comb = igraph_opt("edge.attr.comb"))

net.un <- as.undirected(net,
                        mode="collapse",
                        edge.attr.comb = igraph_opt("edge.attr.comb"))

# fast and greedy
fg <- net.un %>% 
  fastgreedy.community() 

V(net.un)$group <- membership(fg) # assign membership to vertices

sizes(fg)
```

```
## Community sizes
##     1     2     3     4     5     6     7     8     9    10    11 
## 26775 29454 25159 53512 34960 31755 14434 10914  4961  1246   702
```

```r
# fg$membership <- cut_at(fg, 543) 
# sizes(fg)
# for cut_at 543, the first 11 communities have > 1

names <- vertex_attr(net.un)[1]

df <- cbind(as.data.frame(names),
            fg$membership) %>% 
  rename('community' = 'fg$membership')

path.out <- paste0(path, "/outputs/fg.csv")
write_csv(df, path.out)
```

Other community detection attempts.

**Not to run**


```r
# not hierarchical, I can't define cut_at
# l <- net.un %>% 
#   cluster_louvain() %>% 
#   cut_at(no = 543)



# Use spinglass to create a set number of communities
sg <- net %>% 
  cluster_spinglass(spins = 543) # 543 = the  number of constituencies allocated
                                 # to England for the 2023 Review

# Use hierarchical methods and cut_at to create a set number of communities 
walk <- net %>% 
  cluster_walktrap() %>% cut_at(no = 543) 

eb <- net %>% cluster_edge_betweenness() %>% cut_at(no = 10)


membership(net.l)
length(fg)
modularity(net.l)
print(net.l)
sizes(net.l)
is_hierarchical(net.l)

plot_dendrogram(net.l)

plot(lc, g)
```

## Mapping the communities


```r
# OA from https://geoportal.statistics.gov.uk/datasets/ons::output-areas-december-2011-boundaries-ew-bgc-1/about

path.oa <- paste0(path, "/data/Output_Areas__December_2011__Boundaries_EW_BGC.geojson")

oa <- readOGR(path.oa)
```

OGR data source with driver: GeoJSON 
Source: "/home/nw19521/projects/boundaries/data/Output_Areas__December_2011__Boundaries_EW_BGC.geojson", layer: "Output_Areas__December_2011__Boundaries_EW_BGC"
with 181408 features
It has 5 fields

```r
head(oa@data)
```

  OBJECTID    OA11CD   LAD11CD Shape__Area Shape__Length
0        1 E00000001 E09000001    6944.543      421.8616
1        2 E00000003 E09000001    4608.427      307.5489
2        3 E00000005 E09000001    8565.514      385.2048
3        4 E00000007 E09000001   76000.431     1408.6249
4        5 E00000010 E09000001    2102.877      215.2720
5        6 E00000012 E09000001    3319.050      233.9672

```r
# Let's try to map our output. We need the local authorities shape file we have 
# already loaded

# Before we do any further analysis we make sure to convert the la object to WGS84 
# Longitude / Latitude Coordinate System.
wgs84 = '+proj=longlat +datum=WGS84'
oa <- spTransform(oa, CRS(wgs84) )

# This is the @data element of the la spatial object
head(oa@data)
```

  OBJECTID    OA11CD   LAD11CD Shape__Area Shape__Length
0        1 E00000001 E09000001    6944.543      421.8616
1        2 E00000003 E09000001    4608.427      307.5489
2        3 E00000005 E09000001    8565.514      385.2048
3        4 E00000007 E09000001   76000.431     1408.6249
4        5 E00000010 E09000001    2102.877      215.2720
5        6 E00000012 E09000001    3319.050      233.9672

```r
# read data in case the community detection is not run
df <- read_csv(path.out)

# Let's merge it with the communities data frame
oa@data <- merge(oa@data, df, by.x = "OA11CD", by.y = "name", all.x = T)
sum(is.na(oa@data$community))
```

[1] 583

```r
oa@data$community <- as.factor(oa@data$community)

# clean ploygons
oa <- clgeo_Clean(oa)

path.map <- paste0(path, "/outputs/fg_commuting.png")

fg.commuting <- tm_shape(oa) + 
  tm_fill("community", 
          title= "Communities (fast and greedy)") + 
  tm_layout("Commuting flows 2011")

tmap_save(fg.commuting, path.map)
```

```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 4.0.3 (2020-10-10)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 18.04.5 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1
## 
## locale:
##  [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
##  [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
##  [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] tmap_3.3-2      cleangeo_0.2-4  maptools_1.0-2  rgeos_0.5-5    
##  [5] rprojroot_2.0.2 stargazer_5.2.2 stplanr_0.8.0   forcats_0.5.0  
##  [9] stringr_1.4.0   dplyr_1.0.2     purrr_0.3.4     readr_1.4.0    
## [13] tidyr_1.1.2     tibble_3.0.4    ggplot2_3.3.3   tidyverse_1.3.0
## [17] rgdal_1.5-18    sp_1.4-4        knitr_1.30      igraph_1.2.6   
## 
## loaded via a namespace (and not attached):
##  [1] fs_1.5.0           sf_1.0-1           lubridate_1.7.9.2  RColorBrewer_1.1-2
##  [5] httr_1.4.2         tools_4.0.3        backports_1.2.1    utf8_1.1.4        
##  [9] R6_2.5.0           KernSmooth_2.23-18 DBI_1.1.0          colorspace_2.0-0  
## [13] raster_3.4-5       withr_2.3.0        tidyselect_1.1.0   leaflet_2.0.3     
## [17] curl_4.3           compiler_4.0.3     leafem_0.1.6       cli_2.2.0         
## [21] rvest_0.3.6        xml2_1.3.2         scales_1.1.1       classInt_0.4-3    
## [25] digest_0.6.27      foreign_0.8-79     base64enc_0.1-3    dichromat_2.0-0   
## [29] pkgconfig_2.0.3    htmltools_0.5.1.1  dbplyr_2.0.0       htmlwidgets_1.5.3 
## [33] rlang_0.4.10       readxl_1.3.1       rstudioapi_0.13    generics_0.1.0    
## [37] jsonlite_1.7.2     crosstalk_1.1.0.1  magrittr_2.0.1     s2_1.0.6          
## [41] geosphere_1.5-10   Rcpp_1.0.5         munsell_0.5.0      fansi_0.4.1       
## [45] abind_1.4-5        lifecycle_0.2.0    stringi_1.5.3      leafsync_0.1.0    
## [49] tmaptools_3.1-1    grid_4.0.3         parallel_4.0.3     crayon_1.3.4      
## [53] lattice_0.20-41    haven_2.3.1        stars_0.5-3        hms_0.5.3         
## [57] ps_1.5.0           pillar_1.4.7       codetools_0.2-17   wk_0.5.0          
## [61] reprex_0.3.0       XML_3.99-0.6       glue_1.4.2         evaluate_0.14     
## [65] modelr_0.1.8       png_0.1-7          vctrs_0.3.6        cellranger_1.1.0  
## [69] gtable_0.3.0       assertthat_0.2.1   xfun_0.19          lwgeom_0.2-5      
## [73] broom_0.7.3        e1071_1.7-4        class_7.3-17       viridisLite_0.3.0 
## [77] units_0.6-7        ellipsis_0.3.1
```

```r
Sys.time()
```

```
## [1] "2021-07-22 14:42:09 BST"
```

