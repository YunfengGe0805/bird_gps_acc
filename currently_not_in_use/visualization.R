Athi.Map <-
  tm_basemap(c("Esri.WorldImagery",
               "OpenStreetMap")) +
  tm_raster() + 
  tm_shape(SW.sf,
           name = paste("Shearwater GPS (n =", length(unique(SW.sf$id)),")")) +
  tm_dots(size = 0.025,
          title = "Shearwater ID",
          col = "id",
          alpha = 0.5)
Athi.Map
??Athi.Map
?tm_basemap
