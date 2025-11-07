source("./code/not_functions/libraries.R")
source("./code/functions/f.read_excel_allsheets.R")

# --- Data Loading and Preparation ---

data.list<-f.read_excel_allsheets("./data/extern/verwerkt_in_excel/Pumping stations Belgium_Overview.xlsx")
data1<-data.list[[1]]
data2<-data.list[[2]]
colnames(data1)<-make.names(colnames(data1))
colnames(data2)<-make.names(colnames(data2))

# change the name of "Discharge into freshwater or estuary/sea" into "name water body"
data1<-data1 %>%
  rename("name.water.body"="Discharge.into.freshwater.or.estuary.sea") %>%
  left_join(data2[,c("Name","Discharge.into.freshwater.or.estuary.sea")],by="Name") %>%
  group_by(Name) %>%
  mutate(Total.capacity..m3.per.h.= sum(Individual.capacity..m3.per.h., na.rm = TRUE)) %>%
  ungroup()

# Clean up data2 and convert Total.capacity to numeric
data2<-data2 %>%
  select(-Individual.capacity..m3.per.h.) %>%
  # Handle potential issues like "/" in the capacity column during conversion
  mutate(Total.capacity..m3.per.h.=as.numeric(gsub("/","0",Total.capacity..m3.per.h.)))

# Function to add WGS84 coordinates (Latitude/Longitude)
f.add.transformed.coordinates<-function(data){
  points_sf <- st_as_sf(data, coords = c("X", "Y"), crs = 31370)
  # Transform to WGS 1984 (EPSG:4326)
  points_wgs84 <- st_transform(points_sf, 4326)
  # Convert back to dataframe
  df_wgs84 <- as.data.frame(st_coordinates(points_wgs84))  # Extract transformed X (lon) and Y (lat) 
  colnames(df_wgs84) <- c("Longitude", "Latitude")  # Rename columns for clarity
  data <- cbind(data, df_wgs84)  # Add back to original dataframe
  return(data)
}

data1<-f.add.transformed.coordinates(data1)
data2<-f.add.transformed.coordinates(data2)

write.csv(data1,"./data/intern/pumps_belgium.csv",row.names = FALSE)
write.csv(data2,"./data/intern/pumping_stations_belgium.csv",row.names = FALSE)

# --- Mapping Configuration ---

# The layer control parameter is no longer used for toggling but kept for context
control.parameter="Pump.type" 

# Define all parameters you want to appear in the POPUP (using the user's specific list)
label.cols <- c(
  "Name", 
  "Total.capacity..m3.per.h.", 
  "Number.of.pumps", 
  "Pump.type", 
  "Discharge.into.freshwater.or.estuary.sea"
)

# Define user-friendly names for the popup labels, matching the order above
label.names <- c(
  "Station Name", 
  "Total Capacity (m³/h)", 
  "Number of Pumps", 
  "Pump Type", 
  "Discharge Body"
)

# 1. Prepare Spatial Data Frame with all necessary columns
coords <- cbind(long = data2[,"Longitude"],lat = data2[,"Latitude"])
coords_SP <- SpatialPointsDataFrame(
  coords,
  data = data.frame(data2[, label.cols]), 
  proj4string=CRS("+init=epsg:4326")
)

# 2. Create the combined Samples_text (Popup content) using all parameters
Samples_text <- paste0(
  "<b>", label.names[1], "</b>: ", coords_SP@data[, label.cols[1]], "<br>",
  "<b>", label.names[2], "</b>: ", coords_SP@data[, label.cols[2]], "<br>",
  "<b>", label.names[3], "</b>: ", coords_SP@data[, label.cols[3]], "<br>",
  "<b>", label.names[4], "</b>: ", coords_SP@data[, label.cols[4]], "<br>",
  "<b>", label.names[5], "</b>: ", coords_SP@data[, label.cols[5]]
)

# 3. Create the visible marker label (Tooltip on hover) - short and informative
marker_label <- paste0(
  "Station Name: ", coords_SP@data$Name
)

# Define color palette based on capacity
beatCol <- colorNumeric(palette = rev(RColorBrewer::brewer.pal(11, 'RdYlGn')), 
                        domain = coords_SP@data$Total.capacity..m3.per.h.)

# Define the HTML title element (assuming htmltools is available)
title_html <- htmltools::tags$div(
  style = "
    position: fixed; 
    top: 10px; 
    left: 70px; 
    z-index: 999; 
    background: rgba(255, 255, 255, 0.9); 
    padding: 5px 10px; 
    border-radius: 5px; 
    box-shadow: 0 0 5px rgba(0,0,0,0.5);
  ",
  htmltools::tags$strong("Pumping Stations Belgium Overview")
)

# 4. Generate the Leaflet Map
RangePlot<-leaflet() %>%
  # 1. Base Layers - Added both tiles, with groups defined for toggling
  addProviderTiles(providers$CartoDB.Positron, group = "Street Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  
  # 2. Add the title control
  addControl(title_html) %>%
  
  # 3. Add Clustered Markers (Single layer)
  addCircleMarkers(
    data=coords_SP,
    radius=8,
    fillColor = ~beatCol(coords_SP@data$Total.capacity..m3.per.h.),
    fillOpacity=0.8,
    weight=1,
    color='black',
    popup=Samples_text,           
    label=marker_label,           
    labelOptions = labelOptions(noHide = F, textOnly = TRUE),
    clusterOptions = markerClusterOptions(),
    layerId = coords_SP@data$Name, # Required for Search
    group = "Pumping Stations"     # Assign a generic group name for search target
  ) %>%
  
  # 4. Add Legend (must be after palette is defined)
  addLegend(
    pal = beatCol, 
    values = coords_SP@data$Total.capacity..m3.per.h.,
    title = "Total Capacity (m³/h)",
    position = "bottomright"
  ) %>%
  
  # 5. Add Search Features (must be after markers are added)
  leaflet.extras::addSearchFeatures(
    targetGroups = "Pumping Stations", # Target the single, generic marker group
    options = leaflet.extras::searchFeaturesOptions(
      zoom = 16, 
      openPopup = TRUE, 
      textPlaceholder = "Search by Station Name..."
    )
  ) %>%
  
  # 6. Add Layer Controls (Base Map Selector)
  addLayersControl(
    baseGroups = c("Street Map", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

print(RangePlot)