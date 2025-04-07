source("./code/not_functions/libraries.R")
source("./code/functions/f.read_excel_allsheets.R")

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

data2<-data2 %>%
  select(-Individual.capacity..m3.per.h.) %>%
  mutate(Total.capacity..m3.per.h.=as.numeric(gsub("/","0",Total.capacity..m3.per.h.)))

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

control.parameter="Pump.type"
label.parameter="Total.capacity..m3.per.h."

coords <- cbind(long = data2[,"Longitude"],lat = data2[,"Latitude"])
coords_SP <- SpatialPointsDataFrame(coords,data = data.frame(data2[,c(control.parameter,label.parameter)]), proj4string=CRS("+init=epsg:4326"))

Samples_text<-paste0("<b>",control.parameter,"</b>: ",coords_SP@data[,control.parameter],
                     "<br><b>",label.parameter,"</b>: ",coords_SP@data[,label.parameter])

beatCol <- colorNumeric(palette = rev(RColorBrewer::brewer.pal(11, 'RdYlGn')), 
                        domain = coords_SP@data[, label.parameter])

RangePlot<-leaflet()%>%
  addTiles()%>%
  addCircleMarkers(data=coords_SP,radius=8,fillColor = ~beatCol(coords_SP@data[,label.parameter]),fillOpacity=0.8,weight=1,color='black',popup=Samples_text,label=coords_SP@data[,label.parameter],labelOptions = labelOptions(noHide = F),group=coords_SP@data[,control.parameter])%>%
  addLayersControl(overlayGroups = coords_SP@data[,control.parameter],options = layersControlOptions(collapsed = FALSE))
print(RangePlot)

