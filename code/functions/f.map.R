f.map <- function(data,X,Y,crs,control.parameter,label.parameter,plot=FALSE){
  
  coords <- cbind(long = data[,X],lat = data[,Y])
  
  coords_SP <- SpatialPointsDataFrame(coords,data = data.frame(data[,c(control.parameter,label.parameter)]), proj4string = CRS(crs))
  coords_SP <- spTransform(coords_SP, CRS("+init=epsg:4326"))
  
  df <- data.frame(x=coordinates(coords_SP)[,1], y=coordinates(coords_SP)[,2], coords_SP@data)
  write.csv(df, "./data/locaties/intern/coordinaten_overzicht_WSG.csv", row.names=FALSE)
  
  names(coords_SP)[1]<-control.parameter
  names(coords_SP)[2]<-label.parameter

  Samples_text<-paste0("<b>",control.parameter,"</b>: ",coords_SP@data[,control.parameter],
                       "<br><b>",label.parameter,"</b>: ",coords_SP@data[,label.parameter])
  
  beatCol <- colorFactor(palette = 'RdYlGn', coords_SP@data[,control.parameter])

  RangePlot<-leaflet()%>%
    addTiles()%>%
    addCircleMarkers(data=coords_SP,radius=8,fillColor = ~beatCol(coords_SP@data[,control.parameter]),fillOpacity=0.8,weight=1,color='black',popup=Samples_text,label=coords_SP@data[,label.parameter],labelOptions = labelOptions(noHide = F),group=coords_SP@data[,control.parameter])%>%
    addLayersControl(overlayGroups = coords_SP@data[,control.parameter],options = layersControlOptions(collapsed = FALSE))
print(RangePlot)
  if (plot==FALSE){return(coords_SP)} else{return(RangePlot)}
}

f.map.combo<-function(map.list,names){
  text.list<-list()
  for (i in 1:length(map.list)){
    text.list[[i]]<-paste0("<b>",names(map.list[[i]]@data)[1],"</b>: ",map.list[[i]]@data[,1],"<br><b>",names(map.list[[i]]@data)[2],"</b>: ",map.list[[i]]@data[,2])
  }
  bounds <- map.list[[1]] %>% 
    st_bbox() %>% 
    as.character()
  beatCol <- colorFactor(palette = 'RdYlGn', map.list[[1]]@data[,1])
  if (length(map.list)==2){
    RangePlot<-leaflet()%>%
      addTiles()%>%
      addCircleMarkers(data=map.list[[1]],radius=8,fillColor=~beatCol(map.list[[1]]@data[,1]),color='black',fillOpacity=0.3,weight=1,popup=text.list[[1]],label=names[1],labelOptions = labelOptions(noHide = F),group = map.list[[1]]@data[,1])%>%
      addCircleMarkers(data=map.list[[2]],radius=8,fillColor="black",color='black',fillOpacity=0.8,weight=1,popup=text.list[[2]],label=names[2],labelOptions = labelOptions(noHide = F),group = map.list[[2]]@data[,1])%>%
      addLayersControl(overlayGroups = map.list[[2]]@data[,1],options = layersControlOptions(collapsed = FALSE))%>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
  }
  if (length(map.list)==3){
    RangePlot<-leaflet()%>%
      addTiles()%>%
      addCircleMarkers(data=map.list[[1]],radius=8,fillColor=~beatCol(map.list[[1]]@data[,1]),color='black',fillOpacity=0.8,weight=1,popup=text.list[[1]],label=names[1],labelOptions = labelOptions(noHide = F),group = map.list[[1]]@data[,1])%>%
      addCircleMarkers(data=map.list[[2]],radius=8,fillColor="black",color='black',fillOpacity=0.8,weight=1,popup=text.list[[2]],label=names[2],labelOptions = labelOptions(noHide = F),group = map.list[[2]]@data[,1])%>%
      addCircleMarkers(data=map.list[[3]],radius=8,fillColor="blue",color='black',fillOpacity=0.8,weight=1,popup=text.list[[3]],label=names[3],labelOptions = labelOptions(noHide = F),group = map.list[[3]]@data[,1])%>%
      addLayersControl(overlayGroups = map.list[[2]]@data[,1],options = layersControlOptions(collapsed = FALSE))%>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
  }
  if (length(map.list)==4){
    RangePlot<-leaflet()%>%
      addTiles()%>%
      addCircleMarkers(data=map.list[[1]],radius=8,fillColor=~beatCol(map.list[[1]]@data[,1]),color='black',fillOpacity=0.8,weight=1,popup=text.list[[1]],label=names[1],labelOptions = labelOptions(noHide = F),group = map.list[[1]]@data[,1])%>%
      addCircleMarkers(data=map.list[[2]],radius=8,fillColor="black",color='black',fillOpacity=0.8,weight=1,popup=text.list[[2]],label=names[2],labelOptions = labelOptions(noHide = F),group = map.list[[2]]@data[,1])%>%
      addCircleMarkers(data=map.list[[3]],radius=8,fillColor="blue",color='black',fillOpacity=0.8,weight=1,popup=text.list[[3]],label=names[3],labelOptions = labelOptions(noHide = F),group = map.list[[3]]@data[,1])%>%
      addCircleMarkers(data=map.list[[4]],radius=8,fillColor="purple",color='black',fillOpacity=0.8,weight=1,popup=text.list[[4]],label=names[4],labelOptions = labelOptions(noHide = F),group = map.list[[4]]@data[,1])%>%
      addLayersControl(overlayGroups = map.list[[2]]@data[,1],options = layersControlOptions(collapsed = FALSE))%>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
  }
  return(RangePlot)
}