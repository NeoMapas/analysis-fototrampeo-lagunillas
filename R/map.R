

## mapa con las cámaras
plot(latitud~longitud,subset(camaras,latitud>0))

ll <- with(subset(camaras,longitud<0),aggregate(data.frame(longitud,latitud),list(camara=UEM),median))

plot(latitud~longitud,ll,pch=1)
with(ll,text(longitud,latitud,camara,cex=.85,adj=-.5))

## mejor hacer en QGIS
## agregar eventos de avistamientos y museo?

require(OpenStreetMap)
nm <- c("stamen-terrain","hillshade",
        "apple-iphoto","osm","esri", "esri-topo", ##
        "osm-bw","bing","skobbler",  "nps",
        "stamen-toner", "maptoolkit-topo", ##
        "mapbox","stamen-watercolor")

range(camaras$latitud[camaras$latitud>0])
range(camaras$longitud[camaras$longitud<0])

## Probamos diferentes representaciones de Burro Negro
par(mfrow=c(3,4))
for(i in 1:length(nm)){
    map <- openmap(c(10.24,-71.07),
                   c(10.17,-71.01),
                   minNumTiles=9,type=nm[i])
    plot(map)
}

measurements::conv_unit(range(camaras$longitud), to = 'deg_min_sec', from = 'dec_deg')
measurements::conv_unit(range(camaras$latitud), to = 'deg_min_sec', from = 'dec_deg')


measurements::conv_unit(c(10.15,10.24), to = 'deg_min_sec', from = 'dec_deg')
measurements::conv_unit(c(-71.07,-71), to = 'deg_min_sec', from = 'dec_deg')

##Microsoft Bing Maps Imagery Service API’
map <- openmap(c(10.24,-71.07),
                   c(10.15,-71),
                   minNumTiles=9,type="bing")



require(sp)
cmr <- with(subset(camaras,longitud<0),aggregate(data.frame(longitud,latitud),list(camara=UEM),median))
coordinates(cmr) <- c("longitud","latitud")
proj4string(cmr) <- "+datum=WGS84 +proj=longlat"
cmr.xy <- spTransform(cmr,map$tiles[[1]]$projection)

grilla <- expand.grid(x=seq(-71.08,-71.00,by=0.02),y=seq(10.14,10.28,by=0.02))
coordinates(grilla) <- c("x","y")
proj4string(grilla) <- "+datum=WGS84 +proj=longlat"
grilla.xy <- spTransform(grilla,map$tiles[[1]]$projection)


area1 <- rgeos::gConvexHull(cmr.xy[-(6:8),])
area2 <- rgeos::gConvexHull(cmr.xy[(6:8),])


require(raster)

##vzla <- shapefile("/opt/gisdata/vectorial/GADM/VEN_adm1.shp")

vzla <- shapefile("/opt/gisdata/vectorial/ecoSIG/division_pol.shp")
WBS <- shapefile("/opt/gisdata/vectorial/TMworldborders/TM_WORLD_BORDERS-0.3.shp")

require(foreign)
  gaz.ven <- read.dbf("~/tmp/Postgrado/VEN.dbf",as.is=T)
gaz.ven$lon <- as.numeric(gaz.ven$LONG)
gaz.ven$lat <- as.numeric(gaz.ven$LAT)
gaz.zul <- subset(gaz.ven,ADM1 %in% "ZULIA")
coordinates(gaz.zul) <- c("lon","lat")
proj4string(gaz.zul) <- "+datum=WGS84 +proj=longlat"
gaz.xy <- spTransform(gaz.zul,map$tiles[[1]]$projection)

plot(map)
points(grilla.xy,cex=2,col="grey77",lwd=2,pch=3)
 text(coordinates(grilla.xy)[c(15,20,25),1],coordinates(grilla.xy)[c(15,20,25),2],sprintf("%0.2f° N",coordinates(grilla)[c(15,20,25),2]),adj=1.2,col="grey77",cex=1.2)
text(coordinates(grilla.xy)[c(27:29),1],coordinates(grilla.xy)[c(27:29),2]-200,sprintf("%0.2f° O",coordinates(grilla)[c(27:29),1]),col="grey77",cex=1.2)

points(cmr.xy,pch=19,col=2)
text(-7907982,1141189,"Embalse\nPueblo\nViejo",font=2)
text(-7906147,1145288,"Río Chiquito",font=2,col="white")
text(-7904721,1139903,"Río Grande",font=2,col="white")

##text(subset(gaz.xy,NAME %in% c("Ballenato")),"Plan Bonito\n(Ballenato)")
text(-7907982,1136833,"Plan Bonito\n(Ballenato)",col="white")
text(-7910348,1137770,"vía Campo Lara",col="white")

ss <- c(4,6:8)
text(cmr.xy@coords[ss,1]+700,cmr.xy@coords[ss,2]+300,cmr.xy$camara[ss],font=2,cex=1.3)
segments(cmr.xy@coords[ss,1]+100,cmr.xy@coords[ss,2]+100,
         cmr.xy@coords[ss,1]+370,cmr.xy@coords[ss,2]+300)
ss <- 2
text(cmr.xy@coords[ss,1]-700,cmr.xy@coords[ss,2]+300,cmr.xy$camara[ss],font=2,cex=1.3)
segments(cmr.xy@coords[ss,1]-100,cmr.xy@coords[ss,2]+100,
         cmr.xy@coords[ss,1]-370,cmr.xy@coords[ss,2]+300)
ss <- c(1,5)
text(cmr.xy@coords[ss,1]-700,cmr.xy@coords[ss,2]-300,cmr.xy$camara[ss],font=2,cex=1.3)
segments(cmr.xy@coords[ss,1]-100,cmr.xy@coords[ss,2]-100,
         cmr.xy@coords[ss,1]-370,cmr.xy@coords[ss,2]-300)
ss <- c(3)
text(cmr.xy@coords[ss,1]+700,cmr.xy@coords[ss,2]-300,cmr.xy$camara[ss],font=2,cex=1.3)
segments(cmr.xy@coords[ss,1]+100,cmr.xy@coords[ss,2]-100,
         cmr.xy@coords[ss,1]+370,cmr.xy@coords[ss,2]-300)
ss <- c(9)
text(cmr.xy@coords[ss,1],cmr.xy@coords[ss,2]-700,cmr.xy$camara[ss],font=2,cex=1.3)
segments(cmr.xy@coords[ss,1],cmr.xy@coords[ss,2]-100,
         cmr.xy@coords[ss,1],cmr.xy@coords[ss,2]-500)
ss <- c(10)
text(cmr.xy@coords[ss,1]+700,cmr.xy@coords[ss,2],cmr.xy$camara[ss],font=2,cex=1.3)
segments(cmr.xy@coords[ss,1]+100,cmr.xy@coords[ss,2],
         cmr.xy@coords[ss,1]+370,cmr.xy@coords[ss,2])

segments(-7906920,1136288,-7906920+3000,1136288,lwd=5,col="black")
segments(-7906920+1000,1136288,-7906920+2000,1136288,lwd=5,col="white")
text(-7906920+0,1136288+200,"0")
text(-7906920+1000,1136288+200,"1 km")
text(-7906920+2000,1136288+200,"2 km")
## text(coordinates(grilla.xy)[,1],coordinates(grilla.xy)[,2],1:length(grilla.xy))


par(fig = c(0.1,0.4, 0.75, 0.95), new = T,mar=c(0,0,0,0))
plot(vzla,border=NA,col=NA,bg="white",ylim=c(0,13))
plot(WBS,add=T,border="grey33")
plot(vzla,border="grey90",col="grey77",add=T)
points(grilla,lwd=1,cex=1.2,pch=15,col="black")
text(-64.0,8.4,"Venezuela",cex=1)
text(-70.4,5.3,"Colombia",cex=.85)
text(-61.8,3.0,"Brasil",cex=.85)
text(-67.1,12.4,"Mar Caribe",cex=.8)
text(-71.65,8.5,"Lago de\nMaracaibo",cex=.7)

abs(map$bbox$p1[2]-map$bbox$p2[2])/abs(map$bbox$p1[1]-map$bbox$p2[1])

dev.copy(png,file="20180816_BurroNegro_Fig_Ubicacion.png",width=450,height=585,pointsize=12)
dev.off()
