
##Revisar eventos reproductivos

unique(dates(subset(fts,evento %in% c("B01.P01.C01_E8"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P02.C02_E2"))$f1))
unique(dates(subset(fts,evento %in% c("B02.P01.C07_E28"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P03.C02_E19"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P05.LEE02_E47"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P02.C09_E15"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P03.C09_E11"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P03.C10_E6"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P03.C05_E69"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P04.C10_E4"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P04.C09_E8"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P04.C09_E178"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P05.IS14_E25"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P05.IS14_E26"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P06.IS-15_E53"))$f1))
unique(dates(subset(fts,evento %in% c("B01.P06.IS-15_E134"))$f1))

##
(load("~/mi.git/clima_municipios_Venezuela/Rdata/climaZulia.rda"))

ec.xy[grep("PUEBLO VIEJO",ec.xy$ESTACION),]

ec.ss <- subset(ec.xy,SERIAL %in% subset(precipitacion,municipio %in% c("Lagunillas","Valmore Rodríguez"))$serial)

plot(ec.ss)
points(latitud~longitud,camaras,col=2)
text(ec.ss,ec.ss@data$ESTACION)


subset(precipitacion,serial %in% "1083")
subset(ec.xy,SERIAL %in% subset(temperatura,municipio %in% c("Lagunillas","Valmore Rodríguez"))$serial)

barplot(with(subset(precipitacion,serial %in% "1083"),aggregate(precip,by=list(mes),median,na.rm=T))$x)
lines(1:12,with(subset(temperatura,serial %in% "1083"),aggregate(temperatura,by=list(mes),median,na.rm=T))$x)

qry <- merge(with(subset(precipitacion,serial %in% "1083"),aggregate(precip,by=list(año),sum,na.rm=T)),with(subset(precipitacion,serial %in% "1083" & mes %in% 5:10),aggregate(precip,by=list(año),sum,na.rm=T)),by="Group.1")
median(qry$x.y/qry$x.x)


hist(with(subset(precipitacion,serial %in% "1083"),aggregate(precip,by=list(año),sum,na.rm=T))$x)
