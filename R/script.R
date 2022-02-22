##R --vanilla
require(chron)
require(vegan)
require(gdata)

work.dir <- "~/tmp/Therya_MamiferosBurroNegro/"
if (!dir.exists(work.dir))
  dir.create(work.dir)

setwd(work.dir)


con <- url("https://github.com/NeoMapas/fototrampeo-lagunillas/blob/dont-panic/MVcamarasBurroNegro.rda?raw=true")
print(load(con))
close(con) # url() always opens the connection

## Todas las cámaras // All cameras
camaras[order(camaras$URA,camaras$PMY),]

## Esfuerzo de muestreo por bloque y periodo // sampling effort

sum(camaras$f2-camaras$f1)

with(camaras,aggregate(f2-f1,list(bloque=URA,periodo=PMY),sum))

## Datos disponibles // available data

length(unique(fts$evento))
length(unique(notas$evento))
length(unique(ids$evento))

table(unique(fts$evento) %in% ids$evento)
#table(notas$evento)
#table(subset(notas,evento %in% ids$evento)$anotación)

table(notas$equipo)
table(notas$tipo)

## esfuerzo de muestreo
fmt <- list(dates="d/m/y",times="h:m:s")

sfrz <- chron(dates.="01/01/16",format=fmt,out.format=fmt)+(1:800)

mtz <- matrix(nrow=length(sfrz),ncol=nrow(camaras))

##periodo 1 camra 3: problema sin fotos
##periodo 2 camara 4: problema sin fotos
##periodo 4 camara 4: problema sin fotos

##periodo 3 camara 4: dos eventos con miles de fotos...
##periodo 1 camara 6: error en las fotos

##periodo 2 camara 10: (omisión de la cámara) falta para completar muestreo

problemas <- c(subset(camaras,!cdg %in% fts$camara)$cdg,
               "B01.P03.C04","B02.P01.C06","B01.P02.C10")

 subset(camaras,cdg %in% problemas)

##periodo 5 camara 9: estaba en la lista de repetidos, chequear

for (k in 1:nrow(camaras)) {
  if (!camaras[k,"cdg"] %in% problemas)
      mtz[,k] <- sfrz>camaras[k,"f1"] & sfrz<=camaras[k,"f2"]
}

## esfuerzo de muestreo por mes:
esfuerzo <- aggregate(rowSums(mtz,na.rm=T),list(months(sfrz)),sum,na.rm=T)


## esfuerzo de muestreo por camara:
dts <- data.frame(camaras$cdg,
                  sfrz=colSums(mtz,na.rm=T),
                  periodo=camaras$f2-camaras$f1)


                  # ## verificar que esta es la cámara que se robaron...
dts[26,"periodo"] <- 113
                  #
                  # ##dias perdidos
                   dts$periodo -dts$sfrz
                   as.numeric(sum(dts$periodo -dts$sfrz))/as.numeric(sum(dts$periodo))


#####
## para primer parrafo de resultados
with(notas,tapply(evento,list(tipo),function(x) length(unique(x))))
length(unique(notas$evento))



chisq.test( table(notas$equipo,notas$tipo)[1:2,c("falso positivo","mamifero identificado")])
chisq.test( table(notas$equipo,notas$tipo)[,c("falso positivo","mamifero identificado")])

 tapply(notas$evento,list(notas$equipo,notas$tipo),function(x) length(unique(x)))

 tapply(notas$evento,list(notas$tipo),function(x) length(unique(x)))

tapply(notas$evento,list(notas$tipo),function(x) length(unique(x)))/(sum(tapply(notas$evento,list(notas$tipo),function(x) length(unique(x))))-45)

table(tolower(subset(notas,tipo %in% "por procesar")$anotación))

 table(tolower(subset(notas,!evento %in% ids$evento)$anotación))


with(fts,aggregate(filename,list(bloque,periodo),length))

fts[fts$evento %in% notas$evento,"Anotado"] <- T
fts[fts$evento %in% ids$evento,"Identificado"] <- T
merge(merge(with(fts,aggregate(data.frame(fts=filename,evento=evento),
                           list(bloque=substr(camara,9,11),periodo=periodo),function(x) length(unique(x)))),
      with(subset(fts,Anotado),
           aggregate(data.frame(evento.anotado=evento),
                     list(bloque=substr(camara,9,11),periodo=periodo),function(x) length(unique(x)))),by=c("periodo","bloque"),all.x=T),
      with(subset(fts,Identificado),
           aggregate(data.frame(evento.con.id=evento),
                     list(bloque=substr(camara,9,11),periodo=periodo),function(x) length(unique(x)))),by=c("periodo","bloque"),all.x=T)


table(unique(fts$evento) %in% ids$evento)
 table(ids$val )
 table(cut(ids$certeza,c(0,50,80,100)))

oo <- aggregate(ids$certeza,list(ids$val),median)


## orden que piden los arbitros

lista.especies <- (c("Didelphis marsupialis","Marmosa robinsoni","Dasypus novemcinctus","Choloepus hoffmanni","Tamandua mexicana","Tamandua tetradactyla","Myrmecophaga tridactyla","Aotus griseimembra","Alouatta seniculus","Ateles hybridus","Cebus albifrons","Hydrochoeris hydrochaeris","Cuniculus paca","Sigmodon alstoni","Rhipidomys venezuelae","Oligoryzomys fulvescens","Neacomys tenuipes","Necromys urichi","Oecomys speciosus","Zygodontomys brevicauda","Dasyprocta leporina","Proechimys guairae","Coendou prehensilis","Heteromys anomalus","Sciurus granatensis","Sylvilagus floridanus","Cerdocyon thous","Leopardus pardalis","Panthera onca","Puma yagouaroundi","Puma concolor","Conepatus semistriatus","Eira barbara","Procyon cancrivorus","Potos flavus","Tapirus terrestris","Mazama americana","Mazama sp.","Odocoileus virginianus","Pecari tajacu"))




par(mar=c(12,4,0,0))
##boxplot(ids$certeza~factor(ids$val,levels=oo$Group.1[order(oo$x)]),las=2,cex.axis=.8,ylab="Certeza en la identificación [%]",axes=F)
finalid <- droplevels(factor(ids$val,levels=lista.especies))
lista.sp <- c(rep(NA,19),"      sp.")

##boxplot(ids$certeza~finalid,las=2,cex.axis=.8,ylab="Certeza en la identificación [%]",axes=F)
boxplot(ids$certeza~finalid,las=2,cex.axis=.8,ylab="Certainty in identification [%]",xlab=NA,axes=F)
axis(2)
axis(1,1:nrow(oo),gsub("sp.","     ",levels(finalid)), cex=.8,font=3,las=2)
axis(1,1:nrow(oo),lista.sp, cex=.8,font=1,las=2)
##dev.copy(svg,file="20180816_BurroNegro_Fig_Certeza.svg")

dev.copy(pdf,file="20180907_BurroNegro_Fig_Certeza.pdf")
dev.off()

table(ids$val)

finalid <- droplevels(factor(id.fts$val,levels=lista.especies))

ss <- with(id.fts,tapply(evento,list(amanecer,finalid),function(x) length(unique(x))))
##oo <- apply(0:23*ss,2,sum,na.rm=T)/apply(ss,2,sum,na.rm=T)
##ss <- ss[,order(oo)]
ss <- ss[,ncol(ss):1]
s2 <- ss


par(mar=c(6,12,0,0),las=2)

plot(id.fts$amanecer,finalid,pch=NA,xlab="",ylab="",axes=F)
mtz <- data.frame(freq=c(ss),amanecer=rep(0:23,ncol=ss),
                  val=rep(colnames(ss),rep(24,ncol(ss))),
                  k=rep(1:ncol(ss),rep(24,ncol(ss))))
mtz <- subset(mtz,!is.na(freq))
with(mtz,symbols(amanecer,k,circles=sqrt(freq),add=T,inches=.1,bg=ifelse(freq>1,"black","white")))

axis(1,c(0,6,12,18,23),c("06:00","12:00","18:00","24:00","05:00"),line=0,cex.axis=.75,tick=F,lty=0)
axis(1,c(0,6,12,18,23),line=3,cex.axis=.85)
abline(v=12.5,lty=3)
axis(2,1:ncol(ss),gsub("sp.","     ",colnames(ss)),cex=.8,font=3)
axis(2,1,"sp.",cex=.8,font=1)
##mtext("Horas desde el amanecer",1,line=5,las=1)
mtext("Hours after sunrise",1,line=5,las=1)

s2[is.na(s2)] <- 0
cambios <- c()
##
for (k in 1:24) { j=ifelse(k==1,24,k-1); cambios <- c(cambios,1-as.matrix(vegdist(s2>0))[k,j])}
##plot(1:12,cambios,type="l")
##text(1:24,-.15,round(cambios,3),cex=.5,xpd=NA)
##text(0,-1,"IS =",xpd=NA)


##dev.copy(svg,file="20180816_BurroNegro_Fig_ActividadDiaria.svg")
dev.copy(pdf,file="20180907_BurroNegro_Fig_ActividadDiaria.pdf")
##dev.copy(png,file="BurroNegro_Fig_ActividadDiaria.png")
dev.off()






ss[is.na(ss)] <- 0

vegdist(ss)
## se podría buscar la hora en la que ocurre el mayor cambio en la actividad:
##
cambios <- c()
##
for (k in 1:24) { j=ifelse(k==1,24,k-1); cambios <- c(cambios,1-as.matrix(vegdist(ss>0))[k,j])}
plot(1:24,cambios,type="l")




## buscar forma de calcular:
## chisq: distribución de eventos a lo largo del dia
ee <- c(rep(0.05/12,12),rep(0.95/12,12))
chisq.test(ss[,"Dasypus novemcinctus"],p=ee)
chisq.test(ss[,"Cuniculus paca"],p=ee)
chisq.test(ss[,"Leopardus pardalis"],p=ee)
chisq.test(ss[,"Tamandua mexicana"],p=ee)

ee <- c(rep(0.95/12,12),rep(0.05/12,12))

chisq.test(ss[,"Mazama americana"],p=ee)

#

id.fts$finalid <- droplevels(factor(id.fts$val,levels=lista.especies))

##subset(id.fts,camara %in% subset(camaras,URA %in% "B01")$cdg)
s1 <- with(subset(id.fts,camara %in% subset(camaras,URA %in% "B01")$cdg),tapply(evento,list(meses=months(f1),finalid),function(x) length(unique(x))))

s2 <- ss <- with(id.fts,tapply(evento,list(meses=months(f1),finalid),function(x) length(unique(x))))
ee <- esfuerzo$x[c(1:12)]/sum(esfuerzo$x[c(1:12)],na.rm=T)

## chisq: distribución de eventos a lo largo del año (más de 40 registros)
chisq.test(ss[c(1:12),"Dasypus novemcinctus"],p=ee)
chisq.test(ss[c(1:12),"Dasyprocta leporina"],p=ee)
chisq.test(ss[c(1:12),"Leopardus pardalis"],p=ee)
chisq.test(ss[c(1:12),"Cerdocyon thous"],p=ee)
## falta probar correlación con lluvias
## señalar eventos reproductivos con color del circulo



ss <- ss/esfuerzo$x
##oo <- apply(1:12*ss,2,sum,na.rm=T)/apply(ss,2,sum,na.rm=T)
##ss <- ss[,order(oo)]
ss <- ss[,ncol(ss):1]
ss
par(mar=c(5,12,0,0),las=2)

plot(1,1,pch=NA,xlim=c(1,12),ylim=c(1,ncol(ss)),
     xlab="",ylab="",axes=F)

mtz <- data.frame(freq=c(ss),amanecer=rep(1:12,ncol=ss),
                  val=rep(colnames(ss),rep(12,ncol(ss))),
                  k=rep(1:ncol(ss),rep(12,ncol(ss))))
mtz <- subset(mtz,!is.na(freq))
with(mtz,symbols(amanecer,k,circles=sqrt(freq),add=T,inches=.1,bg=ifelse(freq>0,"black","white")))

##axis(1,1:12,c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"))
axis(1,1:12,levels(months(1:12)))
axis(2,1:ncol(ss),gsub("sp.","     ",colnames(ss)),cex=.8,font=3)
axis(2,1,"sp.",cex=.8,font=1)


s1[is.na(s1)] <- 0
s2[is.na(s2)] <- 0
cambios <- c()
##
for (k in 1:12) { j=ifelse(k==1,12,k-1); cambios <- c(cambios,1-as.matrix(vegdist(s1>0))[k,j])}
##plot(1:12,cambios,type="l")
text(1:12,-2,round(cambios,3),cex=.75,xpd=NA)
text(0,-2,expression(I[S]),xpd=NA)
##cambios <- c()
##
##for (k in 1:12) { j=ifelse(k==1,12,k-1); cambios <- c(cambios,1-as.matrix(vegdist(s2>0))[k,j])}
##text(1:12,-2.5,round(cambios,3),cex=.75,xpd=NA)

##dev.copy(svg,file="20180816_BurroNegro_Fig_DeteccionMensual.svg")
dev.copy(pdf,file="20180907_BurroNegro_Figure3_DeteccionMensual.pdf")
dev.off()

ss[is.na(ss)] <- 0
cambios <- c()
##
for (k in 1:12) { j=ifelse(k==1,12,k-1); cambios <- c(cambios,1-as.matrix(vegdist(ss>0))[k,j])}
plot(1:12,cambios,type="l")

plot(hclust(vegdist(ss>0)))


## ordenar por taxonomía o por gremio (herbivoros vs. carnivoros)
## corregir por el esfuerzo de muestreo
## colocar información periodo de lluvias
## agregar eventos reproductivos (copula o presencia de cría)

## se puede agregar:
## relación eventos herbivoros vs. eventos carnivoros



## especies por camara
with(id.fts,tapply(evento,list(val,substr(camara,9,11)),function(x) length(unique(x))))

mtz <-  with(id.fts,tapply(evento,list(val,substr(camara,9,11)),function(x) length(unique(x))))


with(id.fts,plot(val~camara))



with(id.fts,plot(amanecer,factor(val,levels=ss$Group.1[order(ss$x)])))


with(id.fts,plot(times(f1),as.factor(val)))


dotplot(~times(f1)|val,id.fts)

for (j in unique(ids$val)) {
    ss <- subset(id.fts,val %in% j)
    hist(times(ss$f1))
}


ids$camara <- gsub("_E[0-9]+$","",substr(ids$evento,9,100))


mtz <- table(id.fts$camara,id.fts$val)
mtz <- mtz[ order(substr(rownames(mtz),5,7)),]

plot(specaccum(mtz>0),conditional=T)
specpool(mtz>0)
ee <- dts$sfrz[match(rownames(mtz),dts$camaras.cdg)]
curva.sp <- specaccum(mtz>0,condition=F)
curva.sp$sites <- curva.sp$sites*mean(ee)

plot(curva.sp,ci.type="l",lty=2,ci.lty=3,ylim=c(0,30),xlab="Esfuerzo (horas*camara)",ylab="Riqueza de especies")
lines(cumsum(ee),rowSums(apply(mtz,2,cumsum)>0),type="b",pch=19)
points(rep(20,4)*mean(ee),specpool(mtz>0)[,c("chao","jack1","jack2","boot")],pch=17)
text(rep(19,4)*mean(ee),specpool(mtz>0)[,c("chao","jack1","jack2","boot")],
     c("Chao","Jack 1","Jack 2", "Bootstrap"),cex=.8)

dev.copy(svg,file="20180419_BurroNegro_Fig_CurvaAcumulacion.svg")
dev.off()
