setwd("C:/R.WD/")

rm(list=ls())

download.file("http://www.inia.org.uy/gras/agroclima/INIA_WN/datos/TBO/INIA_TBO_LM_tbo_hora.csv", "C:/R.WD/TBO.csv")

X=read.csv("C:/R.WD/TBO.csv",header=TRUE,skip=1,sep=",",dec=".",stringsAsFactors=FALSE)

data.frame(X, row.names = NULL, check.rows = FALSE,check.names=TRUE)

X=X[-1:-15,]

rownames(X) = as.numeric(rownames(X)) - 15

attach(X)

X[, c(2:11)] <- sapply(X[, c(2:11)], as.numeric)

X[X[,11]<0,11] = 0 # Transforma en cero los valores negativos de RSProm

sapply(X,mode)

X1 <- rbind(X[1:9516,],X[9516:9517,],X[9517:dim(X)[1],])
rownames(X1) = c(1:dim(X1)[1])
X1$TIMESTAMP[9517] = "2013-09-17 10:00:00"
X1$TIMESTAMP[9518] = "2013-09-17 11:00:00"

dim(X)
dim(X1)

X1=unique(X1) # Elimina registros horarios duplicados
rownames(X1) = c(1:dim(X1)[1])

dim(X1)

Y = X1

Y[,1] = as.Date(substr(X1[,1],1,10))

detach(X,X1)

colnames(Y)[1]="FECHA"

colnames(Y)[2]="HORA"

cY = c(1:ncol(Y))

dim(Y)

Y=Y[1:(as.integer(nrow(Y)/24)*24),] # Elimina el último día de registros si no está completo (24hs.)

dim(Y)

hreg = c(0:23)

Y$RECORD = 0

Y$RECORD[1:nrow(Y)] = hreg 
# Crea una columna de control para chequear el nro. de registros diarios

Y[Y[,8]>=100,8]=99 # Transforma los registros de Humedad Relativa > 100 en 99


colmedia = c(6,8) # Columnas a las que se calculará la media
colsuma = c(11,9) # Columnas  a la que se les calculará la suma o acumulado (PP=Precipitación)

Z1 = aggregate(x = Y[,colmedia], by = list(Fecha=Y$FECHA), FUN = "mean")
Z2 = aggregate(x = Y[,colsuma], by = list(Fecha=Y$FECHA), FUN = "sum")

Z = merge(Z1,Z2)

names(Z) = c("Fecha","TProm","RH","RSProm","PP")

Z$Fecha=as.Date(Z$Fecha)

attach(Z)


rm(X,X1,Y,Z1,Z2,hreg,colmedia,colsuma)

X=Z

write.csv(X, file = "C:/R.WD/data/TBO data.csv", row.names = FALSE)

ls()
