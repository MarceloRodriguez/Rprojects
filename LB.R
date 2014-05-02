setwd("C:/R.WD/")

rm(list=ls())

download.file("http://www.inia.org.uy/gras/agroclima/INIA_WN/datos/LB/INIA_LB.dat", "C:/R.WD/LB.csv")

X=read.csv("C:/R.WD/LB.csv",header=TRUE,skip=1,sep=",",dec=".",stringsAsFactors=FALSE)

data.frame(X, row.names = NULL, check.rows = FALSE,check.names=TRUE)






colnames(X)[1] = "Tabla"
colnames(X)[2] = "Año"
colnames(X)[3] = "Dia"
colnames(X)[4] = "Hora"

X=X[X$Tabla==222,]

X=X[-1:-22,]
rownames(X) = as.numeric(rownames(X)) - 23


X$Fecha=as.Date("2013-01-01") + X$Dia - 1
X$Fecha=as.Date(X$Fecha)
#format(X$Fecha, format="%Y-%m-%d")

X$Hora=X$Hora/100-1

X[X[,20]<0,20] = 0

sapply(X,mode)


dim(X)

Y = X

Y[,1] = X$Fecha

colnames(Y)[1]="FECHA"

dim(Y)


Y=unique(Y) # Elimina registros horarios duplicados
rownames(Y) = c(1:dim(Y)[1])


dim(Y)

Y=Y[1:(as.integer(nrow(Y)/24)*24),]

dim(Y)

Y$RECORD = 0

hreg = c(0:23)

Y$RECORD[1:nrow(Y)] = hreg

Y[Y[,9]>=100,9]=99

colnames(Y)[4]="HORA"


colmedia = c(6,9) # Columnas a las que se calculará la media
colsuma = c(20,18) # Columnas  a la que se les calculará la suma o acumulado (PP=Precipitación)



Z1 = aggregate(x = Y[,colmedia], by = list(Fecha=Y$FECHA), FUN = "mean")
Z2 = aggregate(x = Y[,colsuma], by = list(Fecha=Y$FECHA), FUN = "sum")

Z = merge(Z1,Z2)

names(Z) = c("Fecha","TProm","RH","RSProm","PP")

Z$Fecha=as.Date(Z$Fecha)

attach(Z)

rm(X,Y,Z1,Z2,hreg,colmedia,colsuma)

X=Z

write.csv(X, file = "C:/R.WD/data/LB data.csv", row.names = FALSE)


ls()

