setwd("C:/R.WD/")

rm(list=ls())
download.file("http://www.inia.org.uy/gras/agroclima/INIA_WN/datos/TyT/INIA_TyT_PP_hora.csv", "C:/R.WD/TT.csv")

X=read.csv("C:/R.WD/TT.csv",header=TRUE,skip=1,sep=",",dec=".",stringsAsFactors=FALSE)

data.frame(X, row.names = NULL, check.rows = FALSE,check.names=TRUE)

X=X[-1:-16,]

rownames(X) = as.numeric(rownames(X)) - 16

attach(X)

X[, c(2:12)] <- sapply(X[, c(2:12)], as.numeric)

X[,11][X[,11]<0] <- 0

sapply(X,mode)

dim(X)

Y = X

Y[,1] = as.Date(substr(X[,1],1,10))

detach(X)

colnames(Y)[1]="FECHA"

dim(Y)

Y=unique(Y) # Elimina registros horarios duplicados
rownames(Y) = c(1:dim(Y)[1])

dim(Y)

Y=Y[1:(as.integer(nrow(Y)/24)*24),] # Elimina el último día de registros si no está completo (24hs.)

dim(Y)


Y$RECORD = 0

hreg = c(0:23)

Y$RECORD[1:nrow(Y)] = hreg

Y[Y[,8]>=100,8]=99

colnames(Y)[2]="HORA"

colmedia = c(6,8) # Columnas a las que se calculará la media
colsuma = c(11,9) # Columnas  a la que se les calculará la suma o acumulado (PP=Precipitación)

Z1 = aggregate(x = Y[,colmedia], by = list(Fecha=Y$FECHA), FUN = "mean")
Z2 = aggregate(x = Y[,colsuma], by = list(Fecha=Y$FECHA), FUN = "sum")

Z = merge(Z1,Z2)

names(Z) = c("Fecha","TProm","RH","RSProm","PP")

Z$Fecha=as.Date(Z$Fecha)

attach(Z)


rm(X,Y,Z1,Z2,hreg,colmedia,colsuma)

X=Z

write.csv(X, file = "C:/R.WD/data/TT data.csv", row.names = FALSE)

ls()


