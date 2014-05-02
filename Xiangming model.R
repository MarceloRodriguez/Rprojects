###################################################################################################################
#			SIMULADOR DON - Desarrollado por Marcelo J. Rodríguez - INIA La Estanzuela			                            #
###################################################################################################################
DON = function(){

reg=c("LE","LB","TT","TBO")
v=c(1:4)

regional=""

directorio="C:/R.WD/data/"

while (sum(reg[v]==regional)==0) regional = toupper(readline("Elija una regional(LE, LB, TT o TBO): "))

archivo=paste(directorio,as.character(regional)," hour data.csv",sep="")

X=read.csv(archivo,header=TRUE,skip=0,sep=",",dec=".",stringsAsFactors=FALSE)
X$Fecha=as.Date(X$Fecha)

fmax = as.Date(max(X$Fecha)-7)
fmin = as.Date(min(X$Fecha)+2)
fpe = as.Date(min(X$Fecha))

cat("\n")
print(summary(X))
cat("\n",paste("Fecha mínima que puede ingresar:",format(fmin,"%d-%m-%y")))
cat("\n",paste("Fecha máxima que puede ingresar:",format(fmax,"%d-%m-%y")),"\n","\n")

while (fpe > fmax || fpe < fmin) fpe = as.Date(readline("Ingrese fecha de comienzo de anthesis (dd-mm-aa): "), "%d-%m-%y")
#print(fpe)


tope = difftime(max(X$Fecha),fpe,units="days")
dias_susceptibles = min(14,tope) # <<<<<<<<<< Período susceptible a partir de anthesis

######### Último día de susceptibilidad ##########
limddc = as.Date(as.Date(fpe)+dias_susceptibles)

X=X[X$Fecha>=fpe,]
X=X[X$Fecha<=limddc,]
rownames(X) = c(1:dim(X)[1])


es = 0.6108 * exp(17.27 * X$Temp / (X$Temp + 237.3))

X$VPD = es * (1 - X$RH / 100) 
X$VPD = X$VPD * 10 # de kPa a milibares

X$RH70 = 0
X$V34 = 0

X[X$RH>=70,8] = 1
X[X$VPD<=3.4,9] = 1

Z1 = aggregate(x = X[,7], by = list(Fecha=X$Fecha), FUN = "mean")
Z2 = aggregate(x = X[,c(8,9)], by = list(Fecha=X$Fecha), FUN = "sum")
names(Z1)[2] = "VPD"
names(Z2)[2] = "RH70"
names(Z2)[3] = "V34"

Z = merge(Z1,Z2)
#--------------------------------------------------------------------------------------------------------------------
# Modelo 1 (15 días a partir anthesis): 5.033 - 0.887×VPD + 0.616×RH70 - 0.849×V34

Z$DON = 5.033 - 0.887*Z$VPD + 0.616*Z$RH70 - 0.849*Z$V34


#----------------------------------------------------------------------------
#				Presentación de Resultados
#----------------------------------------------------------------------------

acu_don = cumsum(Z$DON)
print(Z)

}# Cierre de función DON()
###################################################################################################################
#							FIN DON()								                                                                        		#
###################################################################################################################
DON()
