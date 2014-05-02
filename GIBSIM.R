###################################################################################################################
#			SIMULADOR GIBSIM	- Desarrollado por Marcelo J. Rodríguez - INIA La Estanzuela			#
###################################################################################################################
GIBSIM = function(){

reg=c("LE","LB","TT","TBO")
v=c(1:4)

regional=""

directorio="C:/R.WD/data/"

while (sum(reg[v]==regional)==0) regional = toupper(readline("Elija una regional(LE, LB, TT o TBO): "))

archivo=paste(directorio,as.character(regional)," data.csv",sep="")

X=read.csv(archivo,header=TRUE,skip=0,sep=",",dec=".",stringsAsFactors=FALSE)
X$Fecha=as.Date(X$Fecha)

fmax = as.Date(max(X$Fecha)-7)
fmin = as.Date(min(X$Fecha)+2)
fpe = as.Date(min(X$Fecha))

cat("\n")
print(summary(X))
cat("\n",paste("Fecha mínima que puede ingresar:",format(fmin,"%d-%m-%y")))
cat("\n",paste("Fecha máxima que puede ingresar:",format(fmax,"%d-%m-%y")),"\n","\n")

while (fpe > fmax || fpe < fmin) fpe = as.Date(readline("Ingrese fecha de primer espiga (dd-mm-aa): "), "%d-%m-%y")
#print(fpe)


tope = difftime(max(X$Fecha),fpe,units="days")
dias_susceptibles = min(40,tope) # <<<<<<<<<< Período susceptible a partir de floración (primer grupo de anteras visibles)

######### Último día de susceptibilidad ##########
limddc = as.Date(as.Date(fpe)+dias_susceptibles)

rad = 10 # Límite de radiación máxima a la que mueren las anteras (Mjoules/m2)

X=X[X$Fecha>=fpe,]
X=X[X$Fecha<=limddc,]
rownames(X) = c(1:(dias_susceptibles+1))

#--------------------------------------------------------------------------------------------------------------------

Z=X

n = as.integer(row.names(Z[Z$Fecha==fpe,]))

Z$ddpe = 0
Z$ddpe = as.integer(row.names(Z))- n


X$crd=0
i = as.integer(row.names(Z))
for (i in as.integer(row.names(Z)))
 {
	if (i==1) {rpp = c(i)};
	if (i==2) {rpp = c(1,1,1,i-1,i)};
	if (i==3) {rpp = c(1,1,i-2,i-1,i)};
	if (i==4) {rpp = c(1,i-3,i-2,i-1,i)};
	if (i>=5) {rpp = c(i-4,i-3,i-2,i-1,i)}

	cpp = c(0,0,0,0,0)

	cpp[X$PP[rpp]>0.3]=1
	
	dcp = sum(cpp)

########### Indice de lluvias (CRD) ############	
	if (dcp<=2) {X$crd[i]=dcp};
	if (dcp==3) {X$crd[i]=2.5};
	if (dcp>=4) {X$crd[i]=0.3};
	
	
 }


############# Indice de espigazón ###############
Z$indesp=1-exp(-0.0127*Z$ddpe^2.4352)

Z$indesp[is.na(Z$indesp)]=0

############# Nube de esporas #################
Z$gz=(-0.6306+0.0152*X$RH+0.1076*X$crd)^2


Z$gzm=0
i = as.integer(row.names(Z))
for (i in as.integer(row.names(Z)))
 {
	if (i==1) {ie = c(i)};
	if (i>=2) {ie = c(i-1,i)};
	
	gzmie = 0

	gzmie = mean(Z$gz[ie])
	
	Z$gzm[i] = gzmie
		
 }


Z$infd=0
i = as.integer(row.names(Z))
for (i in as.integer(row.names(Z)))
 {
	if (i==1) {ie = c(i)};
	if (i>=2) {ie = c(i-1,i)};


		dcp=sum(X$PP[ie]>0.3)

		formula = 0.0010289*exp(0.1957*mean(X$TProm[ie]))

		############### Indice de infección diario ################

		if (dcp==2 && mean(X$RH[ie])>=80) {Z$infd[i]= formula}
		if (i>1 && X$PP[i-1]>0.3 && X$RH[i-1]>=80 && X$RH[i]>=85) {Z$infd[i]= formula};
		if (i>1 && X$PP[i]>0.3 && X$RH[i]>=80 && X$RH[i-1]>=85) {Z$infd[i]= formula};
	
}


######### Cálculo de factores a y b ##########

Z$a = 0.255 - 0.029 * X$TProm + 0.0009 * (X$TProm)^2
Z$b = -5.773 + 0.966 * X$TProm - 0.0278 * (X$TProm)^2


Z$ant=0

h=0
i=0

for (h in c(0:30)){
nomore = 0
fin_esp = 0
	

	i = as.integer(row.names(Z))
	for (i in as.integer(row.names(Z))){

		if (round(Z$indesp[i],7)==1) {fin_esp = fin_esp + 1} # Cuenta los días desde que emergió el último cohorte de espigas
		
		if (fin_esp <= 4) {
 	
			ecesp = Z$ddpe[i]-h  #Edad del cohorte de espigas
		
			if (ecesp >=3 && ecesp <=10 ) {
					
				Z$ant[i]=1-exp(-(Z$a[i]) * ecesp ^ Z$b[i])

				if (ecesp >=5){
	
					if (X$RSProm[i] <rad && nomore!=1) {
							
							############### Indice de anteras del cohorte de espigas ################
							Z$ant[i]=1-exp(-(Z$a[i]) * ecesp ^ Z$b[i])

					} else {Z$ant[i]=0; nomore=1;} 

				}				
	
			} else {Z$ant[i]=0}

		} else {Z$ant[i]=0}

	}
		v = paste("ca",h,sep="")
		Z[(v)] = Z$ant
		

}

#Z$ant=NULL


Z$ant=0

h=0
i=0
p=0

for (h in c(0:30)){
	datt=100
	i = as.integer(row.names(Z))
	for (i in as.integer(row.names(Z))){
		
		w = paste("ca",h,sep="")
		cohant = Z[,(w)]
		


		############### Indice de tejido vegetal susceptible ################
		
		if (cohant[i] > 0.25 && Z$indesp[i]>0) {Z$ant[i]=cohant[i]; datt=0;}
		if (cohant[i] > 0 && cohant[i] <= 0.25 && Z$indesp[i]>0) {Z$ant[i]=0.25; datt=0;}
		if (cohant[i]==0 && datt==100) {Z$ant[i]=0}
		if (cohant[i]==0 && Z$indesp[i]>0 && datt >=0 && datt < 8) {Z$ant[i]=0.25; datt=datt+1;}
		if (cohant[i]==0 && Z$indesp[i]>0 && datt >=8 && datt < 15) {Z$ant[i]=0.10; datt=datt+1;}

			

	}

		v = paste("st",h,sep="")
		Z[(v)] = Z$ant
	
}

Z$ant=NULL




st_range = c(which(names(Z)=="st0"):which(names(Z)=="st30"))

Z$st=0

i = as.integer(row.names(Z))
for (i in as.integer(row.names(Z))){
		
		Z$st[i]=sum(Z[i,st_range])

}


Z$stm=0
stmie=0

i = as.integer(row.names(Z))
for (i in as.integer(row.names(Z)))
 {
	if (i==1) {ie = c(i)};
	if (i>=2) {ie = c(i-1,i)};
	
	stmie = mean(Z$st[ie])
	
	Z$stm[i] = stmie
 }



############### Giberella diaria ################
Z$gib=Z$stm*Z$gzm*Z$infd


############### Giberella total #################
GIB=sum(Z$gib[Z$st>0])*100


################## Severidad ####################

#if (GIB>0) {SEV=0.7442+1.46*GIB} else {SEV=0} #Según mis apuntes

if (GIB>0) {SEV=0.97+1.36*GIB} else {SEV=0} #Según el paper


################### Riesgo #######################

#if (SEV<1) {RIESGO="SIN RIESGO"} 	#Según mis apuntes
#if (SEV>=1) {RIESGO="BAJO"} 		#Según mis apuntes
#if (SEV>=5) {RIESGO="MODERADO"} 	#Según mis apuntes
#if (SEV>=7) {RIESGO="ALTO"} 		#Según mis apuntes


if (SEV<=7) {RIESGO="SIN RIESGO"} 				#Según el paper
if (SEV>7 && SEV<=13.4) {RIESGO="BAJO"}			#Según el paper
if (SEV>13.4 && SEV<=19.8) {RIESGO="MODERADO"}		#Según el paper
if (SEV>19.8) {RIESGO="ALTO"}					#Según el paper

#file_out=paste(directorio,"Salida GIBSIM.csv",sep="")
#write.csv(Z, file = file_out, row.names = FALSE)


#----------------------------------------------------------------------------
#				Presentación de Resultados
#----------------------------------------------------------------------------

par(mfrow=c(2,1))
par(mar=c(0.2,4,4,4))


m = as.integer(row.names(Z[Z$Fecha==fpe,]))
n = as.integer(row.names(Z[Z$st>0,]))

# Período que se graficará  ______________________________________________________

inicio = m                						#desde fecha primera espiga
if (max(n)<dim(Z)[1]){fin = max(n)+1} else {fin = max(n)}   #hasta que termina el período susceptible

gzp=c(inicio:fin)

#________________________________________________________________________________

gp = c(gzp)

qr = c(inicio:fin-1)
qs = c(inicio+1:fin)

acu_gib = cumsum(Z$gib)

h_range <- range(Z$Fecha[gp])
g_range <- range(0, Z$gz[gp], Z$indesp[gp],Z$st[gp]/5,acu_gib[gp],1.2)

r9 = paste ("Regional:",regional)
r0 = paste("GIB:",round(GIB,2))
r1=paste("1er. espiga:",format(fpe,"%d-%m-%Y"))
r2=paste("Severidad:",round(SEV,2))
r3=paste("Riesgo:",RIESGO)
r4=paste("Período susceptible:",fin-4,"días")

lin_sep="-"
for (qqkk in c(1:max(nchar(r0),nchar(r1),nchar(r2),nchar(r3),nchar(r4)))) {lin_sep = paste(lin_sep,"-",sep="")}

resultados=c(r0,r1,r2,r3,r4)

cat("\n","RESULTADOS","\n",lin_sep,"\n",r9,"\n",r0,"\n",r1,"\n",r2,"\n",r3,"\n",r4,"\n",lin_sep,"\n")

#par(ask=TRUE) # para pausa antes de la gráfica

legtxt=c("Nube de esporas","Indice de espigazón","Indice de anteras","Riesgo epidemia")

plot(Z$gz[gp],type="l",main=paste("Regional",regional),col="green",ylim=g_range,yaxt="n",xaxt="n",xlab="",ylab="")
legend("topleft", legend=legtxt, cex=1.0, col=c("green","blue","magenta","red"), lty=1,bty="n")
#axis(1, gp, lab=c(Z$Fecha[gp]), labels=False)

points(Z$indesp[gp],type="l",col="blue")
points(Z$st[gp]/5,type="l",col="magenta")
points(acu_gib[gp],type="l",col="red",lwd=2)
points(Z$gib[gp],type="l",col="red",lwd=1,lty=2)

legend("topright", legend=resultados, cex=1.0,bty="n")

#-----------------------------------------------------------------------------------------------------------------

par(yaxs="i")
par(mar=c(4,4,0,4))
fe_range <- range(Z$Fecha[gp])

hr_range <- range(0,100)
pp_range <- range(0,60) #(-2.5,62.5)
tp_range <- range(0,60)

legtxt=c("Humedad relativa (%)","Temperatura promedio","Precipitación")

barplot(Z$PP[gp],col="deepskyblue",ylab="",xaxt="n",yaxt="n",ylim=pp_range,space=0,add=F,xlim=range(1:length(gp)),xpd=F)
axis(2)
axis(1, gp, labels=F,tick=F)

par(new=T)
par(yaxs="i")

plot(Z$Fecha[gp],Z$RH[gp],type="l",ylab="",yaxt="n",xlab="",col="dark green",xlim=fe_range,ylim=hr_range)
axis(1, gp, lab=gp)  
axis(4)
mtext(1,text="Fecha",line=2)
mtext(4,text="Humedad relativa(%)",line=2)

par(new=T)
par(yaxs="i")

plot(Z$Fecha[gp],Z$TProm[gp],type="l",xlab="",ylab="",yaxt="n",col="tomato",xlim=fe_range,ylim=tp_range)
axis(2)
mtext(2,text="Temp.prom.(ºC) y Prec.(mm)",line=2)

abline(h=48,col="dark grey",lty=3)

legend("topleft", legend=legtxt, cex=1.0, col=c("dark green","tomato","deepskyblue"),lty=1,bty="n")

#graf=paste("C:/R.WD/plots/",regional,fpe,"plot.jpg")
#dev.copy(jpeg,graf)
#dev.off()

}# Cierre de función GIBSIM()
###################################################################################################################
#							FIN GIBSIM()										#
###################################################################################################################
GIBSIM()