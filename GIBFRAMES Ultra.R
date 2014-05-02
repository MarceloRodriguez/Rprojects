##########################################################################################################
#							GENERADOR DE FRAMES (RÁPIDO)						   #
##########################################################################################################

GIBFRAMES = function(){



regional = c("LE","LB","TT","TBO","YAGUARON","SANTANA")

directorio="C:/R.WD/data/"

rad = 10
linea = "________________________________________________________________________________________________________________________"


for (v in c(1:4))
{

bloqueo = 0
hay_pb = 0 

archivo=paste(directorio,as.character(regional[v])," data.csv",sep="")

D=read.csv(archivo,header=TRUE,skip=0,sep=",",dec=".",stringsAsFactors=FALSE)
D$Fecha=as.Date(D$Fecha)

D$crd=0
i = c(1:dim(D)[1])
for (i in as.integer(row.names(D)))
 {
	if (i==1) {rpp = c(i)};
	if (i==2) {rpp = c(1,1,1,i-1,i)};
	if (i==3) {rpp = c(1,1,i-2,i-1,i)};
	if (i==4) {rpp = c(1,i-3,i-2,i-1,i)};
	if (i>=5) {rpp = c(i-4,i-3,i-2,i-1,i)}

	cpp = c(0,0,0,0,0)

	cpp[D$PP[rpp]>0.3]=1
	
	dcp = sum(cpp)

########### Indice de lluvias (CRD) ############	
	if (dcp<=2) {D$crd[i]=dcp};
	if (dcp==3) {D$crd[i]=2.5};
	if (dcp>=4) {D$crd[i]=0.3};
		
 }

cat("\n",paste("Regional:",regional[v]),"\n")
cat(linea,"\n")
print(summary(D))

archivo1 = paste(directorio,as.character(regional[v])," frame.csv",sep="")

if (file.exists(archivo1)){	
	
	F = read.csv(archivo1,header=TRUE,skip=0,sep=",",dec=".",stringsAsFactors=FALSE)
      F$Fecha=as.Date(F$Fecha)
	
	

	if (length(F$Fecha) != length(D$Fecha)) {

		F = F[F[,3]==0,]
		
		inicial = dim(F)[1]+1
		bloqueo = 0

	} else {
		
		bloqueo = 1 # Si los datos del FRAME están actualizados salir de la rutina de esta regional
		cat(linea,"\n")
		cat(paste("Los datos de",regional[v],"ya estaban actualizados, no fueron modificados."),"\n","\n")
	}

} else {

      inicial = 2
	
}



if (bloqueo != 1) {

tot_time = as.integer(difftime(max(D$Fecha),min(D$Fecha)))
pb <- winProgressBar(title = "Barra de progreso", min = 0,max = tot_time, width = 400)
Sys.sleep(0.1)

for (ff in D$Fecha[inicial:length(D$Fecha)])
 
{

fpe = ff



##########################################################################################################
dsmax = 17
dias_susceptibles = min(as.integer(difftime(max(D$Fecha),D[D$Fecha==fpe,1])),dsmax)

n = as.integer(row.names(D[D$Fecha==fpe,]))

Z = D[D$Fecha>=fpe & D$Fecha <= fpe+dias_susceptibles,]
row.names(Z) = c(1:dim(Z)[1])


########### Días desde la primer espiga ############
Z$ddpe = as.integer(Z$Fecha-fpe)


########### Indice de lluvias (CRD) ############	
Z$crd


############# Indice de espigazón ###############
Z$indesp=1-exp(-0.0127*Z$ddpe^2.4352)

Z$indesp[is.na(Z$indesp)]=0


############# Nube de esporas #################
Z$gz=(-0.6306+0.0152*Z$RH+0.1076*Z$crd)^2


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

#Z$gzm


Z$infd=0
i = as.integer(row.names(Z))
for (i in as.integer(row.names(Z)))
 {
	if (i==1) {ie = c(i)};
	if (i>=2) {ie = c(i-1,i)};


		dcp=sum(Z$PP[ie]>0.3)
		
		formula = 0.0010289*exp(0.1957*mean(Z$TProm[ie]))

		############### Indice de infección diario ################

		if (dcp==2 && mean(Z$RH[ie])>=80) {Z$infd[i]= formula}
		if (i>1 && Z$PP[i-1]>0.3 && Z$RH[i-1]>=80 && Z$RH[i]>=85) {Z$infd[i]= formula};
		if (i>1 && Z$PP[i]>0.3 && Z$RH[i]>=80 && Z$RH[i-1]>=85) {Z$infd[i]= formula};
	
}

#Z$infd


######### Cálculo de factores a y b ##########

Z$a = 0.255 - 0.029 * Z$TProm + 0.0009 * (Z$TProm)^2
Z$b = -5.773 + 0.966 * Z$TProm - 0.0278 * (Z$TProm)^2



Z$ant=0

h=0
i=0

for (h in c(0:30)){
nomore = 0
fin_esp = 0
	

	i = as.integer(row.names(Z))
	for (i in as.integer(row.names(Z))){
		
		# Cuenta los días desde que emergió el último cohorte de espigas
		if (round(Z$indesp[i],7)==1) {fin_esp = fin_esp + 1} 
		
		if (fin_esp <= 4) {
 	
			ecesp = Z$ddpe[i]-h  # Edad del cohorte de espigas
		
			if (ecesp >=3 && ecesp <=10 ) {
					
				Z$ant[i]=1-exp(-(Z$a[i]) * ecesp ^ Z$b[i])

				if (ecesp >=5){
	
					if (Z$RSProm[i] <rad && nomore!=1) {
							
							############### Indice de anteras del cohorte de espigas ################
							Z$ant[i]=1-exp(-(Z$a[i]) * ecesp ^ Z$b[i])

					} else {Z$ant[i]=0; nomore=1;} 

				}				
	
			} else {Z$ant[i]=0}

		} else {Z$ant[i]=0}

	}
		ww = paste("ca",h,sep="")
		Z[(ww)] = Z$ant
		

}

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

		www = paste("st",h,sep="")
		Z[(www)] = Z$ant
	
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

#Z$stm

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

#cat("GIB:",GIB,"\n")
#print(Z[Z$st>0,])

##########################################################################################################

parcial = 0 

# Si los datos existentes no llegan a cubrir el período de susceptibilidad
if (as.integer(difftime(max(D$Fecha),D[D$Fecha==fpe,1])) < dsmax) 
	{parcial=1} 
else 
	{
	if (v>2 && as.integer(difftime(max(D$Fecha),D[D$Fecha==fpe,1])) < dsmax+7) {parcial=1} 
	# En los casos de regionales con pronósticos se asumen como parciales los registros completos 
	# que utilizan los datos pronósticos
}


if (file.exists(archivo1)){
		
	F[dim(F)[1]+1,1] =  D$Fecha[n]
	F[dim(F)[1],2] = SEV
	F[dim(F)[1],3] = parcial
	F[is.na(F[,3]),3] = 0

	till_now = as.integer(difftime(max(F$Fecha),min(D$Fecha)))

} else {
	col1 = c(D$Fecha[1],D$Fecha[n])
	col2 = c(0,SEV)
	col3 = c(0,parcial)
      F = data.frame(col1,col2,col3)

	till_now = 1
}




   setWinProgressBar(pb, till_now, title=paste("Regional: ",regional[v],"- Fecha: ",F[n,1],
		"- Sev: ", format(round(F[n,2],2), digits = 2, nsmall = 1),"- ",round(till_now/tot_time*100, 0),"% completado"))
	hay_pb = 1
	
names(F)[1] = "Fecha"
names(F)[2] = "Severidad"
names(F)[3] = "Parcial"


write.csv(F, file = archivo1, row.names = FALSE)

#cat(paste("Datos del día",D$Fecha[n],"agregados al archivo",archivo1),"\n")



}# Cierre For de cada Fecha primera espiga

cat(linea,"\n")
cat(paste("Los datos de",regional[v],"fueron actualizados."),"\n","\n")

}# Cierre if de bloqueo

 if (hay_pb == 1) {close(pb); hay_pb = 0;}

}# Cierre For de cada Regional




}# Cierre de función GIBFRAMES()
##########################################################################################################
#								FIN GIBFRAMES()							   #
##########################################################################################################
