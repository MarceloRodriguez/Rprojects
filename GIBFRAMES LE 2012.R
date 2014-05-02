##########################################################################################################
#							GENERADOR DE FRAMES LE 2012						   #
##########################################################################################################

LE12 = function(){



regional = c("LE 2012")

directorio="C:/R.WD/data/"

rad = 10

for (v in c(1:1))
{

bloqueo = 0

archivo=paste(directorio,as.character(regional[v])," data.csv",sep="")

X=read.csv(archivo,header=TRUE,skip=0,sep=",",dec=".",stringsAsFactors=FALSE)
X$Fecha=as.Date(X$Fecha)

X=X[235:365,]
row.names(X)=as.integer(row.names(X))-234

print(paste("Regional:",regional[v]))
print(summary(X))

archivo1 = paste(directorio,as.character(regional[v])," frame.csv",sep="")

if (file.exists(archivo1)){	
	
	F = read.csv(archivo1,header=TRUE,skip=0,sep=",",dec=".",stringsAsFactors=FALSE)
      F$Fecha=as.Date(F$Fecha)
	
	if (length(F$Fecha) != length(X$Fecha)) {

		recorte = dim(F)[1] - 8
		loqqueda = c(1:recorte)
				
# Si v<3 (LE y LB) significa que si se trata de una regional sin datos de pronósticos, si v>3 (TT y TBO)

		if (v>2) {F = F[loqqueda,]}
		print(F)

		inicial = dim(F)[1]+1
		bloqueo = 0

	} else {
		
		print(F)
		bloqueo = 1 # Si los datos del FRAME están actualizados salir de la rutina de esta regional
		print(paste("Los datos de",regional[v],"ya estaban actualizados, no fueron modificados."))
	}

} else {

      inicial = 2
	
}


if (bloqueo != 1) {


for (ff in X$Fecha[10:100])  # 1/9/12 al 30/11/12
 
{

fpe = ff

##########################################################################################################


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


#print(X)



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

#Z$gzm


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

#Z$infd


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
		qw = paste("ca",h,sep="")
		Z[(qw)] = Z$ant
		

}

Z$ant=NULL


Z$ant=0

h=0
i=0
p=0

for (h in c(0:30)){
	datt=100
	i = as.integer(row.names(Z))
	for (i in as.integer(row.names(Z))){
		
		wq = paste("ca",h,sep="")
		cohant = Z[,(wq)]
		


		############### Indice de tejido vegetal susceptible ################
		
		if (cohant[i] > 0.25 && Z$indesp[i]>0) {Z$ant[i]=cohant[i]; datt=0;}
		if (cohant[i] > 0 && cohant[i] <= 0.25 && Z$indesp[i]>0) {Z$ant[i]=0.25; datt=0;}
		if (cohant[i]==0 && datt==100) {Z$ant[i]=0}
		if (cohant[i]==0 && Z$indesp[i]>0 && datt >=0 && datt < 8) {Z$ant[i]=0.25; datt=datt+1;}
		if (cohant[i]==0 && Z$indesp[i]>0 && datt >=8 && datt < 15) {Z$ant[i]=0.10; datt=datt+1;}

			

	}

		qw = paste("st",h,sep="")
		Z[(qw)] = Z$ant
	
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

#print(Z)

##########################################################################################################



if (file.exists(archivo1)){
	
	

	F[dim(F)[1]+1,1] =  Z$Fecha[n]
	F[dim(F)[1],2] = SEV
		

} else {
	col1 = c(Z$Fecha[n])
	col2 = c(SEV)
      F = data.frame(col1,col2)
	
}

names(F)[1] = "Fecha"
names(F)[2] = "Severidad"


#print(regional[v])


write.csv(F, file = archivo1, row.names = FALSE)

print(paste("Datos del día",Z$Fecha[n],"agregados al archivo",archivo1))



}# Cierre For de cada Fecha primera espiga


}# Cierre if de bloqueo

}# Cierre For de cada Regional



}# Cierre de función LE12()
##########################################################################################################
#								FIN LE12()								   #
##########################################################################################################

LE12()







