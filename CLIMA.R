# GENARADOR DE DATOS CLIMÁTICOS 

CLIMA = function(){

sink("log.txt")

setwd("C:/R.WD/")

source("LE.R")

source("LB.R")

source("TT-F.R")

source("TBO-F.R")

unlink("log.txt")

sink()

}

CLIMA()


