##  aids_dairy_products.R
##  
##  Héctor Garrido Henríquez
##  Analista Cuantitativo. Observatorio Laboral Ñuble
##  Docente. Facultad de Ciencias Empresariales
##  Universidad del Bío-Bío
##  Avenida Andrés Bello 720, Casilla 447, Chillán
##  Teléfono: +56-942353973
##  http://www.observatoriolaboralnuble.cl
##
##  Esta rutina se utiliza para estimar un modelo Almost ideal demand System para productos
## lácteos en Chile

rm(list=ls())

if (!require(readxl)) install.packages("readxl"); require(readxl)
if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(stringi)) install.packages("stringi"); require(stringi)
if (!require(micEconAids)) install.packages("micEconAids"); require(micEconAids)

## Carga de los datos 

## Base de gastos
path1 = file.path("~/GoogleDrivePersonal/Research/Papers in progress",
                    "Article - AIDS Dairy Products/dairy_data.xlsx")

gasto <- read_excel(path1,sheet = 2)
kilos <- read_excel(path1, sheet = 3)

gasto = gasto %>% mutate(date = gsub("M\\$","", fecha), 
                         month = stri_sub(date,2,4), 
                         year = stri_sub(date,5,9), 
                         mes = ifelse(month == "ENE",1,
                                 ifelse(month == "FEB",2,
                                 ifelse(month == "MAR",3,
                                 ifelse(month == "ABR",4,
                                 ifelse(month == "MAY",5,
                                 ifelse(month == "JUN",6,
                                 ifelse(month == "JUL",7,
                                 ifelse(month == "AGO",8,
                                 ifelse(month == "SEP",9,
                                 ifelse(month == "OCT",10,
                                 ifelse(month == "NOV",11,
                                 ifelse(month == "DIC",12,NA)))))))))))),
                         fecha_def = paste0(mes,"/",1,"/",year), 
                         fecha_def = as.Date(fecha_def,"%m/%d/%Y")) %>% arrange(fecha_def)

kilos = kilos %>% mutate(date = gsub("M\\$","", fecha), 
                         month = stri_sub(date,2,4), 
                         year = stri_sub(date,5,9), 
                        mes = ifelse(month == "ENE",1,
                        ifelse(month == "FEB",2,
                        ifelse(month == "MAR",3,
                        ifelse(month == "ABR",4,
                        ifelse(month == "MAY",5,
                        ifelse(month == "JUN",6,
                        ifelse(month == "JUL",7,
                        ifelse(month == "AGO",8,
                        ifelse(month == "SEP",9,
                        ifelse(month == "OCT",10,
                        ifelse(month == "NOV",11,
                        ifelse(month == "DIC",12,NA)))))))))))),
                         fecha_def = paste0(mes,"/",1,"/",year), 
                         fecha_def = as.Date(fecha_def,"%m/%d/%Y")) %>% arrange(fecha_def)


options(scipen = 10)

ts.plot(gasto[,3:11], main = "", col = 2:12)
legend("topleft", legend = colnames(gasto[3:11]), col=2:11, lty =1, cex = 0.7)
ts.plot(kilos[,3:11], main = "", col = 2:12)
legend("topleft", legend = colnames(kilos[3:11]), col=2:11, lty =1, cex = 0.7)

precios = cbind(fecha = gasto$fecha_def,(gasto[,2:11]/kilos[,2:11])*1000)

ts.plot(precios[,3:11], col = 2:11)
legend("topleft", legend = colnames(precios[3:11]), col=2:11, lty =1)

## Productos relacionados: Quedo, mantequilla, leche y yoghurt

gasto = gasto %>% rename(leche = `Leche Líquida`, mantequilla = Mantequilla,
                         queso = `Queso Maduro`, yoghurt = Yoghurt) %>% 
        select(leche, mantequilla, queso, yoghurt) %>% 
  mutate(total = leche+mantequilla+queso+yoghurt)

precios = precios %>% rename(leche = `Leche Líquida`, mantequilla = Mantequilla,
                         queso = `Queso Maduro`, yoghurt = Yoghurt) %>% 
  select(leche, mantequilla, queso, yoghurt)

m = gasto$total
w1 = gasto$leche/m
w2 = gasto$mantequilla/m
w3 = gasto$queso/m
w4 = gasto$yoghurt/m

p1 = precios$leche 
p2 = precios$mantequilla
p3 = precios$queso
p4 = precios$yoghurt

datos = data.frame(p1,p2,p3,p4,w1,w2,w3,w4,m)
PriceNames = c("p1","p2","p3","p4")
ShareNames = c("w1","w2","w3","w4")

options(scipen =3)

# Estimación del modelo no-lineal con el índice de Stone
laaids_est = aidsEst(PriceNames,ShareNames,"m", data = datos, method = "IL")

summary(laaids_est)

all.equal( sum( coef( laaids_est )$alpha ), 1 )
all.equal( sum( coef( laaids_est )$beta ), 0 )
all.equal( colSums( coef(laaids_est)$gamma ), rep( 0, 4 ), check.attributes = FALSE )


pMeans = colMeans(datos[,PriceNames])
wMeans = colMeans(datos[,ShareNames])

aidsResultElas = aidsElas(coef(laaids_est), prices = pMeans, shares = wMeans, 
                          coefCov = vcov(laaids_est), 
                          df = df.residual(laaids_est))

summary(aidsResultElas)