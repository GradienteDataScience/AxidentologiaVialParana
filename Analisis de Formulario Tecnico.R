library(readxl)
datos<- read_excel("C:/Users/Melo/Desktop/Gradiente/Formulario_Técnico_Excel.xlsx")
View(datos)
library(readxl)
accidentologia <- read_excel("C:/Users/Melo/Desktop/Gradiente/accidentologia-20190807-preprocesado-2 (1).xlsx")
accidentologia<-accidentologia[1:480,]
table(accidentologia$Lesiones)
11/(11+65+166+215) #fallecidos
65/(11+65+166+215) #lesiones graves
166/(11+65+166+215) #lesiones leves
215/(11+65+166+215) # sin lesiones

#clase binaria
datos$clasebinaria <- ifelse(datos$tipo_lesion=="Lesiones leves" | datos$tipo_lesion=="Sin lesiones", "Leve", "Grave")
table(datos$clasebinaria)
52/(107+52)
107/(107+52)

# cOMISARIA
table(datos$Comisaria)

#causalidad
table(datos$Causalidad)

#CARNET DE CONDUCIR
counts_CARNET <- table(datos$carnet)
counts_CARNET
barplot(counts_CARNET, main="Carnet de Conducir",  
        xlab="",col=c("blue","green"))
45/(114+45)
114/(114+45)

#edad
datos$edad1 <- ifelse(datos$conductor1<=20,"<20",
                      ifelse(datos$conductor1>=21 & datos$conductor1<30,"20",
                             ifelse(datos$conductor1>=31 & datos$conductor1<=50, "30-50","50+")))
                                   
datos$edad2 <- ifelse(datos$conductor2<=20,"<20",
                      ifelse(datos$conductor2>=21 & datos$conductor2<30,"20",
                             ifelse(datos$conductor2>=31 & datos$conductor2<=50, "30-50","50+")))

EDAD<- rbind(datos$edad1,datos$edad2)
EDAD_TABLA=table(EDAD)
barplot(EDAD_TABLA)
29/(29+70+111+75)
70/(29+70+111+75)
111/(29+70+111+75)
75/(29+70+111+75)
#VEHICULOS
counts_VEHICULOS <- table(datos$unidad_transito)
counts_VEHICULOS
barplot(counts_VEHICULOS, main="UNIDAD DE TRANSITO INVOLUCRADAS", horiz=F, 
        xlab="",col=c(1:31),las=2,cex.names=0.4)


#Tipo de Lesion
counts_Lesion <- table(datos$tipo_lesion)
counts_Lesion
barplot(counts_Lesion, main="Tipo de Lesion", horiz=F, 
        xlab="",col=c(1:4),las=1,cex.names=)

table(datos$tipo_lesion_fallecidos)
59/(52+89+18)
89/(52+89+18)
18/(52+89+18)

#Datos judicializados
counts_judicializado <- table(datos$judicializado)
counts_judicializado
barplot(counts_judicializado, main="Judicializados", horiz=F, 
        xlab="",col=c(1:4),las=1,cex.names=)
99/(99+59)
#Datos hora

library(dplyr)
counts_hora <- table(datos$hora)
chisq.test(counts_hora)

barplot(counts_hora, main="Hora", horiz=F, 
        xlab="",col=c(1:24),las=1,cex.names=)

datos$horaNEW <- ifelse(0<=datos$hora & datos$hora<6,"noche",ifelse(6<=datos$hora & datos$hora<=12,"mañana",ifelse(13<=datos$hora & datos$hora<=16,"siesta",ifelse(17<=datos$hora & datos$hora<=21,"tarde","noche"))))
head(datos$hora,10)
datos$horaNEW1 <- ifelse(0<=datos$hora & datos$hora<6,"noche",ifelse(6<=datos$hora & datos$hora<=13,"mañana",ifelse(14<=datos$hora & datos$hora<=21,"tarde","noche")))
head(datos$horaNEW1,10)
 
counts_rango <- table(datos$rango)
chisq.test(table(datos$rango))$expected
barplot(counts_rango, main="rango", horiz=F, 
        xlab="",col=c(1:24),las=1,cex.names=)
# categoria horas pico
datos$hora_pico <- ifelse(6<=datos$hora & datos$hora<=8,"Entrada",
                          ifelse(9<=datos$hora & datos$hora<=12,"laboral_M",
                                 ifelse(13<=datos$hora & datos$hora<=14,"Salida_M",
                                        ifelse(15<=datos$hora & datos$hora<=16,"laboral_S",
                                               ifelse(17<=datos$hora & datos$hora<=19, "Salida_T",
                                                      ifelse(20<=datos$hora & datos$hora<=22, "Salida_N",
                                                             "Noche"))))))





#Datos unidad
table(datos$unidad1)
barplot(table(datos$unidad1))

datos$unidad1_N <- ifelse(datos$unidad1=="Peaton" ,"tipo_0",ifelse(datos$unidad1=="Bicicleta" | datos$unidad1=="Motocicleta","tipo_1",
                                                                   ifelse(datos$unidad1=="Automovil" | datos$unidad1=="Camioneta" | datos$unidad1=="Furgon"| datos$unidad1=="Utilitario", "tipo_2","tipo_3")))
datos$unidad2_N <- ifelse(datos$unidad2=="Peaton" ,"tipo_0",ifelse(datos$unidad2=="Bicicleta" | datos$unidad2=="Motocicleta","tipo_1",
                                                                   ifelse(datos$unidad2=="Automovil" | datos$unidad2=="Camioneta" | datos$unidad2=="Furgon"| datos$unidad2=="Utilitario", "tipo_2",ifelse(datos$unidad2=="Solo","S_U","tipo_3"))))

table(datos$unidad1_N,datos$unidad2_N)

datos$unidades_involucradas <- ifelse((datos$unidad1_N=="tipo_0"  & datos$unidad2_N=="tipo_1")|(datos$unidad1_N=="tipo_1"  & datos$unidad2_N=="tipo_0"),"P_M",ifelse((datos$unidad1_N=="tipo_0"  & datos$unidad2_N=="tipo_2")|(datos$unidad1_N=="tipo_2"  & datos$unidad2_N=="tipo_0"),"P_A",
                                                                                                                                                                     ifelse(datos$unidad1_N=="tipo_1"  & datos$unidad2_N=="tipo_1","M_M",
                                                                                                                                                                            ifelse((datos$unidad1_N=="tipo_2"  & datos$unidad2_N=="tipo_2"),"A_A",
                                                                                                                                                                                   ifelse((datos$unidad1_N=="tipo_1"  & datos$unidad2_N=="tipo_2")|(datos$unidad1_N=="tipo_2"  & datos$unidad2_N=="tipo_1"),"A_M",
                                                                                                                                                                                          ifelse((datos$unidad1_N=="tipo_1"& datos$unidad2_N=="tipo_3") | (datos$unidad1_N=="tipo_3"& datos$unidad2_N=="tipo_1"),"M_C",
                                                                                                                                                                                         ifelse((datos$unidad1_N=="tipo_2"  & datos$unidad2_N=="tipo_3")|(datos$unidad1_N=="tipo_3"  & datos$unidad2_N=="tipo_2"),"A_C","solo")))))))



table(datos$unidades_involucradas)
table(datos$unidades_involucradas,datos$hora)

table(datos$`condicion climatica`)
131/(131+25)
table(datos$rango)
68/(91+68)
table(datos$viento)
table(datos$visibilidad)
table(datos$Iluminacion)
56/(56+89+3)
89/(56+89+3)
3/(56+89+3)
table(datos$calzada)
table(datos$`estado de la calzada`)
29/(118+4+25)
table(datos$`señalizacion vial`)
94/(94+65)

#### ANALISIS BIVARIADO
library(descr)
library(vcd)
###SI dia-noche
tabla0=table(datos$clasebinaria,datos$rango) 
prop.table(tabla0) 
chisq.test(tabla0)
fisher.test(tabla0)
chisq.test(tabla0)$observed
chisq.test(tabla0)$expected
chisq.test(tabla0)$stdres
assocstats(tabla0)
tab0=round(prop.table(tabla0),3)
addmargins(tab0)

###SI hora
tabla=table(datos$tipo_lesion_fallecidos,datos$horaNEW1) 
prop.table(tabla) 
chisq.test(tabla)
chisq.test(tabla)$observed
chisq.test(tabla)$expected
fisher.test(tabla)
chisq.test(tabla)$stdres
assocstats(tabla)
tab=round(prop.table(tabla),3)
addmargins(tab)


#VEHICULO INVOLUCRADOS 

tabla2=table(datos$unidad1_N,datos$unidad2_N) 
tabla2
fisher.test(tabla2)
chisq.test(tabla2)
chisq.test(tabla2)$observed
chisq.test(tabla2)$expected
chisq.test(tabla2)$stdres
assocstats(tabla2)
tab2=round(prop.table(tabla2),3)
addmargins(tab2) 

# LESIONES / VEHICULO INVOLUCRADOS -- no existe asociacion

tabla3=table(datos$clasebinaria,datos$unidades_involucradas) 
tabla3
fisher.test(tabla3)


# CARNET / LESIONES
tabla4 <- table(datos$carnet, datos$tipo_lesion_fallecidos)
tabla4
fisher.test(tabla4)
chisq.test(tabla4)
chisq.test(tabla4)$observed
chisq.test(tabla4)$expected
chisq.test(tabla4)$stdres
assocstats(tabla4)
tab4=round(prop.table(tabla4),3)
addmargins(tab4)

# NO HAY ASOCIACION ENTRE CARNET Y DATOS JUDICIALIZADO
tabla5 <- table(datos$carnet, datos$judicializado)
tabla5
chisq.test(tabla5)

# NO HAY ASOCIACION ENTRE CARNET Y unidades_involucradas
tabla6 <- table(datos$carnet, datos$unidades_involucradas)
tabla6
fisher.test(tabla6)

# NO HAY ASOCIACION ENTRE CARNET Y LA HORA
tabla7 <- table(datos$carnet, datos$horaNEW1)
tabla7
fisher.test(tabla7)
chisq.test(tabla7)

# no hay asociacion entre la edad y el no tener carnet
tabla8 <- table(datos$carnet, datos$edad2)
tabla8
fisher.test(tabla8)
chisq.test(tabla8)


# no hay asociacion entre la edad y que el caso este judicializado
tabla9 <- table(datos$edad2, datos$judicializado)
tabla9
fisher.test(tabla9)
chisq.test(tabla9)

#SI HAY ASOCIACION ENTRE TIPO DE LESION Y LA JUDICIALIZACION DEL CASO
tabla10 <- table(datos$tipo_lesion_fallecidos, datos$judicializado)
tabla10

chisq.test(tabla10)
chisq.test(tabla10)$observed
chisq.test(tabla10)$expected
chisq.test(tabla10)$stdres
assocstats(tabla10)
tab10=round(prop.table(tabla10),3)
addmargins(tab10)

# NO ASOCIACION ENTRE TENER CARNET Y LA JUDICIALIZACION DEL CASO
tabla11 <- table(datos$carnet, datos$judicializado)
tabla11
chisq.test(tabla11)
chisq.test(tabla11)$observed
chisq.test(tabla11)$expected
chisq.test(tabla11)$stdres
assocstats(tabla11)
tab11=round(prop.table(tabla11),3)
addmargins(tab11)



# dia / LESIONES  no hay asociacion
datos$DIA_NEW <- ifelse(datos$dia=="sábado" | datos$dia=="domingo", "fds","l-v" )
tabla12 <- table(datos$DIA_NEW, datos$clasebinaria)
tabla12
#fisher.test(tabla12)
chisq.test(tabla12)

# dia / LESIONES  no hay asociacion
datos$DIA_NEW2 <- ifelse(datos$dia=="sábado" | datos$dia=="viernes", "V-S","L-J-D" )
tabla15 <- table(datos$DIA_NEW2, datos$clasebinaria)
tabla15
chisq.test(tabla15)

#	VISIBILIDAD / LESIONES no hay asocacion
tabla13 <- table(datos$visibilidad, datos$tipo_lesion_fallecidos)
tabla13
fisher.test(tabla13)
chisq.test(tabla13)


#	CLIMA / LESIONES no hay asocacion

tabla14 <- table(datos$`condicion climatica`, datos$tipo_lesion_fallecidos)
tabla14
fisher.test(tabla14)
chisq.test(tabla14)


#	ESTADO CALZADA / LESIONES
tabla15 <- table(datos$`estado de la calzada`, datos$clasebinaria)
tabla15
fisher.test(tabla15)
chisq.test(tabla15)

#ILUMINACION / LESIONES
tabla16 <- table(datos$Iluminacion,datos$clasebinaria)
tabla16
fisher.test(tabla16)
chisq.test(tabla16)

###############################################################################

library("factoextra")
library("FactoMineR")
poison <- data.frame(datos$rango, datos$clasebinaria,datos$carnet,datos$DIA_NEW)
res.mca <- MCA(poison, graph = F)
summary(res.mca)
clasebinaria <- as.factor(poison$datos.clasebinaria)
fviz_mca_var(res.mca, repel = TRUE)

fviz_mca(res.mca , col.ind = "black", habillage = clasebinaria,
         addEllipses = TRUE, repel = TRUE, palette = c("blue", "black")) + theme_minimal()


#fviz_mca_ind(res.mca, col.ind = "blue", habillage = clasebinaria, addEllipses = TRUE, repel = TRUE) + theme_minimal()

#fviz_mca_biplot(res.mca)


###################################################
#base accidentes armar categorias segun importancia de causalidad
library(readxl)
accidentologia<- read_excel("C:/Users/Melo/Desktop/Gradiente/accidentologia-20190807-preprocesado-2 (1).xlsx")
#clase binaria
accidentologia$clasebinaria <- ifelse(accidentologia$Lesiones=="Lesiones leves" | accidentologia$Lesiones=="Sin lesiones"| accidentologia$Lesiones=="NA", "Leve", "Grave")
table(accidentologia$clasebinaria)

accidentologia <- data.frame(accidentologia$Causalidad,accidentologia$clasebinaria)
colnames(accidentologia)=c("Causalidad","Clase")
# hay que re pensar catgeorias
tabla20 <- table(accidentologia)
tabla20


library("factoextra")
library("FactoMineR")

res.mca1 <- MCA(accidentologia, graph = F)
summary(res.mca1)

fviz_mca_var(res.mca1, repel = TRUE)
Clase <- as.factor(accidentologia$Clase)
fviz_mca(res.mca1 , col.ind = "black", habillage = Clase,
         addEllipses = TRUE, repel = TRUE, palette = c("blue", "black")) + theme_minimal()


fviz_mca_ind(res.mca1, col.ind = "blue", habillage = Clase, addEllipses = TRUE, repel = TRUE) + theme_minimal()

fviz_screeplot(res.mca1, addlabels = TRUE)
fviz_contrib(res.mca1, choice ="var", axes = 1)
