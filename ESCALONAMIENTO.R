library(readxl)
datos_gps <- read_excel("C:/Users/Melo/Desktop/Gradiente/datos_gps.xlsx")

datos_gps$clasebinaria <- ifelse(datos_gps$tipo_lesion=="Lesiones leves" | datos_gps$tipo_lesion=="Sin lesiones", "Leve", "Grave")
View(datos_gps)

################################### NA ################################

sum(is.na(datos_gps)) #muestra que hay NA


###para eliminar filas con NA
delete.na <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}

datos_gps <- data.frame(delete.na(datos_gps))
View(datos_gps)
##################################

library(ggplot2)
library(ggrepel)
library(plotrix)
library(smacof) #error
localizacion <- data.frame(datos_gps[,2:3])
D=dist(localizacion)
MCD_D=cmdscale(D, eig=T,k=2)

x=MCD_D$points[,1]
summary(x)
y=MCD_D$points[,2]
summary(y)
data=cbind(-x,-y)
datos=data.frame(data)
colnames(datos)=c("Latitud","Longitud")
rownames(datos)=datos_gps$informe
gpr <- as.factor(datos_gps[, "clasebinaria"])
p<- ggplot(datos,aes(x=Latitud, y=Longitud))
p + geom_point(aes(colour = factor(gpr)))+ scale_x_continuous(limit = c(0,1.5)) 
p + geom_point(aes(colour = factor(gpr)))+ scale_x_continuous(limit = c(0,1.5))+
  geom_text_repel(aes(label=rownames(datos)))

#install.packages("smacof")

library("smacof")


MCD.D=smacofSym(D,ndim=2)
MCD.D
plot(MCD.D, main="Configuracion SMACOF")
plot(MCD.D, plot.type = "stressplot", main="Stressplot")
plot(MCD.D, plot.type = "Shepard", main="Shepard")
plot(MCD.D, plot.type = "resplot", main="Residuos")


#AIzaSyD_oxlLGCLr4EPLPJu1fFjYFC87mkJj-VY