library(readxl)
datos<- read_excel("C:/Users/Melo/Desktop/Gradiente/Formulario_T閏nico_Excel.xlsx")

#clase binaria
datos$clasebinaria <- ifelse(datos$tipo_lesion=="Lesiones leves" | datos$tipo_lesion=="Sin lesiones", "Leve", "Grave")

#edad
datos$edad1 <- ifelse(datos$conductor1<=20,"<20",
                      ifelse(datos$conductor1>=21 & datos$conductor1<30,"20",
                             ifelse(datos$conductor1>=31 & datos$conductor1<=50, "30-50","50+")))

datos$edad2 <- ifelse(datos$conductor2<=20,"<20",
                      ifelse(datos$conductor2>=21 & datos$conductor2<30,"20",
                             ifelse(datos$conductor2>=31 & datos$conductor2<=50, "30-50","50+")))
#rangos horarios

datos$horaNEW <- ifelse(0<=datos$hora & datos$hora<6,"noche",ifelse(6<=datos$hora & datos$hora<=12,"ma馻na",ifelse(13<=datos$hora & datos$hora<=16,"siesta",ifelse(17<=datos$hora & datos$hora<=21,"tarde","noche"))))

datos$horaNEW1 <- ifelse(0<=datos$hora & datos$hora<6,"noche",ifelse(6<=datos$hora & datos$hora<=13,"ma馻na",ifelse(14<=datos$hora & datos$hora<=21,"tarde","noche")))


# categoria horas pico
datos$hora_pico <- ifelse(6<=datos$hora & datos$hora<=8,"Entrada",
                          ifelse(9<=datos$hora & datos$hora<=12,"laboral_M",
                                 ifelse(13<=datos$hora & datos$hora<=14,"Salida_M",
                                        ifelse(15<=datos$hora & datos$hora<=16,"laboral_S",
                                               ifelse(17<=datos$hora & datos$hora<=19, "Salida_T",
                                                      ifelse(20<=datos$hora & datos$hora<=22, "Salida_N",
                                                             "Noche"))))))
#unidades
datos$unidad1_N <- ifelse(datos$unidad1=="Peaton" ,"tipo_0",ifelse(datos$unidad1=="Bicicleta" | datos$unidad1=="Motocicleta","tipo_1",
                                                                   ifelse(datos$unidad1=="Automovil" | datos$unidad1=="Camioneta" | datos$unidad1=="Furgon"| datos$unidad1=="Utilitario", "tipo_2","tipo_3")))
datos$unidad2_N <- ifelse(datos$unidad2=="Peaton" ,"tipo_0",ifelse(datos$unidad2=="Bicicleta" | datos$unidad2=="Motocicleta","tipo_1",
                                                                   ifelse(datos$unidad2=="Automovil" | datos$unidad2=="Camioneta" | datos$unidad2=="Furgon"| datos$unidad2=="Utilitario", "tipo_2",ifelse(datos$unidad2=="Solo","S_U","tipo_3"))))

datos$unidades_involucradas <- ifelse((datos$unidad1_N=="tipo_0"  & datos$unidad2_N=="tipo_1")|(datos$unidad1_N=="tipo_1"  & datos$unidad2_N=="tipo_0"),"P_M",ifelse((datos$unidad1_N=="tipo_0"  & datos$unidad2_N=="tipo_2")|(datos$unidad1_N=="tipo_2"  & datos$unidad2_N=="tipo_0"),"P_A",
                                                                                                                                                                     ifelse(datos$unidad1_N=="tipo_1"  & datos$unidad2_N=="tipo_1","M_M",
                                                                                                                                                                            ifelse((datos$unidad1_N=="tipo_2"  & datos$unidad2_N=="tipo_2"),"A_A",
                                                                                                                                                                                   ifelse((datos$unidad1_N=="tipo_1"  & datos$unidad2_N=="tipo_2")|(datos$unidad1_N=="tipo_2"  & datos$unidad2_N=="tipo_1"),"A_M",
                                                                                                                                                                                          ifelse((datos$unidad1_N=="tipo_1"& datos$unidad2_N=="tipo_3") | (datos$unidad1_N=="tipo_3"& datos$unidad2_N=="tipo_1"),"M_C",
                                                                                                                                                                                                 ifelse((datos$unidad1_N=="tipo_2"  & datos$unidad2_N=="tipo_3")|(datos$unidad1_N=="tipo_3"  & datos$unidad2_N=="tipo_2"),"A_C","solo")))))))
# dia / LESIONES  no hay asociacion
datos$DIA_NEW <- ifelse(datos$dia=="s醔ado" | datos$dia=="domingo", "fds","l-v" )

# dia / LESIONES  no hay asociacion
datos$DIA_NEW2 <- ifelse(datos$dia=="s醔ado" | datos$dia=="viernes", "V-S","L-J-D" )

############################################################################
#Arbol
library(rpart)
library(rpart.plot)

Base1 <- data.frame(datos$clasebinaria,datos$carnet,datos$unidades_involucradas,datos$hora_pico,datos$`estado de la calzada`,datos$`funciona semaforo`, datos$dia)
colnames(Base1)=c("clase","carnet","unidades_involucradas","hora","estado_calzada","semaforo_se馻l","dia")
Base1$carnet<-as.factor(Base1$carnet)
Base1$unidades_involucradas<-as.factor(Base1$unidades_involucradas)
Base1$dia<-as.factor(Base1$dia)
Base1$clase<-as.factor(Base1$clase)
modelo_arbol<-rpart(clase~., data = Base1)

setwd("C:/Users/Melo/Desktop") 
png(file = "arbol5.png" ,  width = 20, height = 10, units = 'in', res = 300)
prp(modelo_arbol, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

library(xlsx)
write.xlsx(Base, file = "Base1.xlsx")

Base2 <- data.frame(datos$Causalidad,datos$clasebinaria)
colnames(Base2)=c("Causalidad","clasebinaria")
Base2$Causalidad<-as.factor(Base2$Causalidad)
Base2$clasebinaria<-as.factor(Base2$clasebinaria)
modelo_arbol<-rpart(clasebinaria~., data = Base2)

png(file = "arbol6.png" ,  width = 20, height = 10, units = 'in', res = 300)
prp(modelo_arbol, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()


# Transforma a transacci贸n para pasarle a apriori()
tr6 <- as(Base2, "transactions")  # Convertir los datos a formato de transacci贸n
itemFrequency(tr6)

# Obtener reglas con par谩metros, Ac谩 fijo STN=1 como consecuente 
reglas <- apriori(tr6, parameter = list(minlen=1,maxlen=20, supp = 0.01, confidence = 0.7))
inspect(reglas)


######################### REGLA DE ASOCIACION





library(Matrix)

library(grid)
library(arules)
library(arulesViz)



# Discretizamos cada columna. Fiajte q a l aprimera le pone nombre feo
Base <- data.frame(datos$clasebinaria,datos$carnet,datos$unidades_involucradas,datos$rango,datos$dia)
colnames(Base)=c("clase","carnet","unidades_involucradas","rango","dia")
Base$carnet<-as.factor(Base$carnet)
Base$unidades_involucradas<-as.factor(Base$unidades_involucradas)
Base$dia<-as.factor(Base$dia)
Base$clase<-as.factor(Base$clase)
Base$rango<-as.factor(Base$rango)


# Transforma a transacci贸n para pasarle a apriori()
tr4 <- as(Base, "transactions")  # Convertir los datos a formato de transacci贸n
itemFrequency(tr4)

# Obtener reglas con par谩metros, Ac谩 fijo STN=1 como consecuente 
reglas <- apriori(tr4, parameter = list(minlen=1,maxlen=20, supp = 0.01, confidence = 0.7),
                  appearance = list(rhs = c("clase=Leve"), default = "lhs"))
inspect(reglas)

reglas.sorted <- sort(reglas, by="support")
inspect(reglas.sorted)

reglas_redundantes <- reglas[is.redundant(x = reglas, measure = "supp")]
inspect(reglas_redundantes)

reglas_maximales <- reglas[is.maximal(reglas)]
reglas_maximales
inspect(reglas_maximales)

reglas.max <- sort(reglas_maximales, by="confidence")
inspect(reglas.max)
#Se recuperan aquellas transacciones para las que se cumple la regla con mayor confianza de entre todas las encontradas.

as(reglas, "data.frame") %>%
  arrange(desc(confidence)) %>%
  head(1) %>%
  pull(rules)
df_transacciones <- as(transacciones, Class = "data.frame")


# Para representarlos con ggplot se convierte a dataframe 
as(top_20_itemsets, Class = "data.frame") %>%
  ggplot(aes(x = reorder(items, support), y = support)) +
  geom_col() +
  coord_flip() +
  labs(title = "Itemsets m醩 frecuentes", x = "itemsets") +
  theme_bw()
table(Base$clase)

library(xlsx)
write.xlsx(Base, file = "Base.xlsx")
