#1 grafico pie

library(plotrix)

num<-sort(gp$num)
paa<-gp$Pais
pie3D(num,radius = 0.8,labels = lbls, explode = 0.1,main="Distribucion de Cantidad de producto por Pais",
      col = c("white","yellow","blue","red"))
legend(x = "topright", legend = c("195", "273","2019","2513"), 
       fill = c("white","yellow","blue","red"), 
       title = "Cantidad")

#2grafico barras gradiente


orden<-order(df_Datos$Precio)
orden
df_Datos[orden,]

View(head(df_Datos[orden,]))
orden_grafico<-head(df_Datos[orden,])

ctd<-orden_grafico$Cantidad
precio<-orden_grafico$Precio
Año<-orden_grafico$Fecha_Ingreso

Prec_Baj<-data.frame(Año,ctd,precio)
ggplotprbajos<-ggplot( Prec_Baj , aes(x= Año ,y= ctd, fill = precio)) + 
  geom_bar(width = 0.5,stat  = "identity") 

ggplotprbajos + scale_fill_gradient(low="purple", high="black")

#3grafico barra color gradiente  


View(tail(df_Datos[orden,]))
elevados<-tail(df_Datos[orden,])
idelevados<-elevados$id

precioelevados<-elevados$Precio

ggplotprecios<-ggplot( Prec_El , aes(x= fecha ,y= cant, fill = Prec)) + 
  geom_bar(width = 0.5,stat  = "identity") 

ggplotprecios + scale_fill_gradient(low="blue", high="red")


#4Grafico barras con case when

grupo_genero<-group_by(df_Datos,Genero)

gg=data.frame(summarise(grupo_genero,num=n()))

ggord<-arrange(gg,desc(num))



Clasif<-ggord5 %>% mutate (Critica= c(case_when(Genero=="Drama" ~ "Super Exitosa",
                                                Genero=="Comedy" ~ "Exitosa",
                                                Genero=="Documentary" ~ "Muy Buena", 
                                                Genero=="Comedy|Drama" ~ "Buena",
                                                Genero=="Drama|Romance" ~ "Regular")))

calsifOrd<-arrange(Clasif,desc(Critica))

calsifOrd

ggplot(data = calsifOrd,aes(x=Genero, y= num, fill= Critica ))+
  geom_bar(stat = "identity",position = "dodge")



#5grafico de puntos 

grupoCA<-group_by(Access,Categoria)

GCA=data.frame(summarise(grupoCA,num=n()))

GCAO<-arrange(GCA,desc(num))
GCAO

qplot(Categoria, num, data = GCAO, color = Categoria, size = num, alpha = I(1))


#6grafico de barras case when 
Company

Clasif<-Company %>% mutate (Critica= c(case_when(num >= 159 ~ "Super Exitosa",
                                                 num  < 159  ~ "Estable",)))
Clasif


ggplot(data = Clasif,aes(x=Companía, y= num, fill= Critica ))+
  geom_bar(stat = "identity",position = "dodge")

Listo
