library(ggplot2)
library(MASS)
library(ggpubr)

#Limpieza de datos
##se importan los csv
  setwd("C:/Users/Noe_r/Downloads")
  co2<-read.csv("co2.csv")
  co2$Uncertainty=NULL
  co2.rev<-co2[c(-56,-57,-58,-59),]
  mar<-read.csv("mar.csv")
  #Se limpian los datos
  mar.rev<-mar[-1:-79,]
  mar.rev$GMSL.uncertainty=NULL
  dataf<-cbind(co2.rev,mar.rev)
  
  
  dataf$Time=NULL
  #añadimos y limpiamos ciertos datos, dejando solo los de interés
  gla<-read.csv("glaciers.csv")
  tem<-read.csv("temperatura.csv")
  tem$Source=NULL ## borre la coumna source que es la columna de las fuentes de información
  
  gla.rev<-gla[c(-1:-14,-70),]
  gla.rev$Number.of.observations=NULL
  dt2<-cbind(dataf,gla.rev)
  names(dt2)<-c("year","Co2","msnm","año","perdglaciar")
  tem<-tem[seq(dim(tem)[1],1),]
  l=c()
  for (i in seq(159,268,2)){
    l=c(l,i)
    } 
  tem<-tem[l,][2]

  dt2$year=NULL
  data2=dt2[,c(3,1,2,4)]
  data2<-cbind(data2,tem)
  names(data2)<-c("año","co2","msnm","dis_gla","temp")

  
  

#Plots de cada evento respecto al tiempo
ggplot(data=data2, aes(x=(año), y=(co2), color="Nivel de \nCO2(PPM)"))+geom_point()+geom_smooth(method='lm', formula= y~x, color="black")+labs(title = "Aumento de las concentraciones de CO2 por año (1959-2013)",x="Año", y = "CO2 PPM", colour="Etiqueta")+
stat_regline_equation(label.x = 1980, label.y = 410, color="black")


ggplot(data=data2, aes(x=(año), y=(msnm), color="Nivel del\n mar(ft)"))+geom_point()+geom_smooth(method='lm', formula= y~x, color="blue")+labs(title = "Aumento del nivel del mar por año (1959-2013)",x="Año", y = "Nivel del mar sobre 0(ft)", colour="Etiqueta")+
stat_regline_equation(label.x = 1980, label.y = 85, color="blue")

ggplot(data=data2, aes(x=(año), y=(dis_gla), color="Masa \nglaciar(%)"))+geom_point()+geom_smooth(method='lm', formula= y~x, color="green")+labs(title = "Pérdida de masa glaciar por año (1959-2013)",x="Año", y = "%Masa original", colour="Etiqueta")+
stat_regline_equation(label.x = 1980, label.y = -20, color="black")

ggplot(data=data2, aes(x=(año), y=(temp), color="Aumento de\ntemperatura(°C)"))+geom_point()+geom_smooth(method='lm', formula= y~x, color="orange")+labs(title = "Promedio de aumento de temperatura respecto a 1950(1959-2013)",x="Año", y = "Aumento de T(°C)", colour="Etiqueta")+
stat_regline_equation(label.x = 1980, label.y = 0.9, color="black")


ggplot(data=data2, aes(x=(co2), y=(msnm)))+ geom_point(color="blue")+geom_smooth(method='lm', formula= y~x, color="black",se=FALSE)+
labs(title = "Gráfica: CO2-Aumento nivel del mar", x = "CO2 PPM",y="Nivel del Mar(ft)",  colour="Etiqueta")


ggplot(data=data2, aes(x=(co2), y=(temp)))+ geom_point(color="red")+geom_smooth(method='lm', formula= y~x, color="black",se=FALSE)+
labs(title = "Gráfica: CO2-Aumento de Temperatura", x = "CO2 PPM",y="Aumento de T(°C)")

ggplot(data=data2, aes(x=(co2), y=(dis_gla)))+ geom_point(color="cyan3")+geom_smooth(method='lm', formula= y~x, color="lack",se=FALSE)+
labs(title = "Gráfica: CO2-Pérdida de masa glaciar", x = "CO2 PPM",y="Disminución masa glacia(%)")

head(data2)

d<-c("CO2(PPM)", "Nivel del mar(ft)","Masa glaciar(%)","Temperatura(°C)")
a<-c(max(data2$co2),max(data2$msnm),-max(abs(data2$dis_gla)),max(data2$temp))
b<-c(min(data2$co2),min(data2$msnm),max((data2$dis_gla)),min(data2$temp))
c<-c(mean(data2$co2),mean(data2$msnm),mean((data2$dis_gla)),mean(data2$temp))

Proms<-data.frame(d,a,b,c)
names(Proms)<-c("Parámetro" , "Máximo", "Mínimo", "Promedio")


#Revisamos la distribución de los datos
plot(density(data2$co2), main ="",col=c("darkslategray4","deeppink1") )
plot(density(data2$msnm),col=c("cornflowerblue","deeppink4"))
plot(density(data2$dis_gla),col=c("darkgoldenrod","deeppink1"))
plot(density(data2$temp),col=c("darkslategray4","deeppink1"))

#Calculamos los test
prueba <-wilcox.test(data2$co2,data2$msnm,paired=TRUE)
prueba2<-wilcox.test(data2$co2,data2$dis_gla,paired=TRUE)
prueba4<-wilcox.test(data2$co2,data2$temp,paired=TRUE)

print(prueba4)


#Correlaciones
A<-c("CO2-nivel del mar","CO2-Masa glaciar","CO2-Temperatura")
B<-c(cor(x=data2$co2,y=data2$msnm),cor(x=data2$co2,y=data2$dis_gla),cor(x=data2$co2,y=data2$temp))
Cor_data<-data.frame(A,B)
names(Cor_data)<-c("Datos","Coeficiente de correlación")
Cor_data


