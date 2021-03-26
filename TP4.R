#---------------PRACTICO CUATRO------------#
rm(list=ls())
library(mfx) # para evaluar el efecto marginal en el valor medio
library(margins) # para evaluar el efecto marginal en un valor cualquiera
library(ggplot2) # para hacer un gráfico bonito
library(haven)
library(stargazer)


#Directorio
dataDir <- "C:/Users/juan_/Desktop/Econometría/Practico4"
setwd(dataDir)

#Abriendo la base de datos:
data <- read_dta("cuarto_trim_2019.dta")
View(data)

mean(ingrerso_per_capita, data)

# Borramos valores faltantes (Dado que la unica variable con NA es "intensi", y es una var relevante para analizar,
#borramos con el comando na.omit las filas que)
#OJOoooooooooooooooooooooo!!!!!!!!!!!!!!!!!!XXXXXXXXXXXXXXXXXXXXX!!!!!!!!!!!!!!!!!!!!!!!
data=na.omit(data)
summary(data)

# Convierto las variables categoricas con el comando factor, ya que R las identifica como formato numero
data$deserta <- factor(data$deserta)
data$mujer <- factor(data$mujer)
data$jmujer <- factor(data$jmujer)



# Chequeamos que se convirtieron en variables categoricas
summary(data)
xtabs(~mujer + deserta, data = data)   #Desertan el 10.2% de las mujeres y el 7.57% de los hombres

#------------------------------------------------Punto 1
#Desarrollado en el documento...

#------------------------------------------------Punto 2
probit <- glm(deserta ~ edad + mujer + hermanos + ingresos_familiares +
                jmujer + educ_jefe + miembros,
              family = binomial(link = "probit"),
              data = data)
summary(probit)

stargazer(probit, type="text", median=TRUE, digits=4, title="Probit")

#------------------------------------------------Punto 3
# Evaluemos en las medias para el modelo probit
margin.fit <- probitmfx(deserta ~ edad + mujer + hermanos + ingresos_familiares + jmujer
                        + educ_jefe + miembros,
                        data = data , atmean = TRUE, robust = TRUE)
margin.fit # "Para el individuo medio ....."

# Veamos el Average Marginal Effect: evalúa en todos los puntos y saca el promedio
summary(margins(probit)) # ver "at", donde puedo partir las variables para ver como cambia el ME segun si asigno algun valor a cada variable...

# EN LUGAR ESPECIFICO
newvalues <- with(data, data.frame(edad=median(edad), hermanos=median(hermanos),
                                   ingresos_familiares=median(ingresos_familiares),
                                   educ_jefe=median(educ_jefe) , miembros=median(miembros)
                                   ,mujer="1",jmujer="1"))
data.2 <- data[, c("edad","hermanos","ingresos_familiares","educ_jefe","miembros","mujer","jmujer")]
data.2[, "prediction"] <- predict(data="data.2",probit,newvalues,type="response")

#------------------------------------------------Punto 4
# MLP
mlp.fit <- lm(deserta ~ edad + mujer + hermanos + ingresos_familiares + jmujer + educ_jefe + miembros,data = data)
stargazer(mlp.fit, type="text", median=TRUE, digits=4, title="MLP")

#------------------------------------------------Punto 5
#limpiar valores <1
data$ingreso_per_capita [data$ingreso_per_capita < 1 ] = 1
data$ln_ing = log(data$ingreso_per_capita)
#------------------------------------------------Punto 6
newvalues2 <- with(data, data.frame(edad=median(edad),
                                    educ_jefe=median(educ_jefe), miembros=median(miembros)
                                    ,mujer,ln_ing,deserta))

#$ln_ing <- data[, c("ln_ing")]


probit3 <- glm(deserta ~ mujer + ln_ing +educ_jefe + miembros + edad
               , family = binomial(link = "probit"),
               data = newvalues2)

newvalues2[,"prediction"] <- predict(data="newvalues2",probit3,type="response")

ggplot(newvalues2, aes(x=ln_ing, y=prediction, colour=mujer)) + geom_line()


newvalues3 <- with(data, data.frame(edad=median(edad),
                                    educ_jefe=median(educ_jefe), miembros=median(miembros)
                                    ,jmujer,ln_ing,deserta))

probit4 <- glm(deserta ~ jmujer + ln_ing +educ_jefe + miembros + edad
               , family = binomial(link = "probit"),
               data = newvalues3)

newvalues3[,"prediction"] <- predict(data="newvalues3",probit4,type="response")

ggplot(newvalues3, aes(x=ln_ing, y=prediction, colour=jmujer)) + geom_line()

#plot(ln_ing,prediction, data=newvalues3,xlim = c(-0.4, 1.4),ylim = c(-0.4, 1.4))
#print(gplot(ln_ing,prediction,data=newvalues3) + xlim(-0.4, 1.4))



#EJERCICIO 7!!!!!!!!!!

probit5 <- glm(deserta ~ jmujer + ln_ing +educ_jefe + hermanos, family = binomial(link = "probit"),
               data = data)

newvalues0 <- with(data, data.frame(ln_ing=median(ln_ing),jmujer="1",hermanos=median(hermanos),
                                    educ_jefe=median(educ_jefe)))
newvalues0[, "prediction"] <- predict(probit5,newvalues0,type="response")


newvalues10 <- with(data, data.frame(ln_ing=median(ln_ing),jmujer="0",hermanos=median(hermanos),
                                     educ_jefe=median(educ_jefe)))
newvalues10[, "prediction"] <- predict(probit5,newvalues10,type="response")                  

Valores[newvalues0,newvalues10]

margin.fit <- probitmfx(deserta ~ jmujer + ln_ing +educ_jefe + hermanos,
                        data = data, atmean = TRUE, robust = TRUE)
margin.fit

stargazer(margin.fit, type="text", median=TRUE, digits=4, title="MLP")


margin.fit <- probitmfx(deserta ~ edad + mujer + hermanos + ingresos_familiares + jmujer
                        + educ_jefe + miembros,
                        data = data , atmean = TRUE, robust = TRUE)

data.2[, "prediction"] <- predict(data="data.2",probit,newvalues,type="response")



#probit2 <- glm(deserta ~ mujer + hermanos + ln_ing
#              , family = binomial(link = "probit"),
#             data = data)
#summary(probit2)
#mydata.2 <- data[, c("mujer","ln_ing")]
#mydata.2[,"prediction"] <- predict(data="mydata.2",probit2,type="response")



#ggplot(mydata.2, aes(x=ln_ing, y=prediction, colour=mujer)) + geom_line()


#probit3 <- glm(deserta ~ ln_ing, family=binomial(link="probit"), data=data)
#mydata.2$desertafit3= probit3$fitted.values