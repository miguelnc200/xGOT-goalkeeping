# LIBRARIES
library(dplyr)
library(readr)


#SUMMARY OF THIS DOCUMENT
#In this document we have loaded the data of different shots on goal, which we 
#have divided between the women's and men's leagues. 

#We have tested the null hypothesis that the mean of the Resultant values
#in each of the leagues is the same deppending if the shot was a goal or not. 
#We will now test whether or not we reject this null hypothesis by using the 
#t.test function and look at the p-value to make our decision.

#LOADING DATA
#Women's leagues

xgot <- read_delim("GitHub/xGOT-goalkeeping/xgot.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Partido = col_skip(), 
                                                                        `Equipo tiro` = col_skip(), `Equipo portera` = col_skip(), 
                                                                        Portera = col_skip()), trim_ws = TRUE)

#Men's leagues
xgot_m <- read_delim("GitHub/xGOT-goalkeeping/xgot_m.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(Partido = col_skip(), 
                                                                          `Equipo tiro` = col_skip(), `Equipo portero` = col_skip(), 
                                                                          Portero = col_skip()), trim_ws = TRUE)


#Calculation of the different zones when the shot on goal was not a goal.

#High centre:
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "High centre")

resultante_filtrada_1 <- resultante_filtrada_1 %>%
  filter(`Gol (0/1)` == 0)

resultante2_filtrada_1 <- xgot %>%
  filter(X == "High centre")

resultante2_filtrada_1 <- resultante2_filtrada_1 %>%
  filter(`Gol (0/1)` == 0)

grupo1 <- resultante_filtrada_1$`Resultante
de cada tiro`
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)
t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)


#High left

resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "High left")
resultante_filtrada_1 <- resultante_filtrada_1 %>%
  filter(`Gol (0/1)` == 0)

resultante2_filtrada_1 <- xgot %>%
  filter(X == "High left")

resultante2_filtrada_1 <- resultante2_filtrada_1 %>%
  filter(`Gol (0/1)` == 0)

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#High right

resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "High right")
resultante_filtrada_1 <- resultante_filtrada_1 %>%
  filter(`Gol (0/1)` == 0)

resultante2_filtrada_1 <- xgot %>%
  filter(X == "High right")

resultante2_filtrada_1 <- resultante2_filtrada_1 %>%
  filter(`Gol (0/1)` == 0)

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#low centre
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "Low centre")
resultante_filtrada_1 <- resultante_filtrada_1 %>%
  filter(`Gol (0/1)` == 0)

resultante2_filtrada_1 <- xgot %>%
  filter(X == "Low centre")

resultante2_filtrada_1 <- resultante2_filtrada_1 %>%
  filter(`Gol (0/1)` == 0)

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#Low left
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "Low left")
resultante_filtrada_1 <- resultante_filtrada_1 %>%
  filter(`Gol (0/1)` == 0)

resultante2_filtrada_1 <- xgot %>%
  filter(X == "Low left")

resultante2_filtrada_1 <- resultante2_filtrada_1 %>%
  filter(`Gol (0/1)` == 0)

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#Low right
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "Low right")
resultante_filtrada_1 <- resultante_filtrada_1 %>%
  filter(`Gol (0/1)` == 0)

resultante2_filtrada_1 <- xgot %>%
  filter(X == "Low right")

resultante2_filtrada_1 <- resultante2_filtrada_1 %>%
  filter(`Gol (0/1)` == 0)

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#Calculando los p valores para la resultante de cada una de las distintas zonas y cada uno de los valores en la columna Gol (0/1)

#High centre:
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "High centre")

resultante_filtrada_1 <- resultante_filtrada_1 %>%
  filter(`Gol (0/1)` == 1)

resultante2_filtrada_1 <- xgot %>%
  filter(X == "High centre")

resultante2_filtrada_1 <- resultante2_filtrada_1 %>%
  filter(`Gol (0/1)` == 1)

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)
t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)


#High left

resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "High left")
resultante_filtrada_1 <- resultante_filtrada_1 %>%
  filter(`Gol (0/1)` == 1)

resultante2_filtrada_1 <- xgot %>%
  filter(X == "High left")

resultante2_filtrada_1 <- resultante2_filtrada_1 %>%
  filter(`Gol (0/1)` == 1)

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#High right

resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "High right")
resultante_filtrada_1 <- resultante_filtrada_1 %>%
  filter(`Gol (0/1)` == 1)

resultante2_filtrada_1 <- xgot %>%
  filter(X == "High right")

resultante2_filtrada_1 <- resultante2_filtrada_1 %>%
  filter(`Gol (0/1)` == 1)

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#low centre
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "Low centre")
resultante_filtrada_1 <- resultante_filtrada_1 %>%
  filter(`Gol (0/1)` == 1)

resultante2_filtrada_1 <- xgot %>%
  filter(X == "Low centre")

resultante2_filtrada_1 <- resultante2_filtrada_1 %>%
  filter(`Gol (0/1)` == 1)

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#Low left
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "Low left")
resultante_filtrada_1 <- resultante_filtrada_1 %>%
  filter(`Gol (0/1)` == 1)

resultante2_filtrada_1 <- xgot %>%
  filter(X == "Low left")

resultante2_filtrada_1 <- resultante2_filtrada_1 %>%
  filter(`Gol (0/1)` == 1)

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#Low right
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "Low right")
resultante_filtrada_1 <- resultante_filtrada_1 %>%
  filter(`Gol (0/1)` == 1)

resultante2_filtrada_1 <- xgot %>%
  filter(X == "Low right")

resultante2_filtrada_1 <- resultante2_filtrada_1 %>%
  filter(`Gol (0/1)` == 1)

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

