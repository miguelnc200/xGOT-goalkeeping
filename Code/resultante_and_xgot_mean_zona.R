# LIBRARIES
library(dplyr)
library(readr)

#SUMMARY OF THIS DOCUMENT
#In this document we have loaded the data of different shots on goal, which we 
#have divided between the women's and men's leagues. 

#We have tested the null hypothesis that the mean of the Xgot and Resultant values
#in each of the leagues is the same. We will now test whether or not we reject this null 
#hypothesis by using the t.test function and look at the p-value to make our decision.


#LOADING DATA

#Women's leagues

xgot <- read_delim("GitHub/xGOT-goalkeeping/xgot.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Partido = col_skip(), 
                                                                        `Equipo tiro` = col_skip(), `Equipo portera` = col_skip(), 
                                                                        Portera = col_skip()), trim_ws = TRUE)

#Men's leages
xgot_m <- read_delim("GitHub/xGOT-goalkeeping/xgot_m.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(Partido = col_skip(), 
                                                                          `Equipo tiro` = col_skip(), `Equipo portero` = col_skip(), 
                                                                          Portero = col_skip()), trim_ws = TRUE)

#We will test if the the null hypothesis that the two means are equal for each
# of the zones. Let's test it with the t.test function.

#High centre:
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "High centre")

resultante2_filtrada_1 <- xgot %>%
  filter(X == "High centre")

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1) 
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)
t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

grupo1 <- resultante_filtrada_1$`Resultante\nde cada tiro`
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$`Resultante\nde cada tiro`
mean(grupo2)
var(grupo2)
t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)


#High left

resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "High left")

resultante2_filtrada_1 <- xgot %>%
  filter(X == "High left")

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1) 
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)
t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

grupo1 <- resultante_filtrada_1$`Resultante\nde cada tiro`
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$`Resultante\nde cada tiro`
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#High right

resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "High right")

resultante2_filtrada_1 <- xgot %>%
  filter(X == "High right")

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1) 
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)
t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)


grupo1 <- resultante_filtrada_1$`Resultante\nde cada tiro`
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$`Resultante\nde cada tiro`
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#low centre
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "Low centre")

resultante2_filtrada_1 <- xgot %>%
  filter(X == "Low centre")

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1) 
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)
t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

grupo1 <- resultante_filtrada_1$`Resultante\nde cada tiro`
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$`Resultante\nde cada tiro`
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#Low left
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "Low left")

resultante2_filtrada_1 <- xgot %>%
  filter(X == "Low left")

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1) 
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)
t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

grupo1 <- resultante_filtrada_1$`Resultante\nde cada tiro`
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$`Resultante\nde cada tiro`
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#Low right
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "Low right")

resultante2_filtrada_1 <- xgot %>%
  filter(X == "Low right")

grupo1 <- resultante_filtrada_1$xGOT
mean(grupo1) 
var(grupo1)

grupo2 <- resultante2_filtrada_1$xGOT
mean(grupo2)
var(grupo2)
t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

grupo1 <- resultante_filtrada_1$`Resultante\nde cada tiro`
mean(grupo1)
var(grupo1)

grupo2 <- resultante2_filtrada_1$`Resultante\nde cada tiro`
mean(grupo2)
var(grupo2)

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)
