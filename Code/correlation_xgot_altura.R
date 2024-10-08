# LIBRARIES
library(dplyr)
library(readr)
library(ggplot2)
library(MASS)


#SUMMARY OF THIS DOCUMENT
#In this document we have loaded the data of different shots on goal, which we 
#have divided between the women's and men's leagues. 

#In this paper we seek to test how much the goalkeeper's height variable 
#influences the value of xGot. To do so, we are going to perform the Pearson 
#test and the Spearman test. 

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



#First of all, let's check in a general way, i.e. without specifying in which 
#area the shot was fired, the value for each of the tests.

#Women's league

cor(x=xgot$xGOT, y= xgot$Altura)
cor(x=xgot$xGOT, y= xgot$Altura, method = "spearman")

#Men's league
cor(x=xgot_m$xGOT, y= xgot_m$Altura)
cor(x=xgot_m$xGOT, y= xgot_m$Altura, method = "spearman")


#Calculation of the Pearson and Spearman test for each zone.

#High centre:
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "High centre")

resultante2_filtrada_1 <- xgot %>%
  filter(X == "High centre")

cor(x=resultante_filtrada_1$xGOT, y= resultante_filtrada_1$Altura)
cor(x=resultante_filtrada_1$xGOT, y=resultante_filtrada_1$Altura, method = "spearman")
cor(x=resultante2_filtrada_1$xGOT, y= resultante2_filtrada_1$Altura)
cor(x=resultante2_filtrada_1$xGOT, y= resultante2_filtrada_1$Altura, method = "spearman")

#High left

resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "High left")

resultante2_filtrada_1 <- xgot %>%
  filter(X == "High left")


cor(x=resultante_filtrada_1$xGOT, y= resultante_filtrada_1$Altura)
cor(x=resultante_filtrada_1$xGOT, y=resultante_filtrada_1$Altura, method = "spearman")
cor(x=resultante2_filtrada_1$xGOT, y= resultante2_filtrada_1$Altura)
cor(x=resultante2_filtrada_1$xGOT, y= resultante2_filtrada_1$Altura, method = "spearman")

#High right

resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "High right")

resultante2_filtrada_1 <- xgot %>%
  filter(X == "High right")


cor(x=resultante_filtrada_1$xGOT, y= resultante_filtrada_1$Altura)
cor(x=resultante_filtrada_1$xGOT, y=resultante_filtrada_1$Altura, method = "spearman")
cor(x=resultante2_filtrada_1$xGOT, y= resultante2_filtrada_1$Altura)
cor(x=resultante2_filtrada_1$xGOT, y= resultante2_filtrada_1$Altura, method = "spearman")

#low centre
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "Low centre")

resultante2_filtrada_1 <- xgot %>%
  filter(X == "Low centre")


cor(x=resultante_filtrada_1$xGOT, y= resultante_filtrada_1$Altura)
cor(x=resultante_filtrada_1$xGOT, y=resultante_filtrada_1$Altura, method = "spearman")
cor(x=resultante2_filtrada_1$xGOT, y= resultante2_filtrada_1$Altura)
cor(x=resultante2_filtrada_1$xGOT, y= resultante2_filtrada_1$Altura, method = "spearman")

#Low left
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "Low left")

resultante2_filtrada_1 <- xgot %>%
  filter(X == "Low left")


cor(x=resultante_filtrada_1$xGOT, y= resultante_filtrada_1$Altura)
cor(x=resultante_filtrada_1$xGOT, y=resultante_filtrada_1$Altura, method = "spearman")
cor(x=resultante2_filtrada_1$xGOT, y= resultante2_filtrada_1$Altura)
cor(x=resultante2_filtrada_1$xGOT, y= resultante2_filtrada_1$Altura, method = "spearman")

#Low right
resultante_filtrada_1 <- xgot_m %>%
  filter(zona == "Low right")

resultante2_filtrada_1 <- xgot %>%
  filter(X == "Low right")


cor(x=resultante_filtrada_1$xGOT, y= resultante_filtrada_1$Altura)
cor(x=resultante_filtrada_1$xGOT, y=resultante_filtrada_1$Altura, method = "spearman")
cor(x=resultante2_filtrada_1$xGOT, y= resultante2_filtrada_1$Altura)
cor(x=resultante2_filtrada_1$xGOT, y= resultante2_filtrada_1$Altura, method = "spearman")

