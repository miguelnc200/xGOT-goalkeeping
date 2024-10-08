# Cargar las bibliotecas necesarias
library(dplyr)
library(readr)
library(ggplot2)

#SUMMARY OF THIS DOCUMENT
#In this document we have loaded the data of different shots on goal, which we 
#have divided between the women's and men's leagues. 

#We have made a number of basic checks to begin to draw conclusions.


#LOADING DATA
#Women's leagues

xgot <- read_delim("GitHub/xGOT-goalkeeping/xgot.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Partido = col_skip(), 
                                                                        `Equipo tiro` = col_skip(), `Equipo portera` = col_skip(), 
                                                                        Portera = col_skip()), trim_ws = TRUE)

mi_dataframe <- data.frame(xgot[, c("Arriba/abajo", "X", "xGOT", "Gol (0/1)", "Altura", "Resultante\nde cada tiro")])

mi_dataframe <- xgot %>%
  select("Arriba/abajo", "X", "xGOT", "Gol (0/1)", "Altura","Resultante\nde cada tiro")

#Men's leagues
xgot_m <- read_delim("GitHub/xGOT-goalkeeping/xgot_m.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(Partido = col_skip(), 
                                                                          `Equipo tiro` = col_skip(), `Equipo portero` = col_skip(), 
                                                                          Portero = col_skip()), trim_ws = TRUE)

mi_dataframe2 <- data.frame(xgot_m[, c("Arriba/abajo", "zona", "xGOT", "Gol (0/1)", "Altura", "Resultante\nde cada tiro")])

mi_dataframe2 <- xgot_m %>%
  select("Arriba/abajo", "zona", "xGOT", "Gol (0/1)", "Altura","Resultante\nde cada tiro")

#First of all we have made a table, where we are going to calculate the average
#of the variable xGot, depending on the height at which the shot was taken.

#Women's leagues
media_xGOT_por_altura <- mi_dataframe %>%
  group_by(`Arriba/abajo`, `Gol (0/1)`) %>%
  summarise(media_xGOT = mean(xGOT, na.rm = TRUE),
            varianza_XGOT = var(xGOT, na.rm = TRUE))
View(media_xGOT_por_altura)

#Men's leagues
media_xGOT_por_altura2 <- mi_dataframe2 %>%
  group_by(`Arriba/abajo`, `Gol (0/1)`) %>%
  summarise(media_xGOT = mean(xGOT, na.rm = TRUE),
            varianza_XGOT = var(xGOT, na.rm = TRUE))
View(media_xGOT_por_altura2)


# Secondly, we have made a table where we calculate the mean and variance for each
#of the leagues. Thanks to this table, we can make a histogram that will help us#
#to better see the similarity or difference between the means of the different zones.

#Women's leagues
media_xGOT_por_zona <- mi_dataframe %>%
  group_by(X, `Gol (0/1)`) %>%
  summarise(media_xGOT = mean(xGOT, na.rm = TRUE),
            varianza_XGOT = var(xGOT, na.rm = TRUE))
View(media_xGOT_por_zona)

#Women's leagues Histogram

# Filter the value 'Gol (0/1)' == 1
datos_filtrados <- media_xGOT_por_zona %>%
  filter(`Gol (0/1)` == 1)

# Histogram generation
ggplot(datos_filtrados, aes(x = X, y = media_xGOT, fill = X)) +
  geom_col() +
  labs(title = "Media de xGOT por zona (Gol = 1)",
       x = "Zona",
       y = "Media de xGOT") +
  theme_minimal()

#Men's leagues
media_xGOT_por_zona2 <- mi_dataframe2 %>%
  group_by(zona, `Gol (0/1)`) %>%
  summarise(media_xGOT = mean(xGOT, na.rm = TRUE),
            varianza_XGOT = var(xGOT, na.rm = TRUE))
View(media_xGOT_por_zona2)

#Men's leagues Histogram

# Filter the value 'Gol (0/1)' == 1
datos_filtrados2 <- media_xGOT_por_zona2 %>%
  filter(`Gol (0/1)` == 1)

# Histogram generation
ggplot(datos_filtrados2, aes(x = zona, y = media_xGOT, fill = zona)) +
  geom_col() +
  labs(title = "Media de xGOT por zona (Gol = 1)",
       x = "Zona",
       y = "Media de xGOT") +
  theme_minimal()


#Thanks to all the data that we have tested, we can make the null hypothesis 
#that the two means are equal. Let's test it with the t.test function.

#Women's Leagues
grupo1 <- mi_dataframe$xGOT

#Men's Leagues
grupo2 <- mi_dataframe2$xGOT

t_test_result <- t.test(grupo1, grupo2)
print(t_test_result)

#Loading the last line, we can check that the p-value is higher than 0.05, so
#we can't reject the null hypothesis. Now we will check the same for the variance.

f_test_result <- var.test(grupo1, grupo2)
print(f_test_result)

#In the same way, we can also see that we cannot reject the hypothesis put forward.


#Lastly, we have made the same checks by changing the variable xGot, 
#for the variable Resultant. Before we continue, let's check whether the mean 
#value of the resulting variable of both leagues are similar. 

#Women's leagues
media_resultante =mean(mi_dataframe$`Resultante\nde cada tiro`)
View(media_resultante)

#Men's leagues
media_resultante2 =mean(mi_dataframe2$`Resultante\nde cada tiro`)
View(media_resultante2)



#Next, we are going to perform similar calculations we did for for the xGot variable,
# but with the variable resulting.

#First of all, where we are going to calculate the average
#of the variable resulting, depending on the height at which the shot was taken.

#Women's leagues

media_resultante_por_altura <- mi_dataframe %>%
  group_by(`Arriba/abajo`,`Gol (0/1)`) %>%
  summarise(media_Resultante = mean(`Resultante\nde cada tiro`, na.rm = TRUE),
            varianza_Resultante = var(`Resultante\nde cada tiro`, na.rm = TRUE))
View(media_resultante_por_altura)


#Men's leagues

media_resultante_por_altura2 <- mi_dataframe2 %>%
  group_by(`Arriba/abajo`,`Gol (0\1)`) %>%
  summarise(media_Resultante = mean(`Resultante\nde cada tiro`, na.rm = TRUE),
            varianza_Resultante = var(`Resultante\nde cada tiro`, na.rm = TRUE))
View(media_resultante_por_altura2)

#Now we are going to check the same but for the 6 goal zones.

#Women's leagues


media_resultante_por_zona <- mi_dataframe %>%
  group_by(X, `Gol (0/1)`) %>%
  summarise(media_Resultante = mean(`Resultante\nde cada tiro`, na.rm = TRUE),
            varianza_Resultante = var(`Resultante\nde cada tiro`, na.rm = TRUE))
View(media_resultante_por_zona)


#Men's leagues

media_resultante_por_zona2 <- mi_dataframe2 %>%
  group_by(zona, `Gol (0/1)`) %>%
  summarise(media_Resultante = mean(`Resultante\nde cada tiro`, na.rm = TRUE),
            varianza_Resultante = var(`Resultante\nde cada tiro`, na.rm = TRUE))

View(media_resultante_por_zona2)

#And finally, posing the null hypothesis that the mean of the resultant is the 
#same, we will use the t.test function to check whether we can reject or accept it.

#Women's leagues
grupo1_r <- mi_dataframe$`Resultante\nde cada tiro`
View(grupo1_r)

#Men's leagues
grupo2_r <- mi_dataframe2$`Resultante\nde cada tiro`
View(grupo2_r)


t_test_result <- t.test(grupo1_r, grupo2_r)
print(t_test_result)


#Loading the last line, we can check that the p-value is higher than 0.05, so
#we can't reject the null hypothesis. Now we will check the same for the variance.


f_test_result <- var.test(grupo1_r, grupo2_r)
print(f_test_result)

