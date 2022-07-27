#taller de regresion 


# librerias ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(ggfortify)
library(lmtest)
library(nortest)


# carga de datos ----------------------------------------------------------

Spotify <- read_excel("Spotify.xlsx")
glimpse(Spotify)


# a)  carga preliminar de datos y limpieza --------

names(Spotify)

Spotify1 <- 
Spotify %>% 
  select( "Beats Per Minute (BPM)" , "Energy",
          "Danceability", "Loudness (dB)", "Liveness", "Positive_mood", "Length (Duration)",
          "Acousticness", "Speechiness", "Popularity")


# b) separacion base 75:25 -----------------------------------------------------------------------

str(Spotify1[,1:10])

(n_filas = dim(Spotify1)[1])

#entrenar la base
set.seed(2022)
ind_train = sample(1:n_filas, size = n_filas*0.75)


Spo_train = Spotify1[ind_train,]
Spo_test = Spotify1[-ind_train,]
glimpse(Spotify1)
dim(Spo_train)
dim(Spo_test)


# c) Entrenamiento modelo -------------------------------------------------

modelo1 <- lm(Popularity ~ ., data = Spo_train)
summary(modelo1)



# D) ----------------------------------------------------------------------


# se quita el campo `Beats Per Minute (BPM)` por P 0.822688 

modelo2 <- lm(Popularity ~ .-`Beats Per Minute (BPM)` , data = Spo_train)
summary(modelo2)

#se quita el campo Acousticness P de mayor valor,  0.703700 

modelo3 <- lm(Popularity ~ .-`Beats Per Minute (BPM)` -Acousticness  , data = Spo_train)
summary(modelo3)

#se quita el campo `Length (Duration)` dado que el p = 0.301361    

modelo4 <- lm(Popularity ~ .-`Beats Per Minute (BPM)` -Acousticness - `Length (Duration)`  , data = Spo_train)
summary(modelo4)

# se  quita el campo Positive_mood dado que el p = 0.167701     

modelo5 <- lm(Popularity ~ .-`Beats Per Minute (BPM)` -Acousticness - `Length (Duration)` -Positive_mood , data = Spo_train)
summary(modelo5)

#se quita el energy dado que se pide trabajar con un p-value menor a 0.05, p = 0.086705

modelo6 <- lm(Popularity ~ .-`Beats Per Minute (BPM)` -Acousticness - `Length (Duration)` -Positive_mood -Energy, data = Spo_train)
summary(modelo6)

#modelo AIC - regresion logistica glm - regresion lineal lm

library(stats)
mod_full <- lm(Popularity ~., data = Spo_train)
summary(mod_full)

#MASS::stepAIC(mod_full) - hace backward


AIC(mod_full)
# 12109.33 

AIC(modelo6)
#12105.46

plot(Spo_train$Danceability, Spo_train$Popularity)

#interpretar coeficiente R2

summary(mod_full)$adj.r.squared
#este modelo representa un R2 de 0.06346321

summary(modelo6)$adj.r.squared
#este modelo representa un R2 de 0.06277041

#validacion del modelo
# e) ----------------------------------------------------------------------

residuos <-  resid(modelo6)

lillie.test(residuos)
#aplicacion test de normalidad
#p-value = 6.526e-14 , se rechaza hipotesis nula de normalidad, dado que el p es menor a 0.05


autoplot(modelo6)[2]

#las colas se alejan de la diagonal 
