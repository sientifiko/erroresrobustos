

library(tidyverse)
library(fixest)
library(boot)
theme_set(theme_bw())

conso <- read.csv2("consolidado.csv")

# función de los beta que bootstrapareamos
coeficientes <- function(datos, ID){
  set <- datos[ID, ]

  betas <- feols(log(tasahomicidio) ~ log(inflacion), 
                 data = set)$coefficients
  betas
}

# ejectuar función de bootstrap con 1000 repeticiones
resultados <- boot(data = conso, 
                   statistic = coeficientes, 
                   R = 1000)

# imprimir resultados
resultados


# función del valor p
pvalboot <- function(betar, beta){
  
  mean(abs(betar-beta) > abs(beta))
  
}

mod <-feols(log(tasahomicidio) ~ log(inflacion), 
            data = conso)

# cálculo de valor p
pvalboot(resultados$t[,2], mod$coefficients[2])


# ====  DESDE ACÁ LA SIMULACIÓN GRANDE =====

# el beta poblacional entonces es 5
poblacion <- data.frame(Y = 5*1:10000 + rnorm(10000)*5,
                        X = 1:10000)

# betas obtenidos a partir de muestra total
replicate(1000, {
  
  muestra <- poblacion %>% 
    sample_n(25)
  
  lm(muestra$Y ~ muestra$X)$coefficients[2]
  
}) -> betasmuestrales


# betas obtenidos a partir de bootstrap, esta parte es súper pesada
coeficientes <- function(datos, ID){
  set <- datos[ID, ]
  
  betas <- lm(Y ~ X, 
                 data = set)$coefficients
  betas
}

replicate(1000, {
  
  muestra <- poblacion %>% 
    sample_n(25)
  
  resultados <- invisible( # esta función es para no imprimir cosas
    boot(data = muestra, 
         statistic = coeficientes, 
         R = 100)# reduzcan este valor si quieren
  ) 
  
  resultados$t0[2]
  
}) -> betasboot

# gráfica ambas distribuciones
ggplot() +
  geom_density(aes(betasmuestrales, color = "Muestrales")) +
  geom_density(aes(betasboot, color = "Bootstrap")) +
  theme(legend.title = element_blank()) +
  labs(x="Betas estimados")








