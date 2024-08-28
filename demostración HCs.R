
library(tidyverse)
library(sandwich)
library(countrycode)
library(lmtest)

homicide <- read.csv("homicide-rate-unodc.csv") %>% 
  filter(Year == 2021)

colnames(homicide)[4] <- "tasahomicidio"

infla <- read.csv("inflation-of-consumer-prices.csv") %>% 
  filter(Year == 2021)

colnames(infla)[4] <- "inflacion"

conso <- homicide %>% 
  left_join(infla) %>% 
  na.omit()

write.csv2(conso, "consolidado.csv", row.names = F)

conso %>% 
  ggplot() +
  aes(log(inflacion), log(tasahomicidio)) +
  geom_jitter() +
  theme_bw()


modelo <- feols(log(tasahomicidio) ~ log(inflacion), 
                data = conso)


ggplot() +
  aes(scale(fitted(modelo)), scale(resid(modelo))) +
  geom_jitter() +
  theme_bw()


HC0 <- summary(modelo, vcov = vcovHC(modelo, type = "HC0"))
HC1 <- summary(modelo, vcov = vcovHC(modelo, type = "HC1"))
HC2 <- summary(modelo, vcov = vcovHC(modelo, type = "HC2"))
HC3 <- summary(modelo, vcov = vcovHC(modelo, type = "HC3"))
HC4 <- summary(modelo, vcov = vcovHC(modelo, type = "HC4"))


etable(HC0, HC1, HC2, HC3, HC4)













