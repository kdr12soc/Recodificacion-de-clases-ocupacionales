ess2.5.10$agea[ess2.5.10$agea == 999] <- NA
range(ess2.5.10$agea, na.rm = T)

ess2.5.10 <- ess2.5.10 %>%
  mutate(edad.joven_adult = cut(agea, breaks = c(15, 25, 35, 45, 55, 65, 102), right = FALSE, labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65 y más")))

ess10 <- subset(ess2.5.10, essround == 10)

table(ess10$agea, ess10$edad.joven_adult)
table(ess10$class5, ess10$edad.joven_adult)

library(tidyr)
library(rstatix)
library(ggplot2)

#######TABLAS MEDIAS
respons.extra_edades <- ess10 %>%
  filter(!is.na(edad.joven_adult)) %>% 
  group_by(edad.joven_adult) %>%                     
  summarise(
    Media.respons.extra = mean(wrkextra, na.rm = TRUE),
    SD.respons.extra = sd(wrkextra, na.rm = TRUE),
    Frecuencia = n()
  )

espera.extra_edades <- ess10 %>%
  filter(!is.na(edad.joven_adult)) %>% 
  group_by(edad.joven_adult) %>%                     
  summarise(
    Media.espera.trabaja.extra = mean(wrklong, na.rm = TRUE),
    SD.espera.trabaja.extra = sd(wrklong, na.rm = TRUE),
    Frecuencia = n()
  )

print(respons.extra_edades)
print(espera.extra_edades)
table(ess10$edad.joven_adult, ess10$wrklong)



#####KRUSKAL WALLIS
kwresp <- kruskal.test(wrkextra ~ edad.joven_adult, data = ess10)
print(kwresp)
ess10 %>% kruskal_effsize(wrkextra ~ edad.joven_adult)
pwc.kwresp <- ess10 %>% 
  dunn_test(wrkextra ~ edad.joven_adult, p.adjust.method = "bonferroni") 
print(pwc.kwresp, n = nrow(pwc.kwresp))


kwespextr <- kruskal.test(wrklong ~ edad.joven_adult, data = ess10)
print(kwespextr)
ess10 %>% kruskal_effsize(wrklong ~ edad.joven_adult)
pwc.kwespextr <- ess10 %>% 
  dunn_test(wrklong ~ edad.joven_adult, p.adjust.method = "bonferroni") 
print(pwc.kwespextr, n = nrow(pwc.kwespextr))


###TABLAS FRECUENCIA
  ############RECODIFICAR VARIABLES PARA QUE EL CÁLCULO SEA MÁS LIMPIO 
  ############VER QUÉ OTRAS VARIABLES METER (desconexión digital, flexibilidad)
tabla.respons.extra <- table(ess10$wrkextra, ess10$edad.joven_adult)
tabla.respons.extra.prop <- prop.table(tabla.respons.extra, 2)

tabla.espera.extra <- table(ess10$wrklong, ess10$edad.joven_adult)
tabla.espera.extra.prop <- prop.table(tabla.espera.extra, 2)
