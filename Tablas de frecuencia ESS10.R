library(dplyr)
ess2.5.10 <- ess2.5.10 %>%
  mutate(edad.joven_adult = cut(agea, breaks = c(15, 25, 35, 45, 55, 65, 102), right = FALSE, labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65 y más")))

ess10 <- subset(ess2.5.10, essround == 10)

tabla.respons.extra <- table(ess10$wrkextra_recoded, ess10$edad.joven_adult)
tabla.respons.extra.prop <- prop.table(tabla.respons.extra, 2)*100

tabla.espera.extra <- table(ess10$wrklong_recoded, ess10$edad.joven_adult)
tabla.espera.extra.prop <- prop.table(tabla.espera.extra, 2)*100

tabla.teletrabajo <- table(ess10$wrkhome_factor, ess10$edad.joven_adult)
tabla.teletrabajo.prop <- prop.table(tabla.teletrabajo, 2)*100

tabla.flexibilidad.horaria <- table(ess10$dcsfwrka, ess10$edad.joven_adult)
tabla.flexibilidad.horaria.prop <- prop.table(tabla.flexibilidad.horaria, 2)*100

tabla.satisfaccion.trabajo <- table(ess10$stfmjob_recoded, ess10$edad.joven_adult)
tabla.satisfaccion.trabajo.prop <- prop.table(tabla.satisfaccion.trabajo, 2)*100


