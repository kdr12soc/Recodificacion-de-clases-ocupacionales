devtools::install_github("DiogoFerrari/occupar")
library(occupar)

ess10$isco08[ess10$isco08 %in% c(66666, 77777, 88888, 99999)] <- NA

ess2.5.10$self.employed <- ifelse(ess2.5.10$emplrel %in% c(6, 7, 8, 9), NA, ifelse(ess2.5.10$emplrel == 2, 1, 0)) #Creaomos una variable que indique si la persona es Autónoma
ess2.5.10$n.employees[ess2.5.10$emplno %in% c(66666, 77777, 88888, 99999)] <- NA #Creamos una variable que indique el número de empleados/as a su cargo


#Transformamos los valores de Isco08 a Isco88 para poder transformalo al esquema de Erikson, Goldthorpe y Portocarera
ess2.5.10$isco88b <- isco08to88(ess2.5.10$isco08)
ess2.5.10 <- ess2.5.10 %>%
  mutate(isco08_final = coalesce(iscoco, isco88b))


ess10$EGP2 <- isco88toEGP(ess10$isco88, n.employees=ess10$n.employees, self.employed=ess10$self.employed,  n.classes=7)

table(ess10$EGP2)

#Existen otros esquemas de clase como el del neomarxista Erik Olin Wright, o el esquema ISEI, que requieren de otras variables.
