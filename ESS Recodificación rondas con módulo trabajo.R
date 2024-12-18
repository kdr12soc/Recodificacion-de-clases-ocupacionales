ess2.5.10<- read.csv("ESS2e03_6-ESS5e03_5-ESS10SC-subset.csv") #Con esta línea de código, creamos un objeto llamado ess2.5.10 a partir de un objeto CSV, cuya ruta de acceso indicamos tras la función “read.csv”.

library(dplyr)

table(ess2.5.10$trdawrk) #Utilizo esta función para previsualizar los valores existentes en la variable/columna que voy a recodificar. Combino esto junto con el libro de códigos que se descarga automáticamente del Data Builder de la ESS
ess2.5.10$trdawrk[ess2.5.10$trdawrk %in% c(6, 7, 8, 9)] <- NA #Función del paquete “dplyr” con la cual indico que valores de la variable original pasarán a ser valores perdidos
ess2.5.10$trdawrk_recoded <- ifelse(ess2.5.10$trdawrk %in% c(1, 2), 1, ess2.5.10$trdawrk) #El comando “ifelse” indica que cualquier valor que no sea 1 o 2 (indicado con “%in% c(1, 2)”) mantiene su valor, mientras que los valores 1 y 2 pasan a ser 1.
            #Decidí juntar esos dos valores por contar con pocas observaciones, y constituir las dos categorías de respuesta relevantes para el análisis

table(ess2.5.10$jbprtfp)
ess2.5.10$jbprtfp[ess2.5.10$jbprtfp %in% c(66, 77, 88, 99)] <- NA

ess2.5.10$pfmfdjba[ess2.5.10$pfmfdjba %in% c(6, 7, 8, 9)] <- NA
ess2.5.10$pfmfdjba_recoded <- ifelse(ess2.5.10$pfmfdjba %in% c(4, 5), 4, ess2.5.10$pfmfdjba)
table(ess2.5.10$pfmfdjba_recoded)

table(ess2.5.10$wrklong)
ess2.5.10$wrklong[ess2.5.10$wrklong %in% c(55, 66, 77, 88, 99)] <- NA
ess2.5.10$wrklong_recoded <- ifelse(ess2.5.10$wrklong %in% c(1, 2, 3, 4), 1, ess2.5.10$wrklong)
ess2.5.10$wrklong_recoded <- factor(ess2.5.10$wrklong_recoded, levels = c(1, 5, 6), labels = c("Al menos una vez al mes", "Casi nunca", "Nunca"))
table(ess2.5.10$wrklong_recoded)


table(ess2.5.10$wrkextra)
ess2.5.10$wrkextra[ess2.5.10$wrkextra %in% c(66, 77, 88, 99)] <- NA
ess2.5.10$wrkextra_recoded <- ess2.5.10$wrkextra
ess2.5.10$wrkextra_recoded[ess2.5.10$wrkextra_recoded %in% c(0,1,2,3,4,5,6,7)] <- 0
ess2.5.10$wrkextra_recoded[ess2.5.10$wrkextra_recoded %in% c(8,9,10)] <- 1
ess2.5.10$wrkextra_recoded <- factor(ess2.5.10$wrkextra_recoded, levels = c(0,1), labels = c("0-7", "8-10"))
table(ess2.5.10$wrkextra_recoded)


ess2.5.10$wkhtot <- as.numeric(ess2.5.10$wkhtot)
ess2.5.10$wkhtot[ess2.5.10$wkhtot %in% c(666, 777, 888, 999)] <- NA
range(ess2.5.10$wkhtot, na.rm = T) #En esta línea pido el rango de la nueva variable. Importante señalar el argumento “na.rm = T”, porque de lo contrario R mediría el rango con los valores perdidos, devolviendo un error. 
          #En este caso pido el rango como forma de comprobación de la correcta modificación, ya que se trata de una variable de intervalo

table(ess2.5.10$wrkctra)
ess2.5.10$wrkctra[ess2.5.10$wrkctra %in% c(6, 7, 8, 9)] <- NA
ess2.5.10$wrkctra_recoded <- ess2.5.10$wrkctra
ess2.5.10$wrkctra_recoded <- factor(ess2.5.10$wrkctra_recoded, levels = c(1, 2, 3), labels = c("Indefinido", "Temporal", "Sin contrato"))

            ###Por si queremos fusionar sin contrato y temporal: table(ess2.5.10$wrklong_recoded)ifelse(ess2.5.10$wrkctra %in% c(2, 3), 2, ess2.5.10$wrkctra)


table(ess2.5.10$stfmjob)
ess2.5.10$stfmjob[ess2.5.10$stfmjob %in% c(66, 77, 88, 99)] <- NA
ess2.5.10$stfmjob_recoded <- ess2.5.10$stfmjob
ess2.5.10$stfmjob_recoded[ess2.5.10$stfmjob_recoded %in% c(0,1,2,3,4,5,6,7)] <- 0
ess2.5.10$stfmjob_recoded[ess2.5.10$stfmjob_recoded %in% c(8,9,10)] <- 1
ess2.5.10$stfmjob_recoded <- factor(ess2.5.10$stfmjob_recoded, levels = c(0,1), labels = c("0-7", "8-10"))
table(ess2.5.10$stfmjob_recoded)

table(ess2.5.10$dcsfwrka)
ess2.5.10$dcsfwrka[ess2.5.10$dcsfwrka %in% c(6, 7, 8, 9)] <- NA
ess2.5.10$dcsfwrka <- factor(ess2.5.10$dcsfwrka, levels = c(1, 2, 3), labels = c("En absoluto", "Hasta cierto punto", "Completamente"))

table(ess2.5.10$wrkhome)
ess2.5.10$wrkhome[ess2.5.10$wrkhome %in% c(66, 77, 88, 99)] <- NA
ess2.5.10$wrkhome_factor <- factor(ess2.5.10$wrkhome, levels = c(1,2,3,4,5,6), labels = c("Todos los días", "Varios días a la semana", "Varios días al mes", "Una vez al mes", "Casi nunca", "Nunca")) 
