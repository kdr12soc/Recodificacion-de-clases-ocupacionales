ess2.5.10<- read.csv("Rondas 2, 5 y 10 (modulo trabajo)/ESS2e03_6-ESS5e03_5-ESS10SC-subset.csv")

library(dplyr)

table(ess2.5.10$trdawrk)
ess2.5.10$trdawrk[ess2.5.10$trdawrk %in% c(6, 7, 8, 9)] <- NA
ess2.5.10$trdawrk_recoded <- ifelse(ess2.5.10$trdawrk %in% c(1, 2), 1, ess2.5.10$trdawrk)

table(ess2.5.10$jbprtfp)
ess2.5.10$jbprtfp[ess2.5.10$jbprtfp %in% c(66, 77, 88, 99)] <- NA

table(ess2.5.10$pfmfdjba_recoded)
ess2.5.10$pfmfdjba[ess2.5.10$pfmfdjba %in% c(6, 7, 8, 9)] <- NA
ess2.5.10$pfmfdjba_recoded <- ifelse(ess2.5.10$pfmfdjba %in% c(4, 5), 4, ess2.5.10$pfmfdjba)


table(ess2.5.10$wrklong)
ess2.5.10$wrklong[ess2.5.10$wrklong %in% c(55, 66, 77, 88, 99)] <- NA
ess2.5.10$wrklong_recoded <- ifelse(ess2.5.10$wrklong %in% c(1, 2, 3, 4), 1, ess2.5.10$wrklong)

table(ess2.5.10$wrkextra)
ess2.5.10$wrkextra[ess2.5.10$wrkextra %in% c(66, 77, 88, 99)] <- NA

ess2.5.10$wkhtot <- as.numeric(ess2.5.10$wkhtot)
ess2.5.10$wkhtot[ess2.5.10$wkhtot %in% c(666, 777, 888, 999)] <- NA
range(ess2.5.10$wkhtot, na.rm = T)

table(ess2.5.10$wrkctra)
ess2.5.10$wrkctra[ess2.5.10$wrkctra %in% c(6, 7, 8, 9)] <- NA
ess2.5.10$wrkctra_recoded <- ifelse(ess2.5.10$wrkctra %in% c(2, 3), 2, ess2.5.10$wrkctra)



library(tidyr)
#Tabla evolución CANSADO/A DESPUÉS DEL TRABAJO
evolucion.cansancio <- ess2.5.10 %>%
  filter(!is.na(class8)) %>% 
  group_by(class8, essround) %>%                     
  summarise(
    Media.Ronda = mean(trdawrk, na.rm = TRUE),
    SD.Ronda = sd(trdawrk, na.rm = TRUE)      
  ) %>%
  pivot_wider(
    names_from = essround,                      
    values_from = c(Media.Ronda, SD.Ronda)  
  )

print(evolucion.cansancio)

evolucion_cansancio_long <- evolucion.cansancio %>%
  pivot_longer(cols = starts_with("Media.Ronda"),
               names_to = "Ronda",
               names_prefix = "Media.Ronda_",
               values_to = "Media") %>%
  mutate(Ronda = as.numeric(Ronda))

# Crear el gráfico
ggplot(evolucion_cansancio_long, aes(x = Ronda, y = Media, color = class8, group = class8)) + geom_line(size = 1) + geom_point(size = 3) + labs(x = "Ronda de Encuesta", y = "Media del Cansancio", color = "Clase") + scale_x_continuous(breaks = c(2, 5, 10)) + theme_minimal() + theme(axis.text.x = element_text(angle = 0))

#Correlación CANSADO DESPUÉS DEL TRABAJO-RONDA
cor(ess2.5.10$essround, ess2.5.10$trdawrk, use = "complete.obs", method = "spearman")
#Kruskal Wallis de CANSADO DESPUÉS DEL TRABAJO
kw.cansancio.OESCH <- kruskal.test(trdawrk ~ essround, data = ess2.5.10)
gh.cansancio.OESCH <- rstatix::games_howell_test(ess2.5.10, trdawrk ~ essround, conf.level = 0.95, detailed = F)
print(gh.cansancio.OESCH, n = 28)

ess2.5.10 %>% rstatix::kruskal_effsize(trdawrk ~ essround)

####################Tabla evolución PAREJA CANSADA PRESIÓN DEL TRABAJO
evolucion.cansanciopareja <- ess2.5.10 %>%
  filter(!is.na(class8)) %>% 
  group_by(class8, essround) %>%                     
  summarise(
    Media.Ronda = mean(pfmfdjba, na.rm = TRUE),
    SD.Ronda = sd(pfmfdjba, na.rm = TRUE)      
  ) %>%
  pivot_wider(
    names_from = essround,                      
    values_from = c(Media.Ronda, SD.Ronda)  
  )

print(evolucion.cansanciopareja)

#Correlación CANSADO DESPUÉS DEL TRABAJO-RONDA
cor(ess2.5.10$essround, ess2.5.10$pfmfdjba, use = "complete.obs", method = "spearman")
#Kruskal Wallis de PAREJA CANSADA DESPUÉS DEL TRABAJO
kw.esperarextra.OESCH <- kruskal.test(pfmfdjba ~ essround, data = ess2.5.10)
gh.esperarextra.OESCH <- rstatix::games_howell_test(ess2.5.10, pfmfdjba ~ essround, conf.level = 0.95, detailed = F)
print(gh.esperarextra.OESCH, n = 28)

ess2.5.10 %>% rstatix::kruskal_effsize(pfmfdjba ~ essround)

####################Tabla evolución HORAS EXTRA
evolucion.horasextra <- ess2.5.10 %>%
  filter(!is.na(class8)) %>% 
  group_by(class8, essround) %>%                     
  summarise(
    Media.Ronda = mean(wkhtot, na.rm = TRUE),
    SD.Ronda = sd(wkhtot, na.rm = TRUE)      
  ) %>%
  pivot_wider(
    names_from = essround,                      
    values_from = c(Media.Ronda, SD.Ronda)  
  )

print(evolucion.horasextra)
