library(tidyr)
library(ggplot2)
library(dplyr) #ya cargada

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
  ) #Con esta línea de código filtramos las observaciones que han sido clasificadas a partir del código de Oesch ("!is.na(class8)"), y definimos los valores que queremos calcular (en este caso, pedimos la media y desviación típica para cada ronda)
    #Los valores de media y desviación típica se calculan para cada clase ocupacional, en cada ronda, gracias a la función "group_by(class8, essround)". Por último, "pivot_wider" organiza la tabla en columnas para cada ronda.

print(evolucion.cansancio)

evolucion_cansancio_long <- evolucion.cansancio %>%
  pivot_longer(cols = starts_with("Media.Ronda"),
               names_to = "Ronda",
               names_prefix = "Media.Ronda_",
               values_to = "Media") %>%
  mutate(Ronda = as.numeric(Ronda)) #Con esto, creamos un dataframe que crea una fila para cada clase ocupacional en cada ronda (seguramente haya un código más parsimonioso)

ess2.5.10$essround

# Crear el gráfico
ggplot(evolucion_cansancio_long, aes(x = Ronda, y = Media, color = class8, group = class8)) + geom_line(size = 1) + 
  geom_point(size = 3) + 
  labs(x = "Ronda de Encuesta", y = "Media del Cansancio", color = "Clase Ocupacional (Esquema Oesch de 8 clases)") +
  ggtitle("Evolución del Cansancio tras la jornada laboral") +
  scale_x_continuous(breaks = c(2, 5, 10)) + 
  theme_classic() +  
  theme(text = element_text(family = "serif"),
        legend.background = element_rect(color="black"), legend.title = element_text(family = "serif", size = 12), legend.text = element_text(family = "serif", size = 11),
        axis.text = element_text(family = "serif", size = 11),
        axis.title = element_text(family = "serif", size = 12, face ="bold"),
        title = element_text(family = "serif", size = 12, face ="bold"))
#Creamos un gráfico en formato APA, indicando las variables del eje X (Ronda), el eje Y (media de cansancio) y las líneas específicas del gráfico (clases ocupacionales). Con la función "theme_classic()" se da formato APA; la función "theme()" indica el formato de los elementos específicos del gráfico

#Correlación CANSADO DESPUÉS DEL TRABAJO-RONDA
cor(ess2.5.10$essround, ess2.5.10$trdawrk, use = "complete.obs", method = "spearman") #Con este código realizamos un análisis de correlaciones con el método de Spearman (por tratarse de una variable ordinal, y no de intervalo); el argumento "use = "complete.obs"" hace que no se tengan en cuenta los valores perdidos.

#Kruskal Wallis de CANSADO DESPUÉS DEL TRABAJO
kw.cansancio.OESCH <- kruskal.test(trdawrk ~ essround, data = ess2.5.10) #Test no paramétrico de comparación de medias entre grupos; permite comprobar si existen diferencias estadísticamente significativas en el cansancio tras el trabajo entre clases ocupacionales
gh.cansancio.OESCH <- rstatix::games_howell_test(ess2.5.10, trdawrk ~ essround, conf.level = 0.95, detailed = F) #Si rechazamos la hipótesis nula de ausencia de asociación con el test Kruskal Wallis, podemos emplear un test Games Howell para evaluar entre qué grupos específicos existen diferencias
print(gh.cansancio.OESCH, n = 28)

ess2.5.10 %>% rstatix::kruskal_effsize(trdawrk ~ essround) #Con esta línea calculamos el tamaño del efecto del Kruskal Wallis.

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
