# Recodificacion-de-clases-ocupacionales
Cómo emplear el paquete Occupar y el código de Daniel Oesch para recodificar variables ISCO y crear estructuras de clase

1.	Utilizando la Encuesta Social Europea
    1.1.	Seleccionar las rondas y variables de interés en el Datafile Builder: https://ess.sikt.no/en/?tab=builder 
    1.2. 	Descargar en el formato deseado (para utilizar los paquetes que recodifican las ocupaciones, es mejor descargar en formato CSV)
    1.3.	Crear un nuevo proyecto en RStudio, en el directorio de trabajo en el que incluiremos el archivo descargado
    1.4.	Abrir un script, y recodificar las variables que nos interesen para eliminar perdidos, o fusionar categorías (en función del objeto de estudio y de los objetivos concretos)
  	  Ver script "ESS Recodificación rondas con módulo trabajo"
  	1.5.  Ejecutar el código de Daniel Oesch
  	  Ver script "ESS Recodificación Oesch todas las rondas"
  	1.6.  Realizar diferentes técnicas de análisis
  	  Ver script "ESS Algunas técnicas de análisis"
  	1.7.  También podemos emplear el paquete Occupar para crear otros esquemas de clase
  	  Ver script "Otros esquemas de clase (EGP)"



2.  Utilizando el sistema de clasificación del INE CNO-11 (o CNO94)
   2.1.  Ver las correspondencias entre CNO-11 y CIUO08 (Isco08)
        Archivo excel corr_cno11_ciuo08.xls
   2.2.  Ver las correspondencias entre CNO-94 y CIUO88 (Isco88)
        Archivo excel cno94_ciup88.xlsx
3.  Comprobar si al utilizar 3 digitos los códigos cambian entre un esquema y otro, y así poder usar el paquete Occupar.
4.  Se puede utilizar el esquema de Oesch con una variable de 2 digitos. Comprobar que los dos digitos de CNO y de CIUO sean equivalentes, para poder emplear esta opción.
