##########################################################################################
# Proyecto de ciencia de datos integrando Phyton y R
# AIML-3
# Oscar Alejandro Duarte Bonilla
##########################################################################################
# DATASET "Habitos estudiantiles y el rendimiento académico"
# El dataset seleccionado está enmarcado en el rendimiento académico de estudiantes y la influencia de los hábitos y otras variables propias del día a día del estudiante.
# Objetivo: Conocer como se relacionan cada una de las variables y cuales marcan una diferencia notable en el rendimiento académico es el objetivo de este análisis 
# exploratorio, esto nos permitirá identificar si existe una correlación directa con el bajo desempeño estudiantil.
##########################################################################################

#### LIBRERIAS R ####
library(reticulate) # Para la interoperabilidad R <-> Python
library(dplyr)      # Para manipulación de datos en R
library(ggplot2)    # Para visualización en R
library(skimr)      # Resúmenes estadísticos detallados

#### HABILITAR EL ENTORNO VIRTUAL DE PYTHON PARA INTEGRARLO CON R ####
remove.packages("reticulate")
install.packages("reticulate", dependencies = TRUE)

library(reticulate)
# Paso opcional: Asegúrate de tener el módulo 'virtualenv' de Python instalado globalmente
# Esto solo es necesario si es la primera vez que usas reticulate para crear entornos
# py_install("virtualenv", pip = TRUE)

# Definir la RUTA donde crear el nuevo entorno.
new_env_path <- "C:/virtual_environment/r_python_env_act3" # ¡Nuevo nombre!

# Crea el nuevo entorno virtual en la ruta especificada.
virtualenv_create(envname = new_env_path)
message(paste("¡Nuevo entorno virtual creado con éxito en:", new_env_path, "!"))

# Indicar a reticulate que use este nuevo entorno.
use_virtualenv(virtualenv = new_env_path, required = TRUE)
message(paste("Reticulate está usando ahora el entorno:", py_config()$python))

# Verificar la configuración de Python una vez más para confirmar.
py_config()

# Intenta usar el entorno virtual
tryCatch({
  use_virtualenv(virtualenv = new_env_path, required = TRUE)
  message(paste("Usando el entorno de Python:", py_config()$python))
  
  # Instalar paquetes Python si no están en el entorno
  py_install(packages = c("pandas", "numpy", "seaborn", "matplotlib"), envname = new_env_path)
  message("Paquetes esenciales de Python instalados en el nuevo entorno.")
  
}, error = function(e) {
  message("Error al inicializar el entorno Python:")
  message(e$message)
  message("Por favor, asegúrate de que la ruta al entorno virtual sea correcta y que esté funcional.")
  stop("El script se detiene debido a un error de configuración de Python.")
})


#### Importar módulos de Python ####
pd <- import("pandas")
np <- import("numpy")
plt <- import("matplotlib.pyplot")
sns <- import("seaborn")

### CARGA DEL DATASET ###
df_r<-read.table("C:/virtual_enviroment/Mod01/R-Lang/actividad_02/student_habits_performance.csv",dec=".",sep=",",header=TRUE,colClasses=c("character","integer","factor","numeric","numeric","numeric","factor","numeric","numeric","factor","integer","factor","factor","integer","factor","numeric"))


# Convertir el data frame de R a un DataFrame de Pandas
df <- r_to_py(df_r)

#### Inspeccion del DF ####
df_head <- df$head()
df_head

#Total de Filas y columnas
df$shape

#Tipos de datos
df$info()

#Vision grl de los datos
glimpse(df_r)

#Duplicados
df$duplicated()$sum()

#Valores unicos
df$nunique()

#Valores nulos
df$isnull()$sum() #Python
print(colSums(is.na(df_r))) #R


####Creando variable target y variables agrupadas de variables continuas
df_r <- df_r %>%
  mutate(
    exam_result = ifelse(exam_score>70,"Aprobado","Reprobado"),
    mental_health_rating_scale = case_when(
      mental_health_rating < 2 ~ "En Crisis",
      mental_health_rating >= 2 & mental_health_rating <4  ~ "Luchando",
      mental_health_rating >= 4 & mental_health_rating <6  ~ "Sobreviviendo",
      mental_health_rating >= 6 & mental_health_rating <8  ~ "Prosperando",
      mental_health_rating >= 8  ~ "Excelente"
    ),
    attendance_percentage_scale = case_when(
      attendance_percentage < 80 ~ "Ausentismo",
      attendance_percentage >= 80 & attendance_percentage<90  ~ "Asistencia Irregular",
      attendance_percentage >= 90  ~ "Asistencia Regular"
    )
  )

# Convertir el data frame de R a un DataFrame de Pandas
df <- r_to_py(df_r)
df
#Resumen estadistico detallado
skim(df_r)

numeric_cols_r <- df_r %>% select_if(is.numeric) %>% names()
numeric_cols_r

#### ANALISIS ####

# Dibujar boxplots

#Asistencia vs nota examen
plt$figure(figsize = c(8, 6))
sns$boxplot(data=df, x="attendance_percentage_scale", y="exam_score")
plt$show()

#Asistencia vs nota examen
plt$figure(figsize = c(8, 6))
sns$boxplot(data=df, x="mental_health_rating_scale", y="exam_score")
plt$show()


#Relacion de variables continuas
#Horas de est6udio al dia vs resultado examen
plt$figure(figsize= c(8, 6))
sns$scatterplot(data=df, x='study_hours_per_day', y='exam_score')
plt$title('Relación entre horas de estudio por dia y el resultado del examen')
plt$xlabel('Horas de estudio por día')
plt$ylabel('Resultado Examen')
plt$grid("True")
plt$show()

#Relacion horas redes sociales vs resultado examen
plt$figure(figsize=c(8, 6))
sns$scatterplot(data=df, x='social_media_hours', y='exam_score')
plt$title('Relación entre horas de redes sociales por dia y el resultado del examen')
plt$xlabel('Horas de redes sociales por día')
plt$ylabel('Resultado Examen')
plt$grid("True")
plt$show()

#Relacion horas netflix vs resultado examen
plt$figure(figsize=c(8, 6))
sns$scatterplot(data=df, x='netflix_hours', y='exam_score')
plt$title('Relación entre horas de netflix por dia y el resultado del examen')
plt$xlabel('Horas de netflix por día')
plt$ylabel('Resultado Examen')
plt$grid("True")
plt$show()


#Mapa de calor
df_num = df$select_dtypes(include="number")
df_num

plt$figure(figsize=c(12,8))
sns$heatmap(df_num$corr(), annot=FALSE, cmap="coolwarm")
plt$show()


#BOXPLOT resultado examen vs Horas de sueño
plt$figure(figsize=c(30,12)) 
plt$xticks(rotation = 90)

sns$boxplot(data =df, x='exam_result',y='sleep_hours', 
            orient='v',palette='Set2')


plt$legend( loc = 'upper right')                                              
plt$title('Horas de sueño vs resultado examen',fontsize= 35)
plt$xlabel("Horas de sueño",fontsize= 30, fontweight="bold")
plt$ylabel("resultado examen",fontsize= 30, fontweight="bold")
plt$xticks(rotation=90, fontsize=30)
plt$yticks(rotation=360, fontsize=30)

plt$show()

#BOXPLOT combinado calificacion vs asistencia agrupados por estado salud mental
plt$figure(figsize=c(30,12)) 
plt$xticks(rotation = 90)

sns$boxplot(data =df, x='attendance_percentage_scale',y='exam_score', 
            hue ='mental_health_rating_scale',orient='v',palette='Set2')


plt$legend( loc = 'upper right')                                              
plt$title('Calificacion vs asistencia',fontsize= 35)
plt$xlabel("aitencia",fontsize= 30, fontweight="bold")
plt$ylabel("examen_score",fontsize= 30, fontweight="bold")
plt$xticks(rotation=90, fontsize=30)
plt$yticks(rotation=360, fontsize=30)

plt$show()



####ANALISIS ####
""" 
    La mitad de la población estudia menos de 3.50 horas al día
    Part_time_job: la mayoria se ubica en el valor 0 y solo hay unos cuantos atipicos que si trabajan medio tiempo
    El 75% de la población duerme menos de 7.5 horas
    Existe una relación directa entre mas horas de estudio realiza el estudiante mejor calificación obtiene
    Según el diagrama de dispersión se denota que hay una gran concentración de la población que saca mejor resultado entre menos horas de netflix consume.
    Con el diagrama de calor confirmamos la relación entre horas de estudio mejor calificacion y tambien la leve relacion inversa entre la calificacion y las horas en redes sociales y netflix
    Al separar los datos en dos conjuntos Aprobados(exam_score>7) y Reprobados no existe algún sesgo o tendencia con relación a exercise_frequency, attendance_percentage, sleep_hours en cada grupo.
    Los Reprobados comprenden el 49% y los Aprobados el 51% de la población
    No se visualiza una relación entre la calificación del exámen y la educación de los padres, pero si se denota que a mejor salud mental hay mejor promedio de rendimiento
    Los valores promedios de las calificaciones mantienen la relación con la salud mental a pesar de la calidad del internet, es decir la calidad del internet no influye.
    Por otro lado la asistencia regular indica una leve mejora en la calificación promedio pero no significativa. solo un 25% de los que cumple con asistencia regular está por debajo de un 6 de calificación y el 75% tiene una calificación por arriba del 6.
    La mitad de la población con asistencia irregular y asistencia regular tiene una calificación arriba de 7(target de aprobados)
"""

#### CONCLUSIONES ####
"""
Los estudiantes que tienen a estudiar más horas al día logran aprobar el exámen
Los estudiantes que consumen menos Netflix o redes sociales en su mayoria obtienen mejores calificaciones
A mejor salud mental la calificación tiende a mejorar
"""





