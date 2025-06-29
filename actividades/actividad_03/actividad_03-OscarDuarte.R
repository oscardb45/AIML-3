##########################################################################################
# Proyecto de ciencia de datos integrando Phyton y R
# AIML-3
# Oscar Alejandro Duarte Bonilla
##########################################################################################
# DATASET "Habitos estudiantiles y el rendimiento acad�mico"
# El dataset seleccionado est� enmarcado en el rendimiento acad�mico de estudiantes y la influencia de los h�bitos y otras variables propias del d�a a d�a del estudiante.
# Objetivo: Conocer como se relacionan cada una de las variables y cuales marcan una diferencia notable en el rendimiento acad�mico es el objetivo de este an�lisis 
# exploratorio, esto nos permitir� identificar si existe una correlaci�n directa con el bajo desempe�o estudiantil.
##########################################################################################

#### LIBRERIAS R ####
library(reticulate) # Para la interoperabilidad R <-> Python
library(dplyr)      # Para manipulaci�n de datos en R
library(ggplot2)    # Para visualizaci�n en R
library(skimr)      # Res�menes estad�sticos detallados

#### HABILITAR EL ENTORNO VIRTUAL DE PYTHON PARA INTEGRARLO CON R ####
remove.packages("reticulate")
install.packages("reticulate", dependencies = TRUE)

library(reticulate)
# Paso opcional: Aseg�rate de tener el m�dulo 'virtualenv' de Python instalado globalmente
# Esto solo es necesario si es la primera vez que usas reticulate para crear entornos
# py_install("virtualenv", pip = TRUE)

# Definir la RUTA donde crear el nuevo entorno.
new_env_path <- "C:/virtual_environment/r_python_env_act3" # �Nuevo nombre!

# Crea el nuevo entorno virtual en la ruta especificada.
virtualenv_create(envname = new_env_path)
message(paste("�Nuevo entorno virtual creado con �xito en:", new_env_path, "!"))

# Indicar a reticulate que use este nuevo entorno.
use_virtualenv(virtualenv = new_env_path, required = TRUE)
message(paste("Reticulate est� usando ahora el entorno:", py_config()$python))

# Verificar la configuraci�n de Python una vez m�s para confirmar.
py_config()

# Intenta usar el entorno virtual
tryCatch({
  use_virtualenv(virtualenv = new_env_path, required = TRUE)
  message(paste("Usando el entorno de Python:", py_config()$python))
  
  # Instalar paquetes Python si no est�n en el entorno
  py_install(packages = c("pandas", "numpy", "seaborn", "matplotlib"), envname = new_env_path)
  message("Paquetes esenciales de Python instalados en el nuevo entorno.")
  
}, error = function(e) {
  message("Error al inicializar el entorno Python:")
  message(e$message)
  message("Por favor, aseg�rate de que la ruta al entorno virtual sea correcta y que est� funcional.")
  stop("El script se detiene debido a un error de configuraci�n de Python.")
})


#### Importar m�dulos de Python ####
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
plt$title('Relaci�n entre horas de estudio por dia y el resultado del examen')
plt$xlabel('Horas de estudio por d�a')
plt$ylabel('Resultado Examen')
plt$grid("True")
plt$show()

#Relacion horas redes sociales vs resultado examen
plt$figure(figsize=c(8, 6))
sns$scatterplot(data=df, x='social_media_hours', y='exam_score')
plt$title('Relaci�n entre horas de redes sociales por dia y el resultado del examen')
plt$xlabel('Horas de redes sociales por d�a')
plt$ylabel('Resultado Examen')
plt$grid("True")
plt$show()

#Relacion horas netflix vs resultado examen
plt$figure(figsize=c(8, 6))
sns$scatterplot(data=df, x='netflix_hours', y='exam_score')
plt$title('Relaci�n entre horas de netflix por dia y el resultado del examen')
plt$xlabel('Horas de netflix por d�a')
plt$ylabel('Resultado Examen')
plt$grid("True")
plt$show()


#Mapa de calor
df_num = df$select_dtypes(include="number")
df_num

plt$figure(figsize=c(12,8))
sns$heatmap(df_num$corr(), annot=FALSE, cmap="coolwarm")
plt$show()


#BOXPLOT resultado examen vs Horas de sue�o
plt$figure(figsize=c(30,12)) 
plt$xticks(rotation = 90)

sns$boxplot(data =df, x='exam_result',y='sleep_hours', 
            orient='v',palette='Set2')


plt$legend( loc = 'upper right')                                              
plt$title('Horas de sue�o vs resultado examen',fontsize= 35)
plt$xlabel("Horas de sue�o",fontsize= 30, fontweight="bold")
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
    La mitad de la poblaci�n estudia menos de 3.50 horas al d�a
    Part_time_job: la mayoria se ubica en el valor 0 y solo hay unos cuantos atipicos que si trabajan medio tiempo
    El 75% de la poblaci�n duerme menos de 7.5 horas
    Existe una relaci�n directa entre mas horas de estudio realiza el estudiante mejor calificaci�n obtiene
    Seg�n el diagrama de dispersi�n se denota que hay una gran concentraci�n de la poblaci�n que saca mejor resultado entre menos horas de netflix consume.
    Con el diagrama de calor confirmamos la relaci�n entre horas de estudio mejor calificacion y tambien la leve relacion inversa entre la calificacion y las horas en redes sociales y netflix
    Al separar los datos en dos conjuntos Aprobados(exam_score>7) y Reprobados no existe alg�n sesgo o tendencia con relaci�n a exercise_frequency, attendance_percentage, sleep_hours en cada grupo.
    Los Reprobados comprenden el 49% y los Aprobados el 51% de la poblaci�n
    No se visualiza una relaci�n entre la calificaci�n del ex�men y la educaci�n de los padres, pero si se denota que a mejor salud mental hay mejor promedio de rendimiento
    Los valores promedios de las calificaciones mantienen la relaci�n con la salud mental a pesar de la calidad del internet, es decir la calidad del internet no influye.
    Por otro lado la asistencia regular indica una leve mejora en la calificaci�n promedio pero no significativa. solo un 25% de los que cumple con asistencia regular est� por debajo de un 6 de calificaci�n y el 75% tiene una calificaci�n por arriba del 6.
    La mitad de la poblaci�n con asistencia irregular y asistencia regular tiene una calificaci�n arriba de 7(target de aprobados)
"""

#### CONCLUSIONES ####
"""
Los estudiantes que tienen a estudiar m�s horas al d�a logran aprobar el ex�men
Los estudiantes que consumen menos Netflix o redes sociales en su mayoria obtienen mejores calificaciones
A mejor salud mental la calificaci�n tiende a mejorar
"""





