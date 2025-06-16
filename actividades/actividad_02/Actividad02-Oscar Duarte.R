###############################################
#Actividad 02 - EDA Proyecto en R
#Oscar Alejadro Duarte Bonilla
##############################################

##Carga de librerias

library(dplyr)      # Manipulación de datos
library(ggplot2)    # Visualización de datos
library(lubridate)  # Transformaciones de fecha
library(skimr)      # Resúmenes estadísticos detallados
library(rstatix)    # Para algunas pruebas estadísticas como normalidad y outliers (opcional, pero útil)
library(car)        # Para la prueba de Levene (homocedasticidad, no varianza cero)
library(forcats)    # Manipulación de factores, incluye fct_infreq()

#####DATAFRAME
#####Habitos estudiantiles y el rendimiento académico######
# El dataset seleccionado está enmarcado en el rendimiento académico de estudiantes y la influencia de los hábitos y otras variables propias del día a día del estudiante.

#####Objetivo####
# Conocer como se relacionan cada una de las variables y cuales marcan una diferencia notable en el rendimiento académico es el objetivo de este análisis exploratorio, esto nos permitirá identificar si existe una correlación directa con el bajo desempeño estudiantil.

#####Carga del Dataset
df<-read.table("C:/virtual_enviroment/Mod01/R-Lang/actividad_02/student_habits_performance.csv",dec=".",sep=",",header=TRUE,colClasses=c("character","integer","factor","numeric","numeric","numeric","factor","numeric","numeric","factor","integer","factor","factor","integer","factor","numeric"))

#Inspección del DF(Tipos de datos)
str(df)#Al asignar el tipo de dato a las columnas para ya no transformar el df
head(df)


####Análisis Exploratorio de Datos

#Vision grl de los datos
glimpse(df)

#Valores faltantes
print(colSums(is.na(df)))
sum(is.na(df))
#no se muestran los valores faltantes identificados por eso imprimiremos los valores de la columna
print(df$parental_education_level)
#Se identifican valores None per en la verificacion anterior no aparecen
##FUNCION MODA
get_mode <- function(x) {
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[which.max(tabulate_x)]
}
##FUNCION IMPUTAR MODA
imputar_moda <- function(x) {
  mode_val <- get_mode(x[!is.na(x)])
  x[is.na(x)] <- mode_val
  return(x)
}
df$parental_education_level <- imputar_moda(df$parental_education_level)

print(df$parental_education_level)


#Registros duplicados, anyDuplicated devuelve el índice del primer elemento duplicado, o 0 si no hay duplicados.
anyDuplicated(df)


####Creando variable target y variables agrupadas de variables continuas
df <- df %>%
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



#####Análisis de Variables Numéricas

#Visualizamos nuevamente de los datos con las nuevas columnas
glimpse(df)

#Resumen estadistico detallado
skim(df)


numeric_cols <- df %>% select_if(is.numeric) %>% names()
numeric_cols
#for (col in numeric_cols) {
#  if (var(df[[col]], na.rm = TRUE) == 0) {
#    message("La columna '", col, "' tiene varianza cero. Todos sus valores son iguales.")
#  } else {
#    message("La columna '", col, "' tiene varianza diferente de cero.")
#  }
#}

# Correlaciones
cor_data <- df %>%
  select(numeric_cols)
#Matriz de correlacion
correlation_matrix <- cor(cor_data, use = "pairwise.complete.obs") # Ignora NAs
print(correlation_matrix)
#Visualizacion del mapa de calor
heatmap(correlation_matrix)

####Análsis de variables categoricas
df %>% count(gender) %>% arrange(desc(n)) %>% print()

df %>% count(part_time_job) %>% arrange(desc(n)) %>% print()

df %>% count(diet_quality) %>% arrange(desc(n)) %>% print()

df %>% count(parental_education_level) %>% arrange(desc(n)) %>% print()

df %>% count(extracurricular_participation) %>% arrange(desc(n)) %>% print()

##Variables agrupadas y target
df %>% count(exam_result) %>% arrange(desc(n)) %>% print()
df %>% count(mental_health_rating_scale) %>% arrange(desc(n)) %>% print()
df %>% count(attendance_percentage_scale) %>% arrange(desc(n)) %>% print()


#####Visualizaciones
#Distribucion de las calificaciones
p1_hist_exam_score <- ggplot(df, aes(x = exam_score)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Calificación",
       #subtitle = "La presencia de outliers afecta la visualización de la mayoría de datos.",
       x = "Calificación",
       y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
print(p1_hist_exam_score)

#Boxplot de Calificaciones por Rango de salud mental
p2_boxplot_mental_health <- ggplot(df, aes(x = mental_health_rating_scale, y = exam_score, fill = mental_health_rating_scale)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 8) +
  labs(title = "Calificaciones por Rango de salud mental",
       #subtitle = "Se muestran los outliers en rojo. Considera una escala logarítmica si el gráfico es ilegible.",
       x = "Rango Salud Mental",
       y = "calificación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas del eje X
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") # Quitar leyenda si el color es el mismo que el eje X
print(p2_boxplot_mental_health)

#Boxplot de Calificacion por educacion de los padres
p3_boxplot_parent_edu <- ggplot(df, aes(x = parental_education_level, y = exam_score, fill = parental_education_level)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 8) +
  labs(title = "Calificaciones por Educacion de los padres",
       #subtitle = "Se muestran los outliers en rojo. Considera una escala logarítmica si el gráfico es ilegible.",
       x = "Educacion de los apdres",
       y = "calificación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas del eje X
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") # Quitar leyenda si el color es el mismo que el eje X
print(p3_boxplot_parent_edu)

#Boxplot de Calificacion por nivel de asistencia del alumno
p33_boxplot_asist <- ggplot(df, aes(x = attendance_percentage_scale, y = exam_score, fill = attendance_percentage_scale)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 8) +
  labs(title = "Calificaciones por Nivel de asistencia",
       #subtitle = "Se muestran los outliers en rojo. Considera una escala logarítmica si el gráfico es ilegible.",
       x = "Nivel de asistencia",
       y = "calificación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas del eje X
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") # Quitar leyenda si el color es el mismo que el eje X
print(p33_boxplot_asist)

#Grafica de puntos para ver la correlacion de horas de estudio y la calificacion, agrupada por escala de salud mental
p4_scatter_std_hours <- ggplot(df, aes(x = study_hours_per_day, y = exam_score, color = mental_health_rating_scale)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Calificacion vs. Horas de estudio al día",
       subtitle = "Color por categoría salud mental para identificar patrones.",
       x = "Horas de estudio",
       y = "Calificacion") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
print(p4_scatter_std_hours)

##Grafica de puntos para ver la correlacion de horas de redes sociales y la calificacion, agrupada por escala de asistencia
p5_scatter_soc_media <- ggplot(df, aes(x = social_media_hours, y = exam_score, color = attendance_percentage_scale)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Calificacion vs. Horas de redes sociales",
       subtitle = "Color por categoría asistencia para identificar patrones.",
       x = "Horas de redes sociales",
       y = "Calificacion") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
print(p5_scatter_soc_media)

#Frecuencia de alumnos por resultado de exámen
p6_bar_aprobados <- ggplot(df, aes(x = fct_infreq(exam_result), fill = exam_result)) + # fct_infreq ordena por frecuencia
  geom_bar(alpha = 0.8) +
  labs(title = "Frecuencia alumnos por resultado de examene",
       x = "resultado",
       y = "Número de Alumnos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")
print(p6_bar_aprobados)


#Resultado de examen por escala de rating en grafica de barras apiladas
ggplot(df, aes(fill=exam_result, y=exam_score, x=mental_health_rating_scale)) + 
  geom_bar(position="stack", stat="identity")



#ANALISIS¶

#La mitad de la población estudia menos de 3.50 horas al día
#Existe una relación directa entre mas horas de estudio realiza el estudiante mejor calificación obtiene
#Según el diagrama de dispersión se denota que hay una gran concentración de la población que saca mejor resultado entre menos horas de redes sociales consume.
#Con el diagrama de calor confirmamos la relación entre horas de estudio mejor calificacion y tambien la leve relacion inversa entre la calificacion y las horas en redes sociales y netflix
#Los Reprobados comprenden el 49% y los Aprobados el 51% de la población
#No se visualiza una relación entre la calificación del exámen y la educación de los padres, pero si se denota que a mejor salud mental hay mejor promedio de rendimiento
#Los valores promedios de las calificaciones mantienen la relación con la salud mental.
#Por otro lado la asistencia regular indica una leve mejora en la calificación promedio pero no significativa. solo un 25% de los que cumple con asistencia regular está por debajo de un 6 de calificación y el 75% tiene una calificación por arriba del 6.
#La mitad de la población con asistencia irregular y asistencia regular tiene una calificación arriba de 7(target de aprobados)


#Conclusiones

#Los estudiantes que tienen a estudiar más horas al día logran aprobar el exámen
#Los estudiantes que consumen menos Netflix o redes sociales en su mayoria obtienen mejores calificaciones
#A mejor salud mental la calificación tiende a mejorar
#La asistencia regular indica mejores calificaciones en promedio
