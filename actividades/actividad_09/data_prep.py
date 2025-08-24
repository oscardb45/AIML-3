# Actividad 09 - Modulo 02
# Desarrollar un script para automatizar una tarea de preparación de datos
# Oscar Alejandro Duarte Bonilla

#-------------------------------------------------------------------------------------------------------------------------------------------------
# Indicaciones: 1- Cargar un csv que se desea procesar de nombre "datos.csv" en la misma ruta de este archivo
#               2- Inicia el entorno viertual de python desde esta misma ruta
#               3- Ejecutar el archivo python data_prep.py
#               4- Resultado: genera un archivo "datos_procesados" con la limpieza, normalizacion y transformacion aplicada
#-------------------------------------------------------------------------------------------------------------------------------------------------
import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler, StandardScaler


# CARGA DE DATOS
def load_data(path):
    """Carga datos desde un archivo CSV."""
    try:
        df = pd.read_csv(path)
        return df
    except Exception as e:
        print(f"Error al cargar los datos: {e}")
        return None

# LIMPIEZA
def clean_data(df):
    """Elimina duplicados y valores nulos."""
    df = df.drop_duplicates()
    df = df.dropna()
    return df

# NORMALIZACIÓN
def normalize_data(df, method='minmax'):
    """
    Normaliza los datos numéricos usando MinMax o Z-score.
    method: 'minmax' o 'zscore'
    """
    num_cols = df.select_dtypes(include=np.number).columns

    if method == 'minmax':
        scaler = MinMaxScaler()
    elif method == 'zscore':
        scaler = StandardScaler()
    else:
        raise ValueError("Método no soportado. Usa 'minmax' o 'zscore'.")

    df[num_cols] = scaler.fit_transform(df[num_cols])
    return df

# TRANSFORMACIÓN
def transform_data(df):
    """Ejemplo: Crear nuevas columnas o transformar categorías."""
    if 'fecha' in df.columns:
        df['año'] = pd.to_datetime(df['fecha']).dt.year
        df['mes'] = pd.to_datetime(df['fecha']).dt.month

    # Otra transformación: codificación de variables categóricas
    cat_cols = df.select_dtypes(include='object').columns
    df = pd.get_dummies(df, columns=cat_cols, drop_first=True)

    return df

# EJECUCIÓN PRINCIPAL
def preprocess_pipeline(path, normalization_method='minmax'):
    """Pipeline completo de preprocesamiento."""
    df = load_data(path)
    if df is None:
        return

    print("Datos cargados. Forma:", df.shape)
    df = clean_data(df)
    print("Después de limpieza. Forma:", df.shape)
    df = normalize_data(df, method=normalization_method)
    print("Después de normalización. Forma:", df.shape)
    df = transform_data(df)
    print("Después de transformación. Forma:", df.shape)

    return df

# USO DEL SCRIPT
if __name__ == "__main__":
    ruta = "datos.csv" 
    df_procesado = preprocess_pipeline(ruta, normalization_method='zscore')
    df_procesado.to_csv("datos_procesados.csv", index=False)
    print("Preprocesamiento completo. Archivo guardado.")
