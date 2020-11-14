import pandas as pd
import numpy as np

df_zip11 = pd.read_csv("basic_stats_11.csv", sep = ';', decimal=",")
df_zip10 = pd.read_csv("basic_stats_10.csv", sep = ';', decimal=",")
df_zip = pd.concat([df_zip10,df_zip11], axis = 0, ignore_index = True)

df_zip.rename(columns={'Avg. amount': 'Gasto_Prom', 'Min. amount': 'Gasto_min', 'Max. amount': 'Gasto_max',
                       'Std. amount': 'Desv_Est', 'Category': 'Categoria', 'Peak day': 'Dia Pico', 'Date': 'Fecha',
                      }, inplace=True)
df_zip_names = list(df_zip.columns)
df_zip["Categoria"] = df_zip["Categoria"].str.replace("es_"," ") 
df_zip["Categoria"] = df_zip["Categoria"].str.strip()
df_zip["Categoria"] = df_zip["Categoria"].str.title()

cod_post = pd.Series(df_zip10['Zone'].unique())
df_zip_dates10 = pd.Series(df_zip10['Date'].unique())
df_zip_dates11 = pd.Series(df_zip11['Date'].unique())
df_zip_dates = pd.concat([df_zip_dates10,df_zip_dates11], axis = 0, ignore_index = True)

s = list(df_zip['Categoria'].unique())
s.insert(0, 'Sin Categoria')
df_zip_cat = pd.Series(s)



df_zip_clean = df_zip.dropna(axis=0,how = 'any')

madrid_zip = pd.read_csv('zip_correccion.csv')
#madrid_zip = madrid_zip.set_index('Postal Code')
madrid_zip = madrid_zip .rename(columns={'Postal Code': 'Zone'})

mean_prim = df_zip_clean.groupby(['Zone','Fecha'], as_index = False)['Gasto_Prom'].sum().reset_index(drop=True)
zone_mean_cat_G = df_zip_clean.groupby(['Zone','Fecha','Categoria'], as_index = False)['Gasto_Prom'].sum().reset_index(drop=True)

zone_mean_cat1 = pd.concat(
 [df_zip_clean.groupby(['Zone','Fecha','Categoria'], as_index = False)['Gasto_Prom'].sum().reset_index(drop=True),
 df_zip_clean.groupby(['Zone','Fecha','Categoria'], as_index = False)['Gasto_min'].sum().reset_index(drop=True)['Gasto_min'],
 df_zip_clean.groupby(['Zone','Fecha','Categoria'], as_index = False)['Gasto_max'].sum().reset_index(drop=True)['Gasto_max'],
  df_zip_clean.groupby(['Zone','Fecha','Categoria'], as_index = False)['Desv_Est'].sum().reset_index(drop=True)['Desv_Est'],
 ],axis = 1)

zone_mean = df_zip_clean.groupby(['Zone','Fecha'], as_index = False)['Gasto_Prom'].sum().sort_values('Fecha')

direc_1 = pd.merge(zone_mean,madrid_zip, left_on = "Zone",
                      right_index = True).sort_index()



