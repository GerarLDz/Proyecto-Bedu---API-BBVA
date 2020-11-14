import pandas
def datos_dia(df_zip, df_loc):
  djoin = pd.merge(df_zip,df_loc, left_on = "Zone",right_index = True).sort_index()
  return djoin
