import pandas as pd
import numpy as np
import sqlite3

df1 = pd.read_csv(r'C:\Users\aless_b73h2sr\Dataset\MSRP\MSRP_in_corone.csv')
df2 = pd.read_csv(r'C:\Users\aless_b73h2sr\Dataset\MSRP\MSRP_euro.csv')

df1.columns = df1.columns.str.strip()
df2.columns = df2.columns.str.strip()

for col in ['Modello','Carburante']:
    df1[col] = df1[col].str.strip()
for col in ['Modello','Carburante']:
    df2[col] = df2[col].str.strip()

df1['Prezzo'] = pd.to_numeric(df1['Prezzo'], errors='coerce').astype('Float64')

#il tasso di cambio per il 2025 risale all'ultima data disponibile quando è stato sviluppato questo codice, ossia il 26 maggio
lista_cambi_eurnok = {'2010': 8.0043, '2011': 7.7934, '2012': 7.4751, '2013': 7.8067, '2014': 8.3544, '2015': 8.9496, '2016': 9.2906, '2017': 9.3270, '2018': 9.5975, '2019': 9.8511, '2020': 10.723, '2021': 10.163, '2022': 10.103, '2023': 11.425, '2024': 11.629, '2025': 11.484}

for year,cambio in lista_cambi_eurnok.items():
    mask = (df1['Anno'] == int(year))
    df1.loc[mask, "Prezzo"] = (df1.loc[mask, 'Prezzo']/cambio).round(0)

df1.loc[df1['Prezzo']==0, 'Prezzo'] = pd.NA
df2.loc[df2['Prezzo']==0, 'Prezzo'] = pd.NA
df1['Prezzo'] = df1['Prezzo'].astype('Int64')
df2['Prezzo'] = df2['Prezzo'].astype('Int64')

df = pd.concat([df1, df2], ignore_index=True)

df_MSRP_medio = df.groupby(['Modello','Carburante','Anno']).agg({'Prezzo':'mean'}).round(0).reset_index().rename(columns={'Prezzo':'MSRP_medio'})
df_merged = pd.merge(df,df_MSRP_medio, on=['Modello','Carburante','Anno'], how='left')
df_no_duplicati = df_merged[['Modello','Carburante','Anno','MSRP_medio']].drop_duplicates()
df_no_duplicati['MSRP_medio'] = df_no_duplicati['MSRP_medio'].fillna(df_no_duplicati.groupby(['Modello','Carburante'])['MSRP_medio'].transform('mean').round(0))
df_no_duplicati['MSRP_medio'] = df_no_duplicati['MSRP_medio'].fillna(df_no_duplicati.groupby(['Modello','Anno'])['MSRP_medio'].transform('mean').round(0))

df_no_duplicati['MSRP_medio'] = df_no_duplicati['MSRP_medio'].astype('Int64')

df_no_duplicati.columns = ['modello', 'carburante', 'anno_auto_annuncio', 'MSRP_medio']

df_no_duplicati.to_csv(r'C:\Users\aless_b73h2sr\MSRP.csv', index=False)

conn = sqlite3.connect('MSRP.db')
df_no_duplicati.to_sql('MSRP', conn, if_exists = 'replace', index=False)
conn.close()
