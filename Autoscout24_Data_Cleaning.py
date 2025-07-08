import pandas as pd
import sqlite3
import numpy as np
from datetime import date

def replace_blanks(value):
    if value == "":
        return None
    else:
        return value

conn = sqlite3.connect('annunci_auto_usate.db')
df = pd.read_sql("SELECT * FROM annunci_auto_usate", conn)

# Elimino tutti gli annunci di auto motorizzate elettriche/diesel, GPL, Metano o con valore assente in quanto ho osservato che sono poco rilevanti per il tipo di analisi
df = df[~df['carburante'].isin(['Elettrica/Diesel', 'GPL', 'Metano',''])]
df = df.dropna(subset=['carburante'])

# Elimino tutti gli annunci che non corrispondono ai modelli selezionati perché più presenti
df = df[df['modello'].isin(['glc 300','e 300','gle 350','c 300','a 250','x1','i3','x3','a3','e-tron','a4','clio','zoe','captur','golf','tiguan','id.3','model 3','model y','model s'])]

# Sostituisco con None tutte le celle vuote ed elimino quelle per km_mese ed età veicolo
df = df.map(replace_blanks)
df = df.dropna(subset=['km_mese','eta_veicolo_in_mesi'])
df['eta_veicolo_in_mesi'] = df['eta_veicolo_in_mesi'].astype(int)
df = df.loc[ (df['eta_veicolo_in_mesi'] > 0)  & (df['km_mese'] > 0)]

# modifico il contenuto delle celle per emissioni e consumi, in quanto presentano al loro interno l'unità di misura. Essa la sposterò nell'intestazione di colonna e mantengo solo il numero nelle celle. Inoltre, correggo un errore precedente di salvarli come testo.
df['emissioni'] = df['emissioni'].str.replace('g/km (comb.)','').str.replace(',','.').astype(float)
df['consumi'] = df['consumi'].str.replace('l/100 km (comb.)','').str.replace(',','.').astype(float)

df.rename(columns = {'emissioni':'emissioni (g/km (comb.)','consumi':'consumi (l/100 km (comb.)'}, inplace = True)

# Correggo gli errori di caricamento degli annunci, ossia vado a trasferire in categoria elettrica le macchine che sono elettriche ma non dichiarate come tali (tra i modelli selezionati)
modelli_elettrici = ['i3','e-tron','zoe','id.3','model 3','model y','model s']
df.loc[df['modello'].isin(modelli_elettrici), 'carburante'] = 'Elettrica'

'''Procedo con il trattamento dei missing values per i veicoli non elettrici: per essi andrò ad applicare il valore medio di consumi ed emissioni ottenuto da veicoli simili.
   Siccome ho già selezionato gli annunci su cui voglio andare a lavorare, allora mi basta raggruppare per modello e tipo di motorizzazione e poi calcolo il valore medio di
   consumi ed emissioni. Dove manca uno dei due (o entrambi) vado ad inserire il valore medio. Per i veicoli elettrici, essendo le emissioni quasi sempre a 0, se indicate,
   e i consumi mai dichiarati, preferisco non procedere con lo stesso trattamento. Nella regressione andrò a costruire una dummy variable per distinguere una macchina non 
   elettrica da una elettrica, cosicché non ci sia un bias dovuto al fatto che per le auto elettriche ci siano informazioni mancanti.'''

df_senza_EV = df.loc[~df['carburante'].isin(['Elettrica'])]
df_senza_EV.loc[df_senza_EV['consumi (l/100 km (comb.)'] == 0, 'consumi (l/100 km (comb.)'] = np.nan
df_senza_EV.loc[df_senza_EV['emissioni (g/km (comb.)'] == 0, 'emissioni (g/km (comb.)'] = np.nan

df_emissioni_medie_modello = df_senza_EV.groupby(['marchio','modello','carburante']).agg({'emissioni (g/km (comb.)':'mean'}).round(0).rename(columns={'emissioni (g/km (comb.)':'emissioni_modello'}).reset_index()
df_emissioni_medie_marchio = df_senza_EV.groupby(['marchio','carburante']).agg({'emissioni (g/km (comb.)':'mean'}).round(0).rename(columns={'emissioni (g/km (comb.)':'emissioni_marchio'}).reset_index()
df_emissioni_medie_merged = pd.merge(df_emissioni_medie_modello, df_emissioni_medie_marchio, on=['marchio','carburante'], how='left')
df_emissioni_medie_merged['emissioni (g/km (comb.)'] = df_emissioni_medie_merged['emissioni_modello'].fillna(df_emissioni_medie_merged['emissioni_marchio'])
df_emissioni_medie = df_emissioni_medie_merged[['marchio','modello','carburante','emissioni (g/km (comb.)']]

df_merged = pd.merge(df_senza_EV, df_emissioni_medie, on=['marchio', 'modello', 'carburante'], how='left')
df_merged['emissioni (g/km (comb.)'] = df_merged['emissioni (g/km (comb.)_x'].fillna(df_merged['emissioni (g/km (comb.)_y'])
df_senza_EV = df_merged.drop(columns=['emissioni (g/km (comb.)_y','emissioni (g/km (comb.)_x'])

df_consumo_medio_modello = df_senza_EV.groupby(['marchio','modello','carburante']).agg({'consumi (l/100 km (comb.)':'mean'}).round(1).rename(columns={'consumi (l/100 km (comb.)':'consumi_modello'}).reset_index()
df_consumo_medio_marchio = df_senza_EV.groupby(['marchio','carburante']).agg({'consumi (l/100 km (comb.)':'mean'}).round(1).rename(columns={'consumi (l/100 km (comb.)':'consumi_marchio'}).reset_index()
df_consumo_medio_merged = pd.merge(df_consumo_medio_modello, df_consumo_medio_marchio, on=['marchio','carburante'], how='left')
df_consumo_medio_merged['consumi (l/100 km (comb.)'] = df_consumo_medio_merged['consumi_modello'].fillna(df_consumo_medio_merged['consumi_marchio'])
df_consumo_medio = df_consumo_medio_merged[['marchio','modello','carburante','consumi (l/100 km (comb.)']]

df_merged_2 = pd.merge(df_senza_EV, df_consumo_medio, on=['marchio', 'modello', 'carburante'], how='left')
df_merged_2['consumi (l/100 km (comb.)'] = df_merged_2['consumi (l/100 km (comb.)_x'].fillna(df_merged_2['consumi (l/100 km (comb.)_y'])
df_2 = df_merged_2.drop(columns=['consumi (l/100 km (comb.)_y','consumi (l/100 km (comb.)_x'])

df_EV = df.loc[df['carburante'] == 'Elettrica']
df_3 = pd.concat([df_2,df_EV], ignore_index=True)

# Vado ad inserire 0 sia nei consumi che nelle emissioni delle auto elettriche
df_3.loc[df_3['carburante']=='Elettrica', ['consumi (l/100 km (comb.)', 'emissioni (g/km (comb.)']] = 0

# Vado ad eliminare tutti quei veicoli con età superiore ai 15 anni
df_3 = df_3.loc[df_3['eta_veicolo_in_mesi'] < 181]

# Vado ad eliminare quegli annunci che presentano motorizzazione evidentemente sbagliata
df_3 = df_3.loc[~((df_3['modello'].isin(['glc 300','e 300','gle 350','c 300','a 250','x1','x3','a3','a4','clio','captur','golf','tiguan'])) & (df_3['carburante'].isin(['Elettrica'])))]

# Ho notato diverse righe che presentano doppioni se non per l'id. Probabilmente è dovuto al fatto che vengano pubblicati più volte gli stessi veicoli
df_3 = df_3.drop_duplicates(subset=[col for col in df_3.columns if col != 'article_id'])

# Vado ad eliminare gli outlier per consumi ed emissioni per auto non elettriche (per esse non servirebbe avendo impostato per default 0 a consumi ed emissioni)
df_non_elettriche = df_3[df_3['carburante'] != 'Elettrica']
df_elettriche = df_3[df_3['carburante'] == 'Elettrica']

Q1_c = df_non_elettriche['consumi (l/100 km (comb.)'].quantile(0.25)
Q3_c = df_non_elettriche['consumi (l/100 km (comb.)'].quantile(0.75)
IQR_c = Q3_c - Q1_c

lower_bound_c = Q1_c - 1.5 * IQR_c
upper_bound_c = Q3_c + 1.5 * IQR_c

df_non_elettriche_consumi_cleaned = df_non_elettriche.loc[(df_non_elettriche['consumi (l/100 km (comb.)'] >= lower_bound_c) & (df_non_elettriche['consumi (l/100 km (comb.)'] <= upper_bound_c)]

Q1_e = df_non_elettriche['emissioni (g/km (comb.)'].quantile(0.25)
Q3_e = df_non_elettriche['emissioni (g/km (comb.)'].quantile(0.75)
IQR_e = Q3_e - Q1_e

lower_bound_e = Q1_e - 1.5 * IQR_e
upper_bound_e = Q3_e + 1.5 * IQR_e

df_non_elettriche_consumi_e_emissioni_cleaned = df_non_elettriche_consumi_cleaned.loc[(df_non_elettriche_consumi_cleaned['emissioni (g/km (comb.)'] >= lower_bound_e) & (df_non_elettriche_consumi_cleaned['emissioni (g/km (comb.)'] <= upper_bound_e)]

df_4 = pd.concat([df_non_elettriche_consumi_e_emissioni_cleaned,df_elettriche], ignore_index=True)

#Vado ad eliminare gli outlier nel prezzo di vendita
Q1_p = df_4['prezzo'].quantile(0.25)
Q3_p = df_4['prezzo'].quantile(0.75)
IQR_p = Q3_p - Q1_p

lower_bound_p = Q1_p - 1.5 * IQR_p
upper_bound_p = Q3_p + 1.5 * IQR_p

df_4 = df_4.loc[(df_4['prezzo'] >= lower_bound_p) & (df_4['prezzo'] <= upper_bound_p)]

#Vado ad eliminare gli outlier nel km_mese
Q1_k = df_4['km_mese'].quantile(0.25)
Q3_k = df_4['km_mese'].quantile(0.75)
IQR_k = Q3_k - Q1_k

lower_bound_k = Q1_k - 1.5 * IQR_k
upper_bound_k = Q3_k + 1.5 * IQR_k

df_4 = df_4.loc[(df_4['km_mese'] >= lower_bound_k) & (df_4['km_mese'] <= upper_bound_k)]

# Inserisco per ogni auto il model year
oggi = date.today()
anno_oggi = oggi.year
df_4['anno_auto_annuncio'] = anno_oggi - df_4['eta_veicolo_in_mesi'] // 12

df_4.to_csv(r'C:\Users\aless_b73h2sr\annunci_auto_ridotto.csv', index=False)

# Carico il dataset ripulito in un database nuovo
conn2 = sqlite3.connect('annunci_auto_usate_cleaned.db')
df_4.to_sql('annunci_auto_usate_cleaned', conn2, if_exists = 'replace', index=False)

conn2.close()
conn.close()