import pandas as pd
import sqlite3

# Inizio con inserire il prezzo di listino di ogni auto del dataset
def aggiunta_msrp(df):
    conn = sqlite3.connect('MSRP.db')
    df2 = pd.read_sql('select * from MSRP', conn)

    df_final = pd.merge(df, df2, how = 'left', on = ['modello','carburante','anno_auto_annuncio'])

    return df_final

# Procedo con il registrare il reddito pro-capite come indicatore del reddito. I valori più recenti sono del 2023 e sono misurati in dollari attuali
def aggiunta_reddito(df):
    consumo_pro_capite = {'i': 39003.3, 'd': 54343.2, 'a': 56033.6, 'e': 33509, 'b': 54700.9, 'nl': 64572, 'f': 44690.9}

    for stato, value in consumo_pro_capite.items():
        df.loc[df['stato']==stato, 'reddito'] = value

    return None

# Procedo con il registrare la capillarità dell'infrastruttura di ricarica per Paese
def aggiunta_capillarita(df):

    numero_pdr_italia = 50961 + 11835
    numero_bev_italia = 300620
    pdr_ogni_100_bev_italia = round((numero_pdr_italia / numero_bev_italia)*100, 1)

    numero_pdr_germania = 130791 + 38057
    numero_bev_germania = 1835105
    pdr_ogni_100_bev_germania = round((numero_pdr_germania / numero_bev_germania)*100, 1)

    numero_pdr_austria = 26125 + 7400
    numero_bev_austria = 214115
    pdr_ogni_100_bev_austria = round((numero_pdr_austria / numero_bev_austria)*100, 1)

    numero_pdr_spagna = 32309 + 9633
    numero_bev_spagna = 210777
    pdr_ogni_100_bev_spagna = round((numero_pdr_spagna / numero_bev_spagna)*100, 1)

    numero_pdr_belgio = 79058 + 5322
    numero_bev_belgio = 288048
    pdr_ogni_100_bev_belgio = round((numero_pdr_belgio / numero_bev_belgio)*100, 1)

    numero_pdr_olanda = 183015 + 5227
    numero_bev_olanda = 593748
    pdr_ogni_100_bev_olanda = round((numero_pdr_olanda / numero_bev_olanda)*100, 1)

    numero_pdr_francia = 132256 + 32630
    numero_bev_francia = 1344844
    pdr_ogni_100_bev_francia = round((numero_pdr_francia / numero_bev_francia)*100, 1)

    capillarita_rete_ricarica = {'i': pdr_ogni_100_bev_italia, 'd': pdr_ogni_100_bev_germania,
                                 'a': pdr_ogni_100_bev_austria, 'e': pdr_ogni_100_bev_spagna,
                                 'b': pdr_ogni_100_bev_belgio, 'nl': pdr_ogni_100_bev_olanda,
                                 'f': pdr_ogni_100_bev_francia}

    for stato, value in capillarita_rete_ricarica.items():
        df.loc[df['stato']==stato,'capillarità rete di ricarica (pdr/bev)'] = value

    return None

def aggiunta_consumi_elettriche(df):
    df2 = pd.read_csv(r'C:\Users\aless_b73h2sr\Dataset\consumi_elettriche.csv')
    df2['modello'] = df2['modello'].str.strip()

    df_final = pd.merge(df, df2, how = 'left', on = ['modello', 'anno_auto_annuncio'])
    df_final.fillna(0, inplace=True)

    return df_final

def main ():
    # Questo codice serve per andare ad arricchire il dataset con le informazioni mancanti, ossia prezzo di listino (MSRP), capillarità dell'infrastruttura di rete e il reddito, misurato tramite consumo pro-capite per nazione.
    conn = sqlite3.connect('annunci_auto_usate_cleaned.db')
    df = pd.read_sql('SELECT * FROM annunci_auto_usate_cleaned', conn)

    aggiunta_reddito(df)
    aggiunta_capillarita(df)
    df = aggiunta_msrp(df)
    df.loc[(df['MSRP_medio'] < df['prezzo']) & (df['anno_auto_annuncio'] == 2025), 'MSRP_medio'] = df['prezzo']
    df['controllo'] = df['MSRP_medio'] < df['prezzo']
    df = df.loc[df['controllo'] == False]
    df.drop('controllo', axis=1, inplace=True)
    df = aggiunta_consumi_elettriche(df)

    df.columns = ['article_id', 'marchio', 'modello', 'prezzo', 'km_mese', 'stato', 'età', 'carburante', 'emissioni', 'consumi', 'anno_produzione', 'reddito', 'capillarità_rete_di_ricarica', 'MSRP', 'consumi_elettriche']
    df.to_csv(r'C:\Users\aless_b73h2sr\annunci_auto_completo.csv', index=False, float_format = '%.1f', decimal = '.')

    return None

if __name__ == '__main__':
    main()



