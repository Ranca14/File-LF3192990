from bs4 import BeautifulSoup
import requests
from datetime import datetime
import time
import sqlite3
import pandas as pd

def url(i, country_code, carb, brand_auto):

    first_part = 'https://www.autoscout24.it/lst/'
    second_part = '?atype=C&'
    third_part = '&damaged_listing=exclude&desc=0&'
    fourth_part = '&ocs_listing=include&offer=U&'
    fifth_part = '&powertype=kw&search_id=18dknrb3tg6&sort=standard&source=listpage_pagination&ustate=N%2CU'
    page = f'page={str(i)}'
    country = f'cy={country_code}'
    carburante = f'fuel={carb}'
    brand = f'{brand_auto}'

    return first_part + brand + second_part + country + third_part + carburante + fourth_part + page + fifth_part

def get_html(url):

    headers = {"User-Agent": 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/135.0.0.0 Safari/537.36 Edg/135.0.0.0'}
    html_text = requests.get(url,headers=headers)
    html = BeautifulSoup(html_text.content, 'lxml')

    return html

def html_parser(html):
    records = []
    no_annunci = html.find('h2', class_ = 'NoResultsBanner_headerTitle__VagTJ')
    if no_annunci is None:
        listings = html.find_all('article')

        for listing in listings:
            consumi = None
            emissioni = None
            carburante = None

            article_id = listing.get('id')
            marchio = listing.get('data-make')
            modello = listing.get('data-model')
            prezzo = int(listing.get('data-price'))
            try:
                chilometraggio = int(listing.get('data-mileage'))
            except (ValueError, TypeError):
                chilometraggio = None
            stato = listing.get('data-listing-country')
            immatricolazione = listing.get('data-first-registration')

            #handling del possibile errore di tipo ValueError o TypeError se la variabile immatricolazione non contiene nessuna data nel formato %m-%Y#
            try:
                anno_immatricolazione = datetime.strptime(immatricolazione, '%m-%Y')
                oggi = datetime.today()
                eta_veicolo_in_mesi = int((oggi.year - anno_immatricolazione.year)) *12 + int((oggi.month - anno_immatricolazione.month))
            except(ValueError, TypeError):
                anno_immatricolazione = None
                eta_veicolo_in_mesi = None

            if chilometraggio and eta_veicolo_in_mesi is not None and eta_veicolo_in_mesi != 0:
                km_al_mese = int(chilometraggio/eta_veicolo_in_mesi)
            else:
                km_al_mese = None

            #scraping del link della pagina dettagliata dell'annuncio da cui ricavare dati su emissioni, consumi e tipo di motorizzazione#
            link_tag = listing.find('a', class_ = 'ListItem_title__ndA4s ListItem_title_new_design__QIU2b Link_link__Ajn7I')
            if link_tag:
                link_incompleto = link_tag.get('href')
                link_annuncio = 'https://www.autoscout24.it' + link_incompleto
            else:
                link_annuncio = None

            html_dati_aggiuntivi = get_html(link_annuncio)

            #scraping della pagina dell'annuncio per estrarre il tipo di motorizzazione#
            tag_carburante = html_dati_aggiuntivi.find_all('div', class_ = 'VehicleOverview_itemText__AI4dA')
            for tag_carb in tag_carburante:
                ricerca_tag_carburante = tag_carb.get_text()
                if ricerca_tag_carburante in ['Elettrica/Benzina', 'Elettrica/Diesel', 'Benzina', 'Metano', 'Diesel', 'Elettrica', 'Idrogeno', 'GPL', 'Etanolo', 'Altro']:
                    carburante = ricerca_tag_carburante

            #scraping della pagina dell'annuncio per estrarre i valori di consumi ed emissioni#
            tag_dati_aggiuntivi = html_dati_aggiuntivi.find_all('dd', class_ = 'DataGrid_defaultDdStyle__3IYpG DataGrid_fontBold__RqU01')
            for tag in tag_dati_aggiuntivi:
                ricerca_dati_aggiuntivi = tag.get_text()
                if 'g/km' in ricerca_dati_aggiuntivi:
                    emissioni = ricerca_dati_aggiuntivi
                elif 'l/100 km' in ricerca_dati_aggiuntivi:
                    consumi = ricerca_dati_aggiuntivi

            record = {
                'article_id': article_id,
                'marchio': marchio,
                'modello': modello,
                'prezzo': prezzo,
                'km_mese': km_al_mese,
                'stato': stato,
                'eta_veicolo_in_mesi': eta_veicolo_in_mesi,
                'carburante': carburante,
                'emissioni': emissioni,
                'consumi': consumi
            }
            records.append(record)

    return records

def create_table(conn, table_name):
    schema = '''
            (
            article_id varchar(36),
            marchio text,
            modello text,
            prezzo int,
            km_mese int,
            stato text,
            eta_veicolo_in_mesi text,
            carburante text,
            emissioni text,
            consumi text,
            PRIMARY KEY (article_id)
            )
            '''
    query = f'CREATE TABLE IF NOT EXISTS {table_name} {schema}'
    conn.execute(query)
    conn.commit()

def load_data(data, conn, table_name):
    #struttura try-except per gestire eventuali annunci doppioni, in modo tale che non si presentino nel dataframe. Se viene identificato lo stesso id, allora il programma non si blocca#
    try:
        query = f'INSERT INTO {table_name} VALUES (:article_id, :marchio, :modello, :prezzo, :km_mese, :stato, :eta_veicolo_in_mesi, :carburante, :emissioni, :consumi)'
        conn.executemany(query, data)
        conn.commit()
        return True
    except sqlite3.IntegrityError:
        return False


def main():
    lunghezza = 0
    conn = sqlite3.connect('annunci_auto_usate.db')
    conn.execute('DROP TABLE IF EXISTS annunci_auto_usate')
    create_table(conn, 'annunci_auto_usate')

    country_codes = ['I', 'D', 'E', 'A', 'B', 'F', 'NL']
    motorizzazioni = {'Elettrica/Benzina': '2', 'Elettrica/Diesel': '3', 'Benzina': 'B', 'Diesel': 'D', 'Elettrica': 'E'}
    brand_auto = ['tesla', 'mercedes-benz', 'bmw', 'audi', 'volkswagen', 'fiat', 'toyota', 'renault', 'peugeot', 'byd', 'citroen', 'kia', 'opel', 'volvo', 'nissan']

    for cy in country_codes:
        print(f'Estraendo dati in {cy}')
        lunghezza = 0
        for motore, carb in motorizzazioni.items():
            print('Motorizzazione:', motore)
            for brand in brand_auto:
                i = 1
                print(f'Brand: {brand}')
                while True:
                    URL = url(i, cy, carb, brand)
                    html = get_html(URL)
                    response = html_parser(html)
                    if response == []:
                        break
                    print(URL)
                    lunghezza += len(response)
                    load_data(response, conn, 'annunci_auto_usate')
                    time.sleep(1)

                    i += 1

        print(f'In {cy} estratti {lunghezza} annunci\n')

    df = pd.read_sql('SELECT * FROM annunci_auto_usate', conn)

    print(df.tail(5))
    print(f'Nel dataframe ci sono {len(df)} annunci')

    df.to_csv(r'C:\Users\aless_b73h2sr\annunci_auto.csv', index=False)
    conn.close()

if __name__ == '__main__':
    main()






