#!/usr/bin/env python3

import pandas as pd
from pathlib import Path
import re
import sys
import xml.etree.ElementTree as ET
from datetime import datetime

DEPT_NAMES = {
    'Ain': '01', 'Aisne': '02', 'Allier': '03', 'Alpes-de-Haute-Provence': '04',
    'Hautes-Alpes': '05', 'Alpes-Maritimes': '06', 'Ard\u00e8che': '07', 'Ardennes': '08',
    'Ari\u00e8ge': '09', 'Aube': '10', 'Aude': '11', 'Aveyron': '12', 'Bouches-du-Rh\u00f4ne': '13',
    'Bouches du Rh\u00f4ne': '13', 'Calvados': '14', 'Cantal': '15', 'Charente': '16',
    'Charente-Maritime': '17', 'Cher': '18', 'Corr\u00e8ze': '19', "C\u00f4te-d'Or": '21',
    "C\u00f4tes-d'Armor": '22', 'Creuse': '23', 'Dordogne': '24', 'Doubs': '25', 'Dr\u00f4me': '26',
    'Eure': '27', 'Eure-et-Loir': '28', 'Finist\u00e8re': '29', 'Gard': '30',
    'Haute-Garonne': '31', 'Gers': '32', 'Gironde': '33', 'H\u00e9rault': '34',
    'Ille-et-Vilaine': '35', 'Indre': '36', 'Indre-et-Loire': '37', 'Is\u00e8re': '38',
    'Jura': '39', 'Landes': '40', 'Loir-et-Cher': '41', 'Loire': '42',
    'Haute-Loire': '43', 'Loire-Atlantique': '44', 'Loiret': '45', 'Lot': '46',
    'Lot-et-Garonne': '47', 'Loz\u00e8re': '48', 'Maine-et-Loire': '49', 'Manche': '50',
    'Marne': '51', 'Haute-Marne': '52', 'Mayenne': '53', 'Meurthe-et-Moselle': '54',
    'Meuse': '55', 'Morbihan': '56', 'Moselle': '57', 'Ni\u00e8vre': '58', 'Nord': '59',
    'Oise': '60', 'Orne': '61', 'Pas-de-Calais': '62', 'Puy-de-D\u00f4me': '63',
    'Pyr\u00e9n\u00e9es-Atlantiques': '64', 'Hautes-Pyr\u00e9n\u00e9es': '65', 'Pyr\u00e9n\u00e9es-Orientales': '66',
    'Bas-Rhin': '67', 'Haut-Rhin': '68', 'Rh\u00f4ne': '69', 'Haute-Sa\u00f4ne': '70',
    'Sa\u00f4ne-et-Loire': '71', 'Sarthe': '72', 'Savoie': '73', 'Haute-Savoie': '74',
    'Paris': '75', 'Seine-Maritime': '76', 'Seine-et-Marne': '77', 'Yvelines': '78',
    'Deux-S\u00e8vres': '79', 'Somme': '80', 'Tarn': '81', 'Tarn-et-Garonne': '82', 'Var': '83',
    'Vaucluse': '84', 'Vend\u00e9e': '85', 'Vienne': '86', 'Haute-Vienne': '87', 'Vosges': '88',
    'Yonne': '89', 'Territoire de Belfort': '90', 'Essonne': '91', 'Hauts-de-Seine': '92',
    'Seine-Saint-Denis': '93', 'Val-de-Marne': '94', "Val-d'Oise": '95',
    'Corse-du-Sud': '2A', 'Haute-Corse': '2B',
    'ILE-DE-FRANCE': ['75', '77', '78', '91', '92', '93', '94', '95'],
    'POITOU-CHARENTES': ['16', '17', '79', '86'],
}


def is_appleDouble_file(xml_path):
    try:
        with open(xml_path, 'rb') as f:
            magic = f.read(4)
        return magic == b'\x00\x05\x16\x07'
    except Exception:
        return False


def extract_date_from_filename(filename):
    match = re.search(r'(\d{4})_(\d{2})_(\d{2})-(\d{2})_(\d{2})', str(filename))
    if match:
        y, m, d, h, mi = match.groups()
        return datetime(int(y), int(m), int(d), int(h), int(mi))
    return None


def extract_dept_codes_from_text(text):
    if not text:
        return []

    codes = set()

    matches = re.findall(r'\((\d{1,2}[AB]?)\)', text)
    for match in matches:
        code = match.zfill(2) if len(match) == 1 and match.isdigit() else match
        codes.add(code)

    for dept_name, code in DEPT_NAMES.items():
        if isinstance(code, list):
            if dept_name in text:
                codes.update(code)
        else:
            if dept_name in text:
                codes.add(code)

    return sorted(list(codes))


def read_bulletin_text(xml_path):
    for enc in ('ISO-8859-1', 'utf-8', 'cp1252'):
        try:
            with open(xml_path, 'r', encoding=enc) as f:
                content = f.read()
            if '<Bulletin>' in content or '<Bulletin ' in content:
                return content, enc
        except (UnicodeDecodeError, Exception):
            continue
    return None, None


def parse_xml_format_2006(content, xml_path):
    phenom_match = re.search(r"Type d'[ée]v[ée]nement\s*:\s*(.+)", content, re.IGNORECASE)
    if not phenom_match:
        return []

    phenomene = phenom_match.group(1).strip().capitalize()

    lieux_match = re.search(
        r"Lieux concern[ée]s[^:]*:\s*(.+?)(?:\n\n|\nD[ée]but|\nFin|\nQualification|\Z)",
        content, re.DOTALL | re.IGNORECASE
    )
    if not lieux_match:
        return []

    dept_codes = extract_dept_codes_from_text(lieux_match.group(1).strip())

    return [{
        'departement': code,
        'phenomene': phenomene,
        'niveau': 'Orange',
        'source_xml': xml_path.name
    } for code in dept_codes]


def parse_xml_format_2007_2012(content, xml_path):
    phenom_match = re.search(r"Evenement type\s*:\s*(.+)", content, re.IGNORECASE)
    if not phenom_match:
        return []

    phenomene = phenom_match.group(1).strip().capitalize()
    dept_codes = set()

    debut_match = re.search(
        r"D[ée]but de suivi pour \d+ d[ée]partement[^:]*:\s*(.+?)(?:\.|$)",
        content, re.IGNORECASE | re.MULTILINE
    )
    if debut_match:
        dept_codes.update(extract_dept_codes_from_text(debut_match.group(1)))

    maintien_match = re.search(
        r"Maintien de suivi pour \d+ d[ée]partement[^:]*:\s*(.+?)(?:\.|$)",
        content, re.IGNORECASE | re.MULTILINE
    )
    if maintien_match:
        dept_codes.update(extract_dept_codes_from_text(maintien_match.group(1)))

    if not dept_codes:
        lieux_match = re.search(
            r"Lieux concern[ée]s[^:]*:\s*(.+?)(?:\n\n|\nD[ée]but|\nFin|\nQualification|\Z)",
            content, re.DOTALL | re.IGNORECASE
        )
        if lieux_match:
            dept_codes.update(extract_dept_codes_from_text(lieux_match.group(1)))

    if not dept_codes:
        return []

    return [{
        'departement': code,
        'phenomene': phenomene,
        'niveau': 'Orange',
        'source_xml': xml_path.name
    } for code in sorted(dept_codes)]


def parse_xml_format_moderne(xml_path):
    try:
        tree = ET.parse(xml_path)
        root = tree.getroot()

        phenomene_elem = root.find('.//Phenomenes')
        if phenomene_elem is None:
            return []

        phenomene = phenomene_elem.get('evenement', '').replace('.', '').strip()
        if not phenomene:
            return []

        texte_rouge = phenomene_elem.find('TexteRouge')
        depts_rouge = set()
        if texte_rouge is not None and texte_rouge.text:
            if 'ROUGE' in texte_rouge.text:
                depts_rouge.update(extract_dept_codes_from_text(texte_rouge.text))

        localisation = root.find('.//Titre[@name="Localisation"]')
        if localisation is None:
            return []

        depts_orange = set()
        for paragraphe in localisation.findall('.//Paragraphe'):
            intitule = paragraphe.find('Intitule')
            if intitule is not None:
                intitule_text = intitule.text or ''
                if any(kw in intitule_text for kw in ['D\u00e9but', 'Maintien']):
                    for texte_elem in paragraphe.findall('.//Texte'):
                        if texte_elem.text and texte_elem.text.strip() != 'Aucun d\u00e9partement':
                            depts_orange.update(extract_dept_codes_from_text(texte_elem.text))

        results = []
        for code in depts_rouge:
            results.append({
                'departement': code,
                'phenomene': phenomene,
                'niveau': 'Rouge',
                'source_xml': xml_path.name
            })
        for code in depts_orange:
            if code not in depts_rouge:
                results.append({
                    'departement': code,
                    'phenomene': phenomene,
                    'niveau': 'Orange',
                    'source_xml': xml_path.name
                })

        return results

    except Exception as e:
        print(f"Warning: failed to parse modern format {xml_path.name}: {e}")
        return []


def parse_xml_auto(xml_path):
    if is_appleDouble_file(xml_path):
        return []

    try:
        with open(xml_path, 'r', encoding='utf-8') as f:
            content = f.read()
    except UnicodeDecodeError:
        content, _ = read_bulletin_text(xml_path)
        if content is None:
            return []
        if re.search(r"Evenement type\s*:", content, re.IGNORECASE):
            return parse_xml_format_2007_2012(content, xml_path)
        return parse_xml_format_2006(content, xml_path)

    if 'Phenomenes' in content:
        return parse_xml_format_moderne(xml_path)

    if re.search(r"Evenement type\s*:", content, re.IGNORECASE):
        return parse_xml_format_2007_2012(content, xml_path)

    if re.search(r"Type d'[ée]v[ée]nement\s*:", content, re.IGNORECASE):
        return parse_xml_format_2006(content, xml_path)

    print(f"Warning: unknown bulletin format: {xml_path.name}")
    return []


def process_xml_folder(folder_path):
    folder    = Path(folder_path)
    xml_files = [f for f in folder.glob('*.xml') if not is_appleDouble_file(f)]

    if not xml_files:
        return None

    date_bulletin = extract_date_from_filename(xml_files[0].name)

    all_results = []
    for xml_file in xml_files:
        all_results.extend(parse_xml_auto(xml_file))

    if not all_results:
        return None

    df = pd.DataFrame(all_results)

    niveau_order = {'Rouge': 2, 'Orange': 1}
    df['niveau_code'] = df['niveau'].map(niveau_order)

    df_agg = (
        df.sort_values('niveau_code', ascending=False)
          .groupby('departement')
          .agg({'phenomene': 'first', 'niveau': 'first', 'source_xml': 'first'})
          .reset_index()
    )

    df_agg['date_bulletin'] = date_bulletin
    df_agg['dossier']       = folder.name

    return df_agg


HARDCODED_DATA = [
    {'date_bulletin': datetime(2008, 8, 2, 16, 20),  'dossier': 'vigilance_2008_08_02', 'departement': '13', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2008, 8, 3, 6, 10),   'dossier': 'vigilance_2008_08_03', 'departement': '13', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2008, 8, 3, 16, 10),  'dossier': 'vigilance_2008_08_03', 'departement': '13', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2008, 8, 4, 6, 10),   'dossier': 'vigilance_2008_08_04', 'departement': '13', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 17, 6, 10),  'dossier': 'vigilance_2009_08_17', 'departement': '69', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 17, 16, 10), 'dossier': 'vigilance_2009_08_17', 'departement': '69', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 17, 16, 10), 'dossier': 'vigilance_2009_08_17', 'departement': '07', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 17, 16, 10), 'dossier': 'vigilance_2009_08_17', 'departement': '26', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 17, 16, 10), 'dossier': 'vigilance_2009_08_17', 'departement': '84', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 18, 9, 30),  'dossier': 'vigilance_2009_08_18', 'departement': '69', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 18, 9, 30),  'dossier': 'vigilance_2009_08_18', 'departement': '07', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 18, 9, 30),  'dossier': 'vigilance_2009_08_18', 'departement': '26', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 18, 9, 30),  'dossier': 'vigilance_2009_08_18', 'departement': '84', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 18, 16, 10), 'dossier': 'vigilance_2009_08_18', 'departement': '69', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 18, 16, 10), 'dossier': 'vigilance_2009_08_18', 'departement': '07', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 18, 16, 10), 'dossier': 'vigilance_2009_08_18', 'departement': '26', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 18, 16, 10), 'dossier': 'vigilance_2009_08_18', 'departement': '84', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 18, 16, 10), 'dossier': 'vigilance_2009_08_18', 'departement': '82', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 18, 16, 10), 'dossier': 'vigilance_2009_08_18', 'departement': '31', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 18, 16, 10), 'dossier': 'vigilance_2009_08_18', 'departement': '81', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 6, 10),  'dossier': 'vigilance_2009_08_19', 'departement': '69', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 6, 10),  'dossier': 'vigilance_2009_08_19', 'departement': '07', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 6, 10),  'dossier': 'vigilance_2009_08_19', 'departement': '26', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 6, 10),  'dossier': 'vigilance_2009_08_19', 'departement': '84', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 6, 10),  'dossier': 'vigilance_2009_08_19', 'departement': '82', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 6, 10),  'dossier': 'vigilance_2009_08_19', 'departement': '31', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 6, 10),  'dossier': 'vigilance_2009_08_19', 'departement': '81', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 16, 10), 'dossier': 'vigilance_2009_08_19', 'departement': '69', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 16, 10), 'dossier': 'vigilance_2009_08_19', 'departement': '07', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 16, 10), 'dossier': 'vigilance_2009_08_19', 'departement': '26', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 16, 10), 'dossier': 'vigilance_2009_08_19', 'departement': '84', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 16, 10), 'dossier': 'vigilance_2009_08_19', 'departement': '82', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 16, 10), 'dossier': 'vigilance_2009_08_19', 'departement': '31', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 19, 16, 10), 'dossier': 'vigilance_2009_08_19', 'departement': '81', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 20, 6, 10),  'dossier': 'vigilance_2009_08_20', 'departement': '69', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 20, 6, 10),  'dossier': 'vigilance_2009_08_20', 'departement': '07', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 20, 6, 10),  'dossier': 'vigilance_2009_08_20', 'departement': '26', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 20, 6, 10),  'dossier': 'vigilance_2009_08_20', 'departement': '84', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 20, 6, 10),  'dossier': 'vigilance_2009_08_20', 'departement': '82', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 20, 6, 10),  'dossier': 'vigilance_2009_08_20', 'departement': '31', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 20, 6, 10),  'dossier': 'vigilance_2009_08_20', 'departement': '81', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 20, 21, 30), 'dossier': 'vigilance_2009_08_20', 'departement': '69', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 20, 21, 30), 'dossier': 'vigilance_2009_08_20', 'departement': '07', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 20, 21, 30), 'dossier': 'vigilance_2009_08_20', 'departement': '26', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
    {'date_bulletin': datetime(2009, 8, 20, 21, 30), 'dossier': 'vigilance_2009_08_20', 'departement': '84', 'phenomene': 'Canicule', 'niveau': 'Orange', 'source_xml': 'hardcoded'},
]


def get_hardcoded_df():
    if not HARDCODED_DATA:
        return None
    df = pd.DataFrame(HARDCODED_DATA)
    niveau_order    = {'Rouge': 2, 'Orange': 1}
    df['niveau_code'] = df['niveau'].map(niveau_order)
    df_agg = (
        df.sort_values('niveau_code', ascending=False)
          .groupby(['dossier', 'departement'])
          .agg({'phenomene': 'first', 'niveau': 'first',
                'date_bulletin': 'first', 'source_xml': 'first'})
          .reset_index()
    )
    df_agg.drop(columns=['niveau_code'], errors='ignore')
    return df_agg


def main():
    if len(sys.argv) < 3:
        print("Usage: python extraction_vigilance.py <vigilance_folder/> <output.csv>")
        sys.exit(1)

    input_path = Path(sys.argv[1])
    output_csv = sys.argv[2]

    if not input_path.exists():
        print(f"Folder not found: {input_path}")
        sys.exit(1)

    folders = [d for d in input_path.iterdir() if d.is_dir()]
    if not folders:
        folders = [input_path]

    print(f"{len(folders)} folders to process")

    all_dfs = []
    for i, folder in enumerate(sorted(folders), 1):
        df = process_xml_folder(folder)
        if df is not None and not df.empty:
            all_dfs.append(df)
            print(f"{i}/{len(folders)}: {folder.name} - {len(df)} depts")
        else:
            print(f"{i}/{len(folders)}: {folder.name} - no data")

    df_hardcoded = get_hardcoded_df()
    if df_hardcoded is not None:
        all_dfs.append(df_hardcoded)

    if not all_dfs:
        print("No data extracted.")
        sys.exit(1)

    df_final = pd.concat(all_dfs, ignore_index=True)
    df_final.to_csv(output_csv, index=False)

    print(f"\nTotal: {len(df_final)} observations")
    print(f"Unique departments: {df_final['departement'].nunique()}")
    print(f"Saved: {output_csv}")


if __name__ == '__main__':
    main()
