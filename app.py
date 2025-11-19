import requests
import os
import csv
from flask import Flask, render_template, jsonify

app = Flask(__name__)
CSV_FILE_PATH = os.path.join(app.static_folder, 'municipios.csv')
url_municipios = "https://servicodados.ibge.gov.br/api/v1/localidades/municipios?orderBy=nome"

def busca_e_organiza_todos_municipios():
    response = requests.get(url_municipios)
    
    if response.status_code == 200:
        dados = response.json()
        estados_municipios = {}
        
        for municipio in dados:
            nome_municipio = municipio.get('nome')
            sigla_uf = None
            
            microrregiao = municipio.get('microrregiao')
            if microrregiao:
                mesorregiao = microrregiao.get('mesorregiao')
                if mesorregiao:
                    uf = mesorregiao.get('UF')
                    if uf:
                        sigla_uf = uf.get('sigla')
            
            if sigla_uf and nome_municipio:
                if sigla_uf not in estados_municipios:
                    estados_municipios[sigla_uf] = []
                    
                estados_municipios[sigla_uf].append(nome_municipio)
        
        dados_finais = {}
        for uf in sorted(estados_municipios.keys()):
            dados_finais[uf] = sorted(estados_municipios[uf])
            
        return dados_finais
    return {}

def salva_municipios_csv(dados_organizados):
    if not os.path.exists(app.static_folder):
        os.makedirs(app.static_folder)

    with open(CSV_FILE_PATH, 'w', newline='', encoding='utf-8') as csvfile:
        fieldnames = ['UF', 'Municipio']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames, delimiter=';')
        
        writer.writeheader()
        
        for uf, municipios in dados_organizados.items():
            for municipio in municipios:
                writer.writerow({'UF': uf, 'Municipio': municipio})

@app.route('/')
def index():
    dados_organizados = busca_e_organiza_todos_municipios()
    
    if dados_organizados:
        salva_municipios_csv(dados_organizados)

    return render_template('index.html')

@app.route('/api/municipios', methods=['GET'])
def api_municipios():
    dados = busca_e_organiza_todos_municipios()
    if dados:
        salva_municipios_csv(dados)
    return jsonify(dados)

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000, debug=True)