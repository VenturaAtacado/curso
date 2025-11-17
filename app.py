import requests
from flask import Flask, render_template, jsonify

app = Flask(__name__)
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
        
        # Ordena municípios dentro de cada estado
        dados_finais = {}
        for uf, municipios in estados_municipios.items():
            dados_finais[uf] = sorted(municipios)
            
        return dados_finais
    return {}

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/api/municipios', methods=['GET'])
def api_municipios():
    dados = busca_e_organiza_todos_municipios()
    return jsonify(dados)

if __name__ == '__main__':
    # Para uso em ambiente de desenvolvimento. 
    # Mude para gunicorn ou similar em produção!
    app.run(debug=True)