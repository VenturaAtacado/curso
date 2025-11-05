import requests
from flask import Flask, render_template, request, jsonify
import os
import datetime

app = Flask(__name__)

BASE_URL = "https://api.open-meteo.com/v1/forecast"
NOME_ARQUIVO_COBOL = "dados_clima.txt" 

def obter_clima(latitude, longitude):
    params = {
        "latitude": latitude,
        "longitude": longitude,
        "current_weather": "true",
        "timezone": "auto"
    }
    
    try:
        lat = float(latitude)
        lon = float(longitude)
    except ValueError:
        return None, "Dados Inválidos"

    try:
        response = requests.get(BASE_URL, params=params)
        response.raise_for_status()
        data = response.json()

        temperatura = data.get("current_weather", {}).get("temperature")
        unidade_temp = data.get("current_weather_units", {}).get("temperature")

        if temperatura is not None and isinstance(temperatura, (int, float)):
            return temperatura, unidade_temp
        else:
            return None, "API Invalida"

    except requests.exceptions.RequestException as e:
        return None, "Conexão"
    except Exception as e:
        return None, "Processamento"

def mensagens(temperatura_valor, unidade):
    if temperatura_valor is None:
        return f"Não foi possível obter a temperatura ({unidade})."

    temp_formatada = f"{temperatura_valor} {unidade}"

    if temperatura_valor < 25:
        return f"Tá ficando **frio** cuidado porque está {temp_formatada}"
    elif temperatura_valor > 28:
        return f"**Calorzão** né, tá {temp_formatada}"
    else:
        return f"O clima está agradável, está {temp_formatada}"

def salvar_para_cobol(latitude, longitude, temperatura_valor, unidade):
    lat_str = f"{latitude:.6f}".ljust(10)[:10] 
    lon_str = f"{longitude:.6f}".ljust(10)[:10] 
    temp_str = str(temperatura_valor).rjust(5) if temperatura_valor is not None else " N/A "
    unidade_str = str(unidade).ljust(5)[:5]
    timestamp = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
    
    linha_dados = f"{timestamp}{lat_str}{lon_str}{temp_str}{unidade_str}\n"

    try:
        with open(NOME_ARQUIVO_COBOL, "a") as f:
            f.write(linha_dados)
        return True
    except Exception as e:
        return False

@app.route("/", methods=["GET"])
def index():
    return render_template("index.html", resultado=None)

@app.route("/clima", methods=["POST"])
def obter_e_processar_clima():
    latitude = request.form.get("latitude")
    longitude = request.form.get("longitude")
    
    if not latitude or not longitude:
        return render_template("index.html", 
                               resultado="Erro: Por favor, forneça a latitude e a longitude.", 
                               classe_alerta="alert-danger")

    try:
        lat_float = float(latitude)
        lon_float = float(longitude)
    except ValueError:
        return render_template("index.html", 
                               resultado="Erro: Latitude e Longitude devem ser números válidos.", 
                               classe_alerta="alert-danger")
    
    temp_valor, temp_unidade = obter_clima(latitude=lat_float, longitude=lon_float)
    
    mensagem_final = mensagens(temp_valor, temp_unidade)
    
    salvamento_ok = salvar_para_cobol(lat_float, lon_float, temp_valor, temp_unidade)
    
    if "frio" in mensagem_final.lower():
        classe_alerta = "alert-info"
    elif "calorzão" in mensagem_final.lower():
        classe_alerta = "alert-warning"
    elif temp_valor is None:
        classe_alerta = "alert-danger"
    else:
        classe_alerta = "alert-success"
        
    
    return render_template("index.html", 
                           resultado=mensagem_final, 
                           classe_alerta=classe_alerta,
                           salvamento=f"💾 Arquivo de dados para COBOL {'SALVO' if salvamento_ok else 'COM ERRO'} ({NOME_ARQUIVO_COBOL}).")

if __name__ == "__main__":
    port = int(os.environ.get('PORT', 5000))
    app.run(host='0.0.0.0', port=port, debug=os.environ.get('DEBUG', 'False').lower() == 'true')