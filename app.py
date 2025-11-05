import os
from flask import Flask, render_template, request
from googleapiclient.discovery import build
import isodate

app = Flask(__name__)

YOUTUBE_API_KEY = "SUA_CHAVE_DE_API_DO_YOUTUBE" 
YOUTUBE_API_SERVICE_NAME = "youtube"
YOUTUBE_API_VERSION = "v3"

def get_video_durations(video_ids):
    youtube = build(YOUTUBE_API_SERVICE_NAME, YOUTUBE_API_VERSION, 
                    developerKey=YOUTUBE_API_KEY)
    
    # Faz uma segunda chamada de API para obter os detalhes do conteúdo (incluindo duração)
    video_response = youtube.videos().list(
        id=",".join(video_ids),
        part="contentDetails"
    ).execute()

    durations = {}
    for video_result in video_response.get("items", []):
        video_id = video_result["id"]
        # A duração vem no formato ISO 8601 (ex: PT5M30S)
        iso_duration = video_result["contentDetails"]["duration"]
        
        # Converte a duração ISO 8601 para segundos
        duration_timedelta = isodate.parse_duration(iso_duration)
        durations[video_id] = int(duration_timedelta.total_seconds())

    return durations

def youtube_search(query):
    youtube = build(YOUTUBE_API_SERVICE_NAME, YOUTUBE_API_VERSION, 
                    developerKey=YOUTUBE_API_KEY)

    search_response = youtube.search().list(
        q=query,
        part="id,snippet",
        maxResults=10,
        type="video"
    ).execute()

    video_ids = []
    videos_list = []
    
    for search_result in search_response.get("items", []):
        if search_result["id"]["kind"] == "youtube#video":
            video_id = search_result["id"]["videoId"]
            video_ids.append(video_id)
            
            videos_list.append({
                "id": video_id,
                "title": search_result["snippet"]["title"],
                "url": f"https://www.youtube.com/watch?v={video_id}",
                "thumbnail": search_result["snippet"]["thumbnails"]["default"]["url"],
                "duration_seconds": 0 
            })
            
    # Obtém as durações em uma única chamada de API (otimização)
    durations_map = get_video_durations(video_ids)
    
    # Atualiza a lista de vídeos com as durações
    for video in videos_list:
        video["duration_seconds"] = durations_map.get(video["id"], 0)
            
    return videos_list

def python_analysis(videos):
    total_seconds = sum(video['duration_seconds'] for video in videos)
    num_videos = len(videos)

    total_minutes = total_seconds // 60
    remaining_seconds = total_seconds % 60

    analysis_result = (
        "========================================\n"
        "ANALISE DE RESULTADOS\n"
        "========================================\n"
        f"1. Quantidade de Vídeos Encontrados: {num_videos:05d}\n"
        f"2. Minutos Totais de Vídeo:          {total_minutes:07d} minutos\n"
        f"   (e {remaining_seconds:02d} segundos restantes)\n"
        f"3. Segundos Totais Brutos:           {total_seconds:010d}\n"
        "========================================"
    )
    return analysis_result

@app.route("/", methods=["GET", "POST"])
def index():
    videos = []
    search_query = ""
    analysis_text = ""

    if request.method == "POST":
        search_query = request.form.get("search_term", "")
        if search_query:
            videos = youtube_search(search_query)
            
            if videos:
                analysis_text = python_analysis(videos)

    return render_template("index.html", videos=videos, query=search_query, analysis=analysis_text)

if __name__ == "__main__":
    if YOUTUBE_API_KEY == "SUA_CHAVE_DE_API_DO_YOUTUBE":
        print("ERRO: Por favor, substitua 'SUA_CHAVE_DE_API_DO_YOUTUBE' pela sua chave de API real no arquivo app.py.")
    else:
        app.run(debug=True)