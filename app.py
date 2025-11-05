import os
from flask import Flask, render_template, request
from googleapiclient.discovery import build

app = Flask(__name__)

YOUTUBE_API_KEY = "SUA_CHAVE_DE_API_DO_YOUTUBE" 
YOUTUBE_API_SERVICE_NAME = "youtube"
YOUTUBE_API_VERSION = "v3"

youtube = build(YOUTUBE_API_SERVICE_NAME, YOUTUBE_API_VERSION, 
                developerKey=YOUTUBE_API_KEY)

def youtube_search(query):
    search_response = youtube.search().list(
        q=query,
        part="id,snippet",
        maxResults=10,
        type="video"
    ).execute()

    videos = []
    for search_result in search_response.get("items", []):
        if search_result["id"]["kind"] == "youtube#video":
            video_id = search_result["id"]["videoId"]
            video_url = f"https://www.youtube.com/watch?v={video_id}"
            
            videos.append({
                "title": search_result["snippet"]["title"],
                "url": video_url,
                "thumbnail": search_result["snippet"]["thumbnails"]["default"]["url"]
            })
            
    return videos

@app.route("/", methods=["GET", "POST"])
def index():
    videos = []
    search_query = ""
    
    if request.method == "POST":
        search_query = request.form.get("search_term", "")
        if search_query:
            videos = youtube_search(search_query)

    return render_template("index.html", videos=videos, query=search_query)

if __name__ == "__main__":
    if YOUTUBE_API_KEY == "SUA_CHAVE_DE_API_DO_YOUTUBE":
        print("ERRO: Por favor, substitua 'SUA_CHAVE_DE_API_DO_YOUTUBE' pela sua chave de API real no arquivo app.py.")
    else:
        app.run(debug=True)