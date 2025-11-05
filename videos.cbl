IDENTIFICATION DIVISION.
       PROGRAM-ID. PROC-CLIMA.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DADOS-ENTRADA ASSIGN TO 'dados_clima.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELATORIO-SAIDA ASSIGN TO 'resultado_processado.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD  DADOS-ENTRADA.
       01 REG-ENTRADA.
           05  IN-TIMESTAMP       PIC X(14).
           05  IN-LATITUDE        PIC X(10).
           05  IN-LONGITUDE       PIC X(10).
           05  IN-TEMPERATURA-RAW PIC X(05).
           05  IN-UNIDADE         PIC X(05).
      
       FD  RELATORIO-SAIDA.
       01 REG-SAIDA              PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 WS-VARIAVEIS.
           05  WS-FIM-ARQUIVO     PIC X(01) VALUE 'N'.
               88  FIM-ENTRADA              VALUE 'S'.
           05  WS-STATUS-TEMP     PIC 9(02).
      
       01 WS-DADOS-TEMP.
           05  WS-TEMPERATURA-NUM PIC S9(03)V99.  
      
       01 WS-REGISTRO-SAIDA.
           05  WS-DATA-PROCESS    PIC X(10).
           05  FILLER             PIC X(03) VALUE SPACES.
           05  WS-COORD-DISPLAY   PIC X(20).
           05  FILLER             PIC X(03) VALUE SPACES.
           05  WS-CLIMA-MSG       PIC X(44).

       
       PROCEDURE DIVISION.
       000-PRINCIPAL.
           PERFORM 100-ABRIR-ARQUIVOS.
           PERFORM 200-LER-PRIMEIRO.
      
           PERFORM 300-PROCESSAR-LOOP
               UNTIL FIM-ENTRADA.
      
           PERFORM 900-FECHAR-ARQUIVOS.
           STOP RUN.
       
       100-ABRIR-ARQUIVOS.
           OPEN INPUT DADOS-ENTRADA
           OPEN OUTPUT RELATORIO-SAIDA.
           
           MOVE "DATA       COORDENADAS          MENSAGEM CLIMA" 
               TO REG-SAIDA
           WRITE REG-SAIDA.
           MOVE ALL '-' TO REG-SAIDA
           WRITE REG-SAIDA.
       
       200-LER-PRIMEIRO.
           READ DADOS-ENTRADA
               AT END SET FIM-ENTRADA TO TRUE
           END-READ.

       300-PROCESSAR-LOOP.
           PERFORM 310-PROCESSAR-REGISTRO.
           PERFORM 320-LER-PROXIMO.

       310-PROCESSAR-REGISTRO.
           INITIALIZE WS-REGISTRO-SAIDA.

           UNSTRING IN-TEMPERATURA-RAW DELIMITED BY ALL ' '
               INTO WS-TEMPERATURA-NUM
               ON OVERFLOW DISPLAY 'ERRO NA CONVERSAO DE TEMPERATURA'
           END-UNSTRING.
           
           EVALUATE TRUE
               WHEN WS-TEMPERATURA-NUM < 25
                   MOVE 'TA FICANDO FRIO' TO WS-CLIMA-MSG
               WHEN WS-TEMPERATURA-NUM > 28
                   MOVE 'CALORZAO' TO WS-CLIMA-MSG
               WHEN OTHER
                   MOVE 'CLIMA AGRADAVEL' TO WS-CLIMA-MSG
           END-EVALUATE.

           MOVE IN-TIMESTAMP(1:8) TO WS-DATA-PROCESS. 
           STRING IN-LATITUDE DELIMITED BY SIZE
                  '/' DELIMITED BY SIZE
                  IN-LONGITUDE DELIMITED BY SIZE
                  INTO WS-COORD-DISPLAY
           END-STRING.

           STRING WS-DATA-PROCESS DELIMITED BY SIZE
                  SPACES DELIMITED BY SIZE
                  WS-COORD-DISPLAY DELIMITED BY SIZE
                  SPACES DELIMITED BY SIZE
                  WS-CLIMA-MSG DELIMITED BY SIZE
                  ' - ' DELIMITED BY SIZE
                  IN-TEMPERATURA-RAW DELIMITED BY SIZE
                  IN-UNIDADE DELIMITED BY SIZE
                  INTO REG-SAIDA
           END-STRING.
           
           WRITE REG-SAIDA.

       320-LER-PROXIMO.
           READ DADOS-ENTRADA
               AT END SET FIM-ENTRADA TO TRUE
           END-READ.

       900-FECHAR-ARQUIVOS.
           CLOSE DADOS-ENTRADA
                 RELATORIO-SAIDA.
       END PROGRAM PROC-CLIMA.
       