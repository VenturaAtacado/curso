IDENTIFICATION DIVISION.
       PROGRAM-ID. ANALISA-VIDEOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Define o arquivo de ENTRADA (dados dos videos)
           SELECT DADOS-IN ASSIGN TO 'RESULTADOS.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
      * Define o arquivo de SAIDA (o resultado da analise)
           SELECT DADOS-OUT ASSIGN TO 'ANALISE.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      * Estrutura de cada registro lido do arquivo de ENTRADA
       FD DADOS-IN.
       01 DADOS-REGISTRO.
           05 DADOS-DURACAO-SEGUNDOS   PIC 9(06).
           05 DADOS-TITULO             PIC X(100).

       FD DADOS-OUT.
       01 DADOS-ANALISE                PIC X(200).

       WORKING-STORAGE SECTION.

      * Variáveis para cálculos e totais
       01 WS-TOTAIS.
           05 WS-QUANTIDADE-VIDEOS   PIC 9(05) VALUE ZEROS.
           05 WS-DURACAO-SEGUNDOS-TOTAL PIC 9(10) VALUE ZEROS.
           05 WS-DURACAO-MINUTOS-TOTAL  PIC 9(07) VALUE ZEROS.
           05 WS-DURACAO-RESTO-SEGUNDOS PIC 9(02) VALUE ZEROS.
           05 WS-FIM-DE-ARQUIVO-FLAG    PIC X(01) VALUE 'N'.
              88 FIM-DE-ARQUIVO                  VALUE 'S'.

      * Variáveis de exibição (para formatar a saída)
       01 WS-LINHA-1.
           05 FILLER         PIC X(40) VALUE 
               'Quantidade de Videos Encontrados: '.
           05 WS-DISPLAY-QTD PIC ZZZZ9.

       01 WS-LINHA-2.
           05 FILLER         PIC X(40) VALUE 
               'Minutos Totais de Video:          '.
           05 WS-DISPLAY-MIN PIC ZZZZZZ9.
           05 FILLER         PIC X(10) VALUE ' minutos'.

       01 WS-LINHA-3.
           05 FILLER         PIC X(40) VALUE 
               'Segundos Totais:                  '.
           05 WS-DISPLAY-SEG PIC ZZZZZZZZZ9.
           
       PROCEDURE DIVISION.

       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZA.
           PERFORM 2000-PROCESSA-ARQUIVO
               UNTIL FIM-DE-ARQUIVO.
           PERFORM 3000-CALCULA-TOTAIS.
           PERFORM 4000-GRAVA-SAIDA.
           PERFORM 9000-FINALIZA.
           STOP RUN.

       1000-INICIALIZA.
           OPEN INPUT DADOS-IN
           OPEN OUTPUT DADOS-OUT.
           READ DADOS-IN
               AT END MOVE 'S' TO WS-FIM-DE-ARQUIVO-FLAG
           END-READ.

       2000-PROCESSA-ARQUIVO.
           IF NOT FIM-DE-ARQUIVO
               ADD 1 TO WS-QUANTIDADE-VIDEOS
               ADD DADOS-DURACAO-SEGUNDOS TO WS-DURACAO-SEGUNDOS-TOTAL
               READ DADOS-IN
                   AT END MOVE 'S' TO WS-FIM-DE-ARQUIVO-FLAG
               END-READ
           END-IF.

       3000-CALCULA-TOTAIS.
           IF WS-DURACAO-SEGUNDOS-TOTAL IS NOT EQUAL TO ZEROS
               DIVIDE WS-DURACAO-SEGUNDOS-TOTAL BY 60
                   GIVING WS-DURACAO-MINUTOS-TOTAL
                   REMAINDER WS-DURACAO-RESTO-SEGUNDOS
           END-IF.

       4000-GRAVA-SAIDA.
      * Move os totais calculados para as variaveis de exibicao formatadas
           MOVE WS-QUANTIDADE-VIDEOS TO WS-DISPLAY-QTD
           MOVE WS-DURACAO-MINUTOS-TOTAL TO WS-DISPLAY-MIN
           MOVE WS-DURACAO-SEGUNDOS-TOTAL TO WS-DISPLAY-SEG

      * Grava as linhas no arquivo de SAIDA
           MOVE WS-LINHA-1 TO DADOS-ANALISE
           WRITE DADOS-ANALISE
           
           MOVE WS-LINHA-2 TO DADOS-ANALISE
           WRITE DADOS-ANALISE

           MOVE WS-LINHA-3 TO DADOS-ANALISE
           WRITE DADOS-ANALISE

      * Adiciona o resto dos segundos
           MOVE SPACES TO DADOS-ANALISE
           STRING "   (" WS-DURACAO-RESTO-SEGUNDOS " segundos restantes)" 
               DELIMITED BY SIZE INTO DADOS-ANALISE
           END-STRING
           WRITE DADOS-ANALISE.

       9000-FINALIZA.
           CLOSE DADOS-IN
           CLOSE DADOS-OUT.