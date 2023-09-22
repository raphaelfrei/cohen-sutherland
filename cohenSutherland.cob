      ******************************************************************
      * Author:  Raphael Frei
      * Date:    19/09/2023
      * Purpose: Cohen Sutherland in COBOL
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. cohenSutherland.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-M               PIC 999V99 VALUE ZEROES.

       77 WS-TOPO            PIC 999V99 VALUE ZEROES.
       77 WS-BASE            PIC 999V99 VALUE ZEROES.
       77 WS-ESQUERDA        PIC 999V99 VALUE ZEROES.
       77 WS-DIREITA         PIC 999V99 VALUE ZEROES.

       77 WS-XMIN            PIC 999V99 VALUE ZEROES.
       77 WS-XMAX            PIC 999V99 VALUE ZEROES.
       77 WS-YMIN            PIC 999V99 VALUE ZEROES.
       77 WS-YMAX            PIC 999V99 VALUE ZEROES.

       77 WS-XONE            PIC 999V99 VALUE ZEROES.
       77 WS-XTWO            PIC 999V99 VALUE ZEROES.
       77 WS-YONE            PIC 999V99 VALUE ZEROES.
       77 WS-YTWO            PIC 999V99 VALUE ZEROES.

       77 WS-STATUS      PIC X(01) VALUE ZEROES.

       PROCEDURE DIVISION.

       P-MAIN.

            INITIALISE WS-XONE
                       WS-XTWO
                       WS-YONE
                       WS-YTWO
                       WS-M
                       WS-TOPO
                       WS-BASE
                       WS-ESQUERDA
                       WS-DIREITA
                       WS-XMIN
                       WS-XMAX
                       WS-YMIN
                       WS-YMAX

            DISPLAY
            "*********************************************************"
            DISPLAY
            "*                                                       *"
            DISPLAY
            "               COHEN SUTHERLAND IN COBOL                "
            DISPLAY
            "*                                                       *"
            DISPLAY
            "*********************************************************"


            PERFORM P-VARIABLES THRU P-VARIABLES-FIM
            .
       P-MAIN-END.

       P-VARIABLES.

      ****** X MINIMO
            DISPLAY
            "INSIRA O X MIN:"

            ACCEPT WS-XMIN

      ****** X MAXIMO
             DISPLAY
            "INSIRA O X MAX:"

            ACCEPT WS-XMAX

      ****** Y MINIMO
            DISPLAY
            "INSIRA O Y MIN:"

            ACCEPT WS-YMIN

      ****** Y MAXIMO
            DISPLAY
            "INSIRA O Y MAX:"

            ACCEPT WS-YMAX

      ****** X1
            DISPLAY
            "INSIRA O X1:"

            ACCEPT WS-XONE

      ****** X2
            DISPLAY
            "INSIRA O X2:"

            ACCEPT WS-XTWO

      ****** Y1
            DISPLAY
            "INSIRA O Y1:"

            ACCEPT WS-YONE

      ****** Y2
            DISPLAY
            "INSIRA O Y2:"

            ACCEPT WS-YTWO

            PERFORM P-CALC THRU P-CALC-FIM
            .
       P-VARIABLES-FIM.

       P-CALC.

            DISPLAY
            "CALCULATING..."

            COMPUTE WS-M = (WS-YTWO - WS-YONE) / (WS-XTWO - WS-XONE)

            COMPUTE WS-ESQUERDA = WS-M * (WS-XMIN - WS-XONE) + WS-YONE
            COMPUTE WS-DIREITA  = WS-M * (WS-XMAX - WS-XONE) + WS-YONE
            COMPUTE WS-TOPO = WS-XONE + ((1 / WS-M) * (WS-YMAX -
                                                             WS-YONE))
            COMPUTE WS-BASE = WS-XONE + ((1 / WS-M) * (WS-YMIN -
                                                             WS-YONE))

            PERFORM P-ENCERRA THRU P-ENCERRA-FIM
            .
       P-CALC-FIM.

       P-ENCERRA.

            DISPLAY " "
            DISPLAY " "
            DISPLAY " "
            DISPLAY
            "*********************************************************"
            DISPLAY
            "O VALOR DE M EH:           " WS-M
            DISPLAY
            "O RESULTADO A ESQUERDA EH: " WS-ESQUERDA
            DISPLAY
            "O RESULTADO A DIREITA EH:  " WS-DIREITA
            DISPLAY
            "O RESULTADO DO TOPO EH:    " WS-TOPO
            DISPLAY
            "O RESULTADO DA BASE EH:    " WS-BASE

            IF WS-XONE >= WS-XMIN AND WS-XONE <= WS-XMAX AND
                        WS-YONE >= WS-YMIN AND WS-YONE <= WS-YMAX THEN
               DISPLAY "O ponto 1 esta dentro da janela de visualiza"
               "cao: (" WS-XONE ", " WS-YONE ")"
            ELSE
               DISPLAY "O ponto 1 esta fora da janela de visualizacao"
               ". Projecao no eixo de corte da reta: (" WS-ESQUERDA
               ", " WS-DIREITA ")"
            END-IF

            IF WS-XTWO >= WS-XMIN AND WS-XTWO <= WS-XMAX
                    AND WS-YTWO >= WS-YMIN AND WS-YTWO <= WS-YMAX THEN
               DISPLAY "O ponto 2 esta dentro da janela de visualizaca"
               "o: (" WS-XTWO ", " WS-YTWO ")"
            ELSE
               DISPLAY "O ponto 2 esta fora da janela de visualizacao"
               ". Projecao no eixo de corte da reta: (" WS-TOPO ", "
               WS-BASE ")"
            END-IF

            DISPLAY " "
            DISPLAY " "
            DISPLAY "Voce deseja calcular novamente?"
            DISPLAY "<S> para calcular"
            DISPLAY "<QUALQUER TECLA> para sair"

            ACCEPT WS-STATUS

            EVALUATE WS-STATUS
               WHEN "S"
                   PERFORM P-MAIN THRU P-MAIN-END
               WHEN "s"
                   PERFORM P-MAIN THRU P-MAIN-END
               WHEN OTHER
                   STOP RUN
            END-EVALUATE.

            STOP RUN.
       P-ENCERRA-FIM.

       END PROGRAM cohenSutherland.
