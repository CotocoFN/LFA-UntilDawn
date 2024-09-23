       IDENTIFICATION DIVISION.
       PROGRAM-ID. UntilDawn.
       DATA                         DIVISION.
       WORKING-STORAGE SECTION.
       77 wss-str-texto        pic x(500).
       77 wss-str-acao         pic x(1) occurs 50 times.

       77 wss-str-conjunto     pic x(50).
       77 i pic 9(09) value 0.
       77 j pic 9(09) value 0.

       01 lista-de-estados             pic 9(03).
           88 est-inicio-do-jogo               value 0.
           88 est-com-machete                  value 1.
           88 est-sem-machete                  value 11.
           88 est-armadilha-machete            value 2.
           88 est-assassino                    value 3.
           88 est-assassino-machete            value 31.
           88 est-assassino-ferido             value 32.
           88 est-sobreviveu                   value 777.
           88 est-morreu                       value 666.

       PROCEDURE                    DIVISION.
           display "Conjunto de instrucoes (0,1,2): "with no advancing.
           accept wss-str-conjunto
           display "...".
           perform checa-acao.
           display "----Inicio do Jogo----".
           move 0 to j.
           set est-inicio-do-jogo to true.
           perform main thru fim.

       checa-acao.
           perform varying i from 1 by 1 until i > 50
               add 1 to j
               unstring wss-str-conjunto delimited by ","
                   into wss-str-acao(j)
                   pointer i
               end-unstring
               subtract 1 from i giving i
               if (wss-str-acao(i) equal ".")
                   exit perform
               end-if
           end-perform.
       main.
           add 1 to j.
           if j > 50
               set est-morreu to true
           end-if.
           evaluate true
               when est-inicio-do-jogo
                   go inicio-do-jogo
               when est-com-machete
                   go com-machete
               when est-sem-machete
                   go sem-machete
               when est-armadilha-machete
                   go armadilha-machete
               when est-assassino-machete
                   go assassino-machete
               when est-assassino
                   go assassino
               when est-assassino-ferido
                   go assassino-ferido
               when est-sobreviveu
                   go sobreviveu
               when est-morreu
                   go morreu
           end-evaluate.


       inicio-do-jogo.
           display "---------------------".
           initialize  wss-str-texto.
           string
           "Voce esta em uma mansao abandonada fugindo de um "
           "assassino. e noite e esta chovendo, o que dificulta "
           "ouvir os passos do resisdente do mal."
               x"0a" x"0a"
           "Num dos quartos ao fundo, junto com algumas "
           "ferramentas enferrujadas, voce encontra uma machete "
           "em cima de uma velha mesa de madeira."
               x"0a" "Pegar?"
               x"0a" "1 - Sim. | 0 - Nao."
               into wss-str-texto
           end-string.
           display wss-str-texto.
           evaluate wss-str-acao(j)
               when 1
                   set est-com-machete to true
               when 0
                   set est-sem-machete to true
               when other
                   display
               "Voce se confundiu e esqueceu o que estava fazendo."
                   set est-inicio-do-jogo to true
           end-evaluate.
           go main.

       com-machete.
           display "---------------------".
           initialize  wss-str-texto.
           string
           "Voce pegou a machete pois achou ser o mais seguro."
               x"0a" x"0a"
           "Continuando o caminho por corredores, um som metalico "
           "estrala ao seu lado, dentro de um armario muito escuro "
           "para se ver o que esta dentro... "
            x"0a" "Pegar?"
            x"0a" "1 - Sim. | 0 - Nao."
               into wss-str-texto
           end-string.
           display wss-str-texto.
           evaluate wss-str-acao(j)
               when 1
                   set est-armadilha-machete to true
               when 0
                   display
               "Voce deixou a curiosidade de lado e seguiu seu caminho."
                   set est-assassino-machete to true
               when other
                   display
               "Voce se confundiu e esqueceu o que estava fazendo."
                   set est-com-machete to true
           end-evaluate.
           go main.
       sem-machete.
           display "---------------------".
           initialize  wss-str-texto.
           string
           "Voce decidiu que a machete poderia ser uma armadilha, "
           "ou acabaria piorando a situaçao. Entao decide seguir o "
           "caminho sem ela."
               x"0a" x"0a"
           "Continuando pelos corredores escuros, um som metalico "
           "estrala ao seu lado, dentro de um armario muito escuro "
           "para se ver o que esta dentro..."
                x"0a" "Pegar?"
                x"0a" "1 - Sim. | 0 - Nao."
               into wss-str-texto
           end-string.
           display wss-str-texto.

           evaluate wss-str-acao(j)
               when 1
       initialize wss-str-texto
               string x"0a"
       "Era uma armadilha! Ela prende tres de seus dedos e "
       "parece que a unica opçao restante e deixar a armadilha " x"0a"
       "cortar seus dedos fora. Um contratempo "
       "extremamente doloroso!"
                   into wss-str-texto
               end-string
               display wss-str-texto
                   set est-assassino-ferido to true
               when 0
                   set est-assassino to true
               when other
                   display
               "Voce se confundiu e esqueceu o que estava fazendo."
                   set est-sem-machete to true
           end-evaluate.
           go main.
       armadilha-machete.
           display "---------------------".
           initialize  wss-str-texto.
           string
           "Ao colocar a mao no escuro, uma garra metalica salta em "
           "voce! Se trata de uma armadilha de urso." x"0a" "Por sorte,"
           " seus reflexos conseguiram fazer com que nao fosse seu "
           "braço inteiro que ficasse preso por ela, mas sim, tres "
           "de seus dedos..." x"0a"
           "So lhe resta forçar com a machete para "
           "abrir a armadilha, ou amputar os dedos."
                x"0a" "O que fazer?"
                x"0a" "1 - Forçar. | 0 - Cortar."
               into wss-str-texto
           end-string.
           display wss-str-texto.
           initialize  wss-str-texto.
           evaluate wss-str-acao(j)
               when 0
           string x"0a"
           "Voce pega um pedaço de madeira e o morde com força "
           "Posiciona a machete contra seus dedos e toma coragem... "
           "A dor que sentiu foi indescritivel, mas voce esta solto!"
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-assassino-ferido to true
               when 1
           string x"0a"
               "Voce conseguiu se soltar sem ficar muito ferido, "
               "mas sua machete quebrou!"
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-assassino to true
               when other
           display
           "Nao ha muito tempo para escolher, pense! Mas lembre-se:"
                   set est-armadilha-machete to true
           end-evaluate.
           go main.
       assassino.
           display "---------------------".
           initialize  wss-str-texto.
           string
           "Andando pelos corredores que mais se assemelhavam a um "
           "labirinto... "
           "Voce se encontra com o assasino!" x"0a"
           "A machete agora seria de grande ajuda, mas ficou para tras."
                x"0a" "O que voce escolhe?"
                x"0a" "1 - Atacar. | 0 - Se esconder. | 2 - Correr."
               into wss-str-texto
           end-string.
           display wss-str-texto.
           initialize  wss-str-texto.
           evaluate wss-str-acao(j)
               when 0
           string x"0a"
           "Voce se enconde embaixo de uma grande mesa de jantar "
           "O assasino te procura mas desiste apos nao te encontrar... "
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-sobreviveu to true
               when 1
           string x"0a"
           "Voce ataca o assasino! "
           "Porem, seu golpe nao foi forte o suficiente... "
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-morreu to true
               when 2
           string  x"0a"
               "Voce corre como se nao houvesse o amanha e "
               "despista o assasino... "
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-sobreviveu to true
               when other
                   display "Nao ha muito tempo para escolher, pense!"
                   set est-assassino to true
           end-evaluate.
           go main.
       assassino-machete.
           display "---------------------".
           initialize  wss-str-texto.
           string
           "Andando pelos corredores que mais se assemelhavam "
           "a um labirinto... "
           "Voce se encontra com o assassino, voce pode usar a machete!"
                x"0a" "O que escolhe?"
                x"0a" "1 - Atacar. | 0 - Se esconder. | 2 - Correr."
               into wss-str-texto
           end-string.
           display wss-str-texto.

           initialize  wss-str-texto.
           evaluate wss-str-acao(j)
               when 0
           string  x"0a"
           "Voce se enconde embaixo de uma grande mesa de jantar, "
           "porem a sua machete cai no chao e o assasino te encontra!"
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-morreu to true
               when 1
           string x"0a"
           "Voce ataca o assasino! " x"0a"
           "O ataque deixa o assasino ferido e voce corre para a saida."
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-sobreviveu to true
               when 2
           string  x"0a"
           "Voce corre como se nao houvesse o amanha e despista "
           "o assasino... "
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-sobreviveu to true
               when other
                   display "Nao ha muito tempo para escolher, pense!"
                   set est-assassino-machete to true
           end-evaluate.
           go main.
       assassino-ferido.
           display "---------------------".
           initialize  wss-str-texto.
           string
           "Apos se soltar da armadilha voce segue a busca pela saida."
           " Andando pelos corredores que mais se assemelhavam a um "
           "labirinto..." x"0a"
           "Voce se encontra com o assassino, mas voce esta ferido!"
                x"0a" "O que escolhe?"
                x"0a" "1 - Atacar. | 0 - Se esconder. | 2 - Correr."
               into wss-str-texto
           end-string.
           display wss-str-texto.
           initialize  wss-str-texto.
           evaluate wss-str-acao(j)
               when 0
           string  x"0a"
           "Voce se enconde embaixo de uma grande mesa de jantar. "
           "Porem seu sangue cria um rastro no chao e o assasino "
           "te encontra..."
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-morreu to true
               when 1
           string  x"0a"
           "Mesmo ferido voce ataca o assasino! "
           "O assassino te subjulgou pelo seu ferimento e voce o pega "
           "de surpresa, o ferindo fortmente..."
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-sobreviveu to true
               when 2
           string  x"0a"
           "Voce corre como se nao houvesse o amanha... "
           "Porem, seus ferimentos te deixaram exauto e o assasino te "
           "alcança... "
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-morreu to true
               when other
                   display "Nao ha muito tempo para escolher, pense!"
                   set est-assassino-ferido to true
           end-evaluate.
           go main.
        sobreviveu.
            display "---------------------".
            display x"0a" "Voce conseguiu escapar e sobreviveu! :D".
            go to fim.    
        morreu.    
            display "---------------------".
            display x"0a" "O assasino te pegou e voce morreu! :(".
            go to fim.    
        fim.    
            stop run.    
