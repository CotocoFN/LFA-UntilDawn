       IDENTIFICATION                   DIVISION.
       PROGRAM-ID.                      UntilDawn.
       AUTHOR.                          Bruno&Paulo.


       DATA                         DIVISION.

       WORKING-STORAGE SECTION.
       77 wss-str-texto        pic x(500).
       77 wss-str-acao         pic x(1).

       77 wss-str-conjunto     pic x(50).
       77 i pic 9(09).
       

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

           display "----Início do Jogo----".
           set est-inicio-do-jogo to true.
           perform main thru fim.
           display "----Fim de Jogo----".

       main.
           display wss-str-acao.
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
           initialize wss-str-acao wss-str-texto.
           string 
           "Você está em uma mansão abandonada fugindo de um "
           "assassino. É noite e está chovendo, o que dificulta "
           "ouvir os passos do resisdente do mal."
               x"0a" x"0a"
           "Num dos quartos ao fundo, junto com algumas "
           "ferramentas enferrujadas, você encontra uma machete "
           "em cima de uma velha mesa de madeira."
               x"0a" "Pegar?"
               x"0a" "1 - Sim. | 0 - Não."
               into wss-str-texto
           end-string.
           display wss-str-texto.
           accept wss-str-acao.

           evaluate wss-str-acao
               when 1
                   set est-com-machete to true
               when 0
                   set est-sem-machete to true
               when other
                   display
               "Você se confundiu e esqueceu o que estava fazendo."
                   set est-inicio-do-jogo to true
           end-evaluate.
           go main.
           
       com-machete.
           initialize wss-str-acao wss-str-texto.
           string
           "Você pegou a machete pois achou ser o mais seguro."
               x"0a" x"0a"
           "Continuando o caminho por corredores, um som metálico "
           "estrala ao seu lado, dentro de um armário muito escuro "
           "para se ver o que está dentro... "
            x"0a" "Pegar?"
            x"0a" "1 - Sim. | 0 - Não."
               into wss-str-texto
           end-string.
           display wss-str-texto.
           
           accept wss-str-acao.
           evaluate wss-str-acao
               when 1
                   set est-armadilha-machete to true
               when 0
                   display 
               "Você deixou a curiosidade de lado e seguiu seu caminho."
                   set est-assassino-machete to true
               when other
                   display
               "Você se confundiu e esqueceu o que estava fazendo."
                   set est-com-machete to true
           end-evaluate.
           go main.
       sem-machete.
           initialize wss-str-acao wss-str-texto.
           string
           "Você decidiu que a machete poderia ser uma armadilha, "
           "ou acabaria piorando a situação. Então decide seguir o "
           "caminho sem ela."
               x"0a" x"0a"
           "Continuando pelos corredores escuros, um som metálico "
           "estrala ao seu lado, dentro de um armário muito escuro "
           "para se ver o que está dentro..."
                x"0a" "Pegar?"
                x"0a" "1 - Sim. | 0 - Não."
               into wss-str-texto
           end-string.
           display wss-str-texto.
           
          accept wss-str-acao.
           evaluate wss-str-acao
               when 1
                   set est-assassino-ferido to true
               when 0
                   set est-assassino to true
               when other
                   display
               "Você se confundiu e esqueceu o que estava fazendo."
                   set est-sem-machete to true
           end-evaluate.
           go main.
       armadilha-machete.
           initialize wss-str-acao wss-str-texto.
           string 
           "Ao colocar a mão no escuro, uma garra metálica salta em "
           "você! Se trata de uma armadilha de urso."x"0a" "Por sorte, "
           "seus reflexos conseguiram fazer com que não fosse seu "
           "braço inteiro que ficasse preso por ela, mas sim, três "
           "de seus dedos... Só lhe resta forçar com a machete para "
           "abrir a armadilha, ou amputar os dedos."
                x"0a" "O que fazer?"
                x"0a" "1 - Forçar. | 0 - Cortar."
               into wss-str-texto
           end-string.
           display wss-str-texto.

           initialize wss-str-acao wss-str-texto.
          accept wss-str-acao.
           evaluate wss-str-acao
               when 0
           string 
           "Você pega um pedaço de madeira e o morde com força "
           "Posiciona a machete contra seus dedos e toma coragem... "
           "A dor que sentiu foi indescritivel, mas você esta solto!"
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-assassino-ferido to true 
               when 1
           string
               "Você conseguiu se soltar sem ficar muito ferido, "
               "mas sua machete quebrou!"
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-assassino to true 
               when other
           display 
           "Não há muito tempo para escolher, pense! Mas lembre-se:"
                   set est-armadilha-machete to true
           end-evaluate.
           go main.
       assassino.
           initialize wss-str-acao wss-str-texto.
           string 
           "Andando pelos corredores que mais se assemelhavam a um "
           "labirinto... "
           "Você se encontra com o assasino! A machete agora "
           "seria de grande ajuda, mas ficou para trás."
                x"0a" "O que você escolhe?"
                x"0a" "1 - Atacar. | 0 - Se esconder. | 2 - Correr."
               into wss-str-texto
           end-string.
           display wss-str-texto.


           initialize wss-str-acao wss-str-texto.
          accept wss-str-acao.
           evaluate wss-str-acao
               when 0
           string
           "Você se enconde embaixo de uma grande mesa de jantar "
           "O assasino te procura mas desiste após não te encontrar... "
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-sobreviveu to true
               when 1
           string
           "Você ataca o assasino! " 
           "Porém, seu golpe não foi forte o suficiente... "
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-morreu to true
               when 2
           string 
               "Você corre como se não houvesse o amanhã e "
               "despista o assasino... "
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-sobreviveu to true
               when other
                   display "Não há muito tempo para escolher, pense!"
                   set est-assassino to true
           end-evaluate.
           go main.
       assassino-machete.
           
           initialize wss-str-acao wss-str-texto.
           string 
           "Andando pelos corredores que mais se assemelhavam "
           "a um labirinto... "
           "Você se encontra com o assassino, você pode usar a machete!"
                x"0a" "O que escolhe?"
                x"0a" "1 - Atacar. | 0 - Se esconder. | 2 - Correr."
               into wss-str-texto
           end-string.
           display wss-str-texto.


           initialize wss-str-acao wss-str-texto.
          accept wss-str-acao.
           evaluate wss-str-acao
               when 0
           string 
           "Você se enconde embaixo de uma grande mesa de jantar,"
           "porém a sua machete cai no chão e o assasino te encontra!"
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-morreu to true
               when 1
           string
           "Você ataca o assasino! " x"0a"
           "O ataque deixa o assasino ferido e você corre para a saída."
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-sobreviveu to true
               when 2
           string 
           "Você corre como se não houvesse o amanhã e despista "
           "o assasino... "
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-sobreviveu to true
               when other
                   display "Não há muito tempo para escolher, pense!"
                   set est-assassino-machete to true
           end-evaluate.
           go main.
       assassino-ferido.
           
           initialize wss-str-acao wss-str-texto.
           string 
           "Após se soltar da armadilha você segue a busca pela saida."
           x"0a"
           "Andando pelos corredores que mais se assemelhavam a um "
           "labirinto..." x"0a"
           "Você se encontra com o assassino, mas voce está ferido!"
                x"0a" "O que escolhe?"
                x"0a" "1 - Atacar. | 0 - Se esconder. | 2 - Correr."
               into wss-str-texto
           end-string.
           display wss-str-texto.

           initialize wss-str-acao wss-str-texto.
          accept wss-str-acao.
           evaluate wss-str-acao
               when 0
           string 
           "Você se enconde embaixo de uma grande mesa de jantar "
           "Porém seu sangue cria um rastro no chão e o assasino "
           "te encontra..."
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-morreu to true
               when 1
           string 
           "Mesmo ferido você ataca o assasino! "
           "O assassino te subjulgou pelo seu ferimento e você o pega "
           "de surpresa, o ferindo fortmente..."
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-sobreviveu to true
               when 2
           string 
           "Você corre como se não houvesse o amanhã "
           "Porém, seus ferimentos te deixaram exauto e o assasino te "
           "alcança... "
               into wss-str-texto
           end-string
           display wss-str-texto
                   set est-morreu to true
               when other
                   display
               "Não há muito tempo para escolher, pense!"
                   set est-assassino-ferido to true
           end-evaluate.
           go main.
       sobreviveu.
           display x"0a" "Você conseguiu escapar e sobreviveu! :D".
           go fim.
       morreu.
           display x"0a" "O assasino te pegou e você morreu! :(".
           go fim.

       fim.
           stop run.