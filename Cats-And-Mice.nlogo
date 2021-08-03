breed[cats cat]
breed[mice mouse]
globals [a b c d w z son-mice son-cats] ; var's para verificar quantos gatos e ratos nasceram
turtles-own[energy poison]  ; Criação de energia e veneno
mice-own[cat-found found]   ; Var para verificar se rato avistou um gato

to setup  ;Prepara os setup's necessários para iniciar a simulação
  clear-all
  ca

  setup-patches
  setup-cheese ;coloca queijo
  setup-traps ;coloca armadilhas
  setup-remedy ;coloca remédio
  setup-agents

  display-labels

  reset-ticks
end

to setup-patches  ;Prepara as patches para a simulação
  ask patches[
    let x 28
    let y 48
    if pycor mod 2 = 0
    [set x 48 set y 28]
    ifelse pxcor mod 2 = 0
    [set pcolor x]
    [set pcolor y]
  ]
end

to setup-agents  ;Prepara os agentes para a simulação

  create-mice N-mice
  [
    set shape "mouse side"
    set color 4
    let x one-of patches with [pcolor != yellow AND pcolor != red AND pcolor != black]
    move-to x

    set cat-found false
    set found one-of ["friendly" "Renegade"] ; Coloca metade dos gatos como amigáveis e outra metade como renegados

  ]

  create-cats N-cats
  [
    set shape "cat"
    set color black
    let x one-of patches with [not any? mice-here and not any? mice-on neighbors and not any? cats-here AND pcolor != yellow AND pcolor != red AND pcolor != black]
    setxy [pxcor] of x [pycor] of x

    set heading one-of [0 90 180 270]
  ]

   ask turtles
  [
    set energy StartEnergy?
    set poison false ;colocar a variavel de veneno a falso
  ]

end

to setup-cheese  ;;Coloca queijo na interface

  ;Colocar queijo aleatoriamente e certificando que não ficam queijos juntos uns dos outros
  ask patches with [pcolor != yellow AND pcolor != red AND [pcolor] of neighbors != black and [pcolor] of neighbors != yellow ]
  [

    if random 100 < percent-cheese
    [
      ifelse poisoned-cheese = true
      [      ;; percentagem de veneno escolhida pelo utilizador
        ifelse random 100 < percent-poison
        [set pcolor black] ;  Caso tenha veneno
        [set pcolor yellow] ; Caso nao tenha

      ]
      [set pcolor yellow] ; Coloca queijos
    ]
  ]
end

to setup-remedy  ;Coloca remedio na interface
  ; colocar cura e impede sobreposição
  ask patches with [pcolor != green AND pcolor != red and [pcolor] of neighbors != black and [pcolor] of neighbors != green]
  [
    if random 100 < percent-remedy
    [set pcolor green]
  ]

end

to setup-traps  ;Coloca armadilhas na interface
  ; Colocar as armadilhas não em cima de outras armadilhas e evitar colocar em cima dos queijos também
  ask patches with [pcolor != red AND pcolor != yellow  AND pcolor != black and [pcolor] of neighbors != red]
  [
    if percent-traps > random 100
    [set pcolor red]
  ]
end

to go           ;Ações para cada tick.

  move-mouse-tarefa1
  move-cat-tarefa1
  move-mice-tarefa2
  move-cat-tarefa2

  if experience = "Final-Battle" [ move-final-fight ]
  if experience = "Smart-Mouse" or experience = "Smart-Cat" or experience = "Final-Battle" [lunch-time]

  if reproduce? [
    reproduction
    ]

  display-labels
  tick

  if count mice = 0  [ user-message "The Cats have inherited the earth" stop ]
  if count cats = 0  [ user-message "The Rats have inherited the earth" stop ]
  if count mice > 180 [ user-message "The Rats have inherited the earth" stop] ; A partir de 180 ou mais, os ratos ganham sempre, logo, foi criada esta condição de paragem
end

to move-mouse-tarefa1  ;Movimentos dos ratos para a tarefa 1

  ask mice[

   ;Experiencia default/original
    if experience = "Original"
    [
      let x one-of neighbors
      move-to x
    ]
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAREFA 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;Original-Upgraded, RATO MELHORADO, move para o lado oposto do rato
   if experience = "Original-Upgraded"
      [
        ;Verificar se há algum gato na vizinhança e fugir para o lado oposto

        ;Começa por verificar se tem algum gato a frente e foge para trás
        ifelse (is-patch? patch-left-and-ahead 45 1 AND any? cats-on patch-left-and-ahead 45 1) OR (is-patch? patch-ahead 1 AND any? cats-on patch-ahead 1) OR (is-patch? patch-right-and-ahead 45 1 AND any? cats-on patch-right-and-ahead 45 1)
        [ifelse is-patch? patch-ahead (-1)
            [fd (-1)]
            [move-to one-of neighbors with [not any? cats-here]]]

        [ ;Verificar se tem algum gato atrás e foge para a frente
          ifelse (is-patch? patch-left-and-ahead 135 1 AND any? cats-on patch-left-and-ahead 135 1) OR (is-patch? patch-ahead (-1) AND any? cats-on patch-ahead (-1)) OR (is-patch? patch-right-and-ahead 135 1 AND any? cats-on patch-right-and-ahead 135 1)
          [ifelse is-patch? patch-ahead 1
              [fd 1]
              [move-to one-of neighbors with [not any? cats-here]]]

        [ ;verificar se tem algum gato a a esquerda e foge para a direita
            ifelse is-patch? patch-left-and-ahead 90 1 AND any? cats-on patch-left-and-ahead 90 1
            [ifelse is-patch? patch-right-and-ahead 90 1 [move-to patch-right-and-ahead 90 1] [move-to one-of neighbors with [not any? cats-here]]]

        [ ;verificar se tem algum gato a a direita  e foge para a esquerda
            ifelse is-patch? patch-right-and-ahead 90 1 AND any? cats-on patch-right-and-ahead 90 1
            [ifelse is-patch? patch-left-and-ahead 90 1 [move-to patch-left-and-ahead 90 1] [move-to one-of neighbors with [not any? cats-here]]]
              [move-to one-of neighbors];Caso não tenha algum rato perto, faz o movimento original
     ]]]
   ]

    if experience = "Original-Upgraded-Cat" ;Codigo original-upgraded para testar nova capacidade do gato
      [
        let x one-of neighbors
        move-to x
      ]

    if experience = "Original-Upgraded-Mouse" ;Codigo original-upgraded para testar nova capacidade do rato
     [
       ;Verificar se há algum gato na vizinhança e fugir para o lado oposto

        ;Começa por verificar se tem algum gato a frente e foge para trás
        ifelse (is-patch? patch-left-and-ahead 45 1 AND any? cats-on patch-left-and-ahead 45 1) OR (is-patch? patch-ahead 1 AND any? cats-on patch-ahead 1) OR (is-patch? patch-right-and-ahead 45 1 AND any? cats-on patch-right-and-ahead 45 1)
        [ifelse is-patch? patch-ahead (-1)
            [fd (-1)]
            [move-to one-of neighbors with [not any? cats-here]]]

        [ ;Verificar se tem algum gato atrás e foge para a frente
          ifelse (is-patch? patch-left-and-ahead 135 1 AND any? cats-on patch-left-and-ahead 135 1) OR (is-patch? patch-ahead (-1) AND any? cats-on patch-ahead (-1)) OR (is-patch? patch-right-and-ahead 135 1 AND any? cats-on patch-right-and-ahead 135 1)
          [ifelse is-patch? patch-ahead 1
              [fd 1]
              [move-to one-of neighbors with [not any? cats-here]]]

        [ ;verificar se tem algum gato a a esquerda e foge para a direita
            ifelse is-patch? patch-left-and-ahead 90 1 AND any? cats-on patch-left-and-ahead 90 1
            [ifelse is-patch? patch-right-and-ahead 90 1 [move-to patch-right-and-ahead 90 1] [move-to one-of neighbors with [not any? cats-here]]]

        [ ;verificar se tem algum gato a a direita  e foge para a esquerda
            ifelse is-patch? patch-right-and-ahead 90 1 AND any? cats-on patch-right-and-ahead 90 1
            [ifelse is-patch? patch-left-and-ahead 90 1 [move-to patch-left-and-ahead 90 1] [move-to one-of neighbors with [not any? cats-here]]]
              [move-to one-of neighbors];Caso não tenha algum rato perto, faz o movimento original
     ]]]
     ]
]
end

to  move-cat-tarefa1  ;Movimentos dos gatos para a tarefa 1

    ask cats[
    if experience = "Original"
    [
    if  patch-ahead 1 != nobody [set a patch-ahead 1]
        if patch-ahead 2 != nobody [set b patch-ahead 2]
        if patch-right-and-ahead 90 1 != nobody [set c patch-right-and-ahead 90 1]
        if patch-right-and-ahead -90 1 != nobody [set d patch-right-and-ahead -90 1]
        if patch-right-and-ahead 45 1 != nobody [set w patch-right-and-ahead 45 1]
        if patch-right-and-ahead -45 1 != nobody [set z patch-right-and-ahead -45 1]
          let y (patch-set a b c d w z)
          let x one-of y
            move-to x
            if random 100 < 25 [set heading one-of [0 90 180 270]]
    ]

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAREFA 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     if experience = "Original-Upgraded"
    [
        ;Devido ao gato ver 2 casas para a frente e apenas saltar para o seu redor, foi acrescentado a chance de 75% de ele poder saltar ou não para a 2 casa a sua frente, já que tem essa visão
        ;Caso ele vir o rato a frente vai saltar para ele, 75% das vezes, tendo 25% das vezes chance de perder visão e saltar para um dos outros sitios
        ifelse is-patch? patch-ahead 2 AND any? mice-on patch-ahead 2
        [;;existe algum rato na patch-ahead 2
          ifelse random 100 < 75
          [;Salta para a frente fazendo comer o rato logo a frente, aumentando assim sucesso na caça, dando assim a chance que não tinha de comer o rato 2 casas a frente
            fd 1
          ]
           [if  patch-ahead 1 != nobody [set a patch-ahead 1]                          ; caso contrário faz o seu movimento normal
            if patch-ahead 2 != nobody [set b patch-ahead 2]
            if patch-right-and-ahead 90 1 != nobody [set c patch-right-and-ahead 90 1]
            if patch-right-and-ahead -90 1 != nobody [set d patch-right-and-ahead -90 1]
            if patch-right-and-ahead 45 1 != nobody [set w patch-right-and-ahead 45 1]
            if patch-right-and-ahead -45 1 != nobody [set z patch-right-and-ahead -45 1]
              let y (patch-set a b c d w z)
              let x one-of y
              move-to x]
        ][;Caso contrário faz o movimento "Original" dele
          if  patch-ahead 1 != nobody [set a patch-ahead 1]
          if patch-ahead 2 != nobody [set b patch-ahead 2]
          if patch-right-and-ahead 90 1 != nobody [set c patch-right-and-ahead 90 1]
          if patch-right-and-ahead -90 1 != nobody [set d patch-right-and-ahead -90 1]
          if patch-right-and-ahead 45 1 != nobody [set w patch-right-and-ahead 45 1]
          if patch-right-and-ahead -45 1 != nobody [set z patch-right-and-ahead -45 1]
            let y (patch-set a b c d w z)
            let x one-of y
              move-to x
              if random 100 < 25 [set heading one-of [0 90 180 270]]
        ]
    ]

    if experience = "Original-Upgraded-Cat"  ;Codigo original-upgraded para testar nova capacidade do gato
    [
        ;Devido ao gato ver 2 casas para a frente e apenas saltar para o seu redor, foi acrescentado a chance de 75% de ele poder saltar ou não para a 2 casa a sua frente, já que tem essa visão
        ;Caso ele vir o rato a frente vai saltar para ele, 75% das vezes, tendo 25% das vezes chance de perder visão e saltar para um dos outros sitios
        ifelse is-patch? patch-ahead 2 AND any? mice-on patch-ahead 2
        [;;existe algum rato na patch-ahead 2
          ifelse random 100 < 75
          [;Salta para a frente fazendo comer o rato logo a frente, aumentando assim sucesso na caça, dando assim a chance que não tinha de comer o rato 2 casas a frente
            fd 1
          ]
           [if  patch-ahead 1 != nobody [set a patch-ahead 1]                          ; caso contrário faz o seu movimento normal
            if patch-ahead 2 != nobody [set b patch-ahead 2]
            if patch-right-and-ahead 90 1 != nobody [set c patch-right-and-ahead 90 1]
            if patch-right-and-ahead -90 1 != nobody [set d patch-right-and-ahead -90 1]
            if patch-right-and-ahead 45 1 != nobody [set w patch-right-and-ahead 45 1]
            if patch-right-and-ahead -45 1 != nobody [set z patch-right-and-ahead -45 1]
              let y (patch-set a b c d w z)
              let x one-of y
              move-to x]
        ][;Caso contrário faz o movimento "Original" dele
          if  patch-ahead 1 != nobody [set a patch-ahead 1]
          if patch-ahead 2 != nobody [set b patch-ahead 2]
          if patch-right-and-ahead 90 1 != nobody [set c patch-right-and-ahead 90 1]
          if patch-right-and-ahead -90 1 != nobody [set d patch-right-and-ahead -90 1]
          if patch-right-and-ahead 45 1 != nobody [set w patch-right-and-ahead 45 1]
          if patch-right-and-ahead -45 1 != nobody [set z patch-right-and-ahead -45 1]
            let y (patch-set a b c d w z)
            let x one-of y
              move-to x
              if random 100 < 25 [set heading one-of [0 90 180 270]]
        ]
    ]
   if experience = "Original-Upgraded-Mouse"  ;Codigo original-upgraded para testar nova capacidade do rato
   [
     if  patch-ahead 1 != nobody [set a patch-ahead 1]
        if patch-ahead 2 != nobody [set b patch-ahead 2]
        if patch-right-and-ahead 90 1 != nobody [set c patch-right-and-ahead 90 1]
        if patch-right-and-ahead -90 1 != nobody [set d patch-right-and-ahead -90 1]
        if patch-right-and-ahead 45 1 != nobody [set w patch-right-and-ahead 45 1]
        if patch-right-and-ahead -45 1 != nobody [set z patch-right-and-ahead -45 1]
          let y (patch-set a b c d w z)
          let x one-of y
            move-to x
            if random 100 < 25 [set heading one-of [0 90 180 270]]
   ]
  ]
end

to move-mice-tarefa2  ;Movimentos dos ratos para a tarefa 2
  ask mice[
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAREFA 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;Experiência rato-esperto-> Maior visão frontal

    if experience = "Smart-Mouse"
      [

         ;Verifica se a variável inicial está a true ou false, indicando se tem algum rato nos arredores,
         ;Caso tenha um rato nos arredores, move-se para onde deixe de ter algum gato nos arredores.
         if cat-found = true [move-to one-of neighbors with [not any? mice-here]]
         ;Verifica se tem algum gato na vizinhança e avisa que tem gato perto
         if any? cats-on neighbors [if any? mice-on neighbors [ask mice-on neighbors [set cat-found true]]] ;se algum gato na vizinhaça -> ve se ha algum rato na vizinhança -> comunica ao outro rato -> ativando o alerta

         ;Tem visão reforçada frontal sobre os gatos, caso encontre algum gato a frente, move-se para trás
          ifelse (is-patch? patch-left-and-ahead 45 1 AND any? cats-on patch-left-and-ahead 45 1) OR
                 (is-patch? patch-left-and-ahead 30 2 AND any? cats-on patch-left-and-ahead 30 2) OR
                 (is-patch? patch-ahead 1 AND any? cats-on patch-ahead 1) OR
                 (is-patch? patch-ahead 2 AND any? cats-on patch-ahead 2) OR
                 (is-patch? patch-right-and-ahead 45 1 AND any? cats-on patch-right-and-ahead 45 1) OR
                 (is-patch? patch-right-and-ahead 30 2 AND any? cats-on patch-right-and-ahead 45 2)
          [ifelse is-patch? patch-ahead (-2) [fd (-2)]
          [move-to one-of neighbors with [not any? cats-here]]
      ]
      [ ; Verifica se tem algum rato na vizinhança, para se mover aleatoriamente aumentando a chance de fugir do rato
          ifelse any? mice-on neighbors
           [
           ;Se o rato estiver perto de um gato, tentar comunicar a qualquer rato nas proximidades da existencia de um gato para ele fugir
              let check mice-on neighbors

              ifelse found = "friendly" AND [found] of one-of check = "friendly" AND random 100 < 10; Caso o rato seja amigável e o vizinho também, existe chance de 10% do rato seguir o vizinho, esta chance faz com que os ratos não se sigam permanentemente
              [
                let y one-of check with [found = "friendly"]       ;;para nao violar a regra de os ratos nao terem direçao definida, guardar a heading original
                                                                                                                          ;;;;;;;;;;let x [heading] of self
                face y ; fica virado para o rato friendly
                fd 1 ; e anda uma casa na direção dele(friendly)
                set heading [heading] of self ; volta a ficar virado para onde estava, para não quebrar regras da tarefa 1

                move-to one-of neighbors with [not any? mice-here] ;de seguida o rato mexe-se para uma das vizinhanças para não ficar sobreposto em alguns casos
                ][    ;Caso seja rato solitário move-se aleatóriamente
                move-to one-of neighbors]
                ][let x one-of neighbors
                  move-to x ]
       ]
      check-food
      set energy energy - 1
  ]

        ;Experiência do gato Inteligente, ratos movem-se como o original, mas alimentam-se e têm energia
        if experience = "Smart-Cat"
        [
           let x one-of neighbors
           move-to x
           check-food
           set energy energy - 1
        ]
  check-vitals

    if cat-found = true [set cat-found false]    ; inicio de cada iteraçao da reset ao valor colocando a false
  ]

end

to move-cat-tarefa2  ;Movimentos dos gatos para a tarefa 2
  ask cats[
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAREFA 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    if experience = "Smart-Mouse"      ;Gato original, rato inteligente
    [
        if  patch-ahead 1 != nobody [set a patch-ahead 1]
        if patch-ahead 2 != nobody [set b patch-ahead 2]
        if patch-right-and-ahead 90 1 != nobody [set c patch-right-and-ahead 90 1]
        if patch-right-and-ahead -90 1 != nobody [set d patch-right-and-ahead -90 1]
        if patch-right-and-ahead 45 1 != nobody [set w patch-right-and-ahead 45 1]
        if patch-right-and-ahead -45 1 != nobody [set z patch-right-and-ahead -45 1]
          let y (patch-set a b c d w z)
          let x one-of y
            move-to x
            if random 100 < 25 [set heading one-of [0 90 180 270]]

     set energy energy - 2
      check-vitals
      if [pcolor] of patch-here = red [die]
    ]


    if experience = "Smart-Cat"[  ; gatos com visão bastante melhorada frontal

      ifelse front-way  2 = 1 [fd 1]  ; vê 2 casas a frente
      [
          ifelse front-way  3 = 1 [fd 2 set energy energy - 2]  ;vê 3 casas a frente
          [rt 45

           ifelse front-way  2 = 1 [fd 1]      ;vê 2 a frente casas na diagonal direita
            [
              ifelse front-way  3 = 1 [fd 2  set energy energy - 2]   ;vê 3 a frente casas na diagonal direita
                [rt 45
                  ifelse front-way  2 = 1 [fd 1]                       ;vê 2 casas a direita
                    [
                      ifelse front-way  3 = 1 [fd 2 set energy energy - 2]  ;vê 3 casas a direita
                    [lt 135
                    ifelse front-way  2 = 1 [fd 1]                             ;vê 2 a frente casas na diagonal esq.
                    [
                      ifelse front-way  3 = 1 [fd 2 set energy energy - 2]     ;vê 3 a frente casas na diagonal esq.
                      [lt 45
                          ifelse front-way  2 = 1 [fd 1]                             ;vê 2 casas a esq.
                         [
                          ifelse front-way  3 = 1 [fd 2 set energy energy - 2]      ;vê 3 casas a esq.

                        [rt 90
                            ifelse random 101 < 50
                                [rt 90 move-to one-of neighbors ]
                            [lt 90 move-to one-of neighbors ]
     ]]]]]]]]]]
     check-vitals
     set energy energy - 5
    ]]
end

to move-final-fight ;Versão final com os ratos e gatos inteligentes juntos, junção do código do smart-mouse + smart-cat
  ask mice [

         ;Verifica se a variável inicial está a true ou false, indicando se tem algum rato nos arredores,
         ;Caso tenha um rato nos arredores, move-se para onde deixe de ter algum gato nos arredores.
         if cat-found = true [move-to one-of neighbors with [not any? mice-here]]
         ;Verifica se tem algum gato na vizinhança e avisa que tem gato perto
         if any? cats-on neighbors [if any? mice-on neighbors [ask mice-on neighbors [set cat-found true]]] ;se algum gato na vizinhaça -> ve se ha algum rato na vizinhança -> comunica ao outro rato -> ativando o alerta

         ;Tem visão reforçada frontal sobre os gatos, caso encontre algum gato a frente, move-se para trás
          ifelse (is-patch? patch-left-and-ahead 45 1 AND any? cats-on patch-left-and-ahead 45 1) OR
                 (is-patch? patch-left-and-ahead 30 2 AND any? cats-on patch-left-and-ahead 30 2) OR
                 (is-patch? patch-ahead 1 AND any? cats-on patch-ahead 1) OR
                 (is-patch? patch-ahead 2 AND any? cats-on patch-ahead 2) OR
                 (is-patch? patch-right-and-ahead 45 1 AND any? cats-on patch-right-and-ahead 45 1) OR
                 (is-patch? patch-right-and-ahead 30 2 AND any? cats-on patch-right-and-ahead 45 2)
          [ifelse is-patch? patch-ahead (-2) [fd (-2)]
          [move-to one-of neighbors with [not any? cats-here]]
      ]
      [ ; Verifica se tem algum rato na vizinhança, para se mover aleatoriamente aumentando a chance de fugir do rato
          ifelse any? mice-on neighbors
           [
           ;Se o rato estiver perto de um gato, tentar comunicar a qualquer rato nas proximidades da existencia de um gato para ele fugir
              let check mice-on neighbors

              ifelse found = "friendly" AND [found] of one-of check = "friendly" AND random 100 < 10; Caso o rato seja amigável e o vizinho também, existe chance de 80% do rato seguir o vizinho, esta chance faz com que os ratos não se sigam permanentemente
              [
                let y one-of check with [found = "friendly"]       ;;para nao violar a regra de os ratos nao terem direçao definida, guardar a heading original
                                                                                                                          ;;;;;;;;;;let x [heading] of self
                face y ; fica virado para o rato friendly
                fd 1 ; e anda uma casa na direção dele(friendly)
                set heading [heading] of self ; volta a ficar virado para onde estava, para não quebrar regras da tarefa 1

                move-to one-of neighbors with [not any? mice-here] ;de seguida o rato mexe-se para uma das vizinhanças para não ficar sobreposto em alguns casos
                ][    ;Caso seja rato solitário move-se aleatóriamente
                move-to one-of neighbors]
                ][let x one-of neighbors
                  move-to x ]
       ]
      check-food
      set energy energy - 1
  ]

  ask cats [
    ifelse front-way  2 = 1 [fd 1]  ; vê 2 casas a frente
      [
          ifelse front-way  3 = 1 [fd 2 set energy energy - 2]  ;vê 3 casas a frente
          [rt 45

           ifelse front-way  2 = 1 [fd 1]      ;vê 2 a frente casas na diagonal direita
            [
              ifelse front-way  3 = 1 [fd 2  set energy energy - 2]   ;vê 3 a frente casas na diagonal direita
                [rt 45
                  ifelse front-way  2 = 1 [fd 1]                       ;vê 2 casas a direita
                    [
                      ifelse front-way  3 = 1 [fd 2 set energy energy - 2]  ;vê 3 casas a direita
                    [lt 135
                    ifelse front-way  2 = 1 [fd 1]                             ;vê 2 a frente casas na diagonal esq.
                    [
                      ifelse front-way  3 = 1 [fd 2 set energy energy - 2]     ;vê 3 a frente casas na diagonal esq.
                      [lt 45
                          ifelse front-way  2 = 1 [fd 1]                             ;vê 2 casas a esq.
                         [
                          ifelse front-way  3 = 1 [fd 2 set energy energy - 2]      ;vê 3 casas a esq.

                        [rt 90
                            ifelse random 101 < 50
                                [rt 90 move-to one-of neighbors ]
                            [lt 90 move-to one-of neighbors ]
     ]]]]]]]]]]
     check-vitals
     set energy energy - 5

  ]
end

to lunch-time       ;se tiver algum gato na vizinhaça rato morre e gato ganha energia
  ask mice[

    if any? cats-on neighbors [
      ifelse poison = true[            ;Caso o rato esteja infetado, o gato provavel de o ter comido fica também infetado
            ask one-of cats-on neighbors [
            set poison true
            ifelse 15 + energy <= capMAX
            [set energy energy + 15]
            [set energy capMAX]
      ]]
      [;else rato nao infetado, logo não infeta gato
          ask one-of cats-on neighbors [
          ifelse 15 + energy <= capMAX
          [set energy energy + 15]
          [set energy capMAX]

      ]]
      die]
      if energy > capMAX[set energy capMAX]    ;Caso energia ultrapasse o máximo, retorna de novo ao máximo
      ]
end

to-report front-way [distancy]  ;Verifica se existem ratos nos arredores

        if(is-patch? patch-ahead distancy)  ;verifica se é patch possível para se mover
        [

        if distancy = 2 [  fd 2
          ifelse any? mice-on neighbors[fd -2 report 1][fd -2 report 0]]  ;Após se mover 2 patches para a frente, verifica se tem ratos ao seu redor, caso tenha reporta 1, caso contrário reporta 0

        if distancy = 1 [  fd 1
          ifelse any? mice-on neighbors[fd -1 report 1][fd -1 report 0]   ;Após se mover 1 patches para a frente, verifica se tem ratos ao seu redor, caso tenha reporta 1, caso contrário reporta 0
        ]

        ]
        report 0 ;Caso o if não se verifique

end

to reproduction ;Reprodução dos animais

  ask mice[  ;reprodução dos ratos
   let energyMICE energy
    if  any? mice-on neighbors [  ; percentagem de reprodução definida pelo utilizador
      ask mice-on neighbors[
      if (energyMICE + energy > 75) [
            if random 101 < reproductionMices [
              set energy round(energyMICE + energy / 2)
              hatch 1 [jump 5]               ; cria um rato e coloca 5 saltos a frente
              set son-mice son-mice + 1      ; contagem
            ]
            ]]]]
  ask cats[  ;reproduçao dos gatos
     let energyCat energy
      if  any? cats-on neighbors [  ;percentagem de reprodução definida pelo utilizador
        ask cats-on neighbors[
         if (energyCat + energy > 75) [
            if random 101 < reproductionCats [
              set energy round(energyCAT + energy / 2)
              hatch 1 [jump 5]                ; cria gato e coloca 5 saltos a frente
              set son-cats son-cats + 1      ; contagem
            ]]]]
     ]
end

to check-food ;Verifica se a patch tem comida, se tem veneno, se é remédio..
  if [pcolor] of patch-here = yellow
    [

     ifelse 10 + energy <= capMAX[set energy energy + 10] ; ganha +10 ao comer o queijo
      [set energy capMAX]

      reset-color               ; Patch volta a cor original
    ]
  if [pcolor] of patch-here = black
    [
      set color blue
       ifelse 5 + energy <= capMAX[set energy energy + 5] ; ganha +5 ao comer o queijo comveneno
      [set energy capMAX]
      set poison true        ; fica envenenado o rato/gato
     ;reset-color            ; Patch volta a cor original
    ]
 if [pcolor] of patch-here = green
    [
      ifelse 15 + energy <= capMAX[set energy energy + 15] ; ganha +15 ao comer medicamento
      [set energy capMAX]
      reset-color                ; Patch volta a cor original
      set poison false
      set color 4                ; uma vez recuperado fica com a cor normal
 ]

 if poison = true[ set energy energy - 10 ]
end

to check-vitals  ;Verifica se gato tem energia para continuar
   if [pcolor] of patch-here = red AND experience != "Smart-Mouse" [die]
   if [pcolor] of patch-here = black[set energy (energy / 2)] ;caso tenha veneno perde metade da vida
   if energy <= 0 [die]

   if energy > capMAX[ set energy capMAX]  ;certifica que energia não passa por uma máximo predef.

   if poison = true[ set energy energy - 10 ] ;Caso tenha veneno perde mais vida que o normal
end

to reset-color      ;funcao para meter a cor de fundo como estava inicialmente
  ask patch-here
  [
    let x 28
    let y 48
    if pycor mod 2 = 0
    [set x 48 set y 28]
    ifelse pxcor mod 2 = 0
    [set pcolor x]
    [set pcolor y]
  ]

end

to display-labels   ;Mostra energia de cada animal na interface
  ask turtles [
      set label ""
        if show-energy? [ set label energy ] ; se botao esta on, mostra labels das turtles
         ]
end
@#$#@#$#@
GRAPHICS-WINDOW
449
10
1251
813
-1
-1
24.061
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

SLIDER
23
63
195
96
N-mice
N-mice
0
75
75.0
1
1
NIL
HORIZONTAL

SLIDER
23
101
195
134
N-cats
N-cats
0
25
11.0
1
1
NIL
HORIZONTAL

BUTTON
24
24
99
57
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
120
24
193
57
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
22
188
97
233
NIL
ticks
17
1
11

SLIDER
229
64
422
97
percent-cheese
percent-cheese
0
5
0.0
1
1
% Queijo
HORIZONTAL

SWITCH
27
519
198
552
poisoned-cheese
poisoned-cheese
1
1
-1000

SLIDER
229
105
421
138
percent-traps
percent-traps
0
5
0.0
1
1
% Armadilhas
HORIZONTAL

SLIDER
229
147
422
180
percent-poison
percent-poison
0
30
0.0
1
1
% Veneno
HORIZONTAL

MONITOR
136
186
215
231
Cheeses 
count patches with [pcolor = yellow]
17
1
11

TEXTBOX
242
19
392
49
Percentagens
25
0.0
1

MONITOR
135
287
215
332
Traps
count patches with [pcolor = red]
17
1
11

MONITOR
21
289
114
334
Poisoned Cheese
count patches with [pcolor = black]
17
1
11

MONITOR
136
237
215
282
Cats Alive
count cats
17
1
11

MONITOR
22
238
97
283
Rats Alive
Count mice
17
1
11

CHOOSER
16
433
205
478
experience
experience
"Original" "Original-Upgraded" "Original-Upgraded-Mouse" "Original-Upgraded-Cat" "Smart-Mouse" "Smart-Cat" "Final-Battle"
4

SLIDER
227
310
444
343
reproductionCats
reproductionCats
0
5
0.0
1
1
%Repro. Gatos
HORIZONTAL

SLIDER
227
349
444
382
reproductionMices
reproductionMices
0
15
15.0
1
1
%Repro. Ratos
HORIZONTAL

MONITOR
20
340
97
385
Mice hatched
son-mice
17
1
11

SWITCH
227
269
436
302
reproduce?
reproduce?
0
1
-1000

MONITOR
136
339
215
384
NIL
son-cats
17
1
11

TEXTBOX
31
149
193
176
   Contadores
22
0.0
0

SWITCH
228
424
436
457
show-energy?
show-energy?
0
1
-1000

SLIDER
228
464
435
497
StartEnergy?
StartEnergy?
0
75
75.0
1
1
Energia Inicial
HORIZONTAL

SLIDER
229
187
422
220
percent-remedy
percent-remedy
0
2
0.0
0.5
1
% Remédio
HORIZONTAL

SLIDER
228
506
435
539
capMAX
capMAX
75
125
125.0
1
1
Energia Maxima
HORIZONTAL

TEXTBOX
247
223
397
254
Reprodução
25
0.0
1

TEXTBOX
282
389
432
418
Energia
24
0.0
1

TEXTBOX
26
397
176
426
Experiencias
24
0.0
1

TEXTBOX
0
477
226
535
Queijo Envenenado ?
24
0.0
1

PLOT
4
559
442
812
Contador de Gatos e Ratos
NIL
Número de Ratos/Gatos
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Nº Gatos" 1.0 0 -2674135 true "" "plot count cats"
"Nº Ratos" 1.0 0 -13345367 true "" "plot count mice"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

cat
false
0
Line -7500403 true 285 240 210 240
Line -7500403 true 195 300 165 255
Line -7500403 true 15 240 90 240
Line -7500403 true 285 285 195 240
Line -7500403 true 105 300 135 255
Line -16777216 false 150 270 150 285
Line -16777216 false 15 75 15 120
Polygon -7500403 true true 300 15 285 30 255 30 225 75 195 60 255 15
Polygon -7500403 true true 285 135 210 135 180 150 180 45 285 90
Polygon -7500403 true true 120 45 120 210 180 210 180 45
Polygon -7500403 true true 180 195 165 300 240 285 255 225 285 195
Polygon -7500403 true true 180 225 195 285 165 300 150 300 150 255 165 225
Polygon -7500403 true true 195 195 195 165 225 150 255 135 285 135 285 195
Polygon -7500403 true true 15 135 90 135 120 150 120 45 15 90
Polygon -7500403 true true 120 195 135 300 60 285 45 225 15 195
Polygon -7500403 true true 120 225 105 285 135 300 150 300 150 255 135 225
Polygon -7500403 true true 105 195 105 165 75 150 45 135 15 135 15 195
Polygon -7500403 true true 285 120 270 90 285 15 300 15
Line -7500403 true 15 285 105 240
Polygon -7500403 true true 15 120 30 90 15 15 0 15
Polygon -7500403 true true 0 15 15 30 45 30 75 75 105 60 45 15
Line -16777216 false 164 262 209 262
Line -16777216 false 223 231 208 261
Line -16777216 false 136 262 91 262
Line -16777216 false 77 231 92 261

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

mouse side
false
0
Polygon -7500403 true true 38 162 24 165 19 174 22 192 47 213 90 225 135 230 161 240 178 262 150 246 117 238 73 232 36 220 11 196 7 171 15 153 37 146 46 145
Polygon -7500403 true true 289 142 271 165 237 164 217 185 235 192 254 192 259 199 245 200 248 203 226 199 200 194 155 195 122 185 84 187 91 195 82 192 83 201 72 190 67 199 62 185 46 183 36 165 40 134 57 115 74 106 60 109 90 97 112 94 92 93 130 86 154 88 134 81 183 90 197 94 183 86 212 95 211 88 224 83 235 88 248 97 246 90 257 107 255 97 270 120
Polygon -16777216 true false 234 100 220 96 210 100 214 111 228 116 239 115
Circle -16777216 true false 246 117 20
Line -7500403 true 270 153 282 174
Line -7500403 true 272 153 255 173
Line -7500403 true 269 156 268 177

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1500" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count cats</metric>
    <metric>count mice</metric>
    <enumeratedValueSet variable="show-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-remedy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-traps">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-cats">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-cheese">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproductionCats">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="capMAX">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-mice">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduce?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproductionMices">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="StartEnergy?">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poisoned-cheese">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-poison">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experience">
      <value value="&quot;Final-Battle&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count mice</metric>
    <metric>count cats</metric>
    <enumeratedValueSet variable="show-energy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-traps">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-cats">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-cheese">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproductionCats">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="capMAX">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-mice">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduce?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproductionMices">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probability_memory">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-remedy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="StartEnergy?">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poisoned-cheese">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-poison">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="experience">
      <value value="&quot;Smart-Cat&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="it-max">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
