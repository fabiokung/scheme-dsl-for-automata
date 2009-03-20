; Atividade de Programacao 2
; 
; Fabio Correia Kung
;
; Exercicio 2: 
;   Construa um simulador de automato finito nao-deterministico em SCHEME
; 
; Macro retirada (e adaptada para aceitar automatos nao deterministicos) do artigo: 
; "S. Krishnamurti. Automata via Macros Journal of Functional Programming, Volume 16 , Issue 3 (May 2006), pp. 253-267"
; 
; Cria uma DSL (Domain Specific Language) para a definicao de automatos nao deterministicos.
; Neste arquivo existem exemplos de como usar a DSL criada para definir os automatos e como usa-los.
(define-syntax nd-automaton 
  (syntax-rules (:) 
    [(_ init-state 
        (state : response ...) 
        ...) 
     (let-syntax 
         ([process-state 
           (syntax-rules (accept ->) 
             [(_ accept) 
              (lambda (stream ) 
                (cond 
                  [(empty? stream ) true] 
                  [else false]))] 
             [(_ (label -> target (... ...) ) (... ...)) 
              (lambda (stream) 
                (cond 
                  [(empty? stream ) false] 
                  [else 
                   (case (first stream ) 
                     [(label )  
                      (display "Simbolo: ")
                      (display (first stream))
                      (display ", Restando: ")
                      (display (rest stream))
                      (display " => Transitando para estado(s): ")
                      ; Imprimindo todos os estados alvo
                      (display 
                       (string-append (symbol->string (quote target)) " ")) 
                      (... ...)
                      (newline)
                      ; MUDANCA: para cada simbolo, podem haver diversas transicoes.
                      ; Todas devem ser avaliadas e basta que um dos caminhos reconheca a cadeia.
                      ; ps.: os estados serao avaliados por profundidade (e nao por largura), 
                      ; na ordem em que foram definidos na transicao.
                      (or (target (rest stream )) (... ...))] 
                     (... ...) 
                     [else false])]))])]) 
       (letrec ([state 
                 (process-state response ...)] 
                ...) 
         init-state ))])) 

; Exemplo de definicao de um automato finito nao deterministico
; que reconhece cadeias da linguagem "c(ad)*r || c(cf)*f"
; O nao determinismo eh definido quando um mesmo simbolo leva a dois diferentes estados
(define reconhece? 
  (nd-automaton init ; estado inicial
                [init : 
                      ; o simbolo c pode levar tanto para o estado more quanto para moref!
                      (c -> more moref)] 
                [more : 
                      (a -> more) 
                      (d -> more) ; transicoes simples
                      (r -> end)] 
                [moref : 
                       (c -> moref)
                       ; novamente o nao determinismo (transicoes multiplas p/ mesma entrada)
                       (f -> moref end)] 
                [end : ; estado de aceitacao
                     accept])) 

; Exemplos de chamada
(reconhece? '(c r)) ; #t
(reconhece? '(c a d a d r)) ; #t
(reconhece? '(c a d a r d r)) ; #f
(reconhece? '(c a f r)) ; #f
(reconhece? '(c f)) ; #t
(reconhece? '(c f f)) ; #t
(reconhece? '(c f f c)) ; #f
(reconhece? '(c f f f)) ; #t
