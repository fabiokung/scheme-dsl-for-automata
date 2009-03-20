; Atividade de Programacao 2
; 
; Fabio Correia Kung
;
; Exercicio 1: 
;   Construa um simulador de automato finito deterministico em SCHEME
; 
; Macro retirada do artigo: 
; "S. Krishnamurti. Automata via Macros Journal of Functional Programming, Volume 16 , Issue 3 (May 2006), pp. 253-267"
; obs.: Foi modificada para incluir informacoes de simulacao da execucao dos automatos
; 
; Cria uma DSL (Domain Specific Language) para a definicao de automatos.
; Neste arquivo existem exemplos de como usar a DSL criada para definir automatos e como usa-los.
(define-syntax automaton 
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
             [(_ (label -> target ) (... ...)) 
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
                      (display " => Transitando para estado: ")
                      (display (quote target))
                      (newline)
                      (target (rest stream ))] 
                     (... ...) 
                     [else false])]))])]) 
       (letrec ([state 
                 (process-state response ...)] 
                ...) 
         init-state ))])) 

; Exemplo de definicao de um automato finito deterministico
; que reconhece cadeias da linguagem "c(ad)*r"
(define cadeia-tipo-cdar? 
  (automaton init ; estado inicial
             ; estados
             [init : 
                   (c -> more)]
             [more : 
                   ; transicoes de cada estado
                   (a -> more) 
                   (d -> more) 
                   (r -> end)] 
             ; estado de aceitacao
             [end : 
                  accept])) 

; Exemplos de chamada
(cadeia-tipo-cdar? '(c a d a d r))
(cadeia-tipo-cdar? '(c a d a r d r))