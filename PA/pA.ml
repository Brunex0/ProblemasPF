(* Trabalho de Programação Funcional
 * Ano Letivo: 2020/2021
 * @author Bruno Monteiro, a43994
 * @author Duarte Arribas, a44585
 * @version 1.1
 * === Descrição da nossa Solução ===
 * De modo a encontrar o menor número de passos necessários para
 * chegar ao número [42] através dum [0 < input < 1_000_000], decidimos
 * percorrer todos os ramos de uma suposta árvore ternária (onde o primeiro
 * ramo será a primeira regra, o segundo a segunda regra, e o terceiro 
 * a terceira regra), segundo um algoritmo de bruteforce. Visto a otimização
 * de recursividade no OCaml, qualquer input dado detem de um output "seamlessly"
 * instantâneo; para tal guardamos nas variáveis [rule1], [rule2] e [rule3] os outputs
 * de cada ramo percorrido e comparamo-os, de modo a receber o menor deles.
 * === Fonte ===
 * Fomos buscar informação aos seguintes locais:
 * 1- http://www.di.ubi.pt/~desousa/PF/pf.html (slides das aulas)
 * 2- https://www.cs.cornell.edu/courses/cs3110/2020sp/textbook (curso do cs.cornell de OCaml)
 * 3- https://dev.realworldocaml.org (real world OCaml)
 *)

(*InvalidInput exception*)
exception InvalidInput of string

(**
 * Calcula mínimo de dois números
 * @param r1 número 1
 * @param r2 número 2
 * @return r1 se r1 < r2 ou r2 se r2 < r1
 *)
let getMin r1 r2 =
  if r1 < r2 then r1 else r2

(**
 * Encontra o menor número de passos para chegar 
 * ao número [42] através do input mandado
 * @param num o número pedido como input ao utilizador.
 * Será mudado recursivamente, para que diversas rules diferentes
 * possam ser utilizadas.
 * @param unchangedNum o número pedido como input ao utilizador.
 * Não será mudado recursivamente, pois é necessário para ser retornado
 * em caso de falhanço.
 * @return o menor número de passos para chegar 
 * ao [42], ou [unchangedNumber + 1] como valor de erro 
 *)
let rec solver num unchangedNum =
  if num < 42 then unchangedNum + 1 (*Caso base 1*)
  else if num = 42 then 0           (*Caso base 2*)
  else(                             (*Passo recursivo*)
    let rule1 =
      if (num mod 2) = 0 then
        1 + solver (num/2) unchangedNum
      else 
        unchangedNum + 1
      and rule2 =
        if ((num mod 4) = 0 || (num mod 3) = 0) && (num mod 10) * (num / 10 mod 10) <> 0 then
          1 + solver (num-((num mod 10) * (num / 10 mod 10))) unchangedNum
        else 
          unchangedNum + 1
        and rule3 =
          if (num mod 5) = 0 then
            1 + solver (num-42) unchangedNum
          else 
            unchangedNum + 1 in getMin rule1 (getMin rule2 rule3) (*Retornar o menor número de passos*)
  )

(* ===== MAIN ===== *)
let () =
let num = read_int () in
  if num > 0 && num < 1_000_000 then
    let result = solver num num in
      if result < num then                   (*Não retornou valor de erro*)
        print_endline (string_of_int result)
      else                                   (*Retornou valor de erro*)
        print_endline ("BAD LUCK")
  else raise (InvalidInput "Invalid input! Input must be: 0 < num < 1_000_000")


(* ===== EXEMPLO ===== *)
(*
*
* result = solver 52 52
*              52 < 42 Falso
*              52 = 42 Falso   
*              rule1 = 
*                      52 é divisivel por 2 ? Verdadeiro
*                     1 + solver (52/2) 52 
*                     26 < 42 Verdade
*                    = 1 + 52
*                    =53
*              rule2 =
*                     52 é divisivel por 4 ou por 3 ? Verdadeiro
*                      1 + solver (52-((52 mod 10) * (52 / 10 mod 10))) 52
*                      42 = 42? Verdade, devolve 0
*                    = 1 + 0
*                    = 1
*              rule3 = 
*                    52 Não é divisivel por 5 ? Verdadeiro
*                    = 52 + 1
*                    = 53
*           
*              getMin 53 (getMin 1 53)=
*                getMin 53 (
*                  1 < 53 Verdade
*                )
*                
*                 = getMin 53 1 =
*                      53 < 1 Falso
*                 = 1
*                 Devolve 1
*   result = 1
*
*   1 < 52 ? Verdade
*      Logo, Escreve no ecrã 1
*)