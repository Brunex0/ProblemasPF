(* Trabalho de Programação Funcional
 * Ano Letivo: 2020/2021
 * @author Bruno Monteiro, a43994
 * @author Duarte Arribas, a44585
 * @version 1.2
 * === Descrição da nossa Solução ===
 * Após ler a matriz do standard input esta é inserida numa árvore quaternária.
 * Cada ramo da árvore corresponde à respetiva orientação geográfica (NW,NE,SW,SE) da imagem, dividida recursivamente.
 * Caso detete um nodo em que as suas imagens são da mesma cor esta é assumida como uma folha.
 * Após isso, cortamos a árvore quaternária na altura correta (log2(N)) e inserimos os elementos na matriz thumbnail. 
 * === Fonte ===
 * Fomos buscar informação aos seguintes locais:
 * 1- http://www.di.ubi.pt/~desousa/PF/pf.html (slides das aulas)
 * 2- https://www.cs.cornell.edu/courses/cs3110/2020sp/textbook (curso do cs.cornell de OCaml)
 * 3- https://dev.realworldocaml.org (real world OCaml)
 *)

(**
 * W - Representa a cor Branca
 * B - Representa a cor Preta
 * G - Utilizado na função verifyLeaf. Caso fosse um nodo devolvia G
*)
type color = W | B | G

(**
 * Imagem pode ser:
 * Leaf -> Tem a cor e o int corresponde ao seu peso, ou seja util para saber qual cor predomina em certo momento
 * Node -> Composto por 4 imagens NW,NE,SW,SE, respetivamente
*)
type image = Leaf of color * int
           | Node of image * image * image * image  

(**
 * @param matrix A matriz que se pretende mostrar
 * @param n tamanho da matriz
*)
let printMatrix matrix n=
  for i=0 to n-1 do
    for j=0 to n-1 do
      if n-1 = j then
        Printf.printf "%d" matrix.(i).(j)
      else
        Printf.printf "%d " matrix.(i).(j)
    done ;
  Printf.printf "\n"
  done

(**
 * Lê a matriz do standard input
 * @param n tamanho da matriz
 * @return matriz lida 
*)
let readMatrix n=
  let matrix =Array.make_matrix n n 3 in
  for i=0 to (n-1) do
    for j=0 to (n-1) do
      if (j=(n-1)) then
        let y = Scanf.scanf "%d\n" (fun x -> x) in matrix.(i).(j) <- y
      else
        let y = Scanf.scanf "%d " (fun x -> x) in matrix.(i).(j) <- y;
    done;
  done;
  matrix

(**
 * Cria uma matrix que vai fazer parte do respetivo Nodo/Folha
 * @param matrix matriz 
 * @param n tamanho da matrix
 * @param linha linha a partir de onde se cria a matriz
 * @param coluna coluna a partir de onde se cria a matriz
 * @return a matriz(aux)
*)
let createMatrix matrix n linha coluna=
  let aux = Array.make_matrix (n/2) (n/2) 3 in
  for i=0 to (n/2)-1 do
    for j=0 to (n/2)-1 do
      aux.(i).(j) <- matrix.(i+linha).(j+coluna)
    done
    done;
    aux

(**
 * Divide a matriz de acordo com a flag passada.
 * A flag corresponde às coordenadas onde começa a matriz desse quadrante.
 * newMatriz.(0).(0)=matrix.(0+0).(0+0) para o caso do nw;
 * newMatriz.(0).(1)=matrix.(0+0).(0+n/2) para o caso do ne;
 * newMatriz.(1).(0)=matrix.(0+0).(0+n/2) para o caso do sw
 * newMatriz.(1).(1)=matrix.(0+n/2).(0+n/2) para o caso do SE;
 * @param matrix Matriz inicial
 * @param n Tamanho da matriz
 * @param flag Coordenada onde tem de dar split
 * @return a matriz dividida na coordenada correspondente
*)
let matrixsplit matrix n flag =
  match flag with
  | "nw" -> createMatrix matrix n 0 0
  | "ne" -> createMatrix matrix n 0 (n/2)
  | "sw" -> createMatrix matrix n (n/2) 0
  | _ -> createMatrix matrix n (n/2) (n/2)

(**
 * Verifica se é a cor branca ou preta
 * @param t recebe 0 ou 1
 * @return se t=1 cor preta ou branca caso contrário
*)
let verifyWhiteBlack t=
  if t = 1 then
    B
  else
    W

(**
 * Verifica se é leaf ou nodo
 * @param nl imagem
 * @return Se for Leaf devolve a cor da folha, caso seja nodo devolve Gray.
*)
let verifyLeaf nl=
  match nl with 
  | Leaf(color, depth) -> color
  | Node(image1, image2, image3, image4) -> G

(**
 * Verificar se as folhas são todas da mesma cor. Caso seja, devolve uma folha com essa cor e com o seu peso incrementado
 * (x4 pois temos as 4 coordenadas da mesma cor). Caso não seja, concluimos que temos um nodo.
 * @param nw Folha da coordenada nw
 * @param ne Folha da coordenada ne
 * @param sw Folha da coordenada sw
 * @param se Folha da coordenada se
 * @return Folha ou Nodo 
*)
let verifyLeafOrNode nw ne sw se =
  if ((verifyLeaf nw) = W && (verifyLeaf ne) = W && (verifyLeaf sw) = W && (verifyLeaf se) = W ) then
    match nw with
    | Leaf(color, depth) -> Leaf (color, depth*4) 
    | _ -> nw
  else
    if ((verifyLeaf nw) = B && (verifyLeaf ne) = B && (verifyLeaf sw) = B && (verifyLeaf se) = B ) then
      match nw with
      | Leaf(color, depth) -> Leaf(color, depth *4)
      |_ -> nw
    else
      Node(nw, ne,sw,se)

(**
 * Função para construir a árvore quaternária recursivamente. Vai dividindo a matriz inicial e formando a árvore. 
 * @param matrix matriz usada para formar a árvore
 * @param n tamanho da matriz
 * @return a árvore
*)
let rec buildTree matrix n =
  if(n=1) then
    Leaf(verifyWhiteBlack matrix.(0).(0),1)
  else(
    let nw = buildTree (matrixsplit matrix n "nw") (n/2) and
    ne = buildTree (matrixsplit matrix n "ne") (n/2) and
    sw = buildTree (matrixsplit matrix n "sw" ) (n/2) and
    se = buildTree (matrixsplit matrix n "se") (n/2) in  
    verifyLeafOrNode nw ne sw se
  )

(**
 * Verifica o peso das folhas para em conjunto com outra função verificar se estamos perante a cor preta ou branca
 * Ao branco atribuimos um peso positivo e ao preto um valor negativo. Deste modo torna mais fácil distinguir a cor predominante
 * @param tree a árvore
 * @return a soma dos pesos para depois tirar conclusões 
*)
let rec seePredominantly tree =
  match tree with
  | Leaf(W, depth) -> depth
  | Leaf(B,depth) -> (-1)*depth
  | Node(a,b,c,d) -> seePredominantly a + seePredominantly b + seePredominantly c + seePredominantly d
  |_ -> 0

(**
 * Verifica o tipo de cor atráves da soma dos pesos que é passada para a função. 
 * @param sumDepth a soma dos pesos
 * @return Se o resultado da soma for negativa ou nula, devolve o 1 (Cor Preta). Caso contrário, devolve 0 (Cor Branca) 
*)
let checkTypeColor sumDepth =
  if(sumDepth <=0)then
    1
  else
    0
  
(**
 * Pegamos na árvore e recursivamente vamos percorrendo a árvore até chegar ao Nodo/Folha necessário para criar a matriz thumbnail.
 * Quando esse ponto é atingido, atribui-se uma posição específica do array thumbnail. Por exemplo:
 * Se tivermos uma matriz 4x4 0 1 0 0
 *                            1 0 0 0
 *                            1 1 0 0
 *                            1 1 0 0
 *  A nossa thumbnail irá ter tamanho 2 x 2. Temos de percorrer a árvore 0 a 1 (log2(2)). nw ne 
 *                                                                                        sw se 
 * Sabemos que nw corresponde a x=(2*x+0) y=(2*y+0) 
 *             ne corresponde a x=(2*x+0) y=(2*y+1)
 *             sw corresponde a x=(2*x+1) y=(2*y+0) 
 *             se corresponde a x=(2*x+1) y=(2*y+1)
 * Com este pensamento é possível chegar à matriz thumbnail, de forma recursiva: 1 0
 *                                                                               1 0                                                                                                  
 *              
 * @param tree árvore
 * @param size tamnho da thumbnail
 * @return matrix thumbnail                                                                        
*)
let makeMatrixThumbnail tree size =
  let thumbnail =Array.make_matrix size size 3 in 
  (*tree - árvore; partitionNumber - Número de vezes que percorremos a árvore*)
  let rec cutThumbnail tree partitionNumber x y=
    match tree with
    | Leaf(color,depth) when (partitionNumber = 0) ->  thumbnail.(x).(y) <- checkTypeColor( seePredominantly (Leaf(color,depth)))
    | Node(a,b,c,d) when (partitionNumber = 0) -> thumbnail.(x).(y) <- checkTypeColor(seePredominantly(Node(a,b,c,d)))
    | Leaf(color,depth) -> cutThumbnail (Leaf(color,depth)) (partitionNumber-1) (2*x+0) (2*y+0); 
                          cutThumbnail (Leaf(color,depth)) (partitionNumber-1) (2*x+0) (2*y+1);
                          cutThumbnail (Leaf(color,depth)) (partitionNumber-1) (2*x+1) (2*y+0);
                          cutThumbnail (Leaf(color,depth)) (partitionNumber-1) (2*x+1) (2*y+1)                   
    | Node(a,b,c,d) -> cutThumbnail a (partitionNumber-1) (2*x+0) (2*y+0);
                      cutThumbnail b (partitionNumber-1) (2*x+0) (2*y+1);
                      cutThumbnail c (partitionNumber-1) (2*x+1) (2*y+0);
                      cutThumbnail d (partitionNumber-1) (2*x+1) (2*y+1)
    in
    cutThumbnail tree (int_of_float (log (float_of_int(size)) /. log 2.)) 0 0;
    thumbnail

(**
 * Conta o número de folhas existentes na árvore
 * @param tree árvore
 * @return numero de folhas
 *)
let rec countLeafs tree =
  match tree with
  | Leaf(a, b) -> 1
  | Node(a,b,c,d) -> countLeafs a + countLeafs b + countLeafs c + countLeafs d

(**
 * Compara o minimo entre dois numeros
 * @param x - numero 1
 * @param y - numero 2
 * @return o mínimo valor
*)
let getMin x y =
  if x < y then
    x
  else
    y

(**
 * Determina o nível da folha mais alta
 * @param tree a árvore  
 * @return o nível da folha mais alta 
*)
let rec countLevelofFirstLeafs tree=
  match tree with
  | Leaf(a, b) -> 0
  | Node(a,b,c,d) -> let nw = 1 + countLevelofFirstLeafs a 
                      and ne = 1 + countLevelofFirstLeafs b  and 
                        sw = 1 + countLevelofFirstLeafs c and 
                        se = 1 + countLevelofFirstLeafs d  in 
                          let min = getMin (getMin nw ne) (getMin se sw) in
                            min
            
(* ===== MAIN ===== *)
let z =read_line();;
let tamanho = read_line() in
let linhaTamanho = String.split_on_char ' ' tamanho in 
let n = int_of_string (List.nth linhaTamanho 0) in
let matrix = readMatrix n in
  let arvore = buildTree matrix n and size= Scanf.scanf "%d\n" (fun x -> x) in
  Printf.printf "%d\n" (countLevelofFirstLeafs arvore);
  Printf.printf "%d\n" (countLeafs arvore);
  printMatrix (makeMatrixThumbnail arvore size) size

(* ===== EXEMPLO ===== *)
(**
 * INPUT:
 * P1
 * 4 4
 * 0 1 0 0
 * 1 0 0 0
 * 1 1 0 0
 * 1 1 0 0
 * 2
 * === EXECUÇÃO ===
 * z=P1
 * tamanho = 8 8
 * linhaTamanho = [8;8]
 * n=8
 * matriz = readMatrix 8;
 * matriz = 0 1 0 0
 *          1 0 0 0
 *          1 1 0 0
 *          1 1 0 1
 *
 * arvore = buildTree matriz 4
 *        n=1? Falso
 *        nw= buildTree (matrixsplit matriz 4 "nw"
 *                        "nw"? Verdade
 *                         createMatrix matrix 4 0 0{
 *                          aux.(0).(0) <- matriz.(0).(0)
 *                          aux.(0).(1) <- matriz.(0).(1)
 *                          aux.(1).(0) <- matriz.(1).(0)
 *                          aux.(1).(1) <- matriz.(1).(1)
 *                        }
 *                      ) (4/2) 
 *              n=1? Falso
 *                 nw= buildTree(matrixsplit matriz 2 "nw"
 *                                "nw"? Verdade
 *                                createMatrix matriz 2 0 (2/2)
 *                                aux.(0).(0) <- matriz.(0).(0)
 *                                ) (2/2)
 *                    n=1? Verdade  
 *                    Leaf(verifyWhiteBlack 0,1) = Leaf(W,1)
 *                 ne=buildTree(matrixsplit matriz 2 "ne"
 *                                "ne"? Verdade
 *                                createMatrix matriz 2 0 (2/2)
 *                                aux.(0).(0) <- matriz.(0).(1)
 *                              ) (2/2)
 *                        n=1? Verdade
 *                          Leaf(verifyWhiteBlack 1,1) = Leaf(B,1)
 *                 sw=buildTree(matrixsplit matriz 2 "sw"
 *                                "sw"? Verdade
 *                                createMatrix matriz 2 (2/2) 0
 *                                aux.(0).(0) <- matriz.(1).(0)
 *                              ) (2/2)
 *                        n=1? Verdade
 *                          Leaf(verifyWhiteBlack 1,1) = Leaf(B,1)
 *                 se=buildTree(matrixsplit matriz 2 "sw"
 *                                "se"? Verdade
 *                                createMatrix matriz 2 (2/2) (2/2)
 *                                aux.(0).(0) <- matriz.(1).(1)
 *                              ) (2/2)
 *                        n=1? Verdade
 *                          Leaf(verifyWhiteBlack 0,1) = Leaf(B,1)
 *                 VerifyLeafOrNode nw ne sw se
 *                      Folhas nw se sw se não são todas iguais logo temos
 *                      Nodo(nw,ne,sw,se)
 *        ne= buildTree (matrixsplit matriz 4 "ne"
 *                        "ne"? Verdade
 *                         createMatrix matrix 4 0 (4/2){
 *                          aux.(0).(0) <- matriz.(0).(2)
 *                          aux.(0).(1) <- matriz.(0).(3)
 *                          aux.(1).(0) <- matriz.(1).(2)
 *                          aux.(1).(1) <- matriz.(1).(3)
 *                        }
 *                      ) (4/2)  
 *                n=1? Falso
 *                 nw= buildTree(matrixsplit matriz 2 "nw"
 *                                "nw"? Verdade
 *                                createMatrix matriz 2 0 (2/2)
 *                                aux.(0).(0) <- matriz.(0).(0)
 *                                ) (2/2)
 *                    n=1? Verdade  
 *                    Leaf(verifyWhiteBlack 0,1) = Leaf(W,1)
 *                 ne=buildTree(matrixsplit matriz 2 "ne"
 *                                "ne"? Verdade
 *                                createMatrix matriz 2 0 (2/2)
 *                                aux.(0).(0) <- matriz.(0).(1)
 *                              ) (2/2)
 *                        n=1? Verdade
 *                          Leaf(verifyWhiteBlack 0,1) = Leaf(B,1)
 *                 sw=buildTree(matrixsplit matriz 2 "sw"
 *                                "sw"? Verdade
 *                                createMatrix matriz 2 (2/2) 0
 *                                aux.(0).(0) <- matriz.(1).(0)
 *                              ) (2/2)
 *                        n=1? Verdade
 *                          Leaf(verifyWhiteBlack 0,1) = Leaf(B,1)
 *                 se=buildTree(matrixsplit matriz 2 "sw"
 *                                "se"? Verdade
 *                                createMatrix matriz 2 (2/2) (2/2)
 *                                aux.(0).(0) <- matriz.(1).(1)
 *                              ) (2/2)
 *                        n=1? Verdade
 *                          Leaf(verifyWhiteBlack 0,1) = Leaf(B,1)
 *                  VerifyLeafOrNode nw ne sw se
 *                       Folhas nw se sw se são todas iguais logo temos
 *                       Leaf(0,4)
 *        sw= buildTree (matrixsplit matriz 4 "sw"
 *                        "ne"? Verdade
 *                         createMatrix matrix 4 (4/2) 0{
 *                          aux.(0).(0) <- matriz.(2).(0)
 *                          aux.(0).(1) <- matriz.(2).(1)
 *                          aux.(1).(0) <- matriz.(3).(0)
 *                          aux.(1).(1) <- matriz.(3).(1)
 *                        }
 *                      ) (4/2)  
 *                n=1? Falso
 *                 nw= buildTree(matrixsplit matriz 2 "nw"
 *                                "nw"? Verdade
 *                                createMatrix matriz 2 0 (2/2)
 *                                aux.(0).(0) <- matriz.(0).(0)
 *                                ) (2/2)
 *                    n=1? Verdade  
 *                    Leaf(verifyWhiteBlack 1,1) = Leaf(W,1)
 *                 ne=buildTree(matrixsplit matriz 2 "ne"
 *                                "ne"? Verdade
 *                                createMatrix matriz 2 0 (2/2)
 *                                aux.(0).(0) <- matriz.(0).(1)
 *                              ) (2/2)
 *                        n=1? Verdade
 *                          Leaf(verifyWhiteBlack 1,1) = Leaf(B,1)
 *                 sw=buildTree(matrixsplit matriz 2 "sw"
 *                                "sw"? Verdade
 *                                createMatrix matriz 2 (2/2) 0
 *                                aux.(0).(0) <- matriz.(1).(0)
 *                              ) (2/2)
 *                        n=1? Verdade
 *                          Leaf(verifyWhiteBlack 1,1) = Leaf(B,1)
 *                 se=buildTree(matrixsplit matriz 2 "sw"
 *                                "se"? Verdade
 *                                createMatrix matriz 2 (2/2) (2/2)
 *                                aux.(0).(0) <- matriz.(1).(1)
 *                              ) (2/2)
 *                        n=1? Verdade
 *                          Leaf(verifyWhiteBlack 1,1) = Leaf(B,1)
 *                  VerifyLeafOrNode nw ne sw se
 *                      Folhas nw se sw se são todas iguais logo temos
 *                      Leaf(1,4)
 *        se= buildTree (matrixsplit matriz 4 "sw"
 *                        "ne"? Verdade
 *                         createMatrix matrix 4 (4/2) (4/2){
 *                          aux.(0).(0) <- matriz.(2).(2)
 *                          aux.(0).(1) <- matriz.(2).(3)
 *                          aux.(1).(0) <- matriz.(3).(2)
 *                          aux.(1).(1) <- matriz.(3).(3)
 *                        }
 *                      ) (4/2)  
 *                n=1? Falso
 *                 nw= buildTree(matrixsplit matriz 2 "nw"
 *                                "nw"? Verdade
 *                                createMatrix matriz 2 0 (2/2)
 *                                aux.(0).(0) <- matriz.(0).(0)
 *                                ) (2/2)
 *                    n=1? Verdade  
 *                    Leaf(verifyWhiteBlack 1,1) = Leaf(W,1)
 *                 ne=buildTree(matrixsplit matriz 2 "ne"
 *                                "ne"? Verdade
 *                                createMatrix matriz 2 0 (2/2)
 *                                aux.(0).(0) <- matriz.(0).(1)
 *                              ) (2/2)
 *                        n=1? Verdade
 *                          Leaf(verifyWhiteBlack 1,1) = Leaf(B,1)
 *                 sw=buildTree(matrixsplit matriz 2 "sw"
 *                                "sw"? Verdade
 *                                createMatrix matriz 2 (2/2) 0
 *                                aux.(0).(0) <- matriz.(1).(0)
 *                              ) (2/2)
 *                        n=1? Verdade
 *                          Leaf(verifyWhiteBlack 1,1) = Leaf(B,1)
 *                 se=buildTree(matrixsplit matriz 2 "sw"
 *                                "se"? Verdade
 *                                createMatrix matriz 2 (2/2) (2/2)
 *                                aux.(0).(0) <- matriz.(1).(1)
 *                              ) (2/2)
 *                        n=1? Verdade
 *                          Leaf(verifyWhiteBlack 1,1) = Leaf(B,1)
 *                  VerifyLeafOrNode nw ne sw se
 *                      Folhas nw se sw se são todas iguais logo temos
 *                      Leaf(1,4)
 *        verifyLeafOrNode nw ne sw se   
 *            Folhas nw se sw se não são todas iguais logo temos
 *              Node(nw,ne,sw,se)
 *
 *        = 
 *                              0 1 0 0
 *                              1 0 0 0
 *                              1 1 0 0
 *                              1 1 0 0
 *                      /      |      |     \                       
 *                    0 1     0 0     1 1     0 0
 *                    1 0     0 0     1 1     0 0
 *                  / | | \                  
 *                 0  1  1 0                          
 *  
 *    size = 2
 *    
 *    countLevelofFirstLeafs arvore(
 *          Leaf(a,b)? Não 
 *            nw= 1 + countLevelofFirstLeafs nw(
 *                              Leaf(a,b)? Não 
 *                              nw = 1 + countLevelofFirstLeafs nw (
 *                                Leaf(a,b)?Sim
 *                                0
 *                              )=1+0=1
 *                          )
 *              = 1 + 1 = 2
 *            ne= 1 + countLevelofFirstLeafs ne(
 *                              Leaf(a,b)? Sim 
 *                                0
 *                              )
 *                          )
 *              = 1 + 0 = 1
 *            sw= 1 + countLevelofFirstLeafs sw(
 *                              Leaf(a,b)? Sim 
 *                                0
 *                              )
 *                          )
 *              = 1 + 0 = 1
 *            se= 1 + countLevelofFirstLeafs se(
 *                              Leaf(a,b)? Sim 
 *                                0
 *                              )
 *                          )
 *              = 1 + 0 = 1
 *
 *            min = getMin (getMin 2 1) (getMin 1 1) 
 *                            getMin nw ne (
 *                              2 < 1?Não
 *                                1
 *                            )
 *                            getMin 1 1 (
 *                              1<1? Não
 *                                1
 *                            )
 *                = getMin 1 1 (
 *                    1<1?Não
 *                      1
 *                  )
 *                = 1
 *      Retorna 1 como o nível da primeira folha
 *    )
 *
 *    countLeafs arvore =
 *         Leaf(a,b)? Não
 *         Node(a,b,c,d)? Sim         
 *              countLeafs a = (
 *                Leaf(a,b)? Não
 *                Node(a,b,c,d)? Sim
 *               countLeafs a (
 *                  Leaf(a,b)? Sim
 *                  1
 *                )=1 
 *                +countLeafs b (
 *                  Leaf(a,b)? Sim
 *                  1
 *                )=1  
 *                + countLeafs c (
 *                  Leaf(a,b)? Sim
 *                  1
 *                )=1 
 *                + countLeafs d(
 *                  Leaf(a,b)? Sim
 *                  1
 *                )=1 
 *              ) = 4
 *              + countLeafs b = (
 *                  Leaf(a,b)? Sim
 *                  1
 *                )=1 + countLeafs c (
 *                  Leaf(a,b)? Sim
 *                  1
 *                )=1 + 
 *                countLeafs d (
 *                  Leaf(a,b)? Sim
 *                  1
 *                )=1 
 *            4 + 1 + 1 + 1 =7 
 *  Devolve 7 folhas
 *  
 *  makeMatrixThumbnail arvore 2 =(
 *      thumbnail = 3 3
 *                  3 3
 *       cutThumbnail arvore (log2(2)) 0 0;
 *            Leaf(a,b) e particionNumber=0?Não
 *            Node(a,b,c,d) e particionNumber=0?Não
 *            Leaf(a,b)? Não
 *            Node(a,b,c,d)? sim(
 *                  cutThumbnail a 1-1 0 0 = 
 *                      Leaf(a,b) e particionNumber=0?Não
 *                      Node(a,b,c,d) e particionNumber=0?Sim
 *                          thumbnail.(0).(0) <- checkTypeColor(seePredominantly(Node(a,b,c,d))) = (
 *                                                      seePredominantly(
 *                                                         Node(a,b,c,d)?sim
 *                                                         seePredominantly a(
 *                                                            Leaf(W,1)?Sim 
 *                                                                1
 *                                                         )=1 + 
 *                                                         seePredominantly b(
 *                                                           Leaf(B,1)?Sim
 *                                                                -1
 *                                                         )=1 + 
 *                                                         seePredominantly c(
 *                                                            Leaf(B,1)?Sim
 *                                                                -1
 *                                                         )=1 +
 *                                                         seePredominantly d(
 *                                                           Leaf(W,1)?Sim
 *                                                            1
 *                                                         )=1   
 *                                                      )=1-1-1+1=0
 *                                                      0<=0? Sim
 *                                                        1
 *                                                    )=1
 *                  cutThumbnail b 1-1 0 1 =
 *                      Leaf(a,b) e particionNumber=0?Sim
 *                        thumbnail.(0).(1) <- checkTypeColor(seePredominantly(Leaf(color,depth))) = (
 *                                                seePredominantly(
 *                                                  Leaf(W,4)? sim
 *                                                    Devolve 4
 *                                                )=4
 *                                                4<=0?Não
 *                                                  0
 *                                              )=0
 *                  cutThumbnail c 1-1 1 0 =
 *                      Leaf(a,b) e particionNumber=0?Sim
 *                        thumbnail.(1).(0) <- checkTypeColor(seePredominantly(Leaf(color,depth))) = (
 *                                                seePredominantly(
 *                                                  Leaf(B,4)? sim
 *                                                    Devolve -4
 *                                                )=-4
 *                                                -4<=0?Sim
 *                                                  1
 *                                              )=1
 *                  cutThumbnail c 1-1 1 1 =
 *                      Leaf(a,b) e particionNumber=0?Sim
 *                        thumbnail.(1).(1) <- checkTypeColor(seePredominantly(Leaf(color,depth))) = (
 *                                                seePredominantly(
 *                                                  Leaf(W,4)? sim
 *                                                    Devolve 1
 *                                                )=4
 *                                                4<=0?Sim
 *                                                  0
 *                                              )=0
 *
 *            )
 *      Devolve thumbnail
 *  )
 *  printMatrix (thumbnail) 2 = 1 0
 *                              1 0
 *   
 * 
 * 
 * OUTPUT:
 *  1
 *  7
 *  1 0
 *  1 0
 *
*)  