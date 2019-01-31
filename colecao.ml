open Printf
;;

class colecao n = object (self)

  val numero:int = n
  val mutable pontos = Array.make n 1
  val mutable quantidades = Array.make n 0

  method getPontos i = pontos.(i)
  method getQuantidade i = quantidades.(i)

  method setPontos i p = pontos.(i) <- p
  method setQuantidade i q = quantidades.(i) <- q

  (*determina quantos cromos existes*)
  method quantidadeTotal =
    let rec soma res i =
      if (i=numero) then res
      else soma (res+quantidades.(i)) (i+1)
    in
      soma 0 0

  (*atualiza os pontos do cromo i*)
  method private atualizaPontos i qt =
    let q = float_of_int (self#getQuantidade i) in
    let p = q /. qt in
      if (p<0.05) then pontos.(i) <- 5
      else if (p<0.10) then pontos.(i) <- 4
      else if (p<0.15) then pontos.(i) <- 3
      else if (p<0.25) then pontos.(i) <- 2
      else pontos.(i) <- 1

  (*atualiza os pontos de cada cromo*)
  method atualizaTodosPontos =
    let quant = float_of_int (self#quantidadeTotal) in
    let rec atualiza i = 
      if (i<numero) then
        (
          self#atualizaPontos i quant;
          atualiza (i+1)
        )
    in 
      atualiza 0

  (*adiciona uma quantidade a um cromo*)
  method adicionaCromo i q =
    (
      quantidades.(i) <- quantidades.(i) + q;
      self#atualizaTodosPontos;
    )

  (*retorna uma string com a lista de pontuações e quantidades por cromo*)
  method toString =
    let rec stringCromo s i =
      if (i=numero) then s
      else
        stringCromo (s ^ (sprintf "%6d|%12d| %d\n" (i+1) quantidades.(i) pontos.(i))) (i+1)
    in
      stringCromo ("Cromo | Quantidade | Pontos\n") 0


end
;;

let novaColecao n = new colecao n
;;
