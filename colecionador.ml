open Printf
;;

class colecionador t n num = object (self)

  val numero:int = num
  val mutable telemovel:string = t
  val mutable nome:string = n
  val mutable quantidades = Array.make num 0 

  method getTelemovel = telemovel
  method getNome = nome
  method getQuantidades = Array.copy quantidades
  method getQuantidade i = quantidades.(i)

  method setTelemovel t = telemovel <- t
  method setNome n = nome <- n
  method setQuantidades q = quantidades <- Array.copy q

  (*retorna true se o colecionador tem o mesmo código do indicado*)
  method equal (c:colecionador) = (telemovel = c#getTelemovel)

  (*retorna uma string que lista o número e quantidade de cada cromo*)
  method private toStringLista =
    let rec stringCromo s i =
      if (i=numero) then s
      else 
        stringCromo (s ^ (sprintf "%6d| %d\n" (i+1) quantidades.(i))) (i+1)
    in
      stringCromo ("") 0

  (*retorna uma string com o código, nome e lista de cromos do colecionador*)
  method toStringCompleto = 
    (sprintf "Nome = %s\n" nome) ^ 
      (sprintf "Cromo | Quantidade\n") ^
      self#toStringLista

  (*retorna uma string com o código e nome*)
  method toString = (sprintf "%s" nome)

  (*adiciona uma quantidade a um cromo*)
  method adicionaCromo i q = quantidades.(i) <- quantidades.(i) + q

  (*retorna true se o número do cromo está disponível para troca*)
  method temCromoTroca i = (quantidades.(i) > 1)

  (*retorna uma lista de números de cromos disponíveis para troca*)
  method listaTroca =
    let rec listaTrocaAux i (lista:int list) =
      if (i=numero) then List.rev lista
      else if (quantidades.(i) > 1) then listaTrocaAux (i+1) (i::lista)
      else listaTrocaAux (i+1) lista
    in
      listaTrocaAux 0 []

  (*retorna uma lista de números de cromos procurados*)
  method listaProcura =
    let rec listaProcuraAux i lista =
      if (i=numero) then List.rev lista
      else if (quantidades.(i) = 0) then listaProcuraAux (i+1) (i::lista)
      else listaProcuraAux (i+1) lista
    in
      listaProcuraAux 0 []

end
;;

let novoColecionador t n num = new colecionador t n num
;;
