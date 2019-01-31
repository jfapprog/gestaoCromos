open Printf
;;
open Scanf
;;

class menu o n = object (self)

  val numero = n
  val mutable opcao:int = o
  val mutable colecionadores:Colecionador.colecionador list = []
  val mutable cromos:Colecao.colecao = Colecao.novaColecao n

  method getOpcao = opcao

  (*retorna uma lista com as linhas de um ficheiro*)
  method private leLinhas ficheiro =
    let rec leLinha lista =
      try
        ( 
          let linha = input_line ficheiro in
            leLinha (linha :: lista)
        )
      with
          End_of_file -> List.rev lista
    in
      leLinha []

  (*retorna uma lista de pares código, nome de colecionadores*)
  method private trataLinhas lista =
    let rec aux l =
      match l with
        |[] -> []
        |topo :: corpo -> 
            let par = (sscanf topo "%s %s" (fun s1 s2 -> (s1, s2))) in
              par :: (aux corpo)
    in
      aux lista

  (*retorna uma lista de colecionadores*)
  method private adicionaColecionadores lista =
    let rec aux l =
      match l with
        |[] -> []
        |topo :: corpo ->
            let col = Colecionador.novoColecionador (fst topo) (snd topo) numero in
              col :: (aux corpo)
    in
      aux lista

  (*inicia a lista de colecionadores*)
  method iniciaColecionadores fin =
    let linhas = self#leLinhas fin in
      close_in fin;
      let pares = self#trataLinhas linhas in
        colecionadores <- (self#adicionaColecionadores pares)


  (*retorna uma lista com as quantidades de todos os colecionador*)
  method private trataLinhas2 lista =
    let rec aux l =
      match l with
        |[] -> []
        |topo :: corpo -> 
            let q = (sscanf topo "%d" (fun i -> i)) in
              q :: (aux corpo)
    in
      aux lista

  (*insere as quantidades no colecionador indicado*)
  method private iniciaQuantidadesCol col vetor = col#setQuantidades vetor

  (*insere as quantidades em todos os utilizadores*)
  method private iniciaTodasQuantidades vetor =
    let rec aux i lista =
      match lista with
        |[] -> print_newline ();
        |topo :: corpo -> 
            let subVetor = Array.sub vetor (i*numero) numero in
              self#iniciaQuantidadesCol topo subVetor;
              aux (i+1) corpo;
    in
      aux 0 colecionadores

  (*insere as quantidades nos colecionadores*)
  method iniciaQuantidades fin =
    let linhas = self#leLinhas fin in
      close_in fin;
      let qts = self#trataLinhas2 linhas in
        self#iniciaTodasQuantidades (Array.of_list qts)

  (*insere as quantidades de um colecionador nos cromos*)
  method private iniciaQtsCromos col = 
    let rec aux i =
      if (i<numero) then
        (
          cromos#setQuantidade i ((col#getQuantidade i)+(cromos#getQuantidade i));
          aux (i+1)
        )
    in aux 0

  (*insere as quantidades de todos os colecionadores nos cromos*)
  method iniciaQuantidadeCromos =
    let rec aux lista =
      match lista with
        |[] -> print_string "";
        |topo :: corpo -> 
            self#iniciaQtsCromos topo;
            aux corpo;
    in
      aux colecionadores

  (*inicializa pontos*)
  method iniciaPontos = cromos#atualizaTodosPontos

  (*lê um número inteiro*)
  method private lerInteiro msg =
    print_string msg;
    try (read_int()) with
      |Failure "int_of_string" -> 
          print_endline "Digite um numero inteiro";
          self#lerInteiro msg

  (*lê a opcao inserida pelo utilizador*)
  method lerOpcao = 
    let op = self#lerInteiro "Opcao = " in
      if (op < 1 || op > 9) then 
        (
          print_endline "Digite um algarismo entre 1 e 9";
          self#lerOpcao
        )
      else opcao <- op

  (*apresenta o menu principal*)
  method principal =
    print_newline ();
    print_endline "----------------------------------------------";
    print_endline "            CromoGest - Opcoes                ";
    print_endline "----------------------------------------------";
    print_endline "1 - Listar colecao completa";
    print_endline "2 - Listar colecionadores";
    print_endline "3 - Registar novo colecionador";
    print_endline "4 - Listar cromos de colecionador";
    print_endline "5 - Listar cromos de colecionador para troca";
    print_endline "6 - Listar cromos procurados por colecionador";
    print_endline "7 - Registar novas quantidades de cromos";
    print_endline "8 - Procurar cromo";
    print_endline "9 - Sair";
    print_endline "---------------------------------------------"

  (* Menu 1 - lista a colecao completa de cromos*)
  method listarColecao =
    printf "Lista de cromos:\n%s" cromos#toString

  (* Menu 2 - lista os colecionadores*)
  method listarColecionadores =
    print_endline "Lista de colecionadores:\nIndice | Telemovel | Nome";
    let rec aux i l =
      match l with
        |[] -> print_newline();
        |topo :: corpo ->
            printf "%7d|%11s| %s\n" (i+1) (topo#getTelemovel) (topo#getNome);
            aux (i+1) corpo
    in
      aux 0 colecionadores

  (* le uma string inserida pelo utilizador*)
  method private leString = read_line()

  (* verifica se colecionador já existe *)
  method private existe col =
    let rec aux lista =
      match lista with
        |[] -> false
        |topo :: corpo ->
            if (topo#equal col) then true
            else aux corpo
    in
      aux colecionadores

  (* Menu 3 - Registar novo colecionador*)
  method registarColecionador =
    print_string "Nome = ";
    let nome = self#leString in
      print_string "Telemovel = ";
      let telemovel = self#leString in
      let novoCol = Colecionador.novoColecionador telemovel nome numero in
        if (self#existe novoCol) then
          printf "Ja existe um colecionador com telemovel numero %s\n" telemovel
        else colecionadores <- novoCol::colecionadores

  (*lê o índice inserido pelo utilizador*)
  method private lerIndice = self#lerInteiro "Indice = "

  (* Menu 4 - listar cromos do colecionador*)
  method listarCromos =
    self#listarColecionadores;
    print_string "Indique o indice do colecionador\n";
    let ind = self#lerIndice in
      try (print_endline (List.nth colecionadores (ind-1))#toStringCompleto)
      with
        |Failure "nth" 
        |Invalid_argument "List.nth" ->
            printf "Digite um indice entre 1 e %d\n" (List.length colecionadores);
            self#listarCromos

  (*escreve uma lista de cromos*)
  method private escreverListaCromos lista =
    print_endline "Cromo | Pontos";
    let rec aux l =
      match l with
        |[] -> printf "\n";
        |i :: corpo -> 
            printf "%6d| %d\n" (i+1) (cromos#getPontos i);
            aux corpo
    in
      aux lista

  (* Menu 5 - Listar cromos para troca de colecionador*)
  method listarCromosTroca =
    self#listarColecionadores;
    print_string "Indique o indice do colecionador\n";
    let ind = self#lerIndice in
      try 
        (
          printf "Cromos para troca de %s:\n" (List.nth colecionadores (ind-1))#getNome;
          self#escreverListaCromos (List.nth colecionadores (ind-1))#listaTroca
        )
      with
        |Failure "nth" 
        |Invalid_argument "List.nth" ->
            printf "Digite um indice entre 1 e %d\n" (List.length colecionadores);
            self#listarCromosTroca

  (* Menu 6 - Listar cromos procurados por colecionador*)
  method listarCromosProcura =
    self#listarColecionadores;
    print_string "Indique o indice do colecionador\n";
    let ind = self#lerIndice in
      try
        (
          printf "Cromos procurados por %s:\n" (List.nth colecionadores (ind-1))#getNome;
          self#escreverListaCromos (List.nth colecionadores (ind-1))#listaProcura
        )
      with
        |Failure "nth" 
        |Invalid_argument "List.nth" ->
            printf "Digite um indice entre 1 e %d\n" (List.length colecionadores);
            self#listarCromosProcura

  (* Menu 7 - Registar novos cromos de colecionador*)
  method registarCromo =
    self#listarColecionadores;
    print_string "Indique o indice do colecionador\n";
    let indCol = self#lerIndice in
      try
        (
          print_string "Indique o numero do cromo que pretende adicionar\n";
          let indCro = self#lerInteiro "Numero do cromo = " in
            if (indCro > 0 && indCro <= numero) then
              (
                let qt = self#lerInteiro "Quantidade (inteiro positivo ou negativo) = " in
                  (List.nth colecionadores (indCol-1))#adicionaCromo (indCro - 1) qt;
                  cromos#adicionaCromo (indCro -1) qt;
                  print_endline "Quantidade adicionada/subtraida na lista do colecionador e na lista global";
              )
            else
              (
                printf "Nao existe um cromo com o numero %d\n" indCro;
                self#registarCromo
              )
        )
      with
        |Failure "nth" 
        |Invalid_argument "List.nth" ->
            printf "Digite um indice entre 1 e %d\n" (List.length colecionadores);
            self#registarCromo


  (* retorna a pontuação de uma lista de cromos *)
  method private pontuacaoLista lista =
    let rec aux l soma =
      match l with
        |[] -> soma
        |topo :: corpo -> aux corpo (soma + (cromos#getPontos topo) )
    in
      aux lista 0

  (* retorna true se i pertence à lista *)
  method private pertence i lista =
    let rec aux l =
      match l with
        |[]-> false
        |x :: corpo -> 
            if (x = i) then true
            else aux corpo
    in
      aux lista

  (* retorna a intersecao da lista de troca dos colecionadores com a lista de procura *)
  method private intersecao lista1 lista2 =
    let rec aux l1 res =
      match l1 with
        |[]-> List.rev res
        |x :: corpo ->
            if (self#pertence x lista2) then aux corpo (x :: res)
            else aux corpo res
    in
      aux lista1 []

  (* escreve a lista dos colecionadores com o cromo i e cuja lista de procura tem elementos da lista de troca indicada*)
  method private escreverTrocasPossiveis i listaTr =
    let rec aux lista =
      match lista with
        |[]->printf "\n";
        |topo::corpo ->
            let inter = self#intersecao (topo#listaProcura) listaTr in
            let pontosCromo = cromos#getPontos i in
            let pontosInter = self#pontuacaoLista inter in
              if ( (topo#temCromoTroca i) && (pontosInter > pontosCromo) ) then
                (
                  printf "%s:\n" topo#getNome;
                  self#escreverListaCromos inter;
                  aux corpo
                )
              else aux corpo
    in
      aux colecionadores

  (* Menu 8 - Procurar cromo*)
  method procurarCromo =
    self#listarColecionadores;
    print_string "Indique o indice do colecionador\n";
    let indCol = self#lerIndice in
      try
        (
          printf "Cromos procurados por %s:\n" (List.nth colecionadores (indCol-1))#getNome;
          self#escreverListaCromos (List.nth colecionadores (indCol-1))#listaProcura;
          print_string "Indique o numero do cromo que pretende procurar\n";
          let indCro = self#lerInteiro "Numero do cromo = " in
            if (self#pertence indCro (List.nth colecionadores (indCol-1))#listaProcura) then
              (
                printf "Lista de cromos para troca de %s:\n" (List.nth colecionadores (indCol-1))#getNome;
                self#escreverListaCromos (List.nth colecionadores (indCol-1))#listaTroca;
                printf "Lista de colecionadores com cromo %d para troca e respetiva lista de procurados:\n" indCro;
                self#escreverTrocasPossiveis (indCro-1) (List.nth colecionadores (indCol-1))#listaTroca
              )
            else
              (
                printf "Cromo nao procurado pelo colecionador\n";
                self#procurarCromo
              )
        )
      with
        |Failure "nth" 
        |Invalid_argument "List.nth" ->
            printf "Digite um indice entre 1 e %d\n" (List.length colecionadores);
            self#procurarCromo

  (*grava o numero de cromos da colecao*)
  method private gravaNumero ficheiro = fprintf ficheiro "%d\n" numero

  (*grava o telefone e o nome de cada colecionador*)
  method private gravaColecionadores ficheiro =
    let rec aux lista =
      match lista with
        |[] -> fprintf ficheiro "";
        |topo :: corpo ->
            fprintf ficheiro "%s %s\n" topo#getTelemovel topo#getNome;
            aux corpo
    in
      aux colecionadores

  (*grava a quantidade de cada cromo do colecionador indicado*)
  method private gravaQuantidadesCol ficheiro col =
    let rec aux i =
      if (i=numero) then fprintf ficheiro ""
      else 
        (
          fprintf ficheiro "%d\n" (col#getQuantidade i);
          aux (i+1)
        )
    in
      aux 0

  (*grava a quantidade de cada cromo de todos os colecionadores*)
  method private gravaQuantidades ficheiro =
    let rec aux lista =
      match lista with
        |[] -> fprintf ficheiro "";
        |topo :: corpo ->
            self#gravaQuantidadesCol ficheiro topo;
            aux corpo
    in
      aux colecionadores

  (*grava os dados todos em ficheiros*)
  method gravaDados =
    let fout = open_out "numero.txt" in
      self#gravaNumero fout;
      close_out fout;
      let fout = open_out "colecionadores.txt" in
        self#gravaColecionadores fout;
        close_out fout;
        let fout = open_out "quantidades.txt" in
          self#gravaQuantidades fout;
          close_out fout;

end
;;

let novoMenu o n = new menu o n
;;
