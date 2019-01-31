open Printf
;;
open Scanf
;;

let cromoGest () =
  try 
    (
      let fin1 = open_in "numero.txt" in
      let fin2 = open_in "colecionadores.txt" in
      let fin3 = open_in "quantidades.txt" in
      let tamanhoColecao = fscanf fin1 "%d" (fun x -> x) in
        close_in fin1;
        let m = Menu.novoMenu 0 tamanhoColecao in
          m#iniciaColecionadores fin2;
          close_in fin2;
          m#iniciaQuantidades fin3;
          close_in fin3;
          m#iniciaQuantidadeCromos;
          m#iniciaPontos;
          let ok1 = ref true in
            while !ok1 do
              m#principal;
              m#lerOpcao;
              let opcao1 = m#getOpcao in
                if (opcao1 = 1) then
                  m#listarColecao;
                if (opcao1 = 2) then
                  m#listarColecionadores;
                if (opcao1 = 3) then
                  m#registarColecionador;
                if (opcao1 = 4) then
                  m#listarCromos;
                if (opcao1 = 5) then
                  m#listarCromosTroca;
                if (opcao1 = 6) then
                  m#listarCromosProcura;
                if (opcao1 = 7) then
                  m#registarCromo;
                if (opcao1 = 8) then
                  m#procurarCromo;
                if (opcao1 = 9) then
                  (
                    m#gravaDados;
                    ok1 := false;
                  )
            done
    )
  with
    |Sys_error "numero.txt: No such file or directory"
    |Sys_error "colecionadores.txt: No such file or directory"
    |Sys_error "quantidades.txt: No such file or directory" ->
        print_endline "Erro ao abrir um ficheiro de dados!";
;;

cromoGest ()
;;
