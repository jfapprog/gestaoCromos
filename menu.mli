type menu =
    < getOpcao: int;
      iniciaColecionadores: in_channel -> unit;
      iniciaQuantidades: in_channel -> unit;
      iniciaQuantidadeCromos: unit;
      iniciaPontos: unit;
      lerOpcao: unit;
      principal: unit;
      listarColecao: unit;
      listarColecionadores: unit;
      registarColecionador: unit;
      listarCromos: unit;
      listarCromosTroca: unit;
      listarCromosProcura: unit;
      registarCromo: unit;
      procurarCromo: unit;
      gravaDados: unit;
    >

val novoMenu: int -> int -> menu
