type colecionador =
    < getTelemovel: string;
      getNome: string;
      getQuantidades: int array;
      getQuantidade: int -> int;
      setTelemovel: string -> unit;
      setNome: string -> unit;
      setQuantidades: int array -> unit;
      equal: colecionador -> bool;
      toStringCompleto: string;
      toString : string;
      adicionaCromo: int -> int -> unit;
      temCromoTroca: int -> bool;
      listaTroca: int list;
      listaProcura: int list;
    >

val novoColecionador : string -> string -> int -> colecionador
