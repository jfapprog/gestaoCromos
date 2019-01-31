type colecao =
    < getPontos: int -> int;
      getQuantidade: int -> int;
      setPontos: int -> int -> unit;
      setQuantidade: int -> int -> unit;
      quantidadeTotal: int;
      atualizaTodosPontos: unit;
      adicionaCromo: int -> int -> unit;
      toString: string;
    >

val novaColecao: int -> colecao
