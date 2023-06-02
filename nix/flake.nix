{
  inputs = {
    namaka.url = "github:nix-community/namaka/v0.1.1";
  };

  outputs = { self, namaka }: {
    checks = namaka.lib.load {
      src = "${./.}/tests";
      inputs = {
        inherit (self) lex;
      };
    };
    lex = import ./.;
  };
}
