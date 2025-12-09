{
  nix-helpers ? import ~/repos/nix-helpers { },
}:
nix-helpers.shellWithHooks {
  name = "warbo-emacs-d";
  src = ./.;
}
