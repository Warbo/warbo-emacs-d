{
  nix-helpers ? import (fetchGitIPFS nix-helpers-tree) { },
  nix-helpers-tree ? {
    # git-ref: pkipfs::yhqn1j1d7i5xb7456639i4uw1e5deeuddjsnyrow46zuxipwgijo master^{tree}
    sha1 = "7ebeb58c22b741df1cd353c0239022b64c68558f";
  },
  fetchGitIPFS ? import (
    with rec {
      # The version of fetchGitIPFS.nix. Shouldn't need updating often.
      hash = "sha256-Cd+/MvPeFksqi4uZ9SaeHEIHKQH0UJTcl6w65TIw3WA=";
      cid = "f01551220${
        builtins.convertHash {
          inherit hash;
          hashAlgo = "sha256";
          toHashFormat = "base16";
        }
      }";

      # fetchurl only takes one URL, so allow it to be overridden by env var.
      override = builtins.getEnv "IPFS_GATEWAY";
      gateway = if override == "" then "https://ipfs.io" else override;
    };
    import <nix/fetchurl.nix> {
      inherit hash;
      url = "${gateway}/ipfs/${cid}";
    }
  ),
}:
nix-helpers.shellWithHooks {
  name = "warbo-emacs-d";
  src = ./.;
}
