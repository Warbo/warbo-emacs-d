(ert-deftest warbo-haskell-tng-loaded ()
  "Make sure haskell-tng is enabled for .hs buffers"
  (with-temp-buffer
    (rename-buffer "warbo-haskell-tng-loaded.hs")
    (normal-mode)
    (should (equal major-mode 'haskell-tng-mode))))
