(ert-deftest warbo-tex-comfortable ()
  "Make sure the desired modes, etc. are activated for TeX files."
  (with-temp-buffer
    (rename-buffer "test.tex")
    (set-auto-mode)
    (should (equal major-mode 'latex-mode))
    (should     visual-line-mode)
    (should     flyspell-mode)
    (should-not whitespace-mode)))

(ert-deftest warbo-markdown-comfortable ()
  "Make sure the desired modes, etc. are activated for Markdown files."
  (with-temp-buffer
    (rename-buffer "test.md")
    (set-auto-mode)
    (should     visual-line-mode)
    (should     flyspell-mode)
    (should-not whitespace-mode)))
