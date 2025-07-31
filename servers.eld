;; servers.eld --- Language server definitions for eglot-installer -- lexical-binding: t; --

;; This file contains language server definitions in ELD format.
;; Each server definition is a cons cell with the server name as key
;; and a plist of configuration options as value.

(
 ;; Rust
 (rust-analyzer
  :download-url "https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-{arch}-unknown-{system}-gnu.gz"
  :executable-name "rust-analyzer"
  :decompress gzip
  :modes (rust-mode rust-ts-mode)
  :platforms (linux macos)
  :condition (lambda () (executable-find "rustc")))

 ;; CMake
 (cmake-language-server
  :installer pip
  :package-name "cmake-language-server"
  :executable-name "cmake-language-server"
  :modes (cmake-mode cmake-ts-mode))

 ;; Vim
 (vim-language-server
  :installer npm
  :package-name "vim-language-server"
  :executable-name "vim-language-server"
  :modes (vimrc-mode)
  :args ("--stdio"))

 ;; Python
 (pylsp
  :installer pip
  :package-name "python-lsp-server[all]"
  :executable-name "pylsp"
  :modes (python-mode python-ts-mode))

 (pyls
  :installer pip
  :package-name "python-language-server"
  :executable-name "pyls"
  :modes (python-mode python-ts-mode))

 (basedpyright
  :installer npm
  :package-name "basedpyright"
  :executable-name "basedpyright-langserver"
  :modes (python-mode python-ts-mode)
  :args ("--stdio"))

 (pyright
  :installer npm
  :package-name "pyright"
  :executable-name "pyright-langserver"
  :modes (python-mode python-ts-mode)
  :args ("--stdio"))

 (jedi-language-server
  :installer pip
  :package-name "jedi-language-server"
  :executable-name "jedi-language-server"
  :modes (python-mode python-ts-mode))

 (ruff-lsp
  :installer pip
  :package-name "ruff-lsp"
  :executable-name "ruff-lsp"
  :modes (python-mode python-ts-mode))

 ;; JSON
 (vscode-json-language-server
  :installer npm
  :package-name "vscode-langservers-extracted"
  :executable-name "vscode-json-language-server"
  :modes (js-json-mode json-mode json-ts-mode jsonc-mode)
  :args ("--stdio"))

 (json-languageserver
  :installer npm
  :package-name "vscode-langservers-extracted"
  :executable-name "vscode-json-language-server"
  :modes (js-json-mode json-mode json-ts-mode jsonc-mode)
  :args ("--stdio"))

 ;; JavaScript/TypeScript
 (typescript-language-server
  :installer npm
  :package-name "typescript-language-server"
  :executable-name "typescript-language-server"
  :modes (js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode)
  :args ("--stdio"))

 ;; Bash/Shell
 (bash-language-server
  :installer npm
  :package-name "bash-language-server"
  :executable-name "bash-language-server"
  :modes (bash-ts-mode sh-mode)
  :args ("start"))

 ;; PHP
 (phpactor
  :installer composer
  :package-name "phpactor/phpactor"
  :executable-name "phpactor"
  :modes (php-mode phps-mode php-ts-mode)
  :args ("language-server")
  :condition (lambda () (executable-find "composer")))

 (php-language-server
  :installer composer
  :package-name "felixfbecker/language-server"
  :executable-name "php"
  :modes (php-mode phps-mode php-ts-mode)
  :args ("vendor/felixfbecker/language-server/bin/php-language-server.php")
  :condition (lambda () (executable-find "php")))

 ;; C/C++
 (clangd
  :download-url "https://github.com/clangd/clangd/releases/latest/download/clangd-{system}-{arch}.zip"
  :executable-name "clangd"
  :decompress zip
  :modes (c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
  :platforms (linux macos windows))

 (ccls
  :installer system
  :executable-name "ccls"
  :modes (c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
  :condition (lambda () (executable-find "ccls")))

 ;; OCaml
 (ocamllsp
  :installer opam
  :package-name "ocaml-lsp-server"
  :executable-name "ocamllsp"
  :modes (caml-mode tuareg-mode reason-mode)
  :condition (lambda () (executable-find "opam")))

 ;; Ruby
 (solargraph
  :installer gem
  :package-name "solargraph"
  :executable-name "solargraph"
  :modes (ruby-mode ruby-ts-mode)
  :args ("socket" "--port" ":autoport")
  :condition (lambda () (executable-find "ruby")))

 ;; Haskell
 (haskell-language-server-wrapper
  :installer cabal
  :package-name "haskell-language-server"
  :executable-name "haskell-language-server-wrapper"
  :modes (haskell-mode)
  :args ("--lsp")
  :condition (lambda () (executable-find "ghc")))

 ;; Elm
 (elm-language-server
  :installer npm
  :package-name "@elm-tooling/elm-language-server"
  :executable-name "elm-language-server"
  :modes (elm-mode)
  :condition (lambda () (executable-find "elm")))

 ;; Mint
 (mint-ls
  :executable-name "mint"
  :modes (mint-mode)
  :args ("ls")
  :condition (lambda () (executable-find "mint"))
  :installer system)

 ;; Kotlin
 (kotlin-language-server
  :download-url "https://github.com/fwcd/kotlin-language-server/releases/latest/download/server.zip"
  :executable-name "kotlin-language-server"
  :decompress zip
  :modes (kotlin-mode kotlin-ts-mode)
  :platforms (linux macos windows))

 ;; Go
 (gopls
  :installer go
  :package-name "golang.org/x/tools/gopls@latest"
  :executable-name "gopls"
  :modes (go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode)
  :condition (lambda () (executable-find "go")))

 ;; R
 (r-languageserver
  :executable-name "R"
  :args ("--slave" "-e" "languageserver::run()")
  :modes (R-mode ess-r-mode)
  :condition (lambda () (executable-find "R"))
  :installer system)

 ;; Java
 (jdtls
  :download-url "https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz"
  :decompress tar.gz
  :executable-name "bin/jdtls"
  :modes (java-mode java-ts-mode)
  :post-install-hook lsp-installer--setup-jdtls)

 ;; Dart
 (dart-language-server
  :executable-name "dart"
  :args ("language-server" "--client-id" "emacs.eglot-dart")
  :modes (dart-mode dart-ts-mode)
  :condition (lambda () (executable-find "dart"))
  :installer system)

 ;; Elixir
 (elixir-ls
  :download-url "https://github.com/elixir-lsp/elixir-ls/releases/latest/download/elixir-ls.zip"
  :executable-name "language_server.sh"
  :decompress zip
  :modes (elixir-mode elixir-ts-mode heex-ts-mode)
  :platforms (linux macos)
  :condition (lambda () (executable-find "elixir")))

 (lexical
  :download-url "https://github.com/lexical-lsp/lexical/releases/latest/download/lexical.zip"
  :executable-name "start_lexical.sh"
  :decompress zip
  :modes (elixir-mode elixir-ts-mode heex-ts-mode)
  :platforms (linux macos)
  :condition (lambda () (executable-find "elixir")))

 ;; Ada
 (ada-language-server
  :download-url "https://github.com/AdaCore/ada_language_server/releases/latest/download/als-{version}-{system}-{arch}.tar.gz"
  :executable-name "ada_language_server"
  :decompress tar.gz
  :modes (ada-mode ada-ts-mode)
  :platforms (linux macos windows))

 (ada-language-server-gpr
  :download-url "https://github.com/AdaCore/ada_language_server/releases/latest/download/als-{version}-{system}-{arch}.tar.gz"
  :executable-name "ada_language_server"
  :decompress tar.gz
  :modes (gpr-mode gpr-ts-mode)
  :args ("--language-gpr")
  :platforms (linux macos windows))

 ;; Scala
 (metals
  :installer coursier
  :package-name "org.scalameta:metals_2.13:latest.release"
  :executable-name "metals"
  :modes (scala-mode)
  :condition (lambda () (executable-find "scala")))

 (metals-emacs
  :installer coursier
  :package-name "org.scalameta:metals_2.13:latest.release"
  :executable-name "metals-emacs"
  :modes (scala-mode)
  :condition (lambda () (executable-find "scala")))

 ;; Racket
 (racket-langserver
  :executable-name "racket"
  :args ("-l" "racket-langserver")
  :modes (racket-mode)
  :condition (lambda () (executable-find "racket"))
  :installer system)

 ;; LaTeX
 (digestif
  :installer system
  :executable-name "digestif"
  :modes (latex-mode plain-tex-mode context-mode texinfo-mode bibtex-mode tex-mode)
  :condition (lambda () (executable-find "digestif")))

 (texlab
  :installer cargo
  :package-name "texlab"
  :executable-name "texlab"
  :modes (latex-mode plain-tex-mode context-mode texinfo-mode bibtex-mode tex-mode))

 ;; Erlang
 (erlang-ls
  :download-url "https://github.com/erlang-ls/erlang_ls/releases/latest/download/erlang_ls-{version}-{system}-{arch}.tar.gz"
  :executable-name "erlang_ls"
  :decompress tar.gz
  :modes (erlang-mode)
  :args ("--transport" "stdio")
  :condition (lambda () (executable-find "erl")))

 ;; YAML
 (yaml-language-server
  :installer npm
  :package-name "yaml-language-server"
  :executable-name "yaml-language-server"
  :modes (yaml-ts-mode yaml-mode)
  :args ("--stdio"))

 ;; Nix
 (nil
  :download-url "https://github.com/oxalica/nil/releases/latest/download/nil-{arch}-unknown-{system}-musl"
  :executable-name "nil"
  :modes (nix-mode)
  :platforms (linux))

 (rnix-lsp
  :installer cargo
  :package-name "rnix-lsp"
  :executable-name "rnix-lsp"
  :modes (nix-mode))

 (nixd
  :download-url "https://github.com/nix-community/nixd/releases/latest/download/nixd-{system}-{arch}"
  :executable-name "nixd"
  :modes (nix-mode)
  :platforms (linux macos))

 ;; Nickel
 (nls
  :installer cargo
  :package-name "nickel-lang-lsp"
  :executable-name "nls"
  :modes (nickel-mode))

 ;; Nushell
 (nu-lsp
  :executable-name "nu"
  :args ("--lsp")
  :modes (nushell-mode nushell-ts-mode)
  :condition (lambda () (executable-find "nu"))
  :installer system)

 ;; GDScript
 (gdscript-lsp
  :executable-name "localhost"
  :port 6008
  :modes (gdscript-mode)
  :installer system)

 ;; Fennel
 (fennel-ls
  :installer luarocks
  :package-name "fennel-ls"
  :executable-name "fennel-ls"
  :modes (fennel-mode)
  :condition (lambda () (executable-find "luarocks")))

 ;; Move
 (move-analyzer
  :installer cargo
  :package-name "move-analyzer"
  :executable-name "move-analyzer"
  :modes (move-mode))

 ;; Fortran
 (fortls
  :installer pip
  :package-name "fortls"
  :executable-name "fortls"
  :modes (fortran-mode f90-mode))

 ;; Futhark
 (futhark-lsp
  :executable-name "futhark"
  :args ("lsp")
  :modes (futhark-mode)
  :condition (lambda () (executable-find "futhark"))
  :installer system)

 ;; Lua
 (lua-language-server
  :download-url "https://github.com/LuaLS/lua-language-server/releases/latest/download/lua-language-server-{version}-{system}-{arch}.tar.gz"
  :executable-name "bin/lua-language-server"
  :decompress tar.gz
  :modes (lua-mode lua-ts-mode)
  :platforms (linux macos windows))

 (lua-lsp
  :installer luarocks
  :package-name "lua-lsp"
  :executable-name "lua-lsp"
  :modes (lua-mode lua-ts-mode)
  :condition (lambda () (executable-find "luarocks")))

 ;; Yang
 (yang-language-server
  :installer npm
  :package-name "yang-language-server"
  :executable-name "yang-language-server"
  :modes (yang-mode))

 ;; Zig
 (zls
  :download-url "https://github.com/zigtools/zls/releases/latest/download/zls-{arch}-{system}.tar.xz"
  :executable-name "zls"
  :decompress tar.xz
  :modes (zig-mode zig-ts-mode)
  :condition (lambda () (executable-find "zig")))

 ;; CSS
 (vscode-css-language-server
  :installer npm
  :package-name "vscode-langservers-extracted"
  :executable-name "vscode-css-language-server"
  :modes (css-mode css-ts-mode)
  :args ("--stdio"))

 (css-languageserver
  :installer npm
  :package-name "vscode-langservers-extracted"
  :executable-name "vscode-css-language-server"
  :modes (css-mode css-ts-mode)
  :args ("--stdio"))

 ;; HTML
 (vscode-html-language-server
  :installer npm
  :package-name "vscode-langservers-extracted"
  :executable-name "vscode-html-language-server"
  :modes (html-mode)
  :args ("--stdio"))

 (html-languageserver
  :installer npm
  :package-name "vscode-langservers-extracted"
  :executable-name "vscode-html-language-server"
  :modes (html-mode)
  :args ("--stdio"))

 ;; Dockerfile
 (docker-langserver
  :installer npm
  :package-name "dockerfile-language-server-nodejs"
  :executable-name "docker-langserver"
  :modes (dockerfile-mode dockerfile-ts-mode)
  :args ("--stdio"))

 ;; Clojure
 (clojure-lsp
  :download-url "https://github.com/clojure-lsp/clojure-lsp/releases/latest/download/clojure-lsp-native-{system}-amd64.zip"
  :executable-name "clojure-lsp"
  :decompress zip
  :modes (clojure-mode clojurescript-mode clojurec-mode clojure-ts-mode)
  :platforms (linux macos))

 ;; C#
 (omnisharp
  :download-url "https://github.com/OmniSharp/omnisharp-roslyn/releases/latest/download/omnisharp-{system}-{arch}.zip"
  :executable-name "OmniSharp"
  :decompress zip
  :modes (csharp-mode csharp-ts-mode)
  :args ("-lsp")
  :platforms (linux macos windows))

 (csharp-ls
  :installer dotnet
  :package-name "csharp-ls"
  :executable-name "csharp-ls"
  :modes (csharp-mode csharp-ts-mode)
  :platforms (linux macos windows))

 ;; PureScript
 (purescript-language-server
  :installer npm
  :package-name "purescript-language-server"
  :executable-name "purescript-language-server"
  :modes (purescript-mode)
  :args ("--stdio")
  :condition (lambda () (executable-find "purs")))

 ;; Perl
 (perl-languageserver
  :executable-name "perl"
  :args ("-MPerl::LanguageServer" "-e" "Perl::LanguageServer::run")
  :modes (perl-mode cperl-mode)
  :condition (lambda () (executable-find "perl"))
  :installer system)

 ;; Markdown
 (marksman
  :download-url "https://github.com/artempyanykh/marksman/releases/latest/download/marksman-{system}-{arch}"
  :executable-name "marksman"
  :modes (markdown-mode)
  :args ("server")
  :platforms (linux macos))

 (vscode-markdown-language-server
  :installer npm
  :package-name "@vscode/markdown-language-server"
  :executable-name "vscode-markdown-language-server"
  :modes (markdown-mode)
  :args ("--stdio"))

 ;; GraphViz DOT
 (dot-language-server
  :installer npm
  :package-name "dot-language-server"
  :executable-name "dot-language-server"
  :modes (graphviz-dot-mode)
  :args ("--stdio"))

 ;; Terraform
 (terraform-ls
  :download-url "https://releases.hashicorp.com/terraform-ls/{version}/terraform-ls_{version}_{system}_{arch}.zip"
  :executable-name "terraform-ls"
  :decompress zip
  :modes (terraform-mode)
  :args ("serve"))

 ;; Uiua
 (uiua-lsp
  :executable-name "uiua"
  :args ("lsp")
  :modes (uiua-ts-mode uiua-mode)
  :condition (lambda () (executable-find "uiua"))
  :installer system)

 ;; SML
 (millet-ls
  :installer cargo
  :package-name "millet-ls"
  :executable-name "millet-ls"
  :modes (sml-mode))

 ;; Blueprint
 (blueprint-compiler-lsp
  :executable-name "blueprint-compiler"
  :args ("lsp")
  :modes (blueprint-mode blueprint-ts-mode)
  :condition (lambda () (executable-find "blueprint-compiler"))
  :installer system)

 ;; Odin
 (ols
  :download-url "https://github.com/DanielGavin/ols/releases/latest/download/ols-{system}-{arch}.bin"
  :executable-name "ols"
  :modes (odin-mode odin-ts-mode)
  :platforms (linux macos windows))

 ;; Additional servers from the existing file

 ;; TypeScript alternatives
 (vtsls
  :installer npm
  :package-name "@vtsls/language-server"
  :executable-name "vtsls"
  :modes (js-mode js2-mode js3-mode typescript-mode tsx-mode)
  :args ("--stdio"))

 (eslint-language-server
  :installer npm
  :package-name "vscode-langservers-extracted"
  :executable-name "vscode-eslint-language-server"
  :modes (js-mode js2-mode js3-mode typescript-mode tsx-mode jtsx-mode jtsx-ts-mode)
  :args ("--stdio"))

 (deno
  :download-url "https://github.com/denoland/deno/releases/latest/download/deno-{arch}-unknown-{system}-gnu.zip"
  :executable-name "deno"
  :decompress zip
  :modes (js-mode js2-mode typescript-mode tsx-mode)
  :args ("lsp")
  :condition (lambda () (file-exists-p "deno.json")))

 ;; Additional CSS/HTML
 (tailwindcss-language-server
  :installer npm
  :package-name "@tailwindcss/language-server"
  :executable-name "tailwindcss-language-server"
  :modes (html-mode css-mode web-mode tsx-mode jsx-mode)
  :args ("--stdio"))

 ;; XML
 (lemminx
  :download-url "https://github.com/eclipse/lemminx/releases/latest/download/lemminx-osx-x86_64.zip"
  :executable-name "lemminx"
  :decompress zip
  :modes (xml-mode nxml-mode)
  :platforms (macos))

 ;; Swift
 (sourcekit-lsp
  :executable-name "sourcekit-lsp"
  :modes (swift-mode)
  :condition (lambda () (executable-find "swift"))
  :installer system)

 ;; Vue
 (vue-language-server
  :installer npm
  :package-name "@vue/language-server"
  :executable-name "vue-language-server"
  :modes (vue-mode)
  :args ("--stdio"))

 ;; Svelte
 (svelte-language-server
  :installer npm
  :package-name "svelte-language-server"
  :executable-name "svelteserver"
  :modes (svelte-mode)
  :args ("--stdio"))

 ;; Ruby alternatives
 (ruby-lsp
  :installer gem
  :package-name "ruby-lsp"
  :executable-name "ruby-lsp"
  :modes (ruby-mode ruby-ts-mode)
  :condition (lambda () (executable-find "ruby")))

 ;; F#
 (fsautocomplete
  :download-url "https://github.com/fsharp/FsAutoComplete/releases/latest/download/fsautocomplete.netcore.zip"
  :executable-name "fsautocomplete"
  :decompress zip
  :modes (fsharp-mode)
  :platforms (linux macos windows))

 ;; Reason/ReScript
 (rescript-language-server
  :installer npm
  :package-name "@rescript/language-server"
  :executable-name "rescript-language-server"
  :modes (reason-mode rescript-mode)
  :args ("--stdio"))

 ;; TOML
 (taplo-lsp
  :installer cargo
  :package-name "taplo-cli"
  :executable-name "taplo"
  :modes (toml-mode toml-ts-mode)
  :args ("lsp" "stdio"))

 ;; Prisma
 (prisma-language-server
  :installer npm
  :package-name "@prisma/language-server"
  :executable-name "prisma-language-server"
  :modes (prisma-mode)
  :args ("--stdio"))

 ;; GraphQL
 (graphql-language-service
  :installer npm
  :package-name "graphql-language-service-cli"
  :executable-name "graphql-lsp"
  :modes (graphql-mode)
  :args ("server" "-m" "stream"))

 ;; Protocol Buffers
 (buf-language-server
  :download-url "https://github.com/bufbuild/buf-language-server/releases/latest/download/buf-language-server-{version}-{system}-{arch}.tar.gz"
  :executable-name "buf-language-server"
  :decompress tar.gz
  :modes (protobuf-mode))

 ;; SQL
 (sqls
  :download-url "https://github.com/lighttiger2505/sqls/releases/latest/download/sqls-{version}-{system}-{arch}.tar.gz"
  :executable-name "sqls"
  :decompress tar.gz
  :modes (sql-mode))

 ;; Nim
 (nimlsp
  :installer nimble
  :package-name "nimlsp"
  :executable-name "nimlsp"
  :modes (nim-mode)
  :condition (lambda () (executable-find "nim")))

 ;; Crystal
 (crystalline
  :download-url "https://github.com/elbywan/crystalline/releases/latest/download/crystalline_{system}_{arch}"
  :executable-name "crystalline"
  :modes (crystal-mode)
  :platforms (linux macos))

 ;; Generic/Multi-purpose
 (efm-langserver
  :download-url "https://github.com/mattn/efm-langserver/releases/latest/download/efm-langserver_{version}_{system}_{arch}.tar.gz"
  :executable-name "efm-langserver"
  :decompress tar.gz
  :modes t ; Can be used with any mode
  :platforms (linux macos windows))

 ;; PHP alternatives
 (intelephense
  :installer npm
  :package-name "intelephense"
  :executable-name "intelephense"
  :modes (php-mode php-ts-mode)
  :args ("--stdio"))

 )
