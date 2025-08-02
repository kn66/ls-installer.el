;; Language Server Configuration for ls-installer
;; This file contains server definitions with npm/pip/binary installation support

;; List format for servers with npm/pip/binary support
(( :name "typescript-language-server"
   :description "TypeScript & JavaScript language server"
   :package-name "typescript-language-server"
   :install-script ls--install-npm-package
   :lsp-config
   ( :server-id 'ts-ls
     :major-modes '(typescript-mode typescript-ts-mode js-mode js-ts-mode)
     :priority 0))

 ( :name "pyright"
   :description "Python language server"
   :package-name "pyright"
   :install-script ls--install-npm-package
   :lsp-config
   ( :server-id 'pyright
     :major-modes '(python-mode python-ts-mode)
     :priority 0))

 ( :name "pylsp"
   :description "Python LSP Server"
   :package-name "python-lsp-server"
   :install-script ls--install-pip-package
   :lsp-config
   ( :server-id 'pylsp
     :major-modes '(python-mode python-ts-mode)
     :priority 1))

 ( :name "rust-analyzer"
   :description "Rust language server"
   :install-script ls--install-github-release
   :github-repo "rust-lang/rust-analyzer"
   :executable-name "rust-analyzer"
   :lsp-config
   ( :server-id 'rust-analyzer
     :major-modes '(rust-mode rust-ts-mode)
     :priority 0))

 ( :name "gopls"
   :description "Go language server (official)"
   :install-script ls--install-go-binary
   :package-name "golang.org/x/tools/gopls@latest"
   :lsp-config
   ( :server-id 'gopls
     :major-modes '(go-mode go-ts-mode)
     :priority 0))

 ( :name "golangci-lint-langserver"
   :description "Go linter language server for golangci-lint"
   :install-script ls--install-go-binary
   :package-name "github.com/nametake/golangci-lint-langserver@latest"
   :lsp-config
   ( :server-id 'golangci-lint-ls
     :major-modes '(go-mode go-ts-mode)
     :priority 1))

 ( :name "go-outline"
   :description "Go outline tool for symbol navigation"
   :install-script ls--install-go-binary
   :package-name "github.com/ramya-rao-a/go-outline@latest"
   :lsp-config
   ( :server-id 'go-outline
     :major-modes '(go-mode go-ts-mode)
     :priority 2))

 ( :name "gomodifytags"
   :description "Go tool to modify struct field tags"
   :install-script ls--install-go-binary
   :package-name "github.com/fatih/gomodifytags@latest"
   :lsp-config
   ( :server-id 'gomodifytags
     :major-modes '(go-mode go-ts-mode)
     :priority 3))

 ( :name "gotests"
   :description "Go test generation tool"
   :install-script ls--install-go-binary
   :package-name "github.com/cweill/gotests/...@latest"
   :lsp-config
   ( :server-id 'gotests
     :major-modes '(go-mode go-ts-mode)
     :priority 4))

 ( :name "impl"
   :description "Go tool for generating method stubs"
   :install-script ls--install-go-binary
   :package-name "github.com/josharian/impl@latest"
   :lsp-config
   ( :server-id 'impl
     :major-modes '(go-mode go-ts-mode)
     :priority 5))

 ( :name "goimports"
   :description "Go tool for automatically fixing imports"
   :install-script ls--install-go-binary
   :package-name "golang.org/x/tools/cmd/goimports@latest"
   :lsp-config
   ( :server-id 'goimports
     :major-modes '(go-mode go-ts-mode)
     :priority 6))

 ( :name "gofumpt"
   :description "Stricter gofmt for Go code formatting"
   :install-script ls--install-go-binary
   :package-name "mvdan.cc/gofumpt@latest"
   :lsp-config
   ( :server-id 'gofumpt
     :major-modes '(go-mode go-ts-mode)
     :priority 7))

 ( :name "staticcheck"
   :description "Go static analysis tool"
   :install-script ls--install-go-binary
   :package-name "honnef.co/go/tools/cmd/staticcheck@latest"
   :lsp-config
   ( :server-id 'staticcheck
     :major-modes '(go-mode go-ts-mode)
     :priority 8))

 ( :name "golangci-lint"
   :description "Go linters runner"
   :install-script ls--install-github-release
   :github-repo "golangci/golangci-lint"
   :executable-name "golangci-lint"
   :strip-components 1
   :lsp-config
   ( :server-id 'golangci-lint
     :major-modes '(go-mode go-ts-mode)
     :priority 9))

 ( :name "clangd"
   :description "C/C++ language server"
   :install-script ls--install-github-release
   :github-repo "clangd/clangd"
   :executable-name "bin/clangd"
   :strip-components 1
   :lsp-config
   ( :server-id 'clangd
     :major-modes '(c-mode c++-mode c-ts-mode c++-ts-mode)
     :priority 0))

 ( :name "zls"
   :description "Zig language server"
   :install-script ls--install-github-release
   :github-repo "zigtools/zls"
   :executable-name "zls"
   :strip-components 1
   :lsp-config
   ( :server-id 'zls
     :major-modes '(zig-mode)
     :priority 0))

 ( :name "lua-language-server"
   :description "Lua language server"
   :install-script ls--install-github-release
   :github-repo "LuaLS/lua-language-server"
   :executable-name "bin/lua-language-server"
   :strip-components 0
   :lsp-config
   ( :server-id 'lua-ls
     :major-modes '(lua-mode)
     :priority 0))

 ( :name "jdtls"
   :description "Java language server"
   :install-script ls--install-binary
   :binary-url "https://download.eclipse.org/jdtls/milestones/1.40.0/jdt-language-server-1.40.0-202409261450.tar.gz"
   :target-subdir "eclipse.jdt.ls"
   :executable-name "bin/jdtls"
   :strip-components 0
   :lsp-config
   ( :server-id 'jdtls
     :major-modes '(java-mode java-ts-mode)
     :priority 0))

 ( :name "csharp-ls"
   :description "C# language server"
   :package-name "csharp-ls"
   :install-script ls--install-dotnet-tool
   :lsp-config
   ( :server-id 'csharp-ls
     :major-modes '(csharp-mode csharp-ts-mode)
     :priority 0))

 ( :name "omnisharp"
   :description "OmniSharp C# language server"
   :install-script ls--install-github-release
   :github-repo "OmniSharp/omnisharp-roslyn"
   :executable-name "OmniSharp"
   :strip-components 0
   :lsp-config
   ( :server-id 'omnisharp
     :major-modes '(csharp-mode csharp-ts-mode)
     :priority 1))

 ( :name "vscode-html-language-server"
   :description "HTML language server"
   :package-name "vscode-langservers-extracted"
   :install-script ls--install-npm-package
   :lsp-config
   ( :server-id 'html-ls
     :major-modes '(html-mode web-mode mhtml-mode)
     :priority 0))

 ( :name "vscode-css-language-server"
   :description "CSS language server"
   :package-name "vscode-langservers-extracted"
   :install-script ls--install-npm-package
   :lsp-config
   ( :server-id 'css-ls
     :major-modes '(css-mode scss-mode sass-mode less-css-mode)
     :priority 0))

 ( :name "vscode-json-language-server"
   :description "JSON language server"
   :package-name "vscode-langservers-extracted"
   :install-script ls--install-npm-package
   :lsp-config
   ( :server-id 'json-ls
     :major-modes '(json-mode json-ts-mode)
     :priority 0))

 ( :name "vue-language-server"
   :description "Vue.js language server"
   :package-name "@vue/language-server"
   :install-script ls--install-npm-package
   :lsp-config
   ( :server-id 'vue-ls
     :major-modes '(vue-mode)
     :priority 0))

 ( :name "svelte-language-server"
   :description "Svelte language server"
   :package-name "svelte-language-server"
   :install-script ls--install-npm-package
   :lsp-config
   ( :server-id 'svelte-ls
     :major-modes '(svelte-mode)
     :priority 0))

 ( :name "bash-language-server"
   :description "Bash language server"
   :package-name "bash-language-server"
   :install-script ls--install-npm-package
   :lsp-config
   ( :server-id 'bash-ls
     :major-modes '(sh-mode shell-script-mode)
     :priority 0))

 ( :name "yaml-language-server"
   :description "YAML language server"
   :package-name "yaml-language-server"
   :install-script ls--install-npm-package
   :lsp-config
   ( :server-id 'yaml-ls
     :major-modes '(yaml-mode yaml-ts-mode)
     :priority 0))

 ( :name "diagnostic-languageserver"
   :description "Diagnostic language server for linters"
   :package-name "diagnostic-languageserver"
   :install-script ls--install-npm-package
   :lsp-config
   ( :server-id 'diagnostic-ls
     :major-modes '(text-mode)
     :priority 0))

 ( :name "jedi-language-server"
   :description "Python language server using Jedi"
   :package-name "jedi-language-server"
   :install-script ls--install-pip-package
   :lsp-config
   ( :server-id 'jedi-ls
     :major-modes '(python-mode python-ts-mode)
     :priority 2))

 ( :name "cmake-language-server"
   :description "CMake language server"
   :package-name "cmake-language-server"
   :install-script ls--install-pip-package
   :lsp-config
   ( :server-id 'cmake-ls
     :major-modes '(cmake-mode)
     :priority 0))

 ( :name "fortls"
   :description "Fortran language server"
   :package-name "fortls"
   :install-script ls--install-pip-package
   :lsp-config
   ( :server-id 'fortls
     :major-modes '(fortran-mode f90-mode)
     :priority 0)))
