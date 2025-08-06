;; Language Server Configuration for lsp-installer
;; This file contains server definitions with unified configuration format

;; Unified format for all servers:
;; :name - Server identifier
;; :description - Human readable description
;; :install-method - Installation method: "npm", "pip", "go", "github", "binary", "dotnet"
;; :source - Package name, GitHub repo, or URL depending on install method
;; :executable - Executable name/path (always required)
;; :version - Package version (optional)
;; :options - Method-specific options (optional)
;; :lsp-config - LSP configuration

(( :name "typescript-language-server"
   :description "TypeScript & JavaScript language server"
   :install-method "npm"
   :source "typescript-language-server"
   :executable "typescript-language-server"
   :lsp-config
   ( :server-id 'ts-ls
     :major-modes '(typescript-mode typescript-ts-mode js-mode js-ts-mode)
     :priority 0))

 ( :name "pyright"
   :description "Python language server"
   :install-method "npm"
   :source "pyright"
   :executable "pyright"
   :lsp-config
   ( :server-id 'pyright
     :major-modes '(python-mode python-ts-mode)
     :priority 0))

 ( :name "pylsp"
   :description "Python LSP Server"
   :install-method "pip"
   :source "python-lsp-server"
   :executable "pylsp"
   :lsp-config
   ( :server-id 'pylsp
     :major-modes '(python-mode python-ts-mode)
     :priority 1))

 ( :name "rust-analyzer"
   :description "Rust language server"
   :install-method "github"
   :source "rust-lang/rust-analyzer"
   :executable "rust-analyzer"
   :lsp-config
   ( :server-id 'rust-analyzer
     :major-modes '(rust-mode rust-ts-mode)
     :priority 0))

 ( :name "gopls"
   :description "Go language server (official)"
   :install-method "go"
   :source "golang.org/x/tools/gopls@latest"
   :executable "gopls"
   :lsp-config
   ( :server-id 'gopls
     :major-modes '(go-mode go-ts-mode)
     :priority 0))

 ( :name "golangci-lint-langserver"
   :description "Go linter language server for golangci-lint"
   :install-method "go"
   :source "github.com/nametake/golangci-lint-langserver@latest"
   :executable "golangci-lint-langserver"
   :lsp-config
   ( :server-id 'golangci-lint-ls
     :major-modes '(go-mode go-ts-mode)
     :priority 1))

 ( :name "go-outline"
   :description "Go outline tool for symbol navigation"
   :install-method "go"
   :source "github.com/ramya-rao-a/go-outline@latest"
   :executable "go-outline"
   :lsp-config
   ( :server-id 'go-outline
     :major-modes '(go-mode go-ts-mode)
     :priority 2))

 ( :name "gomodifytags"
   :description "Go tool to modify struct field tags"
   :install-method "go"
   :source "github.com/fatih/gomodifytags@latest"
   :executable "gomodifytags"
   :lsp-config
   ( :server-id 'gomodifytags
     :major-modes '(go-mode go-ts-mode)
     :priority 3))

 ( :name "gotests"
   :description "Go test generation tool"
   :install-method "go"
   :source "github.com/cweill/gotests/...@latest"
   :executable "gotests"
   :lsp-config
   ( :server-id 'gotests
     :major-modes '(go-mode go-ts-mode)
     :priority 4))

 ( :name "impl"
   :description "Go tool for generating method stubs"
   :install-method "go"
   :source "github.com/josharian/impl@latest"
   :executable "impl"
   :lsp-config
   ( :server-id 'impl
     :major-modes '(go-mode go-ts-mode)
     :priority 5))

 ( :name "goimports"
   :description "Go tool for automatically fixing imports"
   :install-method "go"
   :source "golang.org/x/tools/cmd/goimports@latest"
   :executable "goimports"
   :lsp-config
   ( :server-id 'goimports
     :major-modes '(go-mode go-ts-mode)
     :priority 6))

 ( :name "gofumpt"
   :description "Stricter gofmt for Go code formatting"
   :install-method "go"
   :source "mvdan.cc/gofumpt@latest"
   :executable "gofumpt"
   :lsp-config
   ( :server-id 'gofumpt
     :major-modes '(go-mode go-ts-mode)
     :priority 7))

 ( :name "staticcheck"
   :description "Go static analysis tool"
   :install-method "go"
   :source "honnef.co/go/tools/cmd/staticcheck@latest"
   :executable "staticcheck"
   :lsp-config
   ( :server-id 'staticcheck
     :major-modes '(go-mode go-ts-mode)
     :priority 8))

 ( :name "golangci-lint"
   :description "Go linters runner"
   :install-method "github"
   :source "golangci/golangci-lint"
   :executable "golangci-lint"
   :options (:strip-components 1)
   :lsp-config
   ( :server-id 'golangci-lint
     :major-modes '(go-mode go-ts-mode)
     :priority 9))

 ( :name "clangd"
   :description "C/C++ language server"
   :install-method "github"
   :source "clangd/clangd"
   :executable "bin/clangd"
   :options (:strip-components 1)
   :lsp-config
   ( :server-id 'clangd
     :major-modes '(c-mode c++-mode c-ts-mode c++-ts-mode)
     :priority 0))

 ( :name "zls"
   :description "Zig language server"
   :install-method "github"
   :source "zigtools/zls"
   :executable "zls"
   :options (:strip-components 1)
   :lsp-config
   ( :server-id 'zls
     :major-modes '(zig-mode)
     :priority 0))

 ( :name "lua-language-server"
   :description "Lua language server"
   :install-method "github"
   :source "LuaLS/lua-language-server"
   :executable "bin/lua-language-server"
   :options (:strip-components 0)
   :lsp-config
   ( :server-id 'lua-ls
     :major-modes '(lua-mode)
     :priority 0))

 ( :name "jdtls"
   :description "Java language server"
   :install-method "binary"
   :source "https://download.eclipse.org/jdtls/milestones/1.40.0/jdt-language-server-1.40.0-202409261450.tar.gz"
   :executable "bin/jdtls"
   :options (:target-subdir "eclipse.jdt.ls" :strip-components 0)
   :lsp-config
   ( :server-id 'jdtls
     :major-modes '(java-mode java-ts-mode)
     :priority 0))

 ( :name "csharp-ls"
   :description "C# language server"
   :install-method "dotnet"
   :source "csharp-ls"
   :executable "csharp-ls"
   :lsp-config
   ( :server-id 'csharp-ls
     :major-modes '(csharp-mode csharp-ts-mode)
     :priority 0))

 ( :name "omnisharp"
   :description "OmniSharp C# language server"
   :install-method "github"
   :source "OmniSharp/omnisharp-roslyn"
   :executable "OmniSharp"
   :options (:strip-components 0)
   :lsp-config
   ( :server-id 'omnisharp
     :major-modes '(csharp-mode csharp-ts-mode)
     :priority 1))

 ( :name "vscode-html-language-server"
   :description "HTML language server"
   :install-method "npm"
   :source "vscode-langservers-extracted"
   :executable "vscode-html-language-server"
   :lsp-config
   ( :server-id 'html-ls
     :major-modes '(html-mode web-mode mhtml-mode)
     :priority 0))

 ( :name "vscode-css-language-server"
   :description "CSS language server"
   :install-method "npm"
   :source "vscode-langservers-extracted"
   :executable "vscode-css-language-server"
   :lsp-config
   ( :server-id 'css-ls
     :major-modes '(css-mode scss-mode sass-mode less-css-mode)
     :priority 0))

 ( :name "vscode-json-language-server"
   :description "JSON language server"
   :install-method "npm"
   :source "vscode-langservers-extracted"
   :executable "vscode-json-language-server"
   :lsp-config
   ( :server-id 'json-ls
     :major-modes '(json-mode json-ts-mode)
     :priority 0))

 ( :name "vue-language-server"
   :description "Vue.js language server"
   :install-method "npm"
   :source "@vue/language-server"
   :executable "vue-language-server"
   :lsp-config
   ( :server-id 'vue-ls
     :major-modes '(vue-mode)
     :priority 0))

 ( :name "svelte-language-server"
   :description "Svelte language server"
   :install-method "npm"
   :source "svelte-language-server"
   :executable "svelte-language-server"
   :lsp-config
   ( :server-id 'svelte-ls
     :major-modes '(svelte-mode)
     :priority 0))

 ( :name "bash-language-server"
   :description "Bash language server"
   :install-method "npm"
   :source "bash-language-server"
   :executable "bash-language-server"
   :lsp-config
   ( :server-id 'bash-ls
     :major-modes '(sh-mode shell-script-mode)
     :priority 0))

 ( :name "yaml-language-server"
   :description "YAML language server"
   :install-method "npm"
   :source "yaml-language-server"
   :executable "yaml-language-server"
   :lsp-config
   ( :server-id 'yaml-ls
     :major-modes '(yaml-mode yaml-ts-mode)
     :priority 0))

 ( :name "diagnostic-languageserver"
   :description "Diagnostic language server for linters"
   :install-method "npm"
   :source "diagnostic-languageserver"
   :executable "diagnostic-languageserver"
   :lsp-config
   ( :server-id 'diagnostic-ls
     :major-modes '(text-mode)
     :priority 0))

 ( :name "jedi-language-server"
   :description "Python language server using Jedi"
   :install-method "pip"
   :source "jedi-language-server"
   :executable "jedi-language-server"
   :lsp-config
   ( :server-id 'jedi-ls
     :major-modes '(python-mode python-ts-mode)
     :priority 2))

 ( :name "cmake-language-server"
   :description "CMake language server"
   :install-method "pip"
   :source "cmake-language-server"
   :executable "cmake-language-server"
   :lsp-config
   ( :server-id 'cmake-ls
     :major-modes '(cmake-mode)
     :priority 0))

 ( :name "fortls"
   :description "Fortran language server"
   :install-method "pip"
   :source "fortls"
   :executable "fortls"
   :lsp-config
   ( :server-id 'fortls
     :major-modes '(fortran-mode f90-mode)
     :priority 0)))
