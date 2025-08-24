;; Language Server Configuration for lsp-installer
;; This file contains server definitions with unified configuration format

;; Unified format for all servers:
;; :name - Server identifier
;; :description - Human readable description
;; :install-method - Installation method: "npm", "pip", "go", "gem", "github", "binary", "dotnet"
;; :source - Package name, GitHub repo, or URL depending on install method
;; :executable - Executable name/path (always required)
;; :version - Package version (optional)
;; :options - Method-specific options (optional)
;; :path-dirs - List of directories to add to exec-path (relative to server install dir, required)
;; :lsp-config - LSP configuration

(( :name "typescript-language-server"
   :description "TypeScript & JavaScript language server"
   :install-method "npm"
   :source "typescript-language-server"
   :executable "typescript-language-server"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'ts-ls
     :major-modes '(typescript-mode typescript-ts-mode js-mode js-ts-mode)
     :priority 0))

 ( :name "pyright"
   :description "Python language server"
   :install-method "npm"
   :source "pyright"
   :executable "pyright"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'pyright
     :major-modes '(python-mode python-ts-mode)
     :priority 0))

 ( :name "pylsp"
   :description "Python LSP Server"
   :install-method "pip"
   :source "python-lsp-server"
   :executable "pylsp"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'pylsp
     :major-modes '(python-mode python-ts-mode)
     :priority 1))

 ( :name "rust-analyzer"
   :description "Rust language server"
   :install-method "github"
   :source "rust-lang/rust-analyzer"
   :executable "rust-analyzer"
   :path-dirs (".")
   :lsp-config
   ( :server-id 'rust-analyzer
     :major-modes '(rust-mode rust-ts-mode)
     :priority 0))

 ( :name "gopls"
   :description "Go language server (official)"
   :install-method "go"
   :source "golang.org/x/tools/gopls@latest"
   :executable "gopls"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'gopls
     :major-modes '(go-mode go-ts-mode)
     :priority 0))

 ( :name "golangci-lint-langserver"
   :description "Go linter language server for golangci-lint"
   :install-method "go"
   :source "github.com/nametake/golangci-lint-langserver@latest"
   :executable "golangci-lint-langserver"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'golangci-lint-ls
     :major-modes '(go-mode go-ts-mode)
     :priority 1))

 ( :name "go-outline"
   :description "Go outline tool for symbol navigation"
   :install-method "go"
   :source "github.com/ramya-rao-a/go-outline@latest"
   :executable "go-outline"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'go-outline
     :major-modes '(go-mode go-ts-mode)
     :priority 2))

 ( :name "gomodifytags"
   :description "Go tool to modify struct field tags"
   :install-method "go"
   :source "github.com/fatih/gomodifytags@latest"
   :executable "gomodifytags"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'gomodifytags
     :major-modes '(go-mode go-ts-mode)
     :priority 3))

 ( :name "gotests"
   :description "Go test generation tool"
   :install-method "go"
   :source "github.com/cweill/gotests/...@latest"
   :executable "gotests"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'gotests
     :major-modes '(go-mode go-ts-mode)
     :priority 4))

 ( :name "impl"
   :description "Go tool for generating method stubs"
   :install-method "go"
   :source "github.com/josharian/impl@latest"
   :executable "impl"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'impl
     :major-modes '(go-mode go-ts-mode)
     :priority 5))

 ( :name "goimports"
   :description "Go tool for automatically fixing imports"
   :install-method "go"
   :source "golang.org/x/tools/cmd/goimports@latest"
   :executable "goimports"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'goimports
     :major-modes '(go-mode go-ts-mode)
     :priority 6))

 ( :name "gofumpt"
   :description "Stricter gofmt for Go code formatting"
   :install-method "go"
   :source "mvdan.cc/gofumpt@latest"
   :executable "gofumpt"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'gofumpt
     :major-modes '(go-mode go-ts-mode)
     :priority 7))

 ( :name "staticcheck"
   :description "Go static analysis tool"
   :install-method "go"
   :source "honnef.co/go/tools/cmd/staticcheck@latest"
   :executable "staticcheck"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'staticcheck
     :major-modes '(go-mode go-ts-mode)
     :priority 8))

 ( :name "golangci-lint"
   :description "Go linters runner"
   :install-method "github"
   :source "golangci/golangci-lint"
   :executable "golangci-lint"
   :path-dirs (".")
   :options (:strip-components 1)
   :lsp-config
   ( :server-id 'golangci-lint
     :major-modes '(go-mode go-ts-mode)
     :priority 9))

 ( :name "clangd"
   :description "C/C++ language server"
   :install-method "github"
   :source "clangd/clangd"
   :executable "clangd"
   :path-dirs ("bin")
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
   :path-dirs (".")
   :options (:strip-components 1)
   :lsp-config
   ( :server-id 'zls
     :major-modes '(zig-mode)
     :priority 0))

 ( :name "lua-language-server"
   :description "Lua language server"
   :install-method "github"
   :source "LuaLS/lua-language-server"
   :executable "lua-language-server"
   :path-dirs ("bin")
   :options (:strip-components 0)
   :lsp-config
   ( :server-id 'lua-ls
     :major-modes '(lua-mode)
     :priority 0))

 ( :name "jdtls"
   :description "Java language server"
   :install-method "binary"
   :source "https://download.eclipse.org/jdtls/milestones/1.40.0/jdt-language-server-1.40.0-202409261450.tar.gz"
   :executable "jdtls"
   :path-dirs ("bin")
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
   :path-dirs ("tools")
   :lsp-config
   ( :server-id 'csharp-ls
     :major-modes '(csharp-mode csharp-ts-mode)
     :priority 0))

 ( :name "omnisharp"
   :description "OmniSharp C# language server"
   :install-method "github"
   :source "OmniSharp/omnisharp-roslyn"
   :executable "OmniSharp"
   :path-dirs (".")
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
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'html-ls
     :major-modes '(html-mode web-mode mhtml-mode)
     :priority 0))

 ( :name "vscode-css-language-server"
   :description "CSS language server"
   :install-method "npm"
   :source "vscode-langservers-extracted"
   :executable "vscode-css-language-server"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'css-ls
     :major-modes '(css-mode scss-mode sass-mode less-css-mode)
     :priority 0))

 ( :name "vscode-json-language-server"
   :description "JSON language server"
   :install-method "npm"
   :source "vscode-langservers-extracted"
   :executable "vscode-json-language-server"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'json-ls
     :major-modes '(json-mode json-ts-mode)
     :priority 0))

 ( :name "vue-language-server"
   :description "Vue.js language server"
   :install-method "npm"
   :source "@vue/language-server"
   :executable "vue-language-server"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'vue-ls
     :major-modes '(vue-mode)
     :priority 0))

 ( :name "svelte-language-server"
   :description "Svelte language server"
   :install-method "npm"
   :source "svelte-language-server"
   :executable "svelte-language-server"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'svelte-ls
     :major-modes '(svelte-mode)
     :priority 0))

 ( :name "bash-language-server"
   :description "Bash language server"
   :install-method "npm"
   :source "bash-language-server"
   :executable "bash-language-server"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'bash-ls
     :major-modes '(sh-mode shell-script-mode)
     :priority 0))

 ( :name "yaml-language-server"
   :description "YAML language server"
   :install-method "npm"
   :source "yaml-language-server"
   :executable "yaml-language-server"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'yaml-ls
     :major-modes '(yaml-mode yaml-ts-mode)
     :priority 0))

 ( :name "diagnostic-languageserver"
   :description "Diagnostic language server for linters"
   :install-method "npm"
   :source "diagnostic-languageserver"
   :executable "diagnostic-languageserver"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'diagnostic-ls
     :major-modes '(text-mode)
     :priority 0))

 ( :name "jedi-language-server"
   :description "Python language server using Jedi"
   :install-method "pip"
   :source "jedi-language-server"
   :executable "jedi-language-server"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'jedi-ls
     :major-modes '(python-mode python-ts-mode)
     :priority 2))

 ( :name "cmake-language-server"
   :description "CMake language server"
   :install-method "pip"
   :source "cmake-language-server"
   :executable "cmake-language-server"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'cmake-ls
     :major-modes '(cmake-mode)
     :priority 0))

 ( :name "fortls"
   :description "Fortran language server"
   :install-method "pip"
   :source "fortls"
   :executable "fortls"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'fortls
     :major-modes '(fortran-mode f90-mode)
     :priority 0))

 ( :name "intelephense"
   :description "PHP language server"
   :install-method "npm"
   :source "intelephense"
   :executable "intelephense"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'intelephense
     :major-modes '(php-mode)
     :priority 0))

 ( :name "haskell-language-server"
   :description "Haskell language server"
   :install-method "github"
   :source "haskell/haskell-language-server"
   :executable "haskell-language-server"
   :path-dirs (".")
   :options (:strip-components 1)
   :lsp-config
   ( :server-id 'haskell-ls
     :major-modes '(haskell-mode)
     :priority 0))

 ( :name "elixir-ls"
   :description "Elixir language server"
   :install-method "github"
   :source "elixir-lsp/elixir-ls"
   :executable "language_server.sh"
   :path-dirs (".")
   :options (:strip-components 1)
   :lsp-config
   ( :server-id 'elixir-ls
     :major-modes '(elixir-mode)
     :priority 0))

 ( :name "kotlin-language-server"
   :description "Kotlin language server"
   :install-method "github"
   :source "fwcd/kotlin-language-server"
   :executable "kotlin-language-server"
   :path-dirs ("server/bin")
   :options (:strip-components 1)
   :lsp-config
   ( :server-id 'kotlin-ls
     :major-modes '(kotlin-mode)
     :priority 0))

 ( :name "metals"
   :description "Scala language server"
   :install-method "github"
   :source "scalameta/metals"
   :executable "metals"
   :path-dirs (".")
   :options (:strip-components 0)
   :lsp-config
   ( :server-id 'metals
     :major-modes '(scala-mode)
     :priority 0))

 ( :name "dockerfile-language-server-nodejs"
   :description "Dockerfile language server"
   :install-method "npm"
   :source "dockerfile-language-server-nodejs"
   :executable "docker-langserver"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'dockerfile-ls
     :major-modes '(dockerfile-mode)
     :priority 0))

 ( :name "taplo"
   :description "TOML language server"
   :install-method "github"
   :source "tamasfe/taplo"
   :executable "taplo"
   :path-dirs (".")
   :options (:strip-components 0)
   :lsp-config
   ( :server-id 'taplo
     :major-modes '(toml-mode toml-ts-mode)
     :priority 0))

 ( :name "terraform-ls"
   :description "Terraform language server"
   :install-method "github"
   :source "hashicorp/terraform-ls"
   :executable "terraform-ls"
   :path-dirs (".")
   :options (:strip-components 0)
   :lsp-config
   ( :server-id 'terraform-ls
     :major-modes '(terraform-mode)
     :priority 0))

 ( :name "ansible-language-server"
   :description "Ansible language server"
   :install-method "npm"
   :source "@ansible/ansible-language-server"
   :executable "ansible-language-server"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'ansible-ls
     :major-modes '(yaml-mode)
     :priority 1))

 ( :name "marksman"
   :description "Markdown language server"
   :install-method "github"
   :source "artempyanykh/marksman"
   :executable "marksman"
   :path-dirs (".")
   :options (:strip-components 0)
   :lsp-config
   ( :server-id 'marksman
     :major-modes '(markdown-mode)
     :priority 0))

 ( :name "vscode-eslint-language-server"
   :description "ESLint language server"
   :install-method "npm"
   :source "vscode-langservers-extracted"
   :executable "vscode-eslint-language-server"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'eslint-ls
     :major-modes '(js-mode js-ts-mode typescript-mode typescript-ts-mode)
     :priority 1))

 ( :name "emmet-ls"
   :description "Emmet language server for HTML/CSS"
   :install-method "npm"
   :source "emmet-ls"
   :executable "emmet-ls"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'emmet-ls
     :major-modes '(html-mode web-mode mhtml-mode css-mode scss-mode)
     :priority 1))

 ( :name "prisma-language-server"
   :description "Prisma language server"
   :install-method "npm"
   :source "@prisma/language-server"
   :executable "prisma-language-server"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'prisma-ls
     :major-modes '(prisma-mode)
     :priority 0))

 ( :name "graphql-language-service-cli"
   :description "GraphQL language server"
   :install-method "npm"
   :source "graphql-language-service-cli"
   :executable "graphql-lsp"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'graphql-ls
     :major-modes '(graphql-mode)
     :priority 0))

 ( :name "sql-language-server"
   :description "SQL language server"
   :install-method "npm"
   :source "sql-language-server"
   :executable "sql-language-server"
   :path-dirs ("node_modules/.bin")
   :lsp-config
   ( :server-id 'sql-ls
     :major-modes '(sql-mode)
     :priority 0))

 ( :name "solargraph"
   :description "Ruby language server"
   :install-method "gem"
   :source "solargraph"
   :executable "solargraph"
   :path-dirs ("bin")
   :lsp-config
   ( :server-id 'solargraph
     :major-modes '(ruby-mode ruby-ts-mode)
     :priority 0))

)
