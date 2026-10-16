# GameOfLife

[Euller Macena](https://github.com/eullerm) ([LinkedIn](https://www.linkedin.com/in/euller-macena/)) · [Malkai Oliveira](https://github.com/MalkaiOliveira)

UFF - Niterói, 2021-2022

Projeto desenvolvido na matéria de Linguagem de Programação.

O jogo (autômato celular com células mortas, vivas e zumbis) é implementado em Haskell. O frontend em React roda esse mesmo motor inteiramente no navegador, compilado para WebAssembly (sem backend); o frontend foi desenvolvido por [Euller Macena](https://www.linkedin.com/in/euller-macena/).

# Stack

- Haskell - Linguagem funcional
- WebAssembly (GHC wasm backend) - para rodar o núcleo no navegador
- React + Vite + TypeScript - frontend
- Stack - Ferramenta de build
- Visual Code - Editor de texto

# APP

O programa de console recebe do usuário o tamanho, a matriz inicial e o número de iterações. Retorna o número de iterações até a matriz normalizar e o resultado da mesma.

<img src="image/image1.png" height="400">

Caso alguma célula da matriz não seja passada o código inicia ela como morta.

## Instalação

Este app precisa do [Stack](https://docs.haskellstack.org/en/stable/README/) para funcionar.

Para Windows:

```sh
https://get.haskellstack.org/stable/windows-x86_64-installer.exe
```

Para Linux:

```sh
curl -sSL https://get.haskellstack.org/ | sh
```

### Testar

O `stack.yaml` fica dentro de `core-haskell/`, então rode o Stack de lá:

```sh
$ cd core-haskell
$ stack build
$ stack exec GameOfLife
```

# Casos de teste

Para o teste é possível iniciar o grid de qualquer tamanho desejado, por exemplo:

- 5x5
- 16X32
- 100x3

Sendo o primeiro valor a quantidade de linhas da matriz e o segundo de colunas, esses valores devem ser sempre separados pela letra **X** ou **x**.

Para criar a matriz é necessário que cada célula esteja separada por espaço e a cada nova linha ocorra uma quebra de linha, por exemplo:

```
V V V
M M M
Z Z Z
```

Caso alguma célula não seja iniciada o programa considerará ela como morta, por exemplo em uma matriz 3x3 dado o seguinte input do usuário:

```
V V V
Z Z
```

O programa criará a matriz da seguinte forma:

```
V V V
Z Z M
M M M
```

# Regras

Cada célula tem oito vizinhos (incluindo as diagonais); as bordas não "dão a volta" no tabuleiro. Em cada geração conta-se quantos vizinhos estão vivos (`a`) e quantos estão zumbis (`z`), e a célula é reclassificada segundo as regras abaixo.

- **Morta (`M`)** — célula vazia. Nasce como **Viva** quando tem exatamente 3 vizinhos vivos. Caso contrário continua morta (zumbis não afetam células mortas).
- **Viva (`V`)** — célula viva.
  - Se tiver **pelo menos 1 vizinho zumbi**, vira **Zumbi** (esta regra é aplicada antes das regras de morte).
  - Senão, se tiver **menos de 2** vizinhos vivos, morre (`M`).
  - Senão, se tiver **mais de 3** vizinhos vivos, morre (`M`).
  - Senão (2 ou 3 vizinhos vivos, sem zumbi por perto), continua **Viva**.
- **Zumbi (`Z`)** — célula infectada.
  - Se **não tiver nenhum vizinho vivo**, volta a ser **Morta**.
  - Caso contrário continua **Zumbi** (não morre por falta nem por excesso de vizinhos, e não transforma células mortas em zumbis).

O que uma célula zumbi faz com uma viva: um único zumbi vizinho **infecta** a célula viva, que vira Zumbi independentemente da quantidade de vizinhos vivos. Essa infecção tem prioridade sobre as regras de nascimento e morte — por isso uma célula morta com 3 vizinhos vivos ainda nasce normalmente, mesmo que também toque um zumbi.

# Frontend (React)

O motor Haskell continua sendo o mesmo do trabalho (`core-haskell/src/GameOfLife.hs`), e o React o executa através da interface `CoreTransport` (`src/core/transport.ts`), cuja única implementação roda o núcleo compilado para WebAssembly.

- **WASM** — o mesmo código Haskell compilado para WebAssembly roda no próprio navegador. Não precisa de backend, então o GitHub Pages consegue hospedar o app inteiro.

O frontend foi desenvolvido por [Euller Macena](https://www.linkedin.com/in/euller-macena/).

## Rodar

```sh
$ npm install
$ npm run dev
```

Não precisa de backend: o `.wasm` já vem versionado em `public/gameoflife.{wasm,js}`.

## Núcleo em WebAssembly

O binário `public/gameoflife.wasm` e o glue `public/gameoflife.js` são gerados a partir do Haskell por `core-haskell/wasm/build.sh`, que usa o backend wasm do GHC (`wasm32-wasi-ghc`, do [ghc-wasm-meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta)) num alvo WASI _reactor_, com o JavaScript FFI exportando `runSteps`.

O build exige Linux/WSL2 com o toolchain `~/.ghc-wasm` instalado (não há suporte no host Windows):

```sh
$ npm run wasm        # wsl -d Ubuntu -- bash core-haskell/wasm/build.sh
```

Os artefatos gerados são versionados no repositório (ficam em `public/`), para que `npm run build` e o GitHub Pages funcionem sem precisar do toolchain. Rode `npm run wasm` novamente só quando o núcleo Haskell (`core-haskell/src`) mudar.

## Deploy

O workflow `.github/workflows/deploy-pages.yml` publica o app no GitHub Pages automaticamente a cada `push` (e também pode ser disparado manualmente). Como o núcleo roda em WASM, o build (`npm run build`) já contém tudo o que o site precisa.

## Servidor Haskell (opcional)

O executável `api` (`core-haskell/server/Main.hs`, porta 8787, `GOL_API_PORT` para trocar) continua no projeto e expõe o núcleo por HTTP para uso standalone (por exemplo, testar a API ou integrar outro cliente). O frontend do app **não** usa mais HTTP — ele fala apenas com o núcleo em WebAssembly.

```sh
$ npm run api         # cd core-haskell && stack build && stack exec api
```

## Controles da tela

- **Clique numa célula** para alternar: Morta -> Viva -> Zumbi -> Morta.
- **Iniciar/Pausar** — roda uma geração a cada intervalo escolhido no controle de velocidade (padrão 500 ms).
- **Próxima geração** — avança um passo.
- **Rodar até estabilizar** — roda até o tabuleiro parar de mudar (até 500 passos) e informa em quantas iterações estabilizou.
- **Limpar** — zera todas as células.
- **Linhas/Colunas + Aplicar** — redimensiona a grade (1 a 60), preservando o que couber.

## Estrutura do frontend

```
src/
  core/transport.ts     fachada de acesso ao núcleo (WASM)
  core/wasmCore.ts      núcleo Haskell em WASM, rodando no navegador
  components/Board.tsx  tabuleiro em canvas
  App.tsx               estado da grade, controles e loop de reprodução
  index.css             estilos da aplicação
```

O acesso ao núcleo fica atrás de `CoreTransport` (`src/core/transport.ts`), que roda o Haskell em WASM (`src/core/wasmCore.ts`). Trocar a implementação não exige mudanças nos componentes.
