let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200615/packages.dhall sha256:5d0cfad9408c84db0a3fdcea2d708f9ed8f64297e164dc57a7cf6328706df93a

let overrides =
  { node-http =
      upstream.node-http // { repo = "https://github.com/srghma/purescript-node-http.git", version = "master" }
  }

let additions = {=}

in  upstream // overrides // additions
