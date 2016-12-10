# Hyper

*Type-safe, statically checked composition of HTTP servers, using PureScript.*

## What is this?

This is an **experiment**, trying to improve correctness in web server
programming, and in HTTP middleware and request handlers specifically. To read
about the goals and design of Hyper, see [the
documentation](https://owickstrom.github.io/hyper/).

## Build

Install dependencies and build:

```bash
npm install
bower install
pulp build
```

Running tests:

```bash
pulp test
```

Running examples:

```bash
pulp run -I examples/node-server
```

## License

[Mozilla Public License Version 2.0](LICENSE)
