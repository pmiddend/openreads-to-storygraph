# 📚 openreads-to-storygraph

![Build](https://github.com/pmiddend/openreads-to-storygraph/actions/workflows/build-with-nix.yaml/badge.svg)

Convert your [openreads](https://github.com/mateusz-bak/openreads) backup files into files that [The StoryGraph](https://www.thestorygraph.com/) understands so you can import your books.

## Usage

Build it, either with `nix build` or via `cabal` and then run it via:

```
openreads-to-storygraph backup-file
```

# Attribution

Thanks to the [shellcheck](https://github.com/koalaman/shellcheck) team for providing the template for our GitHub workflow that packages the application.
