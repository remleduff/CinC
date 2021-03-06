# NOTICE
### CinC has been moved to contrib and split between 3 libraries: [tools.analyzer](https://github.com/clojure/tools.analyzer), [tools.analyzer.jvm](https://github.com/clojure/tools.analyzer.jvm) and [tools.emitter.jvm](https://github.com/clojure/tools.emitter.jvm)

# CinC

CinC is an experimental Clojure analyzer and compiler implemented in pure Clojure.

It started off as an extension of Aaron Cohen's [CinC](https://github.com/remleduff/CinC) project but it has been mostly rewritten.

As CinC is still a work-in-progress and it's rapidly iterating, expect the doc to be slightly out-of-sync with the current implementation.
More extensive documentation will be written once CinC is stable enough.

# Features

CinC currently contains both an analyzer composed of multiple passes and an emitter.

Docs on how the compile process works are in [compiler](doc/compiler.md)

# Analyzer

The jvm analyzer (`cinc.analyzer.jvm/analyze`) returns an AST similar to the one returned by the clojurescript analyzer or by jvm.tools.analyzer.
On top of the simple AST, multiple passes are run in order to transform/annotate the ast with information needed for the jvm bytecode emission.

The basic AST nodes are documented in [analyzer](doc/analyzer.md) while the passes are documented in [passes](doc/passes.md).

Note that some passes remove or add new nodes to the AST.

# Emitter

The bytecode emitter (`cinc.compiler.jvm.bytecode/eval`) takes in an expression, gets the AST from its analysis and emits the corresponing bytecode.

How this process works is documented in [emit](doc/emit.md)

# What's not working

There may still be a couple of issues around primitive boxing/unboxing.

`invokePrim` is also not yet implemented.

# License

Copyright © Nicola Mometto, Rich Hickey & contributors.

Licensed under the EPL (see the file epl.html).
