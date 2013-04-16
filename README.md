# cpsc5210

A Clojure library designed to implement logic synthesis functions. The 
cpsc5210.aguirre namespace contains a partial implementation of the Aguirre
2003 paper on the subject. The cpsc5210.rioux namespace contains a full
implementation of the functions for a proposed paper.

## Usage
The rioux experiments can be either run from the command line or the
clojure repl.

### Command Line

```bash
# Runs the prime3 experiment 4 times in parallel with id 1
# id will be used to distinguish the filename in the output.
java -cp target/cpsc5210-0.1.0-SNAPSHOT-standalone.jar prime3 4 1
```

```clojure
(use 'cpsc5210.experiments.rioux :reload-all)
; Run 4 experiments in parallel, should probably capture the results.
(parallel-experiment experiment-prime3 4)
```

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
