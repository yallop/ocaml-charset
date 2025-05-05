BENCHMARKS=isempty              \
           mem                  \
           add                  \
           singleton            \
           remove               \
           union                \
           inter                \
           disjoint             \
           diff                 \
           compare              \
           equal                \
           subset               \
           iter                 \
           fastmap              \
           slowmap              \
           fold                 \
           forall               \
           exists               \
           filter               \
           filter-map           \
           partition            \
           cardinal             \
           elements             \
           minelt               \
           minelt-opt           \
           maxelt               \
           maxelt-opt           \
           choose               \
           choose-opt           \
           split                \
           find                 \
           find-opt             \
           find-first           \
           find-first-opt       \
           find-last            \
           find-last-opt        \
           of-list              \
           to-seq-from          \
           to-seq               \
           to-rev-seq           \
           add-seq              \
           of-seq

BENCHARGS=-quota 0.2 -display blank -clear-columns time

all:
	dune build -p charset

clean:
	dune clean

test:
	dune runtest

bench: $(foreach b,$(BENCHMARKS),bench-$(b))

bench-%:
	mkdir -p benchmarks/csv
	dune exec --profile benchmark -- charset_bench $* $(BENCHARGS) \
	  | benchmarks/munge.py > benchmarks/csv/$*-benchmark.csv
	gnuplot -c benchmarks/graphs.gp $*  benchmarks/csv/$*-benchmark.csv benchmarks/csv/$*-benchmark.svg


update-images:
	for bench in $(BENCHMARKS); do cp benchmarks/csv/$${bench}-benchmark.svg images/; done

.PHONY: all clean test bench update-images
