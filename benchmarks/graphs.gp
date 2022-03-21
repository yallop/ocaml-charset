set datafile separator ","
set term svg enhanced background rgb 'white' size 300,200 font "Verdana,9"
set xlabel "set size"
set ylabel "ns"
set logscale y
set key autotitle columnhead

set style line 1 lc rgb '#8b1a0e' pt 1 ps 1 lt 1 lw 3 # --- red
set style line 2 lc rgb '#5e9c36' pt 6 ps 1 lt 1 lw 3 # --- green

set xrange [0:256]
set yrange [0:]

set title ARG1
set key bottom right

set output ARG3
plot ARG2 using 1:2 with linespoints ls 102, ARG2 using 1:3 with linespoints ls 103
