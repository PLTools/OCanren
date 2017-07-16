set terminal pdf enhanced font "Monaco,10"
set output 'graph.pdf'

set style data histogram
set style histogram cluster gap 1
set style line 2 lc rgb 'black' lt 1 lw 1
set style fill pattern border -1
set auto x
set boxwidth 0.9
set xtics format ""
set grid ytics

# Comment/uncomment to switch logscale off/on
#set logscale y
set yrange [0.01:*]
plot 'data.gnuplot' using 2:xtic(1) title col ls 2 fillstyle pattern 2 \
      , ''          using 3:xtic(1) title col ls 2 fillstyle pattern 4 \
      , ''          using 7:xtic(1) title col ls 2 fillstyle pattern 8 \

set terminal pngcairo size 1024,768 enhanced font "Monaco,14"
set output 'graph.png'

replot
