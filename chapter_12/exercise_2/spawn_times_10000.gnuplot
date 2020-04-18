set datafile separator ','
set ylabel 'Milliseconds'
set xlabel 'Number of Processes'
plot "10000_processes.csv" using 1:2 with lines, '' using 1:3 with lines
