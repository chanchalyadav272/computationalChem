# computationalChem
# Instructions

## Compilation

Save the file and run the following command in your terminal

```bash
gfortran fileName.f90
```

This compiles the file and creates and output file named "a.out"

To compile the file and rename the output file of your desired name use this command

```bash
gfortran -o fileName.out fileName.f90
```

You can use any name in place or "fileName.out"

To compile the multiple files together

```bash
gfortran module.f90 program.f90 subroutine.f90
```

This compiles all the files in a single file and creates and output file named "a.out"

## Executing/Running the program

To run the compiled program, run the following command in your terminal

```bash
./outputFileName
```

for example,

```bash
./Q1.out
```

## Plotting the txt file using gnuplot

First initialize gnuplot in the terminal

```bash
gnuplot
```

To plot first two columns of any text file

```bash
plot 'filename.txt' 
```

By default it plots using points, to use lines

```bash
plot 'filename.txt' with lines
```

To plot more than one curve from the same text file

```bash
plot 'filename.txt' using 1:2 with lines, 'filename.txt' using 1:3 with lines, 'filename.txt' using 2:3 with points
```
or

```bash
plot 'filename.txt' u 1:2 w l, 'filename.txt' u 1:3 w l, 'filename.txt' u 2:3 w p
```

here 1:2 means column1 is x-axis and column2 is y-axis, similarly any required curve can be plotted

To set range manually

```bash
set yrange [a:b]
```

To quit gnuplot

```bash
q
```
