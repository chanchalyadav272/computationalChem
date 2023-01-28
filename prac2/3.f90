program mltpl
implicit none
integer::i,j
real,dimension(3,5)::numbers
 open(10, file = "3_input.txt")
 open(20, file = "3_output.txt")
 
do i = 1,3
read(10,*) (numbers(i,j),j=1,5)
enddo

do i=1,3
WRITE(20,*) ((numbers(i,j)*10), j=1,5)
enddo
end program mltpl
