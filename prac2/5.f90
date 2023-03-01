program squareRoot
implicit none
integer::i
real*8:: b, x1, x2, constant=10E-5

read*, b
x1=b
do 
x2=(x1 + (b/x1))/2

if(abs(x1-x2)<constant)then 
exit
else 
x1=x2
endif

enddo
print*, x2


end program squareRoot
