program cal
use ip
implicit none
open(1,file='1_input.txt')
open(2,file='1_output.txt')
read(1,*) num

allocate(x(num))
allocate(y(num))
allocate(z(num))

do c = 1,num
read(1,*) x(c), y(c)
enddo

print*, "ENTER THE VALUE OF X"
read(1,*) a

call CalPol(a)

end program cal

