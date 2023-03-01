module ip
    real, parameter:: pi = acos(-1.0)
    integer::num,i,j,k,c
    real::a
    real, dimension(:), allocatable::x
    real, dimension(:), allocatable::y
    real, dimension(:), allocatable::z
end module ip


program cal
use ip
implicit none
open(1,file='3_input.txt')

read(1,*) num

allocate(x(num))
allocate(y(num))
allocate(z(num))

do c = 1,num
read(1,*) x(c), y(c)
enddo


read(1,*) a

call Calterm(a)

end program cal

subroutine CalTerm(m)
use ip

real::m, prod,sum=0

do i=1,num
prod = 1
do j=1,num

if(i==j) then
prod = prod *1
else 
prod  = prod * ((m-x(j))/(x(i)-x(j)))
endif

enddo

sum=sum+y(i) * prod
enddo
print*, sum
end subroutine CalTerm

