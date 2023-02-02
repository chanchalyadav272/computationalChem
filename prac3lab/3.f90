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

call CalPol(a)

end program cal

subroutine CalTerm(m)
use ip

real::m, prod

do i=1,num
prod = 1
do j=1,num

if(i==j) then
prod = prod *1
else 
prod  = prod * ((m-x(j))/(x(i)-x(j)))
endif

enddo

z(i)= y(i) * prod
enddo

end subroutine CalTerm

subroutine CalPol(n)
use ip
real::n,sum=0

call CalTerm(n)

do k=1,num
sum = sum + z(k)
enddo

print*, sum

end subroutine CalPol
