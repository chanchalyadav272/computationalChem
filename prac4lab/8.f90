module q8
real,parameter::pi=acos(-1.0)
integer::n,i,j,k
real::ans, prod,m
real,dimension(:),allocatable::x
real,dimension(:,:),allocatable::q

end module q8

program sx
use q8

read*,n,m

allocate(x(0:n), q(0:n,0:n))

do i =0,n
x(i)=cos((2*i+1)*pi/(2*n+2))
q(i,0)= sin(pi*x(i))
enddo

call dif()

do i=0,n
print*, (q(i,j), j=0,n)
enddo

ans = q(0,0)

do i=1,n
call prdct(i,m)
ans = ans + q(0,i)*prod
enddo

print*,ans


end program sx

subroutine dif()
use q8
integer::r,c

do c=1,n
do r=0,n-c
q(r,c)=(q(r+1,c-1)-q(r,c-1))/(x(r+c)-x(r))
enddo
enddo



end subroutine dif

subroutine prdct(a,b)
use q8
integer::a
real::b

prod =1

do k=0,a-1
prod = prod* (b-x(k))
enddo


end subroutine prdct






