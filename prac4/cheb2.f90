module c2
real,parameter::pi= acos(-1.0)
integer::n,i,j,k
real::ans, prod,m
real, dimension(:,:), allocatable:: q
real, dimension(:), allocatable:: x


end module c2

program sx
use c2
implicit none
read*, n,m
allocate(q(0:n,0:n))
allocate(x(0:n))

do i=0,n
x(i) = cos((2*i+1)*pi/(2*n+2))
q(i,0) = sin(pi*x(i))
enddo

! x = (/1.2,1.3,1.4/)
! q(:,0) = (/1.063,1.091,1.119/)



call dif()

do i=0,n
print*, (q(i,j), j =0,n)
enddo

ans = q(0,0)
do i = 1,n
call product(i,m)
ans= ans+ q(0,i)*prod
enddo

print*, ans

end program sx

subroutine dif()
use c2

integer::a,r,c
a=n-1   
do c = 1,n
do r=0,a
q(r,c) = (q(r+1,c-1)-q(r,c-1))/(x(c)-x(0))
enddo
a=a-1
enddo


end subroutine dif

subroutine product(a,b)
use c2
integer::a
real::b

prod =1
do k = 0,a-1
prod = prod * (b-x(k))
enddo
end subroutine product