module q1
real(kind=8),parameter::pi=acos(-1.0)
integer(kind=8)::n=1000,i,j,lwork,info
real(kind=8),dimension(:,:),allocatable::h
real(kind=8),dimension(:),allocatable::eval,x,v,work
real(kind=8)::t,hbar=1,m=1,dx,l=5,e,b=3


end module q1

program fd
use q1
implicit none
lwork =max(1,3*n-1)

allocate(h(n,n),eval(n),x(n),v(n),work(lwork))
open (1,file='hamiltonian.txt')
open (2,file='eigenvalue.txt')
open (3,file='eigenvector.txt')

dx= (5-0)/real(n-1)

t =  (hbar**2)/(2*m*dx*dx)

do i=1,n
x(i)= (i-1)*dx
v(i)=b*x(i)
do j=1,n
if(i==j) then
h(i,j) = 2*t + v(i)
else if(abs(i-j)==1) then
h(i,j) = -t
else
h(i,j)=0
endif
enddo
enddo

do i=1,n
write(1,*) (h(i,j), j=1,n)
enddo

call DSYEV( 'V', 'U', n, h, max(1,n), eval, work, lwork, info )



do i=1,n
e = ((i*2*pi)**2)/(8*m*l*l)
write(2,*) i, eval(i) !, e
write(3,*) x(i),v(i),(h(i,j), j=1,n)
enddo

end program fd
