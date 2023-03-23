module q1
real(kind=8),parameter::pi=acos(-1.0)
real(kind=8),dimension(:),allocatable::x,v,pd,e,tq,tc,kbt,pq,pc
complex(kind=8)::z=(0,-1)
complex(kind=8),dimension(:),allocatable::psi
integer(kind=8)::i,j,n=1000,k,b=140,c=19
real(kind=8)::dx=0.01,de=0.1,x0=4.5,m=9,pavg,dk=0.1,s1,s2
end module q1

program chem
use q1
implicit none
allocate(x(0:n),v(0:n),pd(0:n),psi(0:n),e(0:b),tq(0:b),tc(0:b),kbt(0:c),pq(0:c),pc(0:c))
open(1,file='out1.txt')
open(2,file='out2.txt')
open(3,file='out3.txt')

!calculating x and v
do i=0,1000
x(i)=i*dx
if(x(i)<=x0) then
v(i)= 9*exp((-1)*((x(i)-x0)/0.6)**2)
else 
v(i)= 5*exp((-1)*((x(i)-x0)/0.6)**2) + 4
endif


enddo

! for e=10
call calpsi(m)

do i=0,n
write(1,*) x(i),pd(i)
enddo

! for e=1 to 15
do i=0,b
e(i)= 1 + i*de

call calpsi(e(i))

pavg = (minval(pd(601:1000)) + maxval(pd(601:1000)))/2

tq(i) = 2/(1+pavg)

if(e(i)>9) then 
tc(i) = 1
else 
tc(i) =0
endif


write(2,*) e(i),tq(i),tc(i)
enddo

!for kbt = 0.1 to 2

do i=0,c
kbt(i) = 0.1 + i*dk

s1=0
s2=0
do j=0,b
s1= s1+ tq(j)*exp((-1)*e(j)/kbt(i))
s2= s2+ tc(j)*exp((-1)*e(j)/kbt(i))
enddo

pq(i) = de*s1/kbt(i)
pc(i) = de*s2/kbt(i)



write(3,*) kbt(i), pq(i),pc(i)
enddo 


end program chem

subroutine calpsi(a)
use q1

real(kind=8)::a

psi(0)=1

psi(1)=exp(z*sqrt(2*a)*dx)

do j=1,n-1
psi(j+1)= (2-(2*(a-v(j))*(dx**2)))*psi(j) - psi(j-1)
enddo

do j=0,n
pd(j) = abs(psi(j))**2

enddo



end subroutine calpsi



