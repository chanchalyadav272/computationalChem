module q2
real(kind=8),dimension(:),allocatable ::x,y,c,t
real(kind=8)::m=1.2,sum,ans,z
integer(kind=8)::n=20,i,j,k
real(kind=8),parameter::pi=acos(-1.0)
end module q2

program cheb
use q2
implicit none

allocate(x(0:n),y(0:n),t(0:n),c(0:n))


do j=0,n
x(j)= 5*cos((2*j+1)*pi/(2*n+2)) !calculating and transforming nodes
y(j)=1/(1+16*(x(j)**2))

enddo

do k=0,n
sum=0
do j=0,n
sum=sum+y(j)*cos((2*j+1)*k*pi/(2*n+2))
enddo
if(k==0) then
c(k) = sum/real(n+1)
else 
c(k)= 2*sum/real(n+1)
endif
! print*,c(k)
enddo
z=m/5.0  !transforming the input
ans=0
do k=0,n
t(k)=cos(k*acos(z))

ans = ans+ c(k)*t(k)

enddo

print*,ans

end program cheb
