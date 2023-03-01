module q7
real(kind=8),parameter::pi=acos(-1.0)
integer::n=6,i,j,k,b
real(kind=8)::x,ans
real(kind=8),dimension(:,:),allocatable::f
real(kind=8),dimension(:),allocatable::c,t

end module q7

program ex
use q7
implicit none

allocate(f(0:n,0:1),c(0:n),t(0:n))

do i =0,n
f(i,0)=cos((2*i+1)*pi/(2*n+2))
f(i,1)= exp(f(i,0))
enddo

call coeff()

do b=0,20
ans = 0
x = -1 + b*(2/20.0)
!x=2
call term(x)

do i = 0 ,n
ans = ans + c(i)*t(i)
enddo

print*, x, ans
enddo

print*, (f(i,0), i=0,n)

end program ex

subroutine coeff()
use q7
real(kind=8)::sum
do j=0,n
sum=0
do k=0,n
sum=sum+ (f(k,1)*cos(j*pi*(2*k+1)/(2*n+2)))
enddo
if(j==0) then 
c(j)= sum/real(n+1)
else
c(j)=(2*sum/real(n+1))
endif
enddo


end subroutine coeff

subroutine term(y)
use q7
real(kind=8)::a,y
a=y

t(0)=1
t(1)=a

do k=2,n
t(k)=2*a*t(k-1)-t(k-2)
enddo

end subroutine term
