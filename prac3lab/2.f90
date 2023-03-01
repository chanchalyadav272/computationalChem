module q2
integer::n,i,j,k
real, dimension(:), allocatable::x
real, dimension(:), allocatable::y
real:: xsum, ysum, m,c,st,sr,r, x2sum, prodSum




end module q2


program std
use q2
implicit none
open(1,file='2_input.txt')
read(1,*) n




allocate(x(n))
allocate(y(n))

do k=1,n
read(1,*) x(k), y(k)
x(k) = log(x(k))
y(k) = log(y(k))

enddo

call CalMC()
call goodness()

print*,"a= ", exp(c)
print*,"b= ", m
print*,"r^2= ", r

end program std

subroutine CalSum()
use q2
xsum =0
ysum=0
x2sum=0
prodSum=0

do i=1, n
xsum = xsum + x(i)
ysum = ysum + y(i)
x2sum = x2sum + (x(i))**2
prodSum = prodSum + (x(i)*y(i))
enddo

end subroutine CalSum

subroutine CalMC()
use q2

call CalSum()

m = ((n*prodSum)-(xsum*ysum))/((n*x2sum)-(xsum**2))

c= (ysum - (m*xsum))/n



end subroutine CalMC

subroutine goodness()
use q2

st = 0

sr = 0

do j= 1,n
st = st + ((y(j)-(ysum/real(n)))**2)
sr = sr + ((y(j)-c-(m*x(j)))**2)
enddo

r = (st-sr)/st





end subroutine goodness
