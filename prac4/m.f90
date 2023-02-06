module q1
integer::i,j,k,n
real, dimension(:), allocatable::x
real, dimension(:), allocatable::y
real:: xsum, ysum, xysum, x2sum,m,c,st,sr,r
end module

program good
use q1
implicit none
open(1,file='1_input.txt')
read(1,*) n
allocate(x(n))
allocate(y(n))
do k=1,n
read(1,*) x(k), y(k)
enddo

call calmc()
call goodness()

print*,"m= ", m
print*,"c= ", c
print*,"goodness of fit ", r**2


end program good

subroutine calsum
use q1
xsum=0
ysum=0
xysum=0
x2sum=0

do i=1,n
xsum=xsum+ x(i)
ysum=ysum+ y(i)
xysum= xysum+ x(i)*y(i)
x2sum= x2sum+ (x(i))**2
end do
end subroutine calsum

subroutine calmc
use q1
call calsum


m= ((n*xysum) - (xsum*ysum))/((n*x2sum)- xsum**2)
c= (ysum- m*xsum)/n

end subroutine calmc

subroutine goodness

use q1
call calsum
call calmc
 St=0
 sr=0
 do j=1,n
st= st+ ((y(j)- (ysum/n))**2)
sr= sr+ (y(j)- c- (m*x(j)))**2
end do

r= ((st-sr)/st)**0.5
end subroutine goodness