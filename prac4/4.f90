module q4
integer::n,i,j,k,l,c
real, dimension(0:4)::x
real, dimension(0:4)::y
real, dimension(0:4)::f
real:: a, ans=0, prod, val




end module q4

program newton
use q4
implicit none
open(1, file='4_input.txt')
read(1,*) n

do c = 0,n-1
read(1,*) x(c), y(c)
enddo

read(1,*) a

call CalF()

do k = 0,n-1
call calprod(k,a)
if(k==0) then
ans = ans + f(0)
elseif(k==1) then
ans = ans + f(1)*prod
else
call calfhigh(k)
ans = ans + val*prod
endif
enddo

print*, ans



end program newton

subroutine CalF()
use q4
integer ::v
f(0) = y(0)

do v=1,n-1
f(v)= (y(v) - y(v-1))/(x(v)-(x(v-1)))
enddo

end subroutine Calf

subroutine CalfHigh(m)
use q4
integer::m

val= (f(m) - f(m-1))/(x(m)-x(m-2))

end subroutine CalfHigh


subroutine CalProd(g,h)
use q4
integer::g
real::h
prod =1
do j=0,g-1
prod = prod * (h-x(j))
enddo

end subroutine CalProd


