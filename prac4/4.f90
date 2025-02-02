module q4
integer::n,i,j
real, dimension(:,:), allocatable:: new
real:: a, ans=0, prod, val

end module q4

program newton
use q4
implicit none
open(1, file='4_input.txt')
read(1,*) n

allocate(new(0:n+1,0:n+1))
do i=0,n
read(1,*) new(i,0), new(i,1)
enddo

read(1,*) a
call calf()
! do i=0,n
! print*, (new(i,j),j=0,n)
! enddo
ans = new(0,1)
do i=2,n+1
call calprod(i-1,a)
ans = ans + new(0,i)*prod
print*,prod
enddo

print*, ans
end program newton

subroutine calf()
use q4
integer::t,u,v

do v=2,n+1
do u=0,n+1-v
new(u,v) = (new(u+1,v-1)-new(u,v-1))/(new(v+u-1,0)-new(u,0))
enddo

enddo

end subroutine calf

subroutine CalProd(g,h)
use q4
integer::g
real::h
prod =1
do j=0,g-1
prod = prod * (h-new(j,0))
enddo

end subroutine CalProd