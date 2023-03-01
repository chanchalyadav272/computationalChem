subroutine CalTerm(m)
use ip

real::m, prod

do i=1,num
prod = 1
do j=1,num

if(i==j) then
prod = prod *1
else 
prod  = prod * ((m-x(j))/(x(i)-x(j)))
endif

enddo

z(i)= y(i) * prod
enddo

end subroutine CalTerm

subroutine CalPol(n)
use ip
real::n,sum=0

call CalTerm(n)

do k=1,num
sum = sum + z(k)
enddo

print*, sum

end subroutine CalPol