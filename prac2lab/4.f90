program factorial
implicit none
integer::n, fct=1,i

print*, "ENTER AN INTEGER"
READ*,n

if(n<0) then 
print*, "ENTER A VALID INTEGER"
elseif (n==0) then
print*, fct
else
do i=1,n
fct = fct*i
enddo
print*, fct
endif



end program factorial
