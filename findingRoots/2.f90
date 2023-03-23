module q1
    integer(kind=8)::i,j,k
    real(kind=8)::a=0,b=3,e,f
    real(kind=8),dimension(:,:),allocatable::d
end module q1

program bis
use q1
implicit none

allocate(d(100000,8))

open(1,file='out.txt')


i=1
! e=1E-4
! e = 1E-6

d(1,2)=a
call func(d(1,2))
d(1,3) = f
d(1,4)=b
call func(d(1,4))
d(1,5) = f
d(1,6) = d(1,2) - ((d(1,4)-d(1,2))*d(1,3)/(d(1,5)-d(1,3)))
call func(d(1,6))
d(1,7) = f
d(1,8) = 0

if(d(i,3)*d(i,5)<=0) then 
do
    d(i,1)=i
    call cal
    ! if(d(i+1,8)<=e) then
    !     exit
    ! endif
    i=i+1
    if(i==10) then
        print*, d(i,6)
        exit
    endif
enddo
do j=1,i
    write(1,*) (d(j,k) ,k=1,8)
enddo
else 
    print*, "no real roots in the interval"
endif
end program bis

 subroutine func(p)
use q1
real(kind=8)::p


f = p**2 + p -2

return 
 end subroutine func

 subroutine cal
    use q1
    

    if(d(i,3)*d(i,7)<0) then 
        d(i+1,2) = d(i,2)
        d(i+1,3) = d(i,3)
        d(i+1,4) = d(i,6)
        d(i+1,5) = d(i,7)
    else
        d(i+1,2) = d(i,6)
        d(i+1,3) = d(i,7)
        d(i+1,4) = d(i,4)
        d(i+1,5) = d(i,5)
    endif
    d(i+1,6) = d(i+1,2) - ((d(i+1,4)-d(i+1,2))*d(i+1,3)/(d(i+1,5)-d(i+1,3)))
    call func(d(i+1,6))
    d(i+1,7) = f

    d(i+1,8) = abs((d(i+1,6)- d(i,6))/d(i+1,6))

    
end subroutine cal

