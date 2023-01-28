subroutine CalcDelx()
    use q2
    delx = (xmax-xmin)/real(num-1)
    
    
end subroutine CalcDelx

real function factorial(a)
use q2
integer::a

if (a==0) then 
    factorial=1
else 
    factorial=1
    do i=1,a
        factorial=factorial*i
    enddo
endif
return
end function factorial



subroutine CalSin(a,b)
    use q2
    integer::a
    real::b

    do j=0,a
        
    sum = sum + (((-1)**j)*(b**(2*j+1))/factorial(2*j+1))
    enddo

    
end subroutine CalSin

subroutine printSinX(a)
    use q2
    integer::a

    call CalcDelx
    
    do k=1,num
        x= xmin +(k-1)*delx
        sum=0
        call CalSin(a,x)
        print*, x, sum

    enddo


end subroutine printSinX
