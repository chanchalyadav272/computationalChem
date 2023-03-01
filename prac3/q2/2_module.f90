Module q2
    real, parameter :: pi=acos(-1.0)
    integer::num=50, i,j,k,n
    real:: delx, sum, x, xmin=0, xmax=2*pi
    
    ! contains

    ! subroutine CalcDelx()
    !     delx = (xmax-xmin)/real(num-1)
        
    !     return
    ! end subroutine CalcDelx

    ! real function factorial(a)
    ! integer::a

    ! if (a==0) then 
    !     factorial=1
    ! else 
    !     factorial=1
    !     do i=1,a
    !         factorial=factorial*i
    !     enddo
    ! endif
    ! return
    ! end function factorial

    ! subroutine CalSin(n,x)
    !     integer::n
    !     real::x

    !     do i=0,n
    !     sum = sum + (((-1)**i)*(x**(2*i+1))/factorial(2*i+1))
    !     enddo

    !     return
    ! end subroutine CalSin

    ! subroutine printSinX(n)
    !     integer::n

    !     call CalcDelx
        
    !     do i=1,num
    !         x= xmin +(i-1)*delx
    !         call CalSin(n,x)
    !         print*, x, sum
    !     enddo
    ! return

    ! end subroutine printSinX



    
end module q2