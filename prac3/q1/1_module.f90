Module q1
    real, parameter :: pi=acos(-1.0)
    integer::num=200, i, xmin=0, xmax=1,n,l=1
    real:: delx, psi,  x


    contains 
    subroutine CalDelx()
        
        delx = (xmax-xmin)/real(num-1)
       
        
    end subroutine CalDelx
    
    subroutine CalPsi(n)
        integer::n
        call CalDelx()
        do i=1,num
            x = xmin + (i-1)*delx         
            psi =  sqrt(2.0/l)*sin(n*pi*x/l)
            print*, x, psi    
            end do 
        

        end subroutine CalPsi

    end module q1