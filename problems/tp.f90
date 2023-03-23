module tp 
    real, dimension(:), allocatable:: x, v, pd,e
    complex, dimension(:), allocatable:: psi 
    integer:: j,i,m
    real::k, t, pavg,max,min
    complex::z=(0,1)


end module tp 

program Prob 
    use tp 
    implicit none 
    allocate(x(0:1000),v(0:1000),psi(0:1000),pd(0:1000),e(0:150))
    open(unit=10, file='output5495.txt')

    open(2,file='out.txt')
    call calx
call calpsi(9.0)
    do m = 0,1000
        write(2,*) x(m), pd(m)
    enddo
    

    
    do m=0,140
        E(m)=1+ m*0.1
        call calpsi(E(m))
        call tprob 
        write(10,*) E(m), t
    end do
        

end program Prob 



subroutine calx 

    use tp 
    do j=0,1000
        x(j)=j*0.01

    if(x(j)<=4.5) then 
    v(j)= 9*exp(-((x(j)-4.5)/0.6)**2)
    else 
        v(j)=( 9*exp(-((x(j)-4.5)/0.6)**2) )+ 4

    end if
enddo
end subroutine calx 

subroutine calpsi (b)
        use tp 
        real::b
        
        
    
        psi(0) = 1
        pd(0)=abs(psi(0))**2

        k = sqrt(2*1*(b-V(1))/(1**2))
        psi(1) = exp(z*k*0.01)
        pd(1)=abs(psi(1))**2
        do i=1,999
            psi(i+1) = ((2-(2*1*(b-V(i))*(0.01**2)/(1**2)))*psi(i)) - psi(i-1)

            pd(i+1)= abs(psi(i+1))**2
        enddo

        ! psi(0)=1
        ! pd(0)= abs(psi(0))**2

        ! k=sqrt(2*(b-v(1)))
        

    end subroutine calpsi

    subroutine tprob 
        use tp 
        max=maxval(pd(601:1000))
        min=minval(pd(601:1000))
        pavg= (max+min)/2

        t= 2/(1+pavg)

    end subroutine tprob 


