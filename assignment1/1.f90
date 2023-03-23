module q1
    real,parameter::pi =acos(-1.0)
    integer::i,j,l
    real::m=1,h=1,e=9,k,max,min,p,t
    real, dimension(:), allocatable::x,v,pd
    complex, dimension(:), allocatable::psi
    
    complex::a

end module q1

program q
    use q1
implicit none

allocate(x(0:1000),v(0:1000),psi(0:1000), pd(601:1000))
open(10,file='data1.txt')
open(20,file='data2.txt')

e=9
call wave()
do i=0,1000
write(10,*) x(i), abs(psi(i))**2
enddo

do j=0,1000
    e = 1+ j*0.1
    call wave()
    
    do l=601,1000
        pd(l) = abs(psi(l))**2        
    enddo
    min = minval(pd)
    max = maxval(pd)
    p = (min+max)/2
    t=2/(1+p)

    write(20,*) e, t

enddo


end program q

subroutine wave()
    use q1

    do i=0,1000
        x(i)=i*0.01
        if(x(i)>=4 .and. x(i)<=5) then
            v(i) = 9
        else 
            v(i) =0
        endif
    enddo

    psi(0)=1

    k= sqrt(2*m*(e-v(1))/(h**2))
    
    a=(0,-1)
    psi(1)=exp(a*k*0.01)
    

    do i=1,999
    psi(i+1) = ((2-(2*m*(e-v(i))*(0.01**2)/(h**2)))*psi(i)) - psi(i-1)
    enddo
end subroutine wave