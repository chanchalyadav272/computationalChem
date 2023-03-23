program tunn
    implicit none
    integer::i,j,l
    real::a=9,c=0.6,k,xg=4.5,delx=0.01,e,m=1,h=1,max,min,t,pavg
    real, dimension(:), allocatable::x,v,pd
    complex, dimension(:), allocatable::psi
    complex::z=(0,-1)
        
        allocate(x(0:1000),v(0:1000),psi(0:1000),pd(601:1000))
        open(20,file='data1.txt')
        open(40,file='data2.txt')
        
        do i=0,1000
        x(i)=i*delx
        v(i)=a*exp(-((x(i)-xg)/c)**2)
        end do
        e=9
        psi(0)=1
        k= sqrt(2*m*(e-v(1))/(h**2))
        psi(1)=exp(z*k*delx)
        
        do i=1,999
            psi(i+1) = ((2-(2*m*(e-V(i))*(delx**2)/(h**2)))*psi(i)) - psi(i-1)
        enddo
    do i=0,1000
    write(20,*) x(i), abs(psi(i))**2
    enddo
    
    do j=0,130
    e=j*0.1+1
     k= sqrt(2*m*(e-v(1))/(h**2))
     psi(0)=1
     
      psi(1)=exp(z*k*delx)
        
        do i=1,999
        psi(i+1) = ((2-(2*m*(e-V(i))*(delx**2)/(h**2)))*psi(i)) - psi(i-1)
        enddo
        do l=601,1000
    
        pd(l) = abs(psi(l))**2 
        enddo
        min = minval(pd)
        max = maxval(pd)
        pavg = (min+max)/2
        t=2/(1+pavg)
    
        write(40,*) e, t
    
    enddo
    
    end program tunn