module q
    integer::i,j,grid_num,e_num
    real::m=1,hbar=1,delx=0.01,dele=0.1,xmin=0,xmax=10,emin=1,emax=14,k,pavg
    real,dimension(:),allocatable::x,V,E,pd,t
    complex::z=(0,-1)
    complex,dimension(:),allocatable::psi

end module q

program tunneling
    use q
    implicit none

    grid_num = int((xmax-xmin)/delx)
    e_num = int((emax-emin)/dele)

    allocate(x(0:grid_num),V(0:grid_num),pd(0:grid_num),psi(0:grid_num),E(0:e_num),t(0:e_num))

    open(1,file='ProbabilityDensity_vs_GridPoints.txt')
    open(2,file='TransmissionProbability_vs_IncidentEnergy.txt')
     
    call Cal()
    call CalPd(9.0)

    do i =0,grid_num
        write(1,*) x(i), pd(i)
    enddo

    call CalEnT()

    do i =0,e_num
        write(2,*) E(i), T(i)
    enddo

end program tunneling

subroutine Cal()
    use q
    do i = 0,grid_num
        x(i) = i*delx
        ! v(i) =  9*exp(-((x(i)-4.5)/0.6)**2)
        v(i) = (4*exp(x(i)-1))/((1+exp(x(i)-1))**2)
        ! v(i)=(4*exp(x(i)-1))/((1+exp(x(i)-1)) **2)
    enddo

end subroutine Cal


subroutine CalPd(b)
    use q
    real::b

    k = sqrt(2*m*(b-V(1))/(hbar**2))

    psi(0) = 1
    pd(0) = abs(psi(0))**2
    psi(1) = exp(z*k*delx)
    pd(1) = abs(psi(1))**2
    
    do i=1,grid_num
        psi(i+1) = ((2-(2*m*(b-V(i))*(delx**2)/(hbar**2)))*psi(i)) - psi(i-1)
        pd(i+1) = abs(psi(i+1))**2
    enddo
    
end subroutine CalPd

subroutine CalEnT()
    use q

    do j = 0,e_num
        E(j) = emin + j*dele
        call CalPd(E(j))

        pavg = (minval(pd(601:1000))+maxval(pd(601:1000)))/2

        t(j)= 2/(1+pavg)
    enddo
end subroutine CalEnT








