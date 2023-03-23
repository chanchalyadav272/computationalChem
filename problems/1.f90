module q
    integer::i,j,grid_num,e_num
    real::m=1,hbar=1,delx=0.01,dele=0.1,xmin=0,xmax=5,emin=1,emax=26,k,pavg,abc=10
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
     
    call CalX()
  
    call CalPd(9.0)

    do i =0,grid_num
        write(1,*) x(i), pd(i)
    enddo

    

end program tunneling

subroutine CalX()
    use q
    do i = 0,grid_num
        x(i) = i*delx
        v(i) = abc*x(i)
        write(2,*) x(i), v(i)
    enddo

end subroutine CalX


subroutine CalPsi(b)
    use q
    real::b

    k = sqrt(2*m*(b-V(1))/(hbar**2))

    psi(0) = 1
    psi(1) = exp(z*k*delx)
    
    do i=1,grid_num
        psi(i+1) = ((2-(2*m*(b-V(i))*(delx**2)/(hbar**2)))*psi(i)) - psi(i-1)
    enddo
    
end subroutine CalPsi

subroutine CalPd(a)
    use q
    real::a
    call CalPsi(a)
    do i=0,grid_num
        pd(i) = abs(psi(i))**2
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








