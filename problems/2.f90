program transmission_probability
    implicit none
    integer, parameter :: N = 251 ! Number of points
    real, parameter :: E_min = 1.0 ! Minimum energy (eV)
    real, parameter :: E_max = 26.0 ! Maximum energy (eV)
    real, parameter :: dE = 0.1 ! Energy step size (eV)
    real, parameter :: hbar = 6.582119569e-16 ! Reduced Planck constant (eV*s)
    real, parameter :: m = 9.10938356e-31 ! Mass of electron (kg)
    real, parameter :: L = 1.0e-9 ! Length of region (m)
    real, dimension(0:N) :: psi, psi_prime, k, T
    real :: x, E, V
    integer :: i, j

    open(1,file='data.txt')
    
    ! Loop over energies
    do i = 1, N
        ! Calculate energy and wavevector
        E = E_min + (i-1)*dE
        k(i) = sqrt(2*m*E/hbar**2)
        
        ! Initialize wavefunction at x=0
        psi(0) = 1.0
        psi_prime(0) = k(i)
        
        ! Loop over position
        do j = 1, N
            ! Calculate position and potential
            x = (j-1)*L/N
            V = 0.0
            
            ! Calculate wavefunction and its derivative
            psi(j) = psi(j-1) + psi_prime(j-1)*L/N
            psi_prime(j) = psi_prime(j-1) + 2*m*(V-E)*psi(j-1)*L**2/N**2/hbar**2
            
            ! Check if wavefunction has gone to zero
            if (psi(j)*psi(0) < 0.0) then
                ! Calculate transmission coefficient
                T(i) = (k(i)/k(1))**2
                exit
            endif
        end do
    end do
    
    ! Output transmission probabilities
    do i = 1, N
        E = E_min + (i-1)*dE
        write(1,*) E, T(i)
    end do
    
end program transmission_probability
