program pib
    implicit none
    integer::num=200, i, xmin=0, xmax=1,n 
    real:: delx, psi, l, x, pi=acos(-1.0)
real, dimension(:,:), allocatable::arr
    ! character(len = 4):: filename='psi-'
    ! character(len= 1024)::number

    print*, "ENTER QUANTUM STATE (n)"
    read*, n
    print*, "ENTER LENGTH OF BOX (l)"
    read*, l
    
    ! write(number, *) n

   ! open (10, file=filename//trim(adjustl(number))//".out")
    delx = (xmax-xmin)/real(num-1)
allocate(arr(num,2))
    
    do i=1,num
    x = xmin + (i-1)*delx
    
    psi =  sqrt(2.0/l)*sin(n*pi*x/l)
arr(i,:)=(/x, psi/)

    
    !write(10,*) x,psi
    
    end do 
    
    do i=1,num
	    print*, arr(i,1), arr(i,2)
        enddo
    
    
    end program pib
