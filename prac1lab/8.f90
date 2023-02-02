program pib
    implicit none
    integer::num=200, i, xmin=0, xmax=1,n 
    real:: delx, psi, l, x, pi=acos(-1.0)
    character(len = 4):: filename='psi-'
    character(len= 1024)::number

    print*, "ENTER QUANTUM STATE (n)"
    read*, n
    print*, "ENTER LENGTH OF BOX (l)"
    read*, l
    
    write(number, *) n

    open (10, file=filename//trim(adjustl(number))//".out")
    delx = (xmax-xmin)/real(num-1)
    
    do i=1,num
    x = xmin + (i-1)*delx
    
    psi =  sqrt(2.0/l)*sin(n*pi*x/l)
    
    write(10,*) x,psi
    
    end do 
    
    
    
    
    end program pib
    