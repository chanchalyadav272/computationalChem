program lang
    implicit none
    integer::i,j,n=2,c
    real:: prod,sum=0.0,m
    real,dimension(:), allocatable::x
    real,dimension(:), allocatable::y
    
    allocate(x(0:n),y(0:n))
    
    do c = 0,n
    read*, x(c),y(c)
    end do
    read*,m
    
    do i=0,n
    prod=1.0
    do j=0,n
    
    if(i==j) then
    prod=prod*1
    else
    
    prod= prod*((m-x(j))/(x(i)-x(j)))
    endif
    end do
    sum=sum+ y(i)*(prod)
    enddo
    print*,sum
    end program lang