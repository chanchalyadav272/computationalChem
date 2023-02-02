program schWave
    implicit none

    integer:: grid_num,i,j
    real::j0,j1,n0,n1, xmin, xmax, delx,x
    real,dimension(:,:), allocatable:: arr
    ! character(len=18), dimension(1,5)::ar
    open(1, file="8_input.txt")
    open(2,file="8_output.txt")

    ! ar(1,:) = (/"x    ", "j0(x)", "j1(x)", "n0(x)", "n1(x)"/)
    read(1,*) xmin, xmax, grid_num
    ! write(2,*) ar
    write(2,*) "       x             j0(x)            j1(x)             n0(x)            n1(x)"
    
        
    ! print*,"ENTER THE VALUE OF Xmin"
    ! read*, xmin
    ! print*,"ENTER THE VALUE OF Xmax"
    ! read*, xmax
    ! print*,"ENTER THE VALUE OF TOTAL GRID POINTS"
    ! read*, grid_num

    delx = (xmax-xmin)/real(grid_num-1)

    allocate(arr(grid_num,5))
        
    do i=1,grid_num
        x=xmin + (i-1)*delx
        j0= (sin(x))/x
        j1=(sin(x))/(x**2) - ((cos(x))/x)
        n0=-(cos(x))/x
        n1=- (cos(x))/(x**2) - ((sin(x))/x)

        arr(i,:) = (/x, j0, j1, n1, n0/)
        
    enddo

    do i=1,grid_num
        write(2,*) (arr(i,j), j=1,5)
    enddo





end program schWave



