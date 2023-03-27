module nr
    integer(kind=8)::i,j,k
    real(kind=8), dimension(:,:),allocatable::x
    real(kind=8)::f,g,e
end module nr

program root
    use nr
    implicit none
    open(1,file='out.txt')
    allocate (x(0:10000,3))
    e=1E-6
    i=0
    x(i,1)=i
    x(i,2)=1
    x(i,3) = abs(x(i,2))

    i=1
    do
        x(i,1) = i
        call func(x(i-1,2))
        x(i,2) = x(i-1,2) - (f/g)
        x(i,3) = abs((x(i,2)-x(i-1,2)))

        if(x(i,1)==5) then
            exit
        endif

        i=i+1
    enddo

    do j=0,i
        write(1,*) (x(j,k),k=1,3)
    enddo
    
end program root

subroutine func(a)
    use nr
    real(kind=8)::a
    ! f =  (a**2) -(3*a) + 2
    ! g =  2*a -3    
    f = exp(a) - a - 2
    g = exp(a) -1
end subroutine func