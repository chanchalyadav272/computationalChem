module q3
    integer(kind=8)::i,j,k
    real(kind=8), dimension(:,:),allocatable::x
    real(kind=8)::f,g,e

end module q3

program newton
    use q3
    implicit none
    open(1,file='out.txt')
    allocate (x(10000,3))
    e=1E-6
    i=1
    x(i,1)=i
    x(i,2)=0
    x(i,3) = 1

    do
        x(i+1,1) = i+1
        call func(x(i,2))
        x(i+1,2) = x(i,2) - (f/g)
        x(i+1,3) = abs((x(i+1,2)-x(i,2))/(x(i+1,2)))

        if(x(i+1,3)<e) then
            exit
        endif

        i=i+1
    enddo

    do j=1,i+1
        write(1,*) (x(j,k),k=1,3)
    enddo
    
end program newton

subroutine func(a)
    use q3
    real(kind=8)::a
    f =  (a**2) -(3*a) + 2
    g =  2*a -3    
end subroutine func