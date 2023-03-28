module se
    integer(kind=8)::i,j,k
    real(kind=8), dimension(:,:),allocatable::x
    real(kind=8)::x0=-3.00001,x1=0,f,e
end module se
program root
    use se
    implicit none
    open(1,file='out.txt')
    allocate (x(0:10000,4))

    e=1E-6
    i=0
    x(i,1)=i
    x(i,2)= x0
    call func(x(i,2))
    x(i,3)=f
    i=1
    x(i,1)=i
    x(i,2)= x1
    call func(x(i,2))
    x(i,3)=f

    i=2
    do 
        x(i,1)=i
        x(i,2) = x(i-1,2) - x(i-1,3)*(x(i-1,2)-x(i-2,2))/(x(i-1,3)-x(i-2,3))
        call func(x(i,2))
        x(i,3)=f

        x(i,4) = abs((x(i,2)-x(i-1,2))/x(i,2))
        if(x(i,4)<=e) then
            exit
        endif

        ! if(i==9) then
        !     exit
        ! endif

        i=i+1
    enddo

    do j=0,i
        write(1,*) (x(j,k),k=1,4)
    enddo


end program root
subroutine func(a)
    use se
    real(kind=8)::a
    ! f =  (a**2) -(4*a) -10
    f = a**2 + a - 2
 
end subroutine func