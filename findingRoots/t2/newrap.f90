module nrr
    integer(kind=8)::i,j,k
    real(kind=8)::x,f,g,e,err,temp
end module nrr

program root
    use nrr
    implicit none
    open(1,file='out.txt')

    e=1E-6
    i=0
    x=1
    call func(x)
    err = abs(x)
    write(1,*) i,x,f,g,err
    i=1
    do
        temp=x
        x = temp - (f/g)
        err = abs(x-temp)
        call func(x)

        write(1,*) i,x,f,g,err

        if(i==5) then
            exit
        endif

        i=i+1
    enddo
    
end program root

subroutine func(a)
    use nrr
    real(kind=8)::a
    ! f =  (a**2) -(3*a) + 2
    ! g =  2*a -3    
    f = exp(a) - a - 2
    g = exp(a) -1
end subroutine func