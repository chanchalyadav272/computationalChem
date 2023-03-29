module sec
    integer(kind=8)::i,j,k
    real(kind=8)::x0,x1,f,e,fx0,fx1,err,x2,fx2
end module sec
program root
    use sec
    implicit none
    open(1,file='out.txt')

    e=1E-6
    i=0
    x0=-3.00001
    call func(x0)
    fx0=f
    err=1
    write(1,*) i,x0,fx0,err
    i=1
    x1=0
    call func(x1)
    fx1=f
    err=abs((x1-x0)/x1)
    write(1,*) i,x1,fx1,err

    i=2
    do 
        x2=x1 - fx1*(x1-x0)/(fx1-fx0)
        call func(x2)
        fx2=f
        err=abs((x2-x1)/x2)
        x0=x1
        fx0=fx1
        x1=x2
        fx1=fx2

        write(1,*) i,x2,fx2,err
        if(err<=e) then
            exit
        endif

        ! if(fx2==0) then 
        !     exit
        ! endif

        ! if(i==1000) then
        !     exit
        ! endif

        i=i+1
    enddo
   

end program root
subroutine func(a)
    use sec
    real(kind=8)::a
    ! f =  (a**2) -(4*a) -10
    f = a**2 + a - 2
 
end subroutine func