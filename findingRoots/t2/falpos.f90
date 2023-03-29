module fal
    real(kind=8)::a,fa,b,fb,x,fx,err,e,f,temp
    integer(kind=8)::i,j,k

end module fal

program root
    use fal
    implicit none
    open(1,file='out.txt')
    e=1E-4

    i=0
    a=1
    call func(a)
    fa=f
    b=3
    call func(b)
    fb=f
    x= a - (b-a)*fa/(fb-fa)
    call func(x)
    fx = f
    err=1
    write(1,*) i,a,fa,b,fb,x,fx,err

    
    if(fa*fb<0) then
        i=1
        do
            temp=x
            if(fa*fx<0) then
                b=x
                fb=fx
            else if(fb*fx<0) then
                a=x
                fa=fx
            endif
            x= a - (b-a)*fa/(fb-fa)
            call func(x)
            fx = f
            err = abs((x-temp)/x)
            write(1,*) i,a,fa,b,fb,x,fx,err
            if(err<=e) then
                exit
            endif
            i=i+1
        enddo
    else
        print*,"invalid interval"
    endif


end program root

subroutine func(xi)
    use fal
    real(kind=8)::xi
    f= xi**2 - xi -2
    
end subroutine func