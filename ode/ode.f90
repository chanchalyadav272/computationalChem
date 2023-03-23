module o
    ! e -> [x   y   f(x,y)]
    ! hn -> [x  y(euler)    f(x,y(euler))   y(heun)    f(x,y(heun))]
    ! r -> [x   s1  s2  s3  s4  y]
    real(kind=8),dimension(:,:),allocatable::e,hn,r
    real(kind=8)::h=0.25,x0=1,y0=2,f,s1,s2,s3,s4
    integer(kind=8)::n=4,i,j
end module o

program ode
    use o
    implicit none
    allocate(e(0:n,3),hn(0:n,5), r(0:n,0:5))

    open(1,file='out.txt')
    ! just writing x and y values
    call euler
    write(1,*) "using euler"
    do i=0,n
        write(1,*) e(i,1),e(i,2)
    enddo
    write(1,*)

    call heun
    write(1,*) "using heun"
    do i=0,n
        write(1,*) hn(i,1),hn(i,4)
    enddo
    write(1,*)

    call rk
    write(1,*) "using runge-kutta"
    do i=0,n
        write(1,*) r(i,0),r(i,5)
    enddo

end program ode

subroutine func(a,b)
    use o
    real(kind=8)::a,b
    f=2*b/a
end subroutine func

subroutine euler()
    use o 
    i=0
    e(i,1)=x0
    e(i,2)=y0
    call func(e(i,1),e(i,2))
    e(i,3) =f
    do i=1,n
        e(i,1)=e(i-1,1) + h
        e(i,2)= e(i-1,2) + h*e(i-1,3)
        call func(e(i,1),e(i,2))
        e(i,3) =f
    enddo    
end subroutine euler

subroutine heun()
    use o
    i=0
    hn(i,1)=x0
    hn(i,2)=y0
    call func(hn(i,1),hn(i,2))
    hn(i,3)=f
    hn(i,4)=hn(i,2)
    hn(i,5)=hn(i,3)

    do i=1,n
        hn(i,1) = hn(i-1,1) +h
        hn(i,2)= hn(i-1,4) + h*hn(i-1,5)
        call func(hn(i,1),hn(i,2))
        hn(i,3) = f
        hn(i,4) = hn(i-1,4) + (hn(i-1,5) + hn(i,3))*h/2
        call func(hn(i,1),hn(i,4))
        hn(i,5) =f
    enddo
    
end subroutine heun

subroutine rk()
    use o
    i=0
    r(i,0)=x0
    r(i,5)=y0

    do i=1,4
        r(i,0)=r(i-1,0)+h
        call func(r(i-1,0),r(i-1,5))
        r(i,1)= h* f
        call func(r(i-1,0)+h/2,r(i-1,5)+r(i,1)/2)
        r(i,2)= h* f
        call func(r(i-1,0)+h/2,r(i-1,5)+r(i,2)/2)
        r(i,3)= h* f
        call func(r(i-1,0)+h,r(i-1,5)+r(i,3))
        r(i,4)= h* f
        
        r(i,5)= r(i-1,5) + (r(i,1)+2*r(i,2)+2*r(i,3)+r(i,4))/6
    enddo
    
end subroutine rk
