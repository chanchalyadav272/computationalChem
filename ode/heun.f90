module hn
    real(kind=8),dimension(:,:),allocatable::e
    real(kind=8)::h=0.25,f,x0=1,y0=2
    integer(kind=8)::n=4,i,j


end module hn

program ode
    use hn
    implicit none
    allocate(e(0:n,5))
    open(1,file='out.txt')

    call heun

    do i=0,n
        write(1,*) (e(i,j),j=1,5)
    enddo
    

end program ode

subroutine func(a,b)
    use hn
    real(kind=8)::a,b
    f=2*b/a
end subroutine func

subroutine heun
    use hn
    i=0
    e(i,1)=x0
    e(i,2)=y0
    call func(e(i,1),e(i,2))
    e(i,3)=f
    e(i,4)=e(i,2)
    e(i,5)=e(i,3)

    do i=1,n
        e(i,1) = e(i-1,1) +h
        e(i,2)= e(i-1,4) + h*e(i-1,5)
        call func(e(i,1),e(i,2))
        e(i,3) = f
        e(i,4) = e(i-1,4) + (e(i-1,5) + e(i,3))*h/2
        call func(e(i,1),e(i,4))
        e(i,5) =f
    enddo
    
end subroutine heun