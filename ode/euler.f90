module eu
real(kind=8),dimension(:,:),allocatable::e
real(kind=8)::h=0.25,f,x0=1,y0=2
integer(kind=8)::n=4,i,j

end module eu

program ode
    use eu
    implicit none
    allocate(e(0:n,3))
    open(1,file='out.txt')

    call euler

    do i=0,n
        write(1,*) (e(i,j),j=1,3)
    enddo
    

end program ode

subroutine func(a,b)
    use eu
    real(kind=8)::a,b

    f=3*(a**2) +1
    
end subroutine func

subroutine euler
    use eu
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