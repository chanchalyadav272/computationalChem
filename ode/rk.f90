module r
    real(kind=8),dimension(:,:),allocatable::e
    real(kind=8)::h=0.25,x0=1,y0=2,f,s1,s2,s3,s4
    integer(kind=8)::n=4,i,j
end module r

program ode
    use r
    implicit none
    allocate(e(0:n,0:5))

    open(1,file='out.txt')

    call rk

    do i=0,n
        write(1,*)(e(i,j),j=0,5)
    enddo


end program ode

subroutine func(a,b)
    use r
    real(kind=8)::a,b
    
    ! f= (-2*b)+a+4
    f=2*b/a
    
end subroutine func


    
subroutine rk()
    use r
    i=0
    e(i,0)=x0
    e(i,5)=y0

    do i=1,4
        e(i,0)=e(i-1,0)+h
        call func(e(i-1,0),e(i-1,5))
        e(i,1)= h* f
        call func(e(i-1,0)+h/2,e(i-1,5)+e(i,1)/2)
        e(i,2)= h* f
        call func(e(i-1,0)+h/2,e(i-1,5)+e(i,2)/2)
        e(i,3)= h* f
        call func(e(i-1,0)+h,e(i-1,5)+e(i,3))
        e(i,4)= h* f
        
        e(i,5)= e(i-1,5) + (e(i,1)+2*e(i,2)+2*e(i,3)+e(i,4))/6
    enddo


    
end subroutine rk

