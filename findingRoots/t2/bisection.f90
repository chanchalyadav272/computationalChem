module bi
    integer(kind=8)::i,j,k
    real(kind=8)::a=1,b=2,e,f
    ! d -> [iteration   a   f(a)    b   f(b)    x   f(x)    e ]
    real(kind=8),dimension(:,:),allocatable::d
    
end module bi

program root
    use bi
    implicit none
    allocate(d(0:1000,8))
    open(1,file='out.txt')
    i=0
    e=1E-4
    

    d(i,1)=i
    d(i,2)=a
    call func(d(i,2))
    d(i,3) = f
    d(i,4)=b
    call func(d(i,4))
    d(i,5) = f
    d(i,6) = (d(i,2) + d(i,4))/2
    call func(d(i,6))
    d(i,7) = f
    d(i,8) = 1

    if(d(i,3)*d(i,5)==0) then
        if(d(i,3)==0) then
            print*, d(i,2), "is a root"
        endif
        if(d(i,5)==0) then
            print*, d(i,4), "is a root"
        endif
    else if(d(i,3)*d(i,5)<=0) then 
        i=1
        do
            call bis
            if(d(i,8)<=e) then
                exit
            endif
            i=i+1
        enddo
        do j=0,i
            write(1,*) (d(j,k) ,k=1,8)
        enddo

    else
        print*, "cannot be calculated in this interval"
    endif

end program root

subroutine func(xi)
    use bi
    real(kind=8)::xi
    f= (xi**3) + 4*(xi**2) -10
    
end subroutine func

subroutine bis()
    use bi 
    d(i,1)=i
    if(d(i-1,3)*d(i-1,7)<0) then 
        d(i,2) = d(i-1,2)
        d(i,3) = d(i-1,3)
        d(i,4) = d(i-1,6)
        d(i,5) = d(i-1,7)
    else
        d(i,2) = d(i-1,6)
        d(i,3) = d(i-1,7)
        d(i,4) = d(i-1,4)
        d(i,5) = d(i-1,5)
    endif

    d(i,6) = (d(i,2) + d(i,4))/2
    call func(d(i,6))
    d(i,7) = f

    d(i,8) = abs((d(i,6)- d(i-1,6))/d(i,6))

    
end subroutine bis