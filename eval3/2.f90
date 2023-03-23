module q2
    real(kind=8),parameter::pi=acos(-1.0)
    real(kind=8)::a=0,b=pi,f,x,h,fa,fb,sum,p,dp=20/1000.0
    integer(kind=8)::n=1000,i,k,l,m
    real(kind=8),dimension(:,:),allocatable::j

end module q2


program hello
    use q2
    implicit none

    open(1,file="2_output.txt")
    allocate(j(0:1000,0:3))


    do m=0,2        ! for m= 0,1,2
        do l =0,1000    ! for x = 0 to 20
            p = l*dp   
            j(l,0)=p
            call simp
            j(l,m+1) = sum/pi  
        enddo
    enddo

    do k=0,1000
        write(1,*) (j(k,l), l=0,3)
    enddo


end program hello

subroutine func(xi)
    use q2
    real(kind=8)::xi

    f= cos(m*xi - p*sin(xi))


end subroutine func


subroutine simp
    use q2

    call func(a)
    fa = f
    call func(b)
    fb=f
    h= (b-a)/real(n)

    sum = fa + fb

    do i=1,n/2
        x = a + (2*i-1)*h
        call func(x)
        sum=sum+ 4*f
    enddo

    do i=1,(n/2-1)
        x= a+ 2*i*h
        call func(x)
        sum =sum + 2*f
    enddo

    sum = sum*h/3


end subroutine simp

