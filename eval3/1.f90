module q1
    real(kind=8)::a=0,b=1,f,x,h,fa,fb,sum
    integer(kind=8)::n,i,j,k

end module q1


program hello
    use q1
    implicit none

    open(1,file='1_output.txt')

    do j=1,3
        n=j*2

        print*, "for n=",n
        call rect
        call trap
        call simp
        print*,""

    enddo

end program hello

subroutine func(xi)
    use q1
    real(kind=8)::xi

    f= 1/(1+ (xi**2))


end subroutine func

subroutine rect
    use q1
    h=(b-a)/real(n)

    sum=0
    do i=1,n
        x = a+(i-1)*h
        call func(x)
        sum = sum + f * h
    enddo

    print*,"integral using rectangular is" ,sum

    
end subroutine rect

subroutine trap
    use q1

    call func(a)
    fa = f
    call func(b)
    fb=f
    h= (b-a)/real(n)

    sum = (fa + fb)/2

    do i=1,n-1
        x= a+i*h
        call func(x)
        sum = sum + f
    enddo

    sum = sum*h

    print*,"integral using trapezoidal is" ,sum

end subroutine trap

subroutine simp
    use q1

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

    print*, "integral using simpson's 1/3 rule is ", sum

end subroutine simp
































