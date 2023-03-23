module simp
    real(kind=8),parameter::pi=acos(-1.0)
    real(kind=8)::a=0,b=pi/2,f,x,sum,fa,fb,h
    integer(kind=8)::i,n=6 !n-> even number of segments


end module simp

program integrate
    use simp 
    implicit none

    call func(a)
    fa = f
    call func(b)
    fb =f

    sum = fa + fb
    h=(b-a)/real(n)

    do i=1, n/2
        x = a + (2*i-1)*h
        call func(x)
        sum = sum + 4*f
    enddo

    do i=1, (n/2 -1)
        x = a + (2*i)*h
        call func(x)
        sum = sum + 2*f
    enddo

    sum = sum *h/3

    print*, sum


end program integrate

subroutine func(xi)
    use simp
    real(kind=8)::xi

    f = sqrt(sin(xi))


end subroutine func