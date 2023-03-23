module trap
    real(kind=8)::a=1,b=1.2,f,x,sum,fa,fb
    integer(kind=8)::i,n=1 !n-> number of segments

end module trap

program integrate
    use trap
    implicit none

    call func(a)
    fa = f
    call func(b)
    fb =f

    sum = (fa+fb)/2

    do i=1,n-1
        x = a + i*(b-a)/real(n)
        call func(x)

        sum = sum + f
    enddo

    sum = sum *(b-a)/real(n)

    print*, sum

end program integrate


subroutine func(xi)
    use trap
    real(kind=8)::xi

    f = 1/xi
    
end subroutine func