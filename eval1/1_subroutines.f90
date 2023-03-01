
subroutine CalDelx
    use sch
    delx = (xmax-xmin)/real(num-1)
end subroutine CalDelx

real function fact(a)
    use sch
    integer::a

    if(a<0) then 
        print*, "INPUT FOR FACTORIAL MUST BE AN NON NEGATIVE INTEGER"
    elseif(a==0) then
        fact = 1
    else
        fact = 1
        do i=1,a
            fact = fact*i
        enddo
    endif

end function fact

subroutine bessel(a,b)
    use sch
    integer::a
    real::b

    if(a==0) then
        j(a) = sin(b)/b
        if(b<=0.2) j(a) = (b**a)/fact(a)
        n(a)=((-1)*cos(b)/b)
    elseif(a==1) then
        j(a) = (sin(b)/(b**2))-(cos(b)/b)
        if(b<=0.2) j(a) = (b**a)/fact(a)
        n(a)= (-1)*((cos(b)/(b**2))+(sin(b)/b))
     endif

end subroutine bessel

subroutine higherBessel1(a,b)
    use sch
    integer::a
    real::b

    call bessel(0,b)
    call bessel(1,b)

    do k=1,a-1
        if(b<=0.2) then 
            j(k+1) = (b**(k+1))/fact(a)
        else
            j(k+1) = ((2*k+1)/b)*j(k) - j(k-1)
        endif
    enddo

end subroutine higherBessel1

subroutine higherBessel2(a,b)
    use sch
    integer::a
    real::b

    call bessel(0,b)
    call bessel(1,b)

    do k=1,a-1
        n(k+1) = (((2*k+1)/b)*(n(k)) - n(k-1))
    enddo

end subroutine higherBessel2


