module t
real(kind=8)::b=4,c

end module t

program h
    use t
    implicit none
    print*,f(b)
end program h

real(kind=8) function f(a)
use t
implicit none
real(kind=8)::a

f = a**2
return
end function f