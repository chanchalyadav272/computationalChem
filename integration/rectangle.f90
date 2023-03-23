module rect
    real(kind=8)::a=0,b=1,f,h,x,sum
    integer(kind=8)::i,n=3 !n-> number of points  => n-1 rectangles

end module rect

program integrate
use rect
implicit none

h=(b-a)/real(n-1)
sum=0
do i=1,n-1
    x = a+(i-1)*h
    call func(x)
    sum = sum + f * h
enddo
print*, sum

end program integrate

subroutine func(xi)
    use rect
    real(kind=8)::xi
    f = xi

end subroutine func