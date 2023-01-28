module sch
    real, parameter:: pi = acos(-1.0)
    integer::num=100,l,i,k,c
    real::xmin=0.01,xmax=15,delx,x,factorial
    complex::h
    real, dimension(:), allocatable::j
    real, dimension(:), allocatable::n
end module sch