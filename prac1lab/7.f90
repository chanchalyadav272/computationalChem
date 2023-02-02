program vol
implicit none

real::r,pi=acos(-1.0)

read*, r

print*, "Area of circle is ", pi*(r*r)

print*, "Volume of sphere is ", (4*pi*(r**3))/3.0







end program vol
