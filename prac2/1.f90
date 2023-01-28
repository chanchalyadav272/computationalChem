Program arr
implicit none

integer::n,i
integer, dimension(:), allocatable :: numbers



read*, n
allocate(numbers(n))
read*, (numbers(i), i=1,n)
print*, numbers

end program arr

