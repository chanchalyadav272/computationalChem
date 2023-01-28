program value1
implicit none
real::a
integer::i=5,j=4,k=3

print*, i*j+(i/k)
print*, (i/j)+(1/k)
print*, (j/i)+(1/k)

end program value1



program value2
implicit none 
real::a=5.0,b=4.0,c=3.0

print*, a*b+(1/c)
print*, (a/b)+(1/c)
print*, (b/a)+(1/c)

end program value2

