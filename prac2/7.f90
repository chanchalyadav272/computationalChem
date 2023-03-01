program matrixMul
implicit none
integer::a,b,c,i,j,k
real:: sm
real, dimension(:,:), allocatable:: m1, m2, m3
open(1,file="7_input.txt")
open(2,file="7_output.txt")

read(1,*) a,b,c
allocate(m1(a,b))
allocate(m2(b,c))
allocate(m3(a,c))
do i=1,a
read(1,*) (m1(i,j), j=1,b)
enddo

!print*, m1

do i=1,b
read(1,*) (m2(i,j), j=1,c)
enddo

!print*, m2


do i=1,a
do j=1,c
sm=0
do k=1,b
sm = sm+ m1(i,k)*m2(k,j)
enddo
m3(i,j) = sm
enddo
enddo

write(2,*) "using do-loop and arrays"
do i=1,a
write(2,*)(m3(i,j),j=1,c)
enddo

write(2,*) "using library function"
write(2,*) matmul(m1,m2)







end program matrixmul
