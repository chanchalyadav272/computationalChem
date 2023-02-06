program arr_sum
implicit none

integer:: rw, clm, i,j, sm=0
real, dimension(:,:), allocatable :: arr
read*, rw, clm

allocate(arr(rw,clm))

do i=1, rw
	read*, (arr(i,j), j = 1, clm)
enddo

do i=1,rw
do j=1,clm
	sm = sm + arr(i,j)
enddo
enddo

print*, sm
print*, arr

end program arr_sum
