module q4
    integer(kind=8)::i,j,k
    real(kind=8), dimension(:,:),allocatable::x
    real(kind=8)::f,g1,g2,e

end module q4

program secant
    use q4
    implicit none
    open(1,file='out.txt')
    allocate (x(0:10000,3))
    e=1E-6
    i=0
    x(i,1)=i
    x(i,2)=4
    x(i,3) = 1
    i=1
    x(i,1)=i
    x(i,2)=2
    x(i,3) = abs((x(i,2)-x(i-1,2))/(x(i,2)))

    do 
        x(i+1,1) = i+1

        call func(x(i,2))
        g1=f
        call func(x(i-1,2))
        g2=f

        x(i+1,2) = x(i,2) - (g1*(x(i,2) - x(i-1,2))/(g1-g2))
        x(i+1,3) = abs((x(i+1,2)-x(i,2))/(x(i+1,2)))
        if(i==10) then 
            exit
        endif
        i=i+1
        
    enddo

    do j=0,i+1
        write(1,*) (x(j,k),k=1,3)
    enddo
    
end program secant

subroutine func(a)
    use q4
    real(kind=8)::a
    f =  (a**2) -(4*a) -10
 
end subroutine func