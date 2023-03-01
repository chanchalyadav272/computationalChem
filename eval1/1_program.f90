

program q1
    use sch
    implicit none

    read*,l
    if(l<0) then
      print*, "l cannot be negative" 
      stop
    endif

    allocate(j(0:l))
    allocate(n(0:l))

    open(1,file="bessel.txt")
    open(2,file="hankel.txt")

    call CalDelx

    print*,'here'
    do c=1,num
        x = xmin+ (c-1)*delx
        if(l.eq.0.or.l.eq.1) then
        call bessel(l,x)
        else
        call higherBessel1(l,x)
        call higherBessel2(l,x)
        endif
        write(1,*) x, j(l), n(l)
        write(2,*) x, j(l), n(l), sqrt((j(l)**2)+(n(l)**2))
    enddo
end program q1
