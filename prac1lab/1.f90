program maxNum
    implicit none
    integer :: a,b,c,mx

    open(1, file="1_input.txt")
    open(2, file="1_output.txt")

    read(1,*) a,b,c

    if(a>=b) then
        if(b>=c) then
            mx= a
        elseif(b<=c .and. a>=c) then
            mx=a
        else
            mx = c
        endif
    else
        if(a>=c) then
            mx= b
        elseif(a<=c .and. b>=c) then
            mx=b
        else 
            mx= c

        endif

    endif

        write(2,*) "using if else max = ", mx

        write(2,*) "using library function max = ", max(a,b,c)
    




end program maxNum