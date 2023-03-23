program calc
implicit none

integer::A,B,C

READ*, A,B,C

PRINT*,"A/B*C = ", (REAL(A)/REAL(B))*C

PRINT*,"A*B/C = ", (A*B/REAL(C))

PRINT*,"3*A^B = ", 3*(A**B)

PRINT*,"B+A/C = ", B+(REAL(A)/REAL(C))

PRINT*,"A/B+C = ", (REAL(A)/REAL(B))+C

end program calc
