      INTEGER FUNCTION DICHOTOM(X,A,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-FEB-1996   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER I,I1,I2,J,j1,j2,N,NDIV,STEP
      REAL X,A(N)
      PARAMETER( NDIV = 4 )
      REAL REF(NDIV+1)
      DICHOTOM=1
      IF(X.LE.A(1))GO TO 999
      DICHOTOM=N
      IF(X.GE.A(N))GO TO 999
      I1=1
      I2=N
      do while (i2-i1.gt.1)
        j=(i1+i2)/2
c        print*,'j',j,' a',a(i1),a(i2)
        if(x.eq.a(j))go to 10
        if(x.lt.a(j))then
          i2=j
          else !(x.gt.a(j))
            i1=j
            end if
        end do
   10 continue
c      print*,' x',x
      dichotom=i1
  999 continue
c      print*,' dichotom',dichotom
      RETURN
      END
