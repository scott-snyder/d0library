      SUBROUTINE WRITE_ZDIV(NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : write out Zdivision info in CGS system
C-   Inputs  : NAME = Name of SRCP array
C-   Outputs : None
C-   Controls: None
C-
C-   Created   15-APR-1990   Natalie Roe    from WRTZDV
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:WRITE_UNIT.INC'

      CHARACTER*(*) NAME
      INTEGER IPT,I,K,IER,LN
      INTEGER IARRAY(1000)
      REAL RARRAY(1000)
      EQUIVALENCE(IARRAY(1),RARRAY(1))
      REAL CONV
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA CONV/2.54/
C----------------------------------------------------------------------
C
      LN = LEN(NAME)
      WRITE(OUT_VOL,1)NAME(1:LN)
    1 FORMAT('\ARRAY ',A)
C
      CALL EZGETA(NAME,1,3,1,IARRAY(1),IER)
      WRITE(OUT_VOL,2)IARRAY(1),IARRAY(2),RARRAY(3)
    2 FORMAT(2I8,F8.2,'     !Num Z div, Mat. number, Phi stagger')
      IPT = 3
      DO 10 I = 1,IARRAY(1)
        CALL EZGETA(NAME,IPT+1,IPT+4,1,IARRAY(IPT+1),IER)
        CALL EZGETA(NAME,IPT+5,IPT+4+IARRAY(IPT+4), 1, 
     &    IARRAY(IPT+5),IER)
        WRITE(OUT_VOL,3)IARRAY(IPT+1),RARRAY(IPT+2),RARRAY(IPT+3),
     &    IARRAY(IPT+4),
     &    (CONV*RARRAY(IPT+4+K),K=1,IARRAY(IPT+4) )
    3   FORMAT(2X,'''',A4,'''',2F10.3,'! Name, z begin, zend',
     &    /,I10,' !  Num radial divisions',/,(7F10.3))
        IPT = IPT + 4 + IARRAY(IPT+4)
        CALL EZGETA(NAME,IPT+1,IPT+1,1,IARRAY(IPT+1),IER)
        CALL EZGETA(NAME,IPT+2,IPT+1+IARRAY(IPT+1), 1, 
     &    IARRAY(IPT+2),IER)
        WRITE(OUT_VOL,5)IARRAY(IPT+1), 
     &    (RARRAY(IPT+1+K),K=1,IARRAY(IPT+1) )
    5   FORMAT(I10,' !  Num Eta Divisions',/,(7F10.3)) 
        IPT = IPT + 1 + IARRAY(IPT+1)
        CALL EZGETA(NAME,IPT+1,IPT+1,1,IARRAY(IPT+1),IER)
        CALL EZGETA(NAME,IPT+2,IPT+1+IARRAY(IPT+1), 1, 
     &    IARRAY(IPT+2),IER)
        WRITE(OUT_VOL,7)IARRAY(IPT+1), 
     &    (IARRAY(IPT+1+K),K=1,IARRAY(IPT+1) )
    7   FORMAT(I10,' !  Num Phi Divisions',/,(7I10)) 
        IPT = IPT + 1 + IARRAY(IPT+1)
   10 CONTINUE

      WRITE(OUT_VOL,4)
    4 FORMAT('\END')

  999 RETURN
      END
