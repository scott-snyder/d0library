      SUBROUTINE WRTZDV1(LOUT,NAME,LSR,RSR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : write out Zdivision info in CGS system
C-                         to unit LOUT.
C-   Inputs  : LOUT = output stream
C-             NAME = Name of SRCP array
C-             LSR,RSR contents (integer and real)
C-   Outputs : None
C-   Controls: None
C-
C-   Created   5-NOV-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER LSR(*)
      INTEGER LOUT,I,IPT,K,LN
      REAL RSR(*),CONV
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL GTSRCP('CONV_FACTOR',CONV,1)
      ENDIF
C
      LN = LEN(NAME)
      WRITE(LOUT,1)NAME(1:LN)
    1 FORMAT('\ARRAY ',A)
C
      WRITE(LOUT,2)LSR(1),LSR(2),RSR(3)
    2 FORMAT(2I8,F8.2,' !Num Z div, Mat no,Stagger of module ')
      IPT = 3
      DO 10 I = 1,LSR(1)
        WRITE(LOUT,3)LSR(IPT+1),LSR(IPT+2),
     &    CONV*RSR(IPT+3),CONV*RSR(IPT+4),
     &    LSR(IPT+5),(CONV*RSR(IPT+5+K),K=1,LSR(IPT+5))
    3   FORMAT(2X,'''',A4,'''',1X,'''',A4,'''',
     &    2F10.3,/,I10,/,(7F10.3))
        IPT = IPT + 4 + LSR(IPT+5)+ 1
        WRITE(LOUT,5) LSR(IPT+1),(RSR(IPT+1+K),K=1,LSR(IPT+1))
    5   FORMAT(I10,/,(7F10.3))
        IPT = IPT + LSR(IPT+1) + 1
        WRITE(LOUT,6)LSR(IPT+1),(LSR(IPT+1+K),K=1,LSR(IPT+1))
    6   FORMAT(I10,/,(7I10))
        IPT = IPT + LSR(IPT+1) + 1
C
   10 CONTINUE
      WRITE(LOUT,4)
    4 FORMAT('\END')
  999 RETURN
      END
