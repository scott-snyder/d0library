      INTEGER FUNCTION SAGTBL (TUBE_TYPE, TUBE_LENGTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get SAMUS tube length
C-
C-   Inputs  : TUBE_TYPE - drift tube type
C-   Outputs : TUBE_LENGTH - half length of drift tube of this type
C-   Controls: 
C-
C-   Created  16-OCT-1990   A. Efimov
C-   Updated  30-APR-1991   Andrei Kiryunin: geometry from banks SSTH, etc. 
C-   Updated   5-JAN-1992   Daria Zieminska  use STP banks only once 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER TUBE_TYPE
      REAL    TUBE_LENGTH
      INTEGER LSSTH,GZSSTH, N1, N2,N12
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      REAL C21,C22,CC(10) 
      SAVE N1,N2,C21,C22,CC,LSSTH
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        LSSTH = GZSSTH()
        N1 = IC(LSSTH+20)                 ! Number of standard tubes
        N2 = IC(LSSTH+23)                 ! Number of nonstandard tubes
        N12=N1+N2
        C21=C(LSSTH+21)
        C22=C(LSSTH+22)
        CC(1)=C(LSSTH+24)
        CC(2)=C(LSSTH+25)
        CC(3)=C(LSSTH+26)
        CC(4)=C(LSSTH+27)
        CC(5)=C(LSSTH+28)
        FIRST=.FALSE.
      END IF
      SAGTBL = -1
      IF (LSSTH.EQ.0) GOTO 999
      IF (TUBE_TYPE .GT. 0 .AND. TUBE_TYPE.LE.N1) THEN
        TUBE_LENGTH = C21 + (TUBE_TYPE - 1) * C22
      ELSE IF (TUBE_TYPE .GT. N1 .AND. TUBE_TYPE .LE. N12) THEN
        TUBE_LENGTH = CC(TUBE_TYPE-N1)
      ELSE 
        GO TO 999
      END IF
      TUBE_LENGTH = TUBE_LENGTH * 0.5
      SAGTBL = +1
C
  999 RETURN
      END
