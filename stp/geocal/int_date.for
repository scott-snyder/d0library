      INTEGER FUNCTION INT_DATE(ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : UNPACK DATE AND RETURN INTEGER YYMMDD
C-
C-   Inputs  :     ARRAY     character*4 array
C-   Outputs :     INT_DATE  integer date
C-   Controls: 
C-
C-   Created   6-AUG-1992   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL*4 ARRAY(3)
      CHARACTER*4 CDATE(3)
      CHARACTER*4 MONTHS(12)
      INTEGER ID, IM, IY
      DATA  MONTHS / 'JAN-','FEB-','MAR-','APR-','MAY-','JUN-', 'JUL-',
     &  'AUG-', 'SEP-', 'OCT-', 'NOV-', 'DEC-' /
C
      DO 50 IM = 1, 3
        WRITE(CDATE(IM),'(A4)') ARRAY(IM)
   50 CONTINUE
C
      DO 100 IM = 1, 12
        IF( CDATE(2) .EQ. MONTHS(IM) ) GO TO 110
  100 CONTINUE
      INT_DATE = 0
      GO TO 999

  110 READ(CDATE(1),'(1X,I2,1X)') ID
      READ(CDATE(3),'(2X,I2)') IY
      INT_DATE = 10000*IY + 100*IM + ID
C----------------------------------------------------------------------
  999 RETURN
      END
