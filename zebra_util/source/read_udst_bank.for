      SUBROUTINE READ_UDST_BANK(NWORD,XDATA,XPTR,IGRP,NGRP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : read UDST bank into XDATA/XPTR arrays
C-
C-   Inputs  :  IGRP,NGRP
C-   Outputs :  NWORD,XDATA
C-
C-   Created   6-APR-1993   Balamurali V
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NWORD(*),IGRP,NGRP,I,J,POINTER,IDMAX
      INTEGER LANLS,LUDST,GZUDST,XPTR(IGRP)
      REAL    XDATA(NGRP)
C----------------------------------------------------------------------
      LUDST = GZUDST()
      IF ( LUDST .LE. 0) THEN
        CALL ERRMSG('No UDST Bank','READ_UDST_BANK',' ','F')
        GOTO 999
      ENDIF
C
C Determine IDMAX, NWORD....
C
      IDMAX = IQ(LUDST+2)
      IF(IDMAX.GT.IGRP)CALL ERRMSG('IDMAX>IGRP','READ_UDST_BANK',
     &  'number of groups exceeds maximum','F')
      POINTER = LUDST+2
      DO I = 1, IDMAX
        POINTER = POINTER + 1
        NWORD(I) = IQ (POINTER)
        IF(NWORD(I).GT.NGRP)CALL ERRMSG('NWORD>NGRP',
     &    'READ_UDST_BANK','number of words in group exceeds maximum',
     &    'F')
      ENDDO
C
C fill pointer array XPTR
C
      DO I = 1, IDMAX
        XPTR(I) = POINTER
        POINTER = POINTER + NWORD(I)
      ENDDO
C
  999 CONTINUE
      RETURN
      END
