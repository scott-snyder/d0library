      FUNCTION GZFTUN(HALF,UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FTUN 
C-
C-   Returned value  : 
C-   Inputs  : HALF,UNIT
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZFTUN
      INTEGER HALF,UNIT,LKFTHF,GZFTHF
C----------------------------------------------------------------------
      IF(LFTUN(HALF,UNIT).EQ.0) THEN         ! link not set
        LKFTHF=GZFTHF(HALF)
        IF ( LKFTHF .NE. 0 ) LFTUN(HALF,UNIT)=LC(LKFTHF-(UNIT+1))
        GZFTUN=LFTUN(HALF,UNIT)
      ELSE                              ! link set
        GZFTUN=LFTUN(HALF,UNIT)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
