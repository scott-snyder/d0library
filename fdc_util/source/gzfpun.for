      FUNCTION GZFPUN(HALF,UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FPUN 
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
      INTEGER GZFPUN
      INTEGER HALF,UNIT,LKFPHF,GZFPHF
C----------------------------------------------------------------------
      IF(LFPUN(HALF,UNIT).EQ.0) THEN         ! link not set
        LKFPHF=GZFPHF(HALF)
        IF ( LKFPHF .NE. 0 ) LFPUN(HALF,UNIT)=LC(LKFPHF-(UNIT+1))
        GZFPUN=LFPUN(HALF,UNIT)
      ELSE                              ! link set
        GZFPUN=LFPUN(HALF,UNIT)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
