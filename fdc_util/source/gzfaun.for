      FUNCTION GZFAUN(HALF,UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FAUN 
C-
C-   Returned value  : 
C-   Inputs  : HALF,UNIT
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-MAR-1990   Jeff Bantly  use logical format 
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZFAUN
      INTEGER HALF,UNIT,LKFAHF,GZFAHF
C----------------------------------------------------------------------
      IF(LFAUN(HALF,UNIT).EQ.0) THEN         ! link not set
        LKFAHF=GZFAHF(HALF)
        IF ( LKFAHF .NE. 0 ) LFAUN(HALF,UNIT)=LC(LKFAHF-(UNIT+1))
        GZFAUN=LFAUN(HALF,UNIT)
      ELSE                              ! link set
        GZFAUN=LFAUN(HALF,UNIT)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
