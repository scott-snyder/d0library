      INTEGER FUNCTION GZVCAL(STRUCT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Return link to VCAL bank.  Note: there are 2
C-                          versions of this bank -- choice made with STRUCT
C-
C-   Returned value  : 
C-   Inputs  :
C-   Outputs :
C-   Controls: STRUCT = 0   --> VCAL IN /ZEBSTP/
C-                    .NE.0 --> VCAL IN /ZEBCOM/
C-
C-   Created  14-FEB-1994   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVCAL.LINK'
      INTEGER LVTXH,GZVTXH,STRUCT,GZSVTX
C----------------------------------------------------------------------
      GZVCAL = 0
      IF (STRUCT .EQ. 0) THEN
        LSVTX = GZSVTX()
        IF (LSVTX .GT. 0) GZVCAL = LC(LSVTX - IZVCAL_0)
      ELSE
        LVTXH = GZVTXH()
        IF (LVTXH .GT. 0) GZVCAL = LQ(LVTXH - IZVCAL_1)
      ENDIF
  999 RETURN
      END
