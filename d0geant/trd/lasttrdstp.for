      FUNCTION LASTTRDSTP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns the last (most recent) trd stpfile version
C-
C-   Inputs  : none
C-   Outputs : LASTTRDSTP= last (most recent) trd stpfile version
C-   Controls: none
C-
C-   Created  13-FEB-1992   Alain PLUQUET
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LASTTRDSTP,GZTGEO,LTACH
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTACH.LINK'
      LASTTRDSTP=2        ! default (geometry with delta phi=0)
      LTGEO=GZTGEO()
      IF (LTGEO.GT.0) THEN
        LTACH=LC(LTGEO-IZTACH)          ! Do not use GZTACH !!!
        IF (LTACH.GT.0) THEN
          LASTTRDSTP=IC(LTACH+1)
        END IF 
      END IF   
      END
