      SUBROUTINE FDRIFTDIR(HALF,UNIT,QUAD,SECTOR,WIRE,SDRIFT,CDRIFT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C  Returns sine and cosine of drift direction wrt to x axis for a given
C  FDC sector, using actual wire positions from alignment bank.
C  Assumes only rotation is in the x-y plane (i.e. around the z-axis).
C  Uses lookup table to save time.
C-
C-   Inputs  :HALF,UNIT,QUAD,SECTOR,WIRE     ! (WIRE ignored at present)
C-   Outputs :SDRIFT,CDRIFT 
C-
C-   Created  28-JUN-1991   Robert E. Avery, Replaces old DRIDIR by Daria Z.
C-   Updated   8-APR-1993   Robert E. Avery   Add call to FDC_ALIGNCHK to
C-                              insure that correct alignment banks are used.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
      REAL    SDRIFT,CDRIFT
C
      INTEGER RUNSAV,IDSAV
      INTEGER RUN,ID
      REAL    PHI_THETA(0:MXSECT, 0:MXQUAD, 0:MXHALF)
      REAL    PHI_PHI(0:MXSECP, 0:MXHALF)
C
      LOGICAL FDC_ALIGNCHK
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FDC_ALIGNCHK()  ) THEN
        CALL EVNTID(RUN,ID)
        IF ( RUN .NE. RUNSAV .OR. ID .NE. IDSAV ) THEN
          RUNSAV = RUN
          IDSAV = ID
          FIRST =.TRUE.
        ENDIF
      ENDIF
C
      IF ( FIRST ) THEN
        CALL FDRIFTDIR_TABLE(PHI_THETA,PHI_PHI)
        FIRST = .FALSE.
      ENDIF
C
C Get from lookup table
C 
      IF ( UNIT .EQ. 0 ) THEN
        SDRIFT=SIN(PHI_THETA(SECTOR,QUAD,HALF))
        CDRIFT=COS(PHI_THETA(SECTOR,QUAD,HALF))
      ELSE
        SDRIFT=SIN(PHI_PHI(SECTOR,HALF))
        CDRIFT=COS(PHI_PHI(SECTOR,HALF))
      ENDIF
C
  999 RETURN
      END
