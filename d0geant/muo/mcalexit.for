C VAX/DEC CMS REPLACEMENT HISTORY, Element MCALEXIT.FOR
C *2    22-SEP-1993 13:34:28 STEWART "Jasbir's final mods"
C *1     5-AUG-1993 15:39:53 STEWART "muon gmuh fill code"
C VAX/DEC CMS REPLACEMENT HISTORY, Element MCALEXIT.FOR
      SUBROUTINE MCALEXIT(GOGO)
C----------------------------------------------------------------------
C-   Purpose and Methods : Get calorimeter exit XYZ AND 4-MOM OF PARTICLE
C-                       : FOR GMUH
C-
C-   Inputs  :  GOGO    A4    flow control flag.
C                       'INIT', 'UPDT'
C-   Outputs :
C-   Controls:
C-
C-   Created  23-APR-1993   Jasbir Singh
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  -- Geant common blocks...
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
C  -- variable in argument...
      CHARACTER*4 GOGO
C  -- local variables...
      INTEGER I,J
      INTEGER EXCAL
      REAL RADSQR
      REAL MCAL_VOL(3)
      DATA MCAL_VOL/2.7,266.8,398.1/   !EXT-RAD., Z-COOR AND INT-RAD
C----------------------------------------------------------------------
      IF(GOGO.EQ.'UPDT') THEN
        IF(EXCAL.EQ.1) GO TO 999
        RADSQR=VECT(1)**2+VECT(2)**2
        IF(RADSQR.GT.(MCAL_VOL(2)**2).OR.
     &    (ABS(VECT(3)).GT.MCAL_VOL(3).AND.
     &         RADSQR.GT.(MCAL_VOL(1)**2))) EXCAL=-1
        IF(EXCAL.EQ.0)THEN
          GOTO 999
        ELSE IF(EXCAL.eq.-1) THEN
          EXCAL=1
          CALL GMUH_BUILD_EXIT(ITRA)
        ENDIF
      ELSE IF(GOGO.EQ.'INIT') THEN
        RADSQR=VECT(1)**2+VECT(2)**2
        IF(RADSQR.LT.(MCAL_VOL(2)**2).AND.
     &    (ABS(VECT(3)).LT.MCAL_VOL(3)).OR.
     &         (RADSQR.LT.(MCAL_VOL(1)**2))) THEN
          EXCAL=0   ! THIS PRIMARY OR SECONDARY IS WITHIN MCAL IF 0
        END IF
      ENDIF
  999 RETURN
      END
