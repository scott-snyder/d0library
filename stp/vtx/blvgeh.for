      SUBROUTINE BLVGEH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill banks of VTX_STPFILE
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  16-SEP-1988   Ghita Rahal-Callot
C-   Modified 09-NOV-1988   Peter Grudberg: add z-strip bank VZST
C-   Modified 28-APR-1989   Tom Trippe: new geometry, see D0 note #808
C-   Updated  19-MAR-1991   Peter Grudberg add call to BLVWAL
C-   Updated   2-AUG-1992   Peter Grudberg Remove boooking of SVTX, don't build
C-                                         zstrip banks
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IRUMIN, IRUMAX, NFORM
      REAL XYZ(3), ANG(3), RADS(8), ZCORD(7)
      DATA IRUMIN, IRUMAX / 0, 999999/
      DATA XYZ, ANG / 3*0., 3*0. /
      DATA RADS  / 3.05, 16.95, 3.15, 7.65, 7.75, 12.25, 12.35, 16.85 /
      DATA ZCORD / 48.3, 88.9, 53.3, 88.9, 58.4, 88.9, 89.6/ 
C----------------------------------------------------------------------
C
C ****  Book VGEH unless already booked
C
      IF ( LVGEH .LE. 0 ) THEN
        CALL BKVGEH(LVGEH)
      ENDIF
C
C ****  Fill bank VGEH
C
      IC ( LVGEH + 1 ) = IRUMIN
      IC ( LVGEH + 2 ) = IRUMAX
      CALL UCOPY ( XYZ,   C(LVGEH+3), 3) 
      CALL UCOPY ( ANG,   C(LVGEH+6), 3) 
      CALL UCOPY ( RADS,  C(LVGEH+9), 8) 
      CALL UCOPY ( ZCORD, C(LVGEH+17), 7) 
C
C ****  Create material bank DMAT
C
      CALL BLVMAT
C
C ****  Create and fill dead material bank VWAL
C
      CALL BLVWAL
C
C ****  Create drift cells description bank
C
      CALL BLVRFT
C
  999 RETURN
      END
