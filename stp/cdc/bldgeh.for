      SUBROUTINE BLDGEH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill banks DGEH, DMAT, DWAL, DRFT
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  16-FEB-1988   Ghita Rahal-Callot
C-   Updated  24-FEB-1992   Qizhong Li-Demarteau  corrected the geometry
C-                            to the real CDC and added extension parts
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDGEH.LINK'
      INTEGER IRUMIN, IRUMAX, NFORM
      DATA IRUMIN, IRUMAX / 0, 999999/
      REAL XYZ(3), ANG(3), RADS(5), ZCORD(4)
      REAL ENDRNG(2), EXTCAN, CABLES(2) 
      DATA XYZ, ANG /3*0., 3*0./
      DATA RADS/49.53, 50.4825, 72.94, 73.66, 74.6125/
      DATA ZCORD/89.715, 90.35, 90.678, 91.948/
C
C      EXTCAN has the maximum Z position for the extension parts of the 
C      CD Aluminium can
C
      DATA EXTCAN/138.43/
C
C  CABLES(1) = maximum Z position of the cabling in front of CDC end plates
C  CABLES(2) = inner radius of the cabling under the extension parts of the
C              CD can.
C  The cabling includes the HV, gas and cooling for CDC, TRD and VTX
C
      DATA CABLES/92.938, 72.9025/
C
C ****  ENDRNG = end rings welded on the Aluminium drum (internal radius) 
C
      DATA ENDRNG/73.3425, 2.698/
C
C ****  books the bank SCDC as the top level bank in IDVSTP, if it does 
C ****  not exist yet
C
      IF (LSCDC .LE. 0) THEN
        CALL MZBOOK(IDVSTP, LSCDC, LSTPH, 1, 'SCDC', 5, 5, 0, 2, 0)
      ENDIF
      IF (LDGEH .LE. 0) THEN
        CALL MZFORM('DGEH', '2I -F', NFORM)
        CALL MZBOOK(IDVSTP, LDGEH, LSCDC, -IZDGEH, 'DGEH',
     &     3, 3, 22, NFORM, 0)
      ENDIF
C
C ****  Fill bank DGEH
C
      IC(LDGEH + 1) = IRUMIN
      IC(LDGEH + 2) = IRUMAX
      CALL UCOPY(XYZ, C(LDGEH+3), 3) 
      CALL UCOPY(ANG, C(LDGEH+6), 3) 
      CALL UCOPY(RADS, C(LDGEH+9), 5) 
      CALL UCOPY(ZCORD, C(LDGEH+14), 4) 
      C(LDGEH+18) =  ENDRNG(1)
      C(LDGEH+19) =  C(LDGEH+16) - ENDRNG(2)
      C(LDGEH+20) =  EXTCAN
      CALL UCOPY(CABLES, C(LDGEH+21), 2) 
C
C ****  Create material bank DMAT
C
      CALL BLDMAT
C
C ****  Create Passive parts of the detector bank
C
      CALL BLDWAL
C
C ****  Create drift cells description bank
C
       CALL BLDRFT
  999 RETURN
      END
