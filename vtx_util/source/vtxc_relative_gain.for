      SUBROUTINE VTXC_RELATIVE_GAIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : From expected Z position ( from CDC or FDC )
C-               and pulse areas at both ends of VTX wires, calibrate
C-              the relative gain and input impedances
C-   Inputs  : Read a data file
C-   Outputs :
C-   Controls:
C-
C-   Created  16-SEP-1992   John Hauptman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VTXCOFF.INC/LIST'
      INTEGER M_WIRE,M_SECTOR,M_LAYER,NE,IE
      INTEGER ITERATION,RUNNUM,RUNNO
      REAL R_PER_CM
      PARAMETER (M_wire = 7)
      PARAMETER (M_SECTOR = 31)
      PARAMETER (M_LAYER = 2)
      PARAMETER (R_per_cm = 18.)
      CHARACTER*75 QDIVFILE
      REAL  el_half(0:2)
      REAL x_wire, y_wire
      PARAMETER (ne = (M_wire+1)*(M_sector+1)*(M_layer+1))
      PARAMETER (ie = 2*(M_wire+1)*(M_sector+1)*(M_layer+1))
      INTEGER IUSER,LUN,IERR,K_GAIN
      LOGICAL OPENED
      DATA   el_half / 48.3, 53.3, 58.4 /  ! cm,
      DATA IUSER /777/
C----------------------------------------------------------------------------
      CALL GTUNIT(IUSER,LUN,IERR)
C
      CALL VFILL( Epsilon, ne, 0.)
      CALL VFILL( r_input, ie, 180.)
C
C ****  Close file holding z info and get filename
C
      CALL VTXC_QDIV_CLOSE(QDIVFILE)
C
      DO k_Gain = 1,3       ! Nail down the Gain calibration for fixed r.
        CALL D0OPEN(LUN,QDIVFILE,'IU',OPENED)
        IF ( .NOT. OPENED ) THEN
          CALL ERRMSG('File open error','VTXC_REL_GAIN',
     &      'Unable to open z info file, abort','W')
          GO TO 999
        ENDIF
        CALL VTX_END_GAINS(LUN)
        CLOSE(LUN)
      END DO
      CALL D0OPEN(LUN,QDIVFILE,'IU',OPENED)
      IF ( .NOT. OPENED ) THEN
        CALL ERRMSG('File open error','VTXC_REL_GAIN',
     &    'Unable to open z info file, abort','W')
        GO TO 999
      ENDIF
      CALL VTX_IMPEDANCES(LUN)
      CLOSE(LUN)
C
      CALL VTX_END_GAINS_CLEAR  ! Delete histograms
      CALL VTX_IMPEDANCES_CLEAR ! Delete histograms
      CALL RLUNIT(IUSER,LUN,IERR)
C
  999 RETURN
      END
