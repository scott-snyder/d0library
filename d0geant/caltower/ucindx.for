      SUBROUTINE UCINDX(LAYER,SUBLAY,X,Y,Z,IETA,IPHI,ILYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given X,Y,Z position in a Central Calorimeter
C-                         GEANT volume specified by the LAYER and
C-                         SUBLAYer indices packed into the volume
C-                         IDTYPE, determine the physics indices IETA,
C-                         IPHI, and ILYR of the corresponding cell.
C-
C-      The geometry information for each GEANT volume is stored in
C-      an SRCP structure.  The CC GEANT volumes are associated with
C-      detector sets, and are given IDTYPEs indicative of the layer
C-      and sublayer occupied.  Each of the active CC IDTYPE's corresponds
C-      to a Floor.  An SRCP structure exists for each active Floor
C-      which gives the information needed to determine the cell
C-      boundaries in eta and phi of each module.  See the
C-      documentation file CC_CELLS.SCP for details of the SRCP info.
C-
C-      This routine assumes that the GEANT volume, hence the module,
C-      is already known.  It calculates the IPHI index from X and Y
C-      assuming exact division into 64 phi regions.  It determines
C-      the IETA index from Z and the values of the cell boundaries.
C-      The ILYR index comes directly from the IDTYPE LAYER index,
C-      except for EM3, where additional computation is necessary.
C-
C-   Inputs  : LAYER    Detector layer
C-                      In the CC, LAYER may be 1,2,3,7,11,12,13,15.
C-                      (LAYER = 4,5,6 are not independent volumes)
C-                      Note: No checking is done on this argument.
C-             SUBLAY   Detector sublayer
C-                      In the CC this index is ignored.
C-             X        X position (cm) in the global coordinate
C-                      system
C-             Y        Y position (cm) in the global coordinate
C-                      system
C-             Z        Z position (cm) in the global coordinate
C-                      system
C-   Outputs : IETA     Eta index
C-                      In the CC, IETA runs from -12 to 12 (0 skipped)
C-             IPHI     Phi index
C-                      IPHI runs from 1 to 64
C-             ILYR     Layer index
C-                      In the CC, ILYR may be 1-7, 11-13, or 15
C-   Controls: none
C-
C-   Created  22-JAN-1989   Stuart Fuess
C-   Updated  29-JAN-1989   Stuart Fuess  Use renamed SRCP structure 
C-   Updated  25-MAR-1992   K. Wyatt Merritt  Switch value of PI from
C-                            internal parameter to the value in GCONST
C-                            for greater precision: avoid IPHI = 65
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Arguments
      INTEGER LAYER, SUBLAY
      REAL X, Y, Z
      INTEGER IETA, IPHI, ILYR
C  Include files
      INCLUDE 'D0$INC:GCONST.INC'
C  Integers
      INTEGER FLOOR                     ! Floor, 1 to 8
      INTEGER PTR                       ! Local pointer
      INTEGER LYRFLR(15)                ! Layer to Floor mapping
      INTEGER FLRLYR(8)                 ! Floor to Layer mapping
      INTEGER IBND                      ! Z boundary index
      INTEGER NBND(8)                   ! Number of Z boundaries
      INTEGER IPHI3                     ! EM3 half-phi (0 or 1)
      INTEGER IETA3                     ! EM3 half-eta (0 or 1)
C  Reals
      REAL ZABS                         ! Absolute value of Z
      REAL PHI                          ! Azimuthal angle (radians)
      REAL DPHI                         ! Delta-Azimuthal angle (radians)
      REAL ZBND(22,8)                   ! Array of Z boundaries
      REAL RADPCL                       ! Radians per cell
      REAL CLPRAD                       ! Cells per radian
      REAL HCPRAD                       ! Half-Cells per radian
C  Characters
      CHARACTER*32 NAME                 ! SRCP structure name
      CHARACTER*4 SECTION(8)            ! CC Section name
C  Equivalences
      INTEGER IVAL(50)                  ! Arrays for reading SRCP
      REAL RVAL(50)                     ! structures
      EQUIVALENCE ( IVAL, RVAL )
C  Logicals
      LOGICAL FIRST                     ! First subroutine entry flag
C  Data
      DATA SECTION / 'CCEM', 'CCEM', 'CCEM', 'CCEM',
     &               'CCFH', 'CCFH', 'CCFH', 'CCCH' /
      DATA LYRFLR / 1, 2, 3, 0, 0, 0, 4, 0, 0, 0, 5, 6, 7, 0, 8 /
      DATA FLRLYR / 1, 2, 3, 7, 11, 12, 13, 15 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C  If first entry, then fill working arrays from SRCP structures for
C  each of the CC detector layers.  The CC SRCP information is in the
C  logical file CAL_STPFILE, with name SRCP_UCAL.
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL SLSRCP('SRCP_UCAL')
        RADPCL = PI / 32.
        CLPRAD = 32. / PI
        HCPRAD = 64. / PI 
C----------------------------------------------------------------------
C  Loop over CC Floors.  Extract cell boundary information.
C----------------------------------------------------------------------
        DO 200 FLOOR=1,8
          WRITE(NAME,1001) SECTION(FLOOR),FLOOR
C----------------------------------------------------------------------
C  Get cell boundary information from SRCP, and store in array.  See
C  documentation file CC_CELLS.SCP for SRCP stucture of this info. Note
C  that the first (always zero) and last (module edge) boundaries are
C  ignored.
C----------------------------------------------------------------------
          CALL GTSRCP(NAME,IVAL,1)
          PTR = 4 + IVAL(3)
          NBND(FLOOR) = IVAL(PTR) - 2
          PTR = PTR + 1
          DO 100 IBND=1,NBND(FLOOR)
            ZBND(IBND,FLOOR) = RVAL(PTR+IBND)
  100     CONTINUE
  200   CONTINUE
      ENDIF
C----------------------------------------------------------------------
C  Determine FLOOR from LAYER
C----------------------------------------------------------------------
      FLOOR = LYRFLR(LAYER)
C----------------------------------------------------------------------
C  Use Z position to determine eta index
C----------------------------------------------------------------------
      ZABS = ABS(Z)
      IETA = 1
      DO 300 IBND=1,NBND(FLOOR)
        IF ( ZABS .LT. ZBND(IBND,FLOOR) ) GO TO 400
        IETA = IETA + 1
  300 CONTINUE
  400 CONTINUE
C----------------------------------------------------------------------
C  If EM3, compute half-eta index and correct eta index
C----------------------------------------------------------------------
      IF ( LAYER .EQ. 3 ) THEN
        IETA3 = MOD(IETA+1,2)
        IETA = (IETA+1) / 2
      ELSE
        IETA3 = 0
      ENDIF
C----------------------------------------------------------------------
C  Correct for negative Z values
C----------------------------------------------------------------------
      IF ( Z .LT. 0. ) THEN
        IETA = -  IETA
        IETA3 = 1 - IETA3
      ENDIF
C----------------------------------------------------------------------
C  Use X and Y positions to determine phi index
C----------------------------------------------------------------------
      PHI = ATAN2(-Y,-X) + PI
      IPHI = INT(CLPRAD*PHI) + 1
C----------------------------------------------------------------------
C  If EM3, then also compute which half of phi cell we are in
C----------------------------------------------------------------------
      IF ( LAYER .EQ. 3 ) THEN
        DPHI = PHI - RADPCL * (IPHI-1)
        IPHI3 = INT(HCPRAD*DPHI)
      ENDIF
C----------------------------------------------------------------------
C  If EM3, then determine ILYR.  Else ILYR is same as LAYER.
C----------------------------------------------------------------------
      IF ( LAYER .EQ. 3 ) THEN
        ILYR = 3 + 2 * IETA3 + IPHI3
      ELSE
        ILYR = LAYER
      ENDIF
      RETURN
 1001 FORMAT(A4,'_FLOOR',I1,'_CELLS')
      END
