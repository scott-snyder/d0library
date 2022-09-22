      SUBROUTINE L2_EM_PARAMETERS(NUM_PAR_SETS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in SRCP file for electron tool
C-
C-   Inputs  : L2_EM_RCP bank
C-             NUM_PAR_SETS  number of new parameter sets
C-   Outputs :
C-   Controls:
C-
C-   Created 15-SEP-1989   Yi  Xia
C-   Updated   8-FEB-1992   James T. Linnemann   debugging by Scott Snyder;
C-            add comments on stp contents; read into 3d array
C-   Updated  20-FEB-1992  Yi  Xia add PART of counting the number of
C-     the independent PARAM_SET which given by the Coor's translation
C-   Updated  26-FEB-1992  Yi  Xia add CONE_FRACT_MAX and CONE_R cut
C-                                  to parameters list
C-   Updated  26-JUL-1992  James T. McKinley add another energy bin to
C-                          shape cuts and bin 5x5-3x3/3x3 cut to use in
C-                          EC region.
C-   Updated  01-AUG-1992  James T. McKinley make backwards compatible
C-                          with old STP files.
C-   Updated   6-SEP-1992   James T. Linnemann remove memory code
C-                            (tracking has its own)
C-   Updated  23-SEP-1992  James T. McKinley  use binned cuts for 4x4-2x2/2x2
C-   Updated   6-NOV-1993   James T. Linnemann
C-                                    add ETMIN_CELL (lowest cell energy)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L2_EM.PARAMS'
      INCLUDE 'D0$INC:L2_EM_STP.INC'
      INCLUDE 'D0$INC:L2_EM_CUTS.INC'
      INCLUDE 'D0$INC:L2_EM_CUTS_C.INC'
      INCLUDE 'D0$INC:L2_EM_CUTS_L.INC'
C
      BYTE    NUM_PAR_SETS
      INTEGER I,II,J,NL,NCHR,IER
      REAL THETA,ETA,LY
      LOGICAL EZERROR,OK,OK2
      INTEGER IETAC_BIN_DEFAULT(NETABIN),L2EM_VERSION
C...boundaries of eta bins for cuts
      DATA IETAC_BIN_DEFAULT/2,6,12,13,15,19,25,32/
C
C----------------------------------------------------------------------
C
      IF (NUM_PAR_SETS.GT.0) THEN
C...request to look at new set of cuts
        NPARIN = 0  !now are committed to forgetting previous set of cuts
        OK = .FALSE.
        CALL EZPICK_NOMSG('L2_EM',IER)
        OK = IER.EQ.0
        IF (.NOT.OK) THEN
C...      allow nonexistence of l2_em bank in case of retriggering of nodes
C...      but if l2_em gets an event, there will be trouble.
          GO TO 999
        ENDIF
        IF(IER.EQ.0) THEN
          CALL EZGET_i('NUMBER_OF_SETS',NPARIN,IER)
          IF (NPARIN.GT.NSETS) THEN
            CALL ERRMSG('L2_EM_NPARIN_BIG','L2_EM_PARAMETERS',
     &        'More Parameter Sets Than Storage Available','F')
          ENDIF
        ENDIF
        IF(IER.EQ.0) CALL EZGET_rarr('ETMIN_CC',CCET_CUT,IER)
        IF(IER.EQ.0) CALL EZGET_rarr('ETMIN_EC',ECET_CUT,IER)
        IF(IER.EQ.0) CALL EZGET_iarr('NUM_EM',NUMBER_EM,IER)
        IF(IER.EQ.0) CALL EZGET_rarr('DEL_ETA_TRACK',DETA_TR,IER)
        IF(IER.EQ.0) CALL EZGET_rarr('DEL_PHI_TRACK',DPHI_TR,IER)
        IF(IER.EQ.0) CALL EZGET_rarr('CONE_DELTA_R',CONE_R,IER)
        IF(IER.EQ.0) CALL EZGET_rarr('CONE_FRACT_MAX',CONE_FRACT_MAX,
     &                               IER)
        IF(IER.EQ.0) CALL EZGET_larr('DO_ISOLATION',DO_ISOLATION,IER)
        DO I = 1,NPARIN
          IF (IER.EQ.0) THEN
            CALL EZGETS('SHAPE_CUTS',I,SHAPE_C(I),NCHR,IER)
          ENDIF
          IF (IER.EQ.0) THEN
            CALL EZGETS('TRACK_MATCH',I,CD_TR_C(I),NCHR,IER)
          ENDIF
        ENDDO
        IF (OK) CALL EZRSET     !Reset if PICK was OK
        IF (IER.NE.0) THEN
          CALL ERRMSG('L2_EM_MISS_PARAMETERS','L2_EM_PARAMETERS',
     &      'Trouble getting parameters from L2_EM','F')
          GO TO 999
        ENDIF

C
        CALL L2_CD_MATCH_PARAMETERS
        CALL L2_VERT_PARAMETERS
      ENDIF
C
C...have to re-GET from STP each time to be sure common block is up to date
      CALL EZPICK('L2_EM_RCP')
      OK2 = .NOT.EZERROR(IER)
      IF (.NOT.OK2) GO TO 999     ! allow for no parameters (triggering in run)
C
C...find out which format: undefined is format 0
      IF(IER.EQ.0) CALL EZGET_i('L2EM_VERSION',L2EM_VERSION,IER)
      IF(IER.NE.0)THEN
        L2EM_VERSION = 0
        IER = 0
        CALL ERRMSG('L2_EM_OLD_STP','L2_EM_PARAMETERS',
     &      'Old L2_EM_RCP in STP, will use default binning','W')
      ENDIF

C...eta bin boundaries
      IF (L2EM_VERSION.GE.1) THEN
        IF(IER.EQ.0) CALL EZGET_iarr('ETA_BOUNDS',ETA_BOUNDS,IER)
      ELSE
        CALL UCOPY(IETAC_BIN_DEFAULT,ETA_BOUNDS,NETABIN)
      ENDIF
C...energy bin boundaries
      IF(L2EM_VERSION.GE.1)THEN
        IF(IER.EQ.0) CALL EZGET_rarr ('E_BOUNDS',E_BOUNDS,IER)
      ELSE
        CALL UFILL(E_BOUNDS,1,2*(NENRGBIN-1),999999.)      !init bounds to HUGE
        IF(IER.EQ.0) CALL EZGET ('ETH1',E_BOUNDS(1,1),IER) ! old bins
        IF(IER.EQ.0) CALL EZGET ('ETH2',E_BOUNDS(2,1),IER)
        E_BOUNDS(1,2) = E_BOUNDS(1,1)       ! EC bounds same as CC in old format
        E_BOUNDS(2,2) = E_BOUNDS(2,1)
      ENDIF
C...flag for E or Et in CAEP bank
      IF(IER.EQ.0) CALL EZGET_l('ET_IN_CAEP',ET_IN_CAEP,IER)
C
C...indices of EMCUTS are cut #, energy band, eta range
C...Cut numbers: (1-9 are longitudinal; 10, 11 & 12 are transverse cuts)
C 1,2 min,max for floor 1 divided by sum of floors 1-4
C 3,4                   1+2 summed
C 5,6                   3
C 7,8                   4
C 9   max for floor 5 fraction (no Min)
C 10  max for difference of <r> in 5x5 - 3x3
C 11  max for EM3 (5x5 - 3x3)/3x3
C 12  max for EM3 (4x4 - 2x2)/2x2 (abs(ieta).lt.12)
      IF(IER.EQ.0) CALL EZGET ('ELCCL1',EMCUTS(1,1,1),IER)
      IF(IER.EQ.0) CALL EZGET ('ELCCL2',EMCUTS(1,1,2),IER)
      IF(IER.EQ.0) CALL EZGET ('ELCCL3',EMCUTS(1,1,3),IER)
      IF(IER.EQ.0) CALL EZGET ('ELCCM1',EMCUTS(1,2,1),IER)
      IF(IER.EQ.0) CALL EZGET ('ELCCM2',EMCUTS(1,2,2),IER)
      IF(IER.EQ.0) CALL EZGET ('ELCCM3',EMCUTS(1,2,3),IER)
      IF(IER.EQ.0) CALL EZGET ('ELCCH1',EMCUTS(1,3,1),IER)
      IF(IER.EQ.0) CALL EZGET ('ELCCH2',EMCUTS(1,3,2),IER)
      IF(IER.EQ.0) CALL EZGET ('ELCCH3',EMCUTS(1,3,3),IER)
      IF(L2EM_VERSION.GE.1)THEN
        IF(IER.EQ.0) CALL EZGET ('ELCCX1',EMCUTS(1,4,1),IER)
        IF(IER.EQ.0) CALL EZGET ('ELCCX2',EMCUTS(1,4,2),IER)
        IF(IER.EQ.0) CALL EZGET ('ELCCX3',EMCUTS(1,4,3),IER)
      ENDIF
      IF(IER.EQ.0) CALL EZGET ('E12LCT',EMCUTS(1,1,4),IER)
      IF(IER.EQ.0) CALL EZGET ('E12MCT',EMCUTS(1,2,4),IER)
      IF(IER.EQ.0) CALL EZGET ('E12HCT',EMCUTS(1,3,4),IER)
      IF(L2EM_VERSION.GE.1)THEN
        IF(IER.EQ.0) CALL EZGET ('E12XCT',EMCUTS(1,4,4),IER)
      ENDIF
      IF(IER.EQ.0) CALL EZGET ('E15LCT',EMCUTS(1,1,5),IER)
      IF(IER.EQ.0) CALL EZGET ('E15MCT',EMCUTS(1,2,5),IER)
      IF(IER.EQ.0) CALL EZGET ('E15HCT',EMCUTS(1,3,5),IER)
      IF(L2EM_VERSION.GE.1)THEN
        IF(IER.EQ.0) CALL EZGET ('E15XCT',EMCUTS(1,4,5),IER)
      ENDIF
      IF(IER.EQ.0) CALL EZGET ('ELECL1',EMCUTS(1,1,6),IER)
      IF(IER.EQ.0) CALL EZGET ('ELECL2',EMCUTS(1,1,7),IER)
      IF(IER.EQ.0) CALL EZGET ('ELECL3',EMCUTS(1,1,8),IER)
      IF(IER.EQ.0) CALL EZGET ('ELECM1',EMCUTS(1,2,6),IER)
      IF(IER.EQ.0) CALL EZGET ('ELECM2',EMCUTS(1,2,7),IER)
      IF(IER.EQ.0) CALL EZGET ('ELECM3',EMCUTS(1,2,8),IER)
      IF(IER.EQ.0) CALL EZGET ('ELECH1',EMCUTS(1,3,6),IER)
      IF(IER.EQ.0) CALL EZGET ('ELECH2',EMCUTS(1,3,7),IER)
      IF(IER.EQ.0) CALL EZGET ('ELECH3',EMCUTS(1,3,8),IER)
      IF(L2EM_VERSION.GE.1)THEN
        IF(IER.EQ.0) CALL EZGET ('ELECX1',EMCUTS(1,4,6),IER)
        IF(IER.EQ.0) CALL EZGET ('ELECX2',EMCUTS(1,4,7),IER)
        IF(IER.EQ.0) CALL EZGET ('ELECX3',EMCUTS(1,4,8),IER)
      ELSE
C
C...old format: copy E bin 3 into E bin 4
        DO J = 1,NETABIN
          DO I=1,10
            EMCUTS(I,4,J)=EMCUTS(I,3,J)
          ENDDO
        ENDDO
      ENDIF
C...cuts for transverse shape: coefficients for <r> in 3x3 bounds
      IF(IER.EQ.0) CALL EZGET ('S3A2',A2,IER)
      IF(IER.EQ.0) CALL EZGET ('S3B1',B1,IER)
      IF(IER.EQ.0) CALL EZGET ('S3C0',C0,IER)
      IF(IER.EQ.0) CALL EZGET ('SSIG3',SS3,IER)
C...cut on peak cell/3x3 in CC
      IF(IER.EQ.0) CALL EZGET ('EP3',EM3L,IER)
C...energy-independent transverse shape cuts (see cut 10 above for E-dep cut)
C Cut indices are:
C 1-4 not used
C 5,6 same for EC, but ( E7x7/E5x5 - 1) (forward only)
C 7 not used; 8 max <r> in 5x5  (CC)
C 9,10 min,max cuts on (E4x4/E2x2 - 1)
      IF(IER.EQ.0) CALL EZGET ('ELECT3',ESIZE,IER)
      IF(L2EM_VERSION.LT.1)THEN   !copy eta, e independent values of old format
        DO I=1,NENRGBIN           !for 5x5-3x3/3x3
          DO J=1,4      !lower half of eta bins
            EMCUTS(11,I,J)=ESIZE(2)
          ENDDO
          DO J=5,NETABIN
            EMCUTS(11,I,J)=ESIZE(4)
          ENDDO
        ENDDO
      ENDIF
      IF(L2EM_VERSION.LT.2)THEN !copy eta, e independent values of old format
        DO I=1,NENRGBIN         !for 4x4-2x2/2x2
          DO J=1,NETABIN
            EMCUTS(12,I,J)=ESIZE(10)
          ENDDO
        ENDDO
      ENDIF
C
C...not get the parameters defining the depths of the isolation cone
      IF(IER.EQ.0) CALL EZGET_i('LO_GAMMA_FLOOR',LO_GAMMA_FLOOR,IER)
      IF(IER.EQ.0) CALL EZGET_i('HI_GAMMA_FLOOR',HI_GAMMA_FLOOR,IER)
      IF(IER.EQ.0) CALL EZGET_i('LO_CONE_LAYER',LO_CONE_LAYER,IER)
      IF(IER.EQ.0) CALL EZGET_i('HI_CONE_LAYER',HI_CONE_LAYER,IER)
      IF(IER.EQ.0) CALL EZGET_l('CONE_USE_ICD',CONE_USE_ICD,IER)
      IF(L2EM_VERSION.GE.3)THEN !get ETMIN_CELL parameter
        IF(IER.EQ.0) CALL EZGET('ETMIN_CELL',ETMIN_CELL,IER)
      ELSE
        ETMIN_CELL = -1.E30   !any energy allowed
      ENDIF

      IF (OK2) CALL EZRSET     !Reset if PICK was OK
      IF (IER.NE.0) THEN
        CALL ERRMSG('L2_EM_STP_BAD','L2_EM_PARAMETERS',
     &      'Trouble getting CUTS from L2_EM_RCP','F')
        GO TO 999
      ENDIF
C
      CALL CL2_INI
      CALL EMSV_LINK_INI()
C
  999 RETURN
      END
