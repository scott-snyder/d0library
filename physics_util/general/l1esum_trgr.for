      SUBROUTINE L1ESUM_TRGR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills the TRGR ESUM summary bank from the
C-                         TRGR bank
C-   Controls: None.
C-
C-   Created MAY-15-1992  Amber S. Boehnlein
C-   Modified JUL-10-1992 ASB, updated to include L0 and phi for mpt
C-                        changed to use new zebra_util routines for
C-                        for unpacking.
C-   Modified DEC-30-1992 ASB, added call to rebuild jet list on overflow
C-                        of the candidtate list, various bug fixes
C-   Modified 10-MAR-1993 ASB, added muon l1 information
C-   Modified 09-SEP-1993 ASB, added large tile info
C-   Modified 14-JUL-1994 M. Fortner, Muon controlled through L1ESUM_MUON
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:DBITT.INC'
      INTEGER NLIST_MAX
      PARAMETER (NLIST_MAX=128)
      INTEGER LTRGR_LEVEL1,GZTRGR
      INTEGER GZFIND_CRATE
      LOGICAL OK
      INTEGER L2J_GET_PT_TO_L1FADC
      INTEGER INDEX_ETA,INDEX_PHI
      INTEGER LIST_TYPE,EMHOT,JETHOT,LTHOT,NHOT
      INTEGER    ETA_PHI(1:16,0:1)
      INTEGER   SPEC_TRIG_MASK, TWR_ADDR
      PARAMETER ( SPEC_TRIG_MASK = 0, TWR_ADDR = 1 )
      INTEGER   ENTRY_LIST ( SPEC_TRIG_MASK:TWR_ADDR, 1:TOWER_MAX )
      INTEGER NTOWER,FLAG_WORD
      INTEGER PHI,ETA,ETA_LT,IAVEPHI,IAVEETA
      INTEGER NETA_OFF_LOW,NPHI_OFF_LOW,NETA_OFF_HI,NPHI_OFF_HI
      REAL    CAL_ETA, CAL_PHI
      REAL    EM_ET, HD_ET
      REAL    EM_TT_ENERGY, JET_TT_ENERGY
      REAL    ETMISS,ETSUM,PX,PY
      REAL    VERT_X, VERT_Y, VERT_Z, DUMMY
      REAL    VERTEX_BIN_WIDTH
      INTEGER Z_BIN
      LOGICAL GOOD
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA VERTEX_BIN_WIDTH/6.25/
      DATA EMHOT/0/
      DATA JETHOT/1/
      DATA LTHOT/2/
      DATA INDEX_ETA/0/
      DATA INDEX_PHI/1/
      DATA NETA_OFF_LOW/0/
      DATA NPHI_OFF_LOW/0/
C
      IF (FIRST) THEN
        CALL CL2_RING_INIT    ! FILL common block for conversion
                              ! eta and phi to offline phi.
        FIRST = .FALSE.
        NETA_OFF_HI = NTTLTETA-1
        NPHI_OFF_HI = NTTLTPHI-1
      END IF
C
C
C ****  Get the trigger block
C
      LTRGR_LEVEL1 = GZFIND_CRATE( 'TRGR', GZTRGR(), 11 )
      IF (LTRGR_LEVEL1.GT.0) THEN
        DO LIST_TYPE = EMHOT  , LTHOT
          CALL L1UTIL_JET_LIST_BUILDER_FIRST(LIST_TYPE)
          CALL L1UTIL_JET_LIST_BUILDER( LTRGR_LEVEL1, LIST_TYPE,
     &                      NLIST_MAX,
     &                      NHOT, ENTRY_LIST) !returns MASK and ADDRESS
          IF(NHOT.GE.NLIST_MAX) THEN
            CALL ERRMSG(
     &          'CANDIDATE LIST NOT COMPLETE',
     &          'L1ESUM_TRGR','TRGR BANK HOT TOWER LIST OVERFLOW','W')
            NHOT = NLIST_MAX
          ENDIF
          DO NTOWER = 1  , NHOT
            CAL_PHI=0.0
            CAL_ETA=0.0
            CALL CDBITT(ENTRY_LIST(TWR_ADDR,NTOWER),ETA,PHI,OK)
            IF(LIST_TYPE.EQ.EMHOT.OR.LIST_TYPE.EQ.JETHOT)THEN
              CALL L1EXTRACT_TRGTWR_ADC_ENERGY(IQ(LTRGR_LEVEL1),
     &                 ETA,PHI,EM_TT_ENERGY,JET_TT_ENERGY) !GET TOWER ENERGY
C
              CALL PHYS_COR(PHI,ETA,CAL_PHI,CAL_ETA)    !GET physics coordinate
            ELSEIF(LIST_TYPE.EQ.LTHOT) THEN
              ETA_LT = ETA
              IF ( ETA_LT .GT. 0 ) THEN         ! Convert to 1:2*NETAL
                ETA_LT = ETA_LT + 20
C
C ****  The large tiles are referenced by the minimum eta and phi TT.  To
C       sum for the large tiles, subtract no towers and add the number
C       of towers to be read out.
C
                CALL L2J_ETAPHI_AVE(ETA_LT,PHI,NETA_OFF_LOW,NPHI_OFF_LOW
     &                                       ,NETA_OFF_HI,NPHI_OFF_HI,
     &                                       Q(L2J_GET_PT_TO_L1FADC()),
     &                                       IAVEETA,IAVEPHI,
     &                                       EM_TT_ENERGY,
     &                                       JET_TT_ENERGY)
                IAVEETA=IAVEETA-20
              ELSE
                ETA_LT = ETA_LT + 21
                CALL L2J_ETAPHI_AVE(ETA_LT,PHI,-NETA_OFF_HI,NPHI_OFF_LOW
     &                                       ,NETA_OFF_LOW,NPHI_OFF_HI,
     &                                       Q(L2J_GET_PT_TO_L1FADC()),
     &                                       IAVEETA,IAVEPHI,
     &                                       EM_TT_ENERGY,
     &                                       JET_TT_ENERGY)
                IAVEETA=IAVEETA-21
              ENDIF
              CALL PHYS_COR(IAVEPHI,IAVEETA,CAL_PHI,CAL_ETA)    !GET physics coordinate
            ENDIF
C
C ****  Fill esum bank with objects
C
            FLAG_WORD = 0
            IF (LIST_TYPE.EQ.EMHOT) THEN
              CALL ESUMFL('TRGR',ID_ELECTRON,EM_TT_ENERGY,CAL_ETA,
     &                         CAL_ETA,CAL_PHI,FLAG_WORD)
              CALL ESUMFL('TRGR',ID_PHOTON,EM_TT_ENERGY,CAL_ETA,
     &                         CAL_ETA,CAL_PHI,FLAG_WORD)
            ELSEIF(LIST_TYPE.EQ.JETHOT) THEN
              CALL ESUMFL('TRGR',ID_JET,JET_TT_ENERGY,CAL_ETA,
     &                         CAL_ETA,CAL_PHI,FLAG_WORD)
            ELSEIF(LIST_TYPE.EQ.LTHOT) THEN
              CALL ESUMFL('TRGR',ID_JET_1,JET_TT_ENERGY,CAL_ETA,
     &                         CAL_ETA,CAL_PHI,FLAG_WORD)
            ENDIF         !ESUM filling
          ENDDO           !number of hot towers
        ENDDO             !list_type
C
C ****  get fast vertex bin, convert to nominal position in cm
C
        CALL L1EXTRACT_L0_FAST_Z_DATA(IQ(LTRGR_LEVEL1),Z_BIN,GOOD)
        DUMMY     = 0.0
        VERT_X    = 0.0
        VERT_Y    = 0.0
        VERT_Z    = FLOAT(Z_BIN)*VERTEX_BIN_WIDTH
        CALL ESUMFL('TRGR',ID_VERTEX,DUMMY,VERT_X,VERT_Y,VERT_Z,
     &              GOOD)

C
C ****  Get missing pt and sum et
C
        CAL_ETA = 0.0
        CAL_PHI = 0.0
        FLAG_WORD =0
        CALL L1EXTRACT_MOMENTUM(IQ(LTRGR_LEVEL1),PX,PY,ETMISS)
        IF (PX.NE.0.0) THEN
          CAL_PHI   = ATAN2(-PY,-PX)  ! make phi run from zero to 2pi
        ELSE
          CAL_PHI = SIGN(SNGL(HALFPI),-PY)
        ENDIF

        IF(CAL_PHI.LT.0.0) CAL_PHI = CAL_PHI + SNGL(TWOPI)
C
        CALL L1EXTRACT_TRANSV_ENERGIES(IQ(LTRGR_LEVEL1),EM_ET,HD_ET,
     &                                 ETSUM,ETMISS)

C
C ****  use px and py to get etmiss for now
C

        ETMISS = SQRT(PX**2+PY**2)
        CALL ESUMFL('TRGR',ID_ETSUM,ETSUM, CAL_ETA,
     &              CAL_ETA,CAL_PHI,   FLAG_WORD)
        CALL ESUMFL('TRGR',ID_ETMISS,ETMISS, CAL_ETA,
     &              CAL_ETA,  CAL_PHI,   FLAG_WORD)

      ENDIF
C
C   Fill muon level 1 triggers
C
      CALL L1ESUM_MUON
C
      CALL ESUM_PUSH  !compact the banks
  999 RETURN
      END
