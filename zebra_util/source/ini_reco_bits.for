      SUBROUTINE INI_RECO_BITS(SCALES,IWORDS,BYTE_LOC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       fill arrays starting from PARAMS files
C-   
C-   Outputs : 
C-     SCALES(10) = scales for reco bytes
C-     IWORDS(10) = words order for bytes
C-     BYTE_LOC(4)= byte order
C-
C-   Created  30-MAR-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IWORDS(10),BYTE_LOC(4)
      REAL    SCALES(10)
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        SCALES(ID_PHOTON)=SCA_PHOTON
        IWORDS(ID_PHOTON)=IW_PHOTON
        SCALES(ID_ELECTRON)=SCA_ELECTRON
        IWORDS(ID_ELECTRON)=IW_ELECTRON
        SCALES(ID_MUON)=SCA_MUON
        IWORDS(ID_MUON)=IW_MUON
        SCALES(ID_TAU)=SCA_TAU
        IWORDS(ID_TAU)=IW_TAU
        SCALES(ID_JET)=SCA_JET
        IWORDS(ID_JET)=IW_JET
        SCALES(ID_ETMISS)=SCA_ETMISS
        IWORDS(ID_ETMISS)=IW_ETMISS
        SCALES(ID_ETSUM)=SCA_ETSUM
        IWORDS(ID_ETSUM)=IW_ETSUM
        SCALES(ID_JET_1)=SCA_JET_1
        IWORDS(ID_JET_1)=IW_JET_1
        SCALES(ID_JET_2)=SCA_JET_2
        IWORDS(ID_JET_2)=IW_JET_2
        SCALES(ID_JET_3)=SCA_JET_3
        IWORDS(ID_JET_3)=IW_JET_3
        BYTE_LOC(1)=BYTE1
        BYTE_LOC(2)=BYTE2
        BYTE_LOC(3)=BYTE3
        BYTE_LOC(4)=BYTE4
        FIRST=.FALSE.
      ENDIF
  999 RETURN
      END
