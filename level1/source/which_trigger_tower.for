      SUBROUTINE WHICH_TRIGGER_TOWER (IETAC, IPHIC, LAYERC,
     +                                ETA_SIGN, ETA, PHI, CHANNEL,
     +                                EXIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For each Calorimeter cell (identified by
C-                         IETAC, IPHIC and LAYERC indices) finds out
C-                         the Trigger Tower indices if any.
C-
C-   Inputs  : IETAC, IPHIC, LAYERC : Calorimeter cell identification.
C-
C-   Outputs : ETA_SIGN, ETA, PHI, CHANNEL : Trigger Tower identification.
C-             EXIST : tells whether the cell belongs to a Trigger Tower.
C-
C-   Controls: None.
C-
C-   Created  13-MAR-1990   Sylvain Tisserant (MSU)
C-   Updated  18-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                      - Removed extra RETURN statements to meet D0 standards. 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
C
      LOGICAL CEXIST
C
      INTEGER IETAC, IPHIC, LAYERC, ETA_SIGN, ETA, PHI, CHANNEL
      LOGICAL EXIST
C
C----------------------------------------------------------------------
C
C     Does the cell exist ? Does the cell belong to a Trigger Tower ?
C     ===============================================================
C
      EXIST = .FALSE.
      IF(.NOT.CEXIST(IETAC,IPHIC,LAYERC)) GOTO 999
      IF(   ((LAYERC.GE.MNLYMG).AND.(LAYERC.LE.MXLYMG))
     +   .OR.(LAYERC.GT.MXLYFH))                        GOTO 999
C             do not add ICD or massless gaps to trigger towers
      IF(LAYERC.EQ.8.OR.LAYERC.EQ.9.OR.LAYERC.EQ.10) GOTO 999
C
C     Which Trigger Tower ?
C     =====================
C
      IF(IETAC.GT.0) THEN
        ETA      =  IETAC
        ETA_SIGN =  POS_ETA
      ELSE
        ETA      = -IETAC
        ETA_SIGN =  NEG_ETA
      ENDIF
      IF(ETA.LE.32) THEN
        ETA = (ETA+1)/2
      ELSE
        ETA = ETA - 16
      ENDIF
      IF(ETA.GT.ETA_MAX) GOTO 999
C
      PHI = (IPHIC+1)/2
C
      IF(LAYERC.LE.MXLYEM) THEN
        CHANNEL = EM_TOWER
      ELSE
        CHANNEL = HD_TOWER
      ENDIF
C
C     Is the Trigger Tower active ?
C     =============================
C
      IF(DAC_BYTE(ETA_SIGN,ETA,PHI,CHANNEL).EQ.0) GOTO 999
      EXIST = .TRUE.
  999 RETURN
C
      END
