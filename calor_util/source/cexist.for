      FUNCTION CEXIST(IETA,IPHI,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       check if indices correspond to a physical cell
C-       the first call generates a look up table
C-
C-   Returned value  : true if cell exists
C-   Inputs  : 
C-      IETA = eta index
C-      IPHI = phi index
C-      LAYER= layer index
C-
C-   Created   3-FEB-1989   Serban D. Protopopescu
C-   Updated  23-NOV-1992   Joan Guida for missing main-ring channels
C-   Updated  29-OCT-1993   Joan Guida  fix missing main-ring channels 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA,IPHI,LAYER,ABSETA
      LOGICAL CEXIST
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER IETAL,LYRL
      LOGICAL EXTABL(-NETAL:NETAL,NLYRL),FIRST
      SAVE EXTABL,FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN             ! initialization, done only once
C
C        construct table
C
      DO 1 IETAL=-NETAL,NETAL
        ABSETA=IABS(IETAL)
        DO 1 LYRL=1,NLYRL
          EXTABL(IETAL,LYRL)=.FALSE.
C
C        check valid CC ranges
C
          IF(ABSETA.LT.12.AND.LYRL.LT.8) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
          ELSEIF(IETAL.EQ.12.AND.LYRL.LT.5) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
          ELSEIF(IETAL.EQ.-12.AND.LYRL.LT.3) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
          ELSEIF(IETAL.EQ.-12.AND.(LYRL.EQ.5.OR.LYRL.EQ.6)) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
          ELSEIF(ABSETA.LT.10.AND.LYRL.GT.10.AND.LYRL.LT.14) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
            IF(ABSETA.EQ.9.AND.LYRL.EQ.13) EXTABL(IETAL,LYRL)=.FALSE.
          ELSEIF(ABSETA.EQ.10.AND.LYRL.EQ.11) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
          ELSEIF(LYRL.EQ.15.AND.ABSETA.LT.7) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
C
C        check valid CC/EC ranges (massless gaps and ICD)
C
          ELSEIF(LYRL.EQ.8.AND.ABSETA.GT.7.AND.ABSETA.LT.13) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
          ELSEIF(LYRL.EQ.9.AND.ABSETA.GT.8.AND.ABSETA.LT.15) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
          ELSEIF(LYRL.EQ.10.AND.ABSETA.GT.7.AND.ABSETA.LT.14) THEN
            EXTABL(IETAL,LYRL)=.TRUE.     
C
C        check valid EC ranges
C
C               ECOH cells
          ELSEIF(LYRL.EQ.15.AND.ABSETA.GT.7.AND.ABSETA.LT.13) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
          ELSEIF(LYRL.EQ.16.AND.ABSETA.GT.8.AND.ABSETA.LT.14) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
          ELSEIF(LYRL.EQ.17.AND.ABSETA.GT.10.AND.ABSETA.LT.15) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
C               ECMH cells near ECOH
          ELSEIF(ABSETA.LT.14.AND.ABSETA.GT.10.AND.(LYRL.EQ.11.OR.
     &      LYRL.EQ.12)) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
            IF(ABSETA.EQ.11.AND.LYRL.EQ.12) EXTABL(IETAL,LYRL)=.FALSE.
          ELSEIF(ABSETA.EQ.13.AND.(LYRL.EQ.13.OR.LYRL.EQ.14)) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
C               ECMH and ECIH cells
          ELSEIF(IETAL.EQ.14.AND.(LYRL.GT.4.AND.LYRL.LT.8)) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
          ELSEIF(IETAL.EQ.-14.AND.(LYRL.EQ.3.OR.LYRL.EQ.4
     &  .OR.LYRL.EQ.7)) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
          ELSEIF(ABSETA.GT.13.AND.ABSETA.LT.27) THEN
            IF(LYRL.LT.8.AND.ABSETA.GT.14) THEN
              EXTABL(IETAL,LYRL)=.TRUE.
            ELSEIF(LYRL.GT.10.AND.LYRL.LT.16) THEN
              EXTABL(IETAL,LYRL)=.TRUE.
            ENDIF
C             remove pseudo-gaps between ECIH and ECMH
            IF(ABSETA.EQ.18.AND.LYRL.EQ.13) EXTABL(IETAL,LYRL)=.FALSE.
            IF(ABSETA.EQ.19.AND.LYRL.EQ.14) EXTABL(IETAL,LYRL)=.FALSE.
          ELSEIF(ABSETA.GT.26.AND.ABSETA.LT.36) THEN
            IF(LYRL.GT.10.AND.LYRL.LT.16) THEN
              EXTABL(IETAL,LYRL)=.TRUE.
            ELSEIF(LYRL.LT.4) THEN
              EXTABL(IETAL,LYRL)=.TRUE.
            ELSEIF(LYRL.EQ.7) THEN
              EXTABL(IETAL,LYRL)=.TRUE.
            ENDIF
          ELSEIF(ABSETA.EQ.36.AND.LYRL.GT.10.AND.LYRL.LT.16) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
          ELSEIF(ABSETA.EQ.37.AND.LYRL.GT.12.AND.LYRL.LT.16) THEN
            EXTABL(IETAL,LYRL)=.TRUE.
          ENDIF
C
          IF(IETAL.EQ.0) EXTABL(IETAL,LYRL)=.FALSE.  ! remove eta=0
C
    1 CONTINUE
C
      FIRST=.FALSE.
      ENDIF
C
C        Use look-up table
C
      CEXIST=.FALSE.
C
C        check first if indices within range
C
      IF(IPHI.LE.0.OR.IPHI.GT.NPHIL) GOTO 999
      IF(LAYER.LE.0.OR.LAYER.GT.NLYRL) GOTO 999
      IF(IETA.GT.NETAL.OR.IETA.LT.-NETAL) GOTO 999               
      CEXIST=EXTABL(IETA,LAYER)
C           eliminate even iphi from high eta
      IF(IETA.GT.32.OR.IETA.LT.-32) THEN
        IF(MOD(IPHI,2).EQ.0) CEXIST=.FALSE.
      ENDIF
C
C   MISSING EC MAIN RING CHANNELS
C
      IF (IPHI.EQ.18) THEN
        IF ((IABS(IETA).EQ.8  .AND. LAYER.EQ.15) .OR.
     &      (IABS(IETA).EQ.9  .AND. LAYER.EQ.16) .OR.
     &      (IABS(IETA).EQ.10 .AND. LAYER.EQ.16) .OR.
     &      (IABS(IETA).EQ.11 .AND. LAYER.EQ.17)) CEXIST=.FALSE.
      ENDIF
C
  999 RETURN
      END
