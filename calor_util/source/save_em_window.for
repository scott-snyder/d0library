      SUBROUTINE SAVE_EM_WINDOW(LCLUS,WINDOW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save EM and FH cells in a window around the
C-                         hottest tower in a PELC/PPHO object
C-
C-   Inputs  :  LCLUS  [I]  -  Pointer to PELC/PPHO
C-              WINDOW [I]  -  WINDOW SIZE (e.g., 5 FOR 5x5; must be odd)
C-
C-   Outputs :  none
C-
C-   Controls:  CAHITS_RCP
C-
C-   Created  17-OCT-1994   Ian Adam
C-   Updated  20-JUL-1995   R. J. Genik II   Fix Bugs, Comment
C-   Updated  22-JUL-1995   Ian Adam  Extra error checks
C-   Updated  25-JUL-1995   R. J. Genik II   Add booking of CAW3 and CAW5
C-   Updated   3-OCT-1995   Ian Adam  Correct for CAHT scales for CASH
C-                           compatibility after D0FIX
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'

      INTEGER IETA_C,IPHI_C,WINDOW,WINDOW_MAX,NCELLMAX,NCELLWIN
      PARAMETER( WINDOW_MAX = 7 )
      PARAMETER( NCELLMAX = WINDOW_MAX*WINDOW_MAX*8 )
      INTEGER PADR(NCELLMAX)
      REAL    CELLWINE(NCELLMAX)

      REAL    EEM,EFH,ETEM,ETFH
      INTEGER LCAEH,GZCAEH,LCAEP,GZCAEP, IOK
      REAL    X,Y,Z
      INTEGER IETA,IPHI,ILYR,IETA_MIN,IETA_MAX,IPHI_MIN,IPHI_MAX
      INTEGER IETACAEP,IPHICAEP,ILYRCAEP
      INTEGER NRP,CELL_INDEX,IPHI_INDEX
      LOGICAL CEXIST,OK

      INTEGER LCACL,LCASH,LCLUS,LCAW,LCAHT,GZCAHT
      INTEGER IETA_HOT(5),IPHI_HOT(5)
      REAL    EDPTH(5),PDPTH(5),ENERGY_HOT(5)
      INTEGER I
      REAL    CC_SCALE,ECN_SCALE,ECS_SCALE,SCALE
C----------------------------------------------------------------------
      IF (LCLUS.GT.0) THEN
        IF (WINDOW.GT.WINDOW_MAX) THEN
          CALL ERRMSG('WIN TOO BIG','SAVE_EM_WINDOW','YOU IDIOT','W')
          GOTO 999
        ENDIF
      ELSE
        CALL ERRMSG('INVALID LINK PASSED','SAVE_EM_WINDOW',' ','W')
      ENDIF

      IF (REAL(WINDOW)/2..EQ.int(WINDOW/2)) then ! even WINDOW
        CALL ERRMSG('INVALID WINDOW SIZE','SAVE_EM_WINDOW',
     +    'Must be Odd WINDOW size ','W')
      ENDIF

      IF (WINDOW.LE.0) THEN
        CALL ERRMSG('INVALID WINDOW SIZE','SAVE_EM_WINDOW',
     +    'Must be positive WINDOW size ','W')
      ENDIF

      LCACL = LQ(LCLUS-2)

      IF (LCACL.GT.0) THEN
        LCASH = LQ(LCACL-2)
      ELSE
        LCASH = 0
      ENDIF
      IF (LCASH.GT.0) THEN
        CALL CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
      ELSE
        CALL ERRMSG('NO CASH','SAVE_EM_WINDOW','YOU BROKE','W')
        GOTO 999
      ENDIF

      IETA_C = IETA_HOT(3)
      IPHI_C = IPHI_HOT(3)

C- ZERO OUTPUTS

      EEM  = 0.0
      EFH  = 0.0
      ETEM = 0.0
      ETFH = 0.0

C- CHECK FOR CAEH

      CALL ENSURE_CAEH(OK)
      IF (.NOT.OK) THEN
        CALL ERRMSG('CAEH NOT FOUND OR MADE','SAVE_EM_WINDOW',' ','W')
        GOTO 999
      ENDIF

C-
      LCAEH = GZCAEH()
      LCAEP = GZCAEP()

      LCAHT = GZCAHT()
      IF (LCAHT.LE.0) THEN
        CALL ERRMSG('NO CAHT','SAVE_EM3_WINDOW',' ','W')
        GOTO 999
      ENDIF

      IF (IQ(LCAHT-1).GE.4) THEN
        ECN_SCALE = Q(LCAHT+2)
        CC_SCALE  = Q(LCAHT+3)
        ECS_SCALE = Q(LCAHT+4)
      ELSE
        ECN_SCALE = 1.0
        CC_SCALE  = 1.0
        ECS_SCALE = 1.0
      ENDIF


      IETA_MAX = IETA_C + (WINDOW-1)/2
      IETA_MIN = IETA_C - (WINDOW-1)/2
      IPHI_MAX = IPHI_C + (WINDOW-1)/2
      IPHI_MIN = IPHI_C - (WINDOW-1)/2

      IF (IETA_MIN*IETA_MAX.LE.0) THEN
        IF (IETA_C.LT.0) THEN
          IETA_MAX = IETA_MAX + 1
        ELSE IF (IETA_C.GT.0) THEN
          IETA_MIN = IETA_MIN - 1
        ELSE
          CALL ERRMSG('IETA_C=0','SAVE_EM_WINDOW',' ','W')
        ENDIF
      ENDIF

      NRP = IQ(LCAEH+2)
      NCELLWIN = 0

      DO 100 IETA = IETA_MIN,IETA_MAX
        IF (IETA.NE.0) THEN
          DO 60 IPHI_INDEX = IPHI_MIN,IPHI_MAX
            IPHI = IPHI_INDEX
            IF (IPHI.GT.64) IPHI=IPHI-64
            IF (IPHI.LT. 1) IPHI=IPHI+64
            DO 50 ILYR=1,7
              IF (CEXIST(IETA,IPHI,ILYR)) THEN
                CALL CELXYZ(IETA,IPHI,ILYR,X,Y,Z,IOK)
                IF (IOK.EQ.0) THEN
                  CELL_INDEX = PTCAEP(IETA,IPHI,ILYR)
                  IF (CELL_INDEX.GT.0) THEN
                    EEM  = Q( LCAEH + 7 + (CELL_INDEX-1)*NRP )
                    ETEM = Q( LCAEH + 8 + (CELL_INDEX-1)*NRP )
                    NCELLWIN = NCELLWIN + 1
                    IF (ABS(IETA).LE.12) THEN
                      SCALE = CC_SCALE
                    ELSE IF (IETA.LE.-13) THEN
                      SCALE = ECN_SCALE
                    ELSE IF (IETA.GE.13) THEN
                      SCALE = ECS_SCALE
                    ENDIF
                    CELLWINE(NCELLWIN) = EEM/SCALE
                    PADR(NCELLWIN)     = IQ(LCAEP+4+(CELL_INDEX-1)*2)
                    CALL CAEP_INDICES(PADR(NCELLWIN),IETACAEP,IPHICAEP,
     &                ILYRCAEP)
                    IF (IETACAEP.NE.IETA.OR.IPHICAEP.NE.IPHI.OR.
     &                ILYRCAEP.NE.ILYR) THEN
                      CALL ERRMSG('CAEP/CAEH MISMATCH',
     &                  'SAVE_EM_WINDOW',' ','W')
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
   50       CONTINUE
            ILYR = 11
            IF (CEXIST(IETA,IPHI,ILYR)) THEN
              CALL CELXYZ(IETA,IPHI,ILYR,X,Y,Z,IOK)
              IF (IOK.EQ.0) THEN
                CELL_INDEX = PTCAEP(IETA,IPHI,ILYR)
                IF (CELL_INDEX.GT.0) THEN
                  EFH  = Q( LCAEH + 7 + (CELL_INDEX-1)*NRP )
                  ETFH = Q( LCAEH + 8 + (CELL_INDEX-1)*NRP )
                  NCELLWIN = NCELLWIN + 1
                  IF (ABS(IETA).LE.12) THEN
                    SCALE = CC_SCALE
                  ELSE IF (IETA.LE.-13) THEN
                    SCALE = ECN_SCALE
                  ELSE IF (IETA.GE.13) THEN
                    SCALE = ECS_SCALE
                  ENDIF
                  PADR(NCELLWIN)     = IQ(LCAEP+4+(CELL_INDEX-1)*2)
                  CELLWINE(NCELLWIN) = EFH/SCALE
                  CALL CAEP_INDICES(PADR(NCELLWIN),IETACAEP,IPHICAEP,
     &                ILYRCAEP)
                  IF (IETACAEP.NE.IETA.OR.IPHICAEP.NE.IPHI.OR.
     &                ILYRCAEP.NE.ILYR) THEN
                    CALL ERRMSG('CAEP/CAEH MISMATCH',
     &                  'SAVE_EM_WINDOW',' ','W')
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
   60     CONTINUE
        ENDIF
  100 CONTINUE

C- Book and fill Requested Bank

      IF (WINDOW.GT.5) THEN
        CALL BKCAW7(LCASH,NCELLWIN,LCAW)
      ELSE IF (WINDOW.EQ.5) THEN
        CALL BKCAW5(LCASH,NCELLWIN,LCAW)
      ELSE IF (WINDOW.EQ.3) THEN
        CALL BKCAW3(LCASH,NCELLWIN,LCAW)
      ELSE
        CALL ERRMSG('No Logical Path to this error','SAVE_EM_WINDOW',
     +    'Source code altered by Moron','W')
        GOTO 999
      ENDIF
      IF (LCAW.GT.0) THEN
        DO 200 I=1,NCELLWIN
          IQ(LCAW+2*I+1) = PADR(I)
          Q (LCAW+2*I+2) = CELLWINE(I)
  200   CONTINUE
      ENDIF

  999 RETURN
      END
