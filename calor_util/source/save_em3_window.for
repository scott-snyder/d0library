      SUBROUTINE SAVE_EM3_WINDOW(LCLUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save EM3 Cells in a 3x3 window around the hottest
C-   EM3 Cell
C-
C-   Inputs  :  LCLUS  [I]  -  Pointer to PELC/PPHO
C-
C-   Outputs :  none
C-
C-   Controls:  CAHITS_RCP
C-
C-   Created  17-OCT-1994   Ian Adam
C-   Updated  20-JUL-1995   R. J. Genik II   Fix Bugs, Comment
C-   Updated  22-JUL-1995   Ian Adam  Extra error checks
C-   Updated  25-JUL-1995   R. J. Genik II   Add booking of CAW3 and CAW5
C-   Updated   8-AUG-1995   R. J. Genik II  Change to save only em3 3x3
C-                           window
C-   Updated   3-OCT-1995   Ian Adam  Correct cells down by CAHT scales for
C-                           compatibility with CASH after D0FIX
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      INTEGER IETA_C,IPHI_C,NCELLMAX,NCELLWIN, ILYR_C
C
      PARAMETER( Ncellmax = 9 )
      INTEGER PADR(NCELLMAX)
      REAL    CELLWINE(NCELLMAX)
C
      REAL    EEM,ETEM
      INTEGER LCAEH,GZCAEH,LCAEP,GZCAEP, IOK
      REAL    X,Y,Z
      INTEGER IETA,IPHI,ILYR,IETA_MIN,IETA_MAX,IPHI_MIN,IPHI_MAX
      INTEGER IETACAEP,IPHICAEP,ILYRCAEP
      INTEGER NRP,CELL_INDEX,IPHI_INDEX
      LOGICAL CEXIST,OK,PSEUDO_NEAREST_EM3
C
      INTEGER LCACL,LCASH,LCLUS,LCAWC,LCAHT,GZCAHT
      INTEGER I
      REAL    CC_SCALE,ECN_SCALE,ECS_SCALE,SCALE
C----------------------------------------------------------------------
      IF (LCLUS.LE.0) THEN
        CALL ERRMSG('INVALID LINK PASSED','SAVE_EM3_WINDOW',' ','W')
        GOTO 999
      ENDIF
C
      LCACL = LQ(LCLUS-2)
C
      IF (LCACL.GT.0) THEN
        LCASH = LQ(LCACL-2)
      ELSE
        LCASH = 0
      ENDIF
      IF (LCASH.GT.0) THEN
        CALL HOTTEST_EM3_CELL(LCASH,IETA_C,IPHI_C,ILYR_C)
      ELSE
        CALL ERRMSG('NO CASH','SAVE_EM3_WINDOW','YOU BROKE','W')
        GOTO 999
      ENDIF
C
C- CHECK FOR CAEH
C
      CALL ENSURE_CAEH(OK)
      IF (.NOT.OK) THEN
        CALL ERRMSG('CAEH NOT FOUND OR MADE','SAVE_EM3_WINDOW',' ','W')
        GOTO 999
      ENDIF
C
C-
      LCAEH = GZCAEH()
      LCAEP = GZCAEP()
C
      IETA_MAX = IETA_C + 1
      IETA_MIN = IETA_C - 1
      IPHI_MAX = IPHI_C + 1
      IPHI_MIN = IPHI_C - 1
C
      IF (IETA_MIN*IETA_MAX.LE.0) THEN
        IF (IETA_C.LT.0) THEN
          IETA_MAX = IETA_MAX + 1
        ELSE IF (IETA_C.GT.0) THEN
          IETA_MIN = IETA_MIN - 1
        ELSE
          CALL ERRMSG('IETA_C=0','SAVE_EM3_WINDOW',' ','W')
        ENDIF
      ENDIF
C
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

      NRP = IQ(LCAEH+2)
      NCELLWIN = 0
C
      DO 100 IETA = IETA_MIN,IETA_MAX
        IF (IETA.NE.0) THEN
          DO 60 IPHI_INDEX = IPHI_MIN,IPHI_MAX
            IPHI = IPHI_INDEX
            IF (IPHI.GT.64) IPHI=IPHI-64
            IF (IPHI.LT. 1) IPHI=IPHI+64
            DO 50 ILYR=LYEM3A,LYEM3D
              IF (CEXIST(IETA,IPHI,ILYR)) THEN
                CALL CELXYZ(IETA,IPHI,ILYR,X,Y,Z,IOK)
                IF (IOK.EQ.0) THEN
                  CELL_INDEX = PTCAEP(IETA,IPHI,ILYR)
                  IF ((CELL_INDEX.GT.0).AND.
     +              (PSEUDO_NEAREST_EM3(IETA_C,IPHI_C,ILYR_C,IETA,IPHI,
     +              ILYR))) THEN
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
     &                  'SAVE_EM3_WINDOW',' ','W')
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
   50       CONTINUE
   60     CONTINUE
        ENDIF
  100 CONTINUE
C
C- Book and fill CAlorimeter Window Core Bank
C
      CALL BKCAWC(LCASH,NCELLWIN,LCAWC)
      IF (LCAWC.GT.0) THEN
        DO 200 I=1,NCELLWIN
          IQ(LCAWC+2*I+1) = PADR(I)
          Q (LCAWC+2*I+2) = CELLWINE(I)
  200   CONTINUE
      ENDIF
  999 RETURN
      END
