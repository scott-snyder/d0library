      SUBROUTINE L0_PADS_HIT(PADS_HIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loads the PADS_HIT array with 1's for each
C-                         scintillator pad hit.
C-
C-   Inputs  : none
C-   Outputs : PADS_HIT(x pos,y pos,end) = (0=no valid hit in counter,
C-                                          1= valid hit in counter,
C-                                          2= both chan w valid hit
C-                                             long counters)
C-   Controls: none
C-
C-   Created  29-JUN-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER PADS_HIT(-4:4,-4:4,2,2)
      INTEGER CBUNCH,IBUNCH,NCH,NWD
      INTEGER RAW_TIME(80)
      INTEGER BUNCH_ID(80)
      INTEGER RAW_CHARGE(80)
      INTEGER CORRECT_TIME(80)
      INTEGER CHAN_TO_PADX(72)
      INTEGER CHAN_TO_PADY(72)
      INTEGER CHAN_TO_PADX1(72)
      INTEGER CHAN_TO_PADY1(72)
      INTEGER CHAN_TO_PADX2(72)
      INTEGER CHAN_TO_PADY2(72)
      INTEGER END,I,VERSION
      INTEGER ERR,ICH
      INTEGER IDATA_WORDS(20)
      INTEGER LINK, GZPLV0, GZL0AD
      EXTERNAL GZPLV0,GZL0AD
C
      REAL CHGPED(72)
      REAL TIMPED(72)
      REAL PEDSIG_TMUL,PEDSIG_QMUL
      REAL PED_ARRAY(5,72)
      REAL CHARGE, CHARGE_SCALE
      REAL DATA_WORDS(20)
      EQUIVALENCE ( DATA_WORDS, IDATA_WORDS )
C
      LOGICAL FIRST
      LOGICAL EZERROR
C
      SAVE FIRST,CHAN_TO_PADX,CHAN_TO_PADY,TIMPED,CHGPED
C      SAVE CHAN_TO_PADX2,CHAN_TO_PADY2
      DATA FIRST/.TRUE./
      DATA CHAN_TO_PADX1/1,0,-1,0,1,-1,-1,1,2,0,-2,0,2,1,-1,-2,
     &                -2,-1,1,2,3,0,-3,0,3,0,-3,0,4,0,-4,0,4,0,-4,0,
     &                1,0,-1,0,1,-1,-1,1,2,0,-2,0,2,1,-1,-2,
     &                -2,-1,1,2,3,0,-3,0,3,0,-3,0,4,0,-4,0,4,0,-4,0/
      DATA CHAN_TO_PADY1/0,1,0,-1,1,1,-1,-1,0,2,0,-2,1,2,2,1,-1,-2,
     &                -2,-1,0,3,0,-3,0,3,0,-3,0,4,0,-4,0,4,0,-4,
     &                0,1,0,-1,1,1,-1,-1,0,2,0,-2,1,2,2,1,-1,-2,
     &                -2,-1,0,3,0,-3,0,3,0,-3,0,4,0,-4,0,4,0,-4/
      DATA CHAN_TO_PADX2/1,0,-1,0,1,-1,-1,1,2,0,-2,0,2,1,-1,-2,
     &                -2,-1,1,2,3,3,-3,-3,3,0,-3,0,4,4,-4,-4,4,0,-4,0,
     &                1,0,-1,0,1,-1,-1,1,2,0,-2,0,2,1,-1,-2,
     &                -2,-1,1,2,3,3,-3,-3,3,0,-3,0,4,4,-4,-4,4,0,-4,0/
      DATA CHAN_TO_PADY2/0,1,0,-1,1,1,-1,-1,0,2,0,-2,1,2,2,1,-1,-2,
     &                -2,-1,-3,3,3,-3,0,3,0,-3,-4,4,4,-4,0,4,0,-4,
     &                0,1,0,-1,1,1,-1,-1,0,2,0,-2,1,2,2,1,-1,-2,
     &                -2,-1,-3,3,3,-3,0,3,0,-3,-4,4,4,-4,0,4,0,-4/
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('LEVEL0-no-rcp','LV0HIS',
     &                                'LEVEL0_RCP not found.','W')
        ELSE
          CALL EZGET('PEDSIG_TMUL',PEDSIG_TMUL,ERR)
          CALL EZGET('PEDSIG_QMUL',PEDSIG_QMUL,ERR)
          CALL EZGET('PED_ARRAY(1)',PED_ARRAY(1,1),ERR)
          CALL EZGET('CHARGE_SCALE',CHARGE_SCALE,ERR)
          IF ( CHARGE_SCALE.LE.0.0 ) CHARGE_SCALE = 150.
          CALL EZRSET
        ENDIF
C
        CALL L0_GET_VERSION(VERSION)
        IF ( VERSION.NE.2 ) VERSION=1
        CALL VZERO(TIMPED,72)
        CALL VZERO(CHGPED,72)
        DO I=1,72
          ICH = INT(PED_ARRAY(1,I))
          TIMPED(ICH) = PED_ARRAY(2,I) -
     &                              PEDSIG_TMUL*PED_ARRAY(3,I)
          CHGPED(ICH) = PED_ARRAY(4,I) +
     &                              PEDSIG_QMUL*PED_ARRAY(5,I)
          IF ( VERSION.EQ.1 ) THEN
            CHAN_TO_PADX(I)=CHAN_TO_PADX1(I)
            CHAN_TO_PADY(I)=CHAN_TO_PADY1(I)
          ELSE
            CHAN_TO_PADX(I)=CHAN_TO_PADX2(I)
            CHAN_TO_PADY(I)=CHAN_TO_PADY2(I)
          ENDIF
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
      CALL VZERO(PADS_HIT,324)
C
C  Cut on raw charge and raw time values in L0AD banks.
C
      LINK=GZL0AD()
      IF ( LINK.GT.0 ) THEN
C
        CALL L0_COR_BUNCH(CBUNCH)
C
        CALL GTL0AD(CBUNCH,IBUNCH,NCH,NWD,RAW_TIME,BUNCH_ID,
     &                                     RAW_CHARGE,CORRECT_TIME)
C
        DO ICH=1,72
c          IF ( RAW_TIME(ICH).LE.TIMPED(ICH) ) THEN
c            IF ( RAW_TIME(ICH).GT.10 ) THEN
c              IF ( RAW_CHARGE(ICH).GT.CHGPED(ICH) ) THEN
          IF ( CORRECT_TIME(ICH).GT.10 ) THEN
C
            END = (ICH/37) + 1
            PADS_HIT(CHAN_TO_PADX(ICH),CHAN_TO_PADY(ICH),END,1)=
     &            PADS_HIT(CHAN_TO_PADX(ICH),CHAN_TO_PADY(ICH),END,1)+1
C
            CHARGE=FLOAT(RAW_CHARGE(ICH))-CHGPED(ICH)
            CHARGE=(CHARGE/CHARGE_SCALE) + 1.0
            IF ( CHARGE.GT.5.0 ) CHARGE=5.0
            PADS_HIT(CHAN_TO_PADX(ICH),CHAN_TO_PADY(ICH),END,2) =
     &                                                      INT(CHARGE)
C
          ENDIF
C              ENDIF
C            ENDIF
C          ENDIF
        ENDDO
        GOTO 999
C
      ENDIF
C
   20 CONTINUE
C
C  Otherwise, se PLV0 bank used channels bits if available.
C
      LINK=GZPLV0()
      IF ( LINK.GT.0 ) THEN
        CALL GTPLV0(IDATA_WORDS)
        IF ( IDATA_WORDS(7).LE. 0 ) GOTO 10
        DO ICH=1,20
          END = 1
          IF ( BTEST(IDATA_WORDS(11),(ICH-1)) ) THEN
            PADS_HIT(CHAN_TO_PADX(ICH),CHAN_TO_PADY(ICH),END,1) = 1
          ENDIF
        ENDDO
        DO ICH=21,36
          END = 1
          IF ( BTEST(IDATA_WORDS(13),(ICH-21)) ) THEN
            PADS_HIT(CHAN_TO_PADX(ICH),CHAN_TO_PADY(ICH),END,1)=
     &        PADS_HIT(CHAN_TO_PADX(ICH),CHAN_TO_PADY(ICH),END,1)+1
          ENDIF
        ENDDO
        DO ICH=37,56
          END = 2
          IF ( BTEST(IDATA_WORDS(12),(ICH-37)) ) THEN
            PADS_HIT(CHAN_TO_PADX(ICH),CHAN_TO_PADY(ICH),END,1) = 1
          ENDIF
        ENDDO
        DO ICH=57,72
          END = 2
          IF ( BTEST(IDATA_WORDS(13),(ICH-57+16)) ) THEN
            PADS_HIT(CHAN_TO_PADX(ICH),CHAN_TO_PADY(ICH),END,1)=
     &        PADS_HIT(CHAN_TO_PADX(ICH),CHAN_TO_PADY(ICH),END,1)+1
          ENDIF
        ENDDO
C        GOTO 999
      ENDIF
C
   10 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
