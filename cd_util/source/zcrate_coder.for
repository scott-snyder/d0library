      SUBROUTINE ZCRATE_CODER(FADCCR,FADCCD,SHPRCR,SHPRCD,UPRLWR,
     &                                                  IER,ITASK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert FADC crates and cards to Shaper
C-                         crates and cards and vice-versa.
C-
C-   Inputs/Outputs  : FADCCR, FADCCD = FADC crate and card
C-                     SHPRCR, SHPRCD = Shaper crate and card
C-                     UPRLWR         = Upper or Lower row of shaper card(0/1)
C-   Outputs : IER   =  0 if ok
C-                   = -1 if bad FADC crate id number
C-                     -2 if bad shaper crate id number
C-                     -3 if bad detector type
C-                     -4 if bad FADC card number
C-                     -5 if bad shaper card number
C-                     -6 if bad itask value
C-                     -7 if bad UPRLWR value
C-   Controls: ITASK = 1 means convert FADC to Shaper
C-                     2 means convert Shaper to FADC
C-
C-   Created  19-OCT-1990   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER FADCCR,FADCCD,SHPRCR,SHPRCD,ITASK,IER
      INTEGER DETECTOR,MAXCRTID(3:6),UPRLWR,ITYPE,CRATE1,CRATE2
      INTEGER DETCRATE,FIRSTCD,TMPCHAN,ICHAN,ICARD,IDET,IDETCR
      INTEGER F_TO_S(0:11,3:6)
      DATA MAXCRTID/93,54,115,76/
      DATA F_TO_S/120,120,121,121,111,111,112,112,122,122,0,0,
     &            110,130,100,110,100,130,0,0,0,0,0,0,
     &            80,150,81,82,151,152,80,150,81,82,151,152,
     &            132,131,102,101,102,101,132,131,0,0,0,0/
C----------------------------------------------------------------------
      IF(ITASK.LT.1 .OR. ITASK.GT.2) THEN
        IER=-6
        GOTO 999
      ENDIF
      IF(ITASK.EQ.1) THEN
C
C  Find the detector type and check FADC crate ID.
C     (VTX=3,CDC=4,FDC=5,TRD=6)
C
        DETECTOR = FADCCR - 10*(FADCCR/10)
        IF(DETECTOR.LT.3 .OR. DETECTOR.GT.6) THEN
          IER=-3                          ! Bad detector type
          GOTO 999
        ENDIF
        IF(FADCCR.LT.DETECTOR .OR. FADCCR.GT.MAXCRTID(DETECTOR)) THEN
          IER=-1                          ! Bad FADC crate id number
          GOTO 999
        ENDIF
        DETCRATE = FADCCR/10              ! range = 0-11 max.
        SHPRCR=F_TO_S(DETCRATE,DETECTOR)
        IF(SHPRCR.EQ.0) THEN
          IER=-1                          ! Bad FADC crate id number
          GOTO 999
        ENDIF
C
C  Check FADC card and convert to shaper card and upper/lower row.
C
        SHPRCD = 15 - FADCCD
        IF(DETECTOR.EQ.3) THEN            ! VTX FADC crates
          FIRSTCD = 0
          IF(DETCRATE.LE.1) FIRSTCD = 8
          IF(DETCRATE.GE.6) FIRSTCD = 3
          IF(FADCCD.LT.FIRSTCD .OR. FADCCD.GT.15) THEN
            IER=-4                      ! Bad FADC card number
            GOTO 999
          ENDIF
          IF( (DETCRATE-2*(DETCRATE/2)).EQ.0 ) THEN
            UPRLWR = 0
          ELSE
            UPRLWR = 1
          ENDIF
        ELSEIF(DETECTOR.EQ.4) THEN        ! CDC FADC crates
          FIRSTCD = 0
          IF(FADCCD.LT.FIRSTCD .OR. FADCCD.GT.15) THEN
            IER=-4                      ! Bad FADC card number
            GOTO 999
          ENDIF
          IF(FADCCR.EQ.4) THEN
            UPRLWR = 0
          ELSEIF(FADCCR.EQ.34) THEN
            UPRLWR = 1
          ELSE
            UPRLWR = 1
            IF(FADCCR.GE.44) SHPRCD = 7 - FADCCD
            IF(FADCCD.GE.8) THEN
              UPRLWR=0
              SHPRCD = 23 - FADCCD
              IF(FADCCR.GE.44) SHPRCD = 15 - FADCCD
            ENDIF
          ENDIF
        ELSEIF(DETECTOR.EQ.5) THEN        ! FDC FADC crates
          FIRSTCD = 0
          IF( (DETCRATE-6*(DETCRATE/6)) .GT.1 ) FIRSTCD = 7
          IF(FADCCD.LT.FIRSTCD .OR. FADCCD.GT.15) THEN
            IER=-4                      ! Bad FADC card number
            GOTO 999
          ENDIF
          IF( FADCCR.GE.65 ) THEN
            UPRLWR = 0
          ELSE
            UPRLWR = 1
          ENDIF
        ELSEIF(DETECTOR.EQ.6) THEN        ! TRD FADC crates
          FIRSTCD = 4
          IF(FADCCD.LT.FIRSTCD .OR. FADCCD.GT.15) THEN
            IER=-4                      ! Bad FADC card number
            GOTO 999
          ENDIF
          IF( FADCCR.GE.46 ) THEN
            UPRLWR = 0
          ELSE
            UPRLWR = 1
          ENDIF
        ELSE
          IER=-3                          ! Bad detector type
          GOTO 999
        ENDIF
        GOTO 999                      ! Done.
C
C  Convert Shaper crate to FADC crate.
C
      ELSEIF(ITASK.EQ.2) THEN
        CRATE1 = 0
        CRATE2 = 0
        DO 10 IDET=3,6
          DO 20 IDETCR=0,11
            IF( F_TO_S(IDETCR,IDET).EQ.SHPRCR ) THEN
              IF(CRATE1.EQ.0) THEN
                CRATE1 = IDETCR*10 + IDET
                DETECTOR = IDET
                GOTO 20
              ELSEIF(CRATE2.EQ.0) THEN
                CRATE2 = IDETCR*10 + IDET
                GOTO 30
              ENDIF
            ENDIF
   20     CONTINUE
   10   CONTINUE
C
   30   CONTINUE
        IF(CRATE1.EQ.0 .OR. CRATE2.EQ.0) THEN
          IER=-2                        ! Bad shaper crate ID
          GOTO 999
        ENDIF
C
C  Select one crate using UPRLWR value.
C
        IF(DETECTOR.EQ.3) THEN
          IF(UPRLWR.EQ.0) THEN
            FADCCR = MIN(CRATE1,CRATE2)
          ELSE
            FADCCR = MAX(CRATE1,CRATE2)
          ENDIF
        ELSEIF(DETECTOR.EQ.4) THEN
          IF(SHPRCR.EQ.110) THEN
            IF(UPRLWR.EQ.0) THEN
              FADCCR = MIN(CRATE1,CRATE2)
            ELSE
              FADCCR = MAX(CRATE1,CRATE2)
            ENDIF
          ELSE
            IF(SHPRCD.LE.7) THEN
              FADCCR = MAX(CRATE1,CRATE2)
            ELSE
              FADCCR = MIN(CRATE1,CRATE2)
            ENDIF
          ENDIF
        ELSEIF(DETECTOR.EQ.5) THEN
          IF(UPRLWR.EQ.0) THEN
            FADCCR = MAX(CRATE1,CRATE2)
          ELSE
            FADCCR = MIN(CRATE1,CRATE2)
          ENDIF
        ELSEIF(DETECTOR.EQ.6) THEN
          IF(UPRLWR.EQ.0) THEN
            FADCCR = MAX(CRATE1,CRATE2)
          ELSE
            FADCCR = MIN(CRATE1,CRATE2)
          ENDIF
        ELSE
          IER=-3                        ! Bad detector type
          GOTO 999
        ENDIF
C
C  FADC card and check for valid card number.
C
        FADCCD = 15 - SHPRCD
        IF(DETECTOR.EQ.3) THEN            ! VTX FADC crates
          FIRSTCD = 0
          IF(DETCRATE.LE.1) FIRSTCD = 8
          IF(DETCRATE.GE.6) FIRSTCD = 3
          IF(FADCCD.LT.FIRSTCD .OR. FADCCD.GT.15) THEN
            IER=-5                      ! Bad shaper card number
            GOTO 999
          ENDIF
        ELSEIF(DETECTOR.EQ.4) THEN        ! CDC FADC crates
          IF(SHPRCR.NE.110) THEN
            IF(FADCCR.GE.44) FADCCD = 7 - SHPRCD
            IF(UPRLWR.EQ.0) THEN
              FADCCD = 23 - SHPRCD
              IF(FADCCR.GE.44) FADCCD = 15 - SHPRCD
            ELSEIF(UPRLWR.NE.1) THEN
              IER=-7                        ! Bad UPRLWR value
              GOTO 999
            ENDIF
          ENDIF
          FIRSTCD = 0
          IF(FADCCD.LT.FIRSTCD .OR. FADCCD.GT.15) THEN
            IER=-5                      ! Bad shaper card number
            GOTO 999
          ENDIF
        ELSEIF(DETECTOR.EQ.5) THEN        ! FDC FADC crates
          FIRSTCD = 0
          IF( (DETCRATE-6*(DETCRATE/6)) .GT.1 ) FIRSTCD = 7
          IF(FADCCD.LT.FIRSTCD .OR. FADCCD.GT.15) THEN
            IER=-5                      ! Bad shaper card number
            GOTO 999
          ENDIF
        ELSEIF(DETECTOR.EQ.6) THEN        ! TRD FADC crates
          FIRSTCD = 4
          IF(FADCCD.LT.FIRSTCD .OR. FADCCD.GT.15) THEN
            IER=-5                      ! Bad shaper card number
            GOTO 999
          ENDIF
        ELSE
          IER=-3                          ! Bad detector type
          GOTO 999
        ENDIF
      ENDIF
C
C  Done.
C
C----------------------------------------------------------------------
  999 RETURN
      END
