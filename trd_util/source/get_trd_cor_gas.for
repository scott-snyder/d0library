      SUBROUTINE GET_TRD_COR_GAS
     &  (LAYER,CORRECTION,TCAN,TTRD,PCAN,PTRD,GCAN,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculates angular correction
C-
C-   Inputs  : PLANE       integer   1,2,3 (anodes) or 4,5,6 (cathodes)
C-   Outputs : CORRECTION  real
C_             TCAN,TTRD   real      temperatures canary et TRD (Celsius)
C-             PCAN,PTRD   real      pressure canary et TRD (hPa)
C-             GCAN        real      "gain" canary (typically 500)
C-             ERROR       integer   0 = OK
C-                                   1 = correction not required in TRD.RCP
C-                                   2 = TROP bank not found
C-                                   3 = error during TRD_COR_GAS
C-                                   4 = data base is not read
C-                                   5 = data base error
C-                                   6 = CTCOR not available and fault during
C-                                       calculation of correction (run 1A only)
C-                                   7 = CTCOR not available and not TROP bank
C-                                       or Data Base problem (run 1A only)
C-
C-   Controls: TRD.RCP
C-
C-   Created  15-JAN-1993   Alain PLUQUET
C-   Updated  29-SEP-1994   Alain PLUQUET  For run 1A, gas correction=Laurent's
C-                                         fit.
C-                                         For run 1B, gas correction=Yves's
C-                                         correction from uranium analysis.
C-                                         Added LAYER argument.
C-   Updated   8-NOV-1994   A. ZYLBERSTEJN  :test on CTCOR value
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTROP.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTCY1.LINK'
      INTEGER GZTCAN,LTCY1,LOC
      INTEGER IER,ERROR,DBM_FIRST_RUN,LTROP,STATUS,LAYER
      REAL A,CORRECTION,TCAN,TTRD,PCAN,PTRD,GCAN,CTCOR
      REAL ENRJCAN,ENRJREF,TEMPCAN,TEMPTRD,TEMPREF
      REAL ENRAP,TCANRAP,TTRDRAP,RELATIVE_AGEING
      LOGICAL FIRST,DO_CORRECTION,READ_DBMON,RUN1A,LCFITOK
      DATA FIRST /.TRUE./
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZLOC ('TRD_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP ('TRD_RCP',IER)
        CALL EZPICK('TRD_RCP')
        CALL EZGET_l('COR_GAS',DO_CORRECTION,IER)
        CALL EZGET_l('READ_DBMON',READ_DBMON,IER)
        CALL EZGET_i('DBM_FIRST_RUN',DBM_FIRST_RUN,IER)
        CALL EZRSET
        IF(RUN1A())THEN
          LTCAN=GZTCAN()
          LCFITOK=.FALSE.
          LTCY1=0
          IF (LTCAN.GT.0) THEN
            LTCY1=LC(LTCAN-IZTCY1)
            IF (LTCY1.GT.0 )THEN
              LCFITOK=.TRUE.
            ELSE
C           PRINT*,' in GET_TRD_COR_GAS, ltcy1',LTCY1
              CALL ERRMSG (' problem correction canary',
     &          'GET_TRD_COR_GAS', 'Bank TCY1 not found', 'W')
            ENDIF
          ELSE
            CALL ERRMSG
     &        (' TRD_DST_COR','TRD_DST_COR','Bank TCAN not found','W')
          ENDIF
        END IF
      ENDIF
C        PRINT*,' lcfitok ',LCFITOK,' ltcy1',ltcy1,' nnc',nnc
      IF(DO_CORRECTION) THEN
        IF (RUN1A()) THEN
          A=RELATIVE_AGEING()
          IF (LCFITOK) THEN
            IF(A.GT.0.) CORRECTION=A*CTCOR()
          ELSE
            CORRECTION=A
          END IF
C          PRINT*, ' correction in GET_TRD_COR_GAS,',
C     &      ' after test on lcfitok', CORRECTION,' a',A
          IF(A.LE.0.)THEN
            IF(READ_DBMON.AND.IQ(LHEAD+12).GT.DBM_FIRST_RUN) THEN
              LTROP=LC(LTGEN-IZTROP)
C              PRINT*,' ltrop',LTROP
              IF(LTROP.NE.0)THEN
                STATUS=0 ! until TROP bank is released
                IF (STATUS.EQ.0) THEN
                  TEMPCAN=C(LTROP+56)+273.15
                  TEMPTRD=C(LTROP+57)+273.15
                  ENRJCAN=C(LTROP+5)
                ELSE
                  CALL ERRMSG('GET_TRD_COR_GAS','GET_TRD_COR_GAS',
     &                  ' Bad TROP bank, take default','W')
                ENDIF
                TEMPREF=293.15
                ENRJREF=650.
                IF (ENRJCAN.EQ.0.) THEN
                  ENRJCAN=650. ! approximates CANARY
                ENDIF
                IF (TEMPCAN.EQ.0.) THEN
                  TEMPCAN=293.15 ! approximates temperature
                ENDIF
                IF (TEMPTRD.EQ.0.) THEN
                  TEMPTRD=293.15 ! approximates temperature
                ENDIF
                IF (ENRJCAN.GT.0..AND.TEMPTRD.GT.0..AND.
     &              TEMPCAN.GT.0..AND.ENRJREF.GT.0..AND.
     &              TEMPREF.GT.0.) THEN
                  ENRAP=ENRJCAN/ENRJREF
                  TCANRAP=TEMPCAN/TEMPREF
                  TTRDRAP=TEMPTRD/TEMPREF
                  IF((ENRJCAN.LT.200..OR.ENRJCAN.GT.900.).OR.
     &                ABS(TTRDRAP-1.).GT.0.5.OR.
     &                ABS(TCANRAP-1.).GT.0.5) THEN
                    CORRECTION=1.
                    ERROR=6
                    CALL ERRMSG('GET_TRD_COR_GAS','GET_TRD_COR_GAS',
     &                  ' Canary values out of bound','W')
                  ELSE
                    CORRECTION=(ENRJREF/ENRJCAN)*
     &                  ((TEMPCAN/TEMPTRD)**7.1)*RELATIVE_AGEING()
                    ERROR=0
                  ENDIF
                ELSE
                  CORRECTION=1.
                  ERROR=6
                  CALL ERRMSG('GET_TRD_COR_GAS','GET_TRD_COR_GAS',
     &                ' No P,T,canary data','W')
                ENDIF
              ELSE
                CORRECTION=1.
                ERROR=7
              ENDIF
            ELSE
              CORRECTION=1.
              ERROR=7
            ENDIF ! TROP
          ENDIF ! LCFITOK
        ELSE ! RUN1B now
          IF(READ_DBMON.AND.IQ(LHEAD+12).GT.DBM_FIRST_RUN) THEN
            LTROP=LC(LTGEN-IZTROP)
            IF(LTROP.NE.0)THEN
C           STATUS=JBYT(IC(LTROP+2),1,4)
              STATUS=0 ! until TROP bank is released
              IF (STATUS.EQ.0) THEN
                TCAN=C(LTROP+56)
                TTRD=C(LTROP+57)
                GCAN=C(LTROP+5)
                PCAN=C(LTROP+55)+C(LTROP+59)
                PTRD=C(LTROP+54)+C(LTROP+60)
                CALL TRD_CALURAPT(TTRD,PTRD,LAYER,CORRECTION,IER)
                IF (IER.NE.0) THEN
                  CORRECTION=1.
                  ERROR=3
                ELSE
                  ERROR=0
                ENDIF
              ELSE
                CORRECTION=1.
                ERROR=5
              ENDIF
            ELSE
              CORRECTION=1.
              ERROR=2
            ENDIF
          ELSE
            CORRECTION=1.
            ERROR=4
          ENDIF
        ENDIF
      ELSE ! DO_CORRECTION
        CORRECTION=1.
        ERROR=1
      ENDIF ! DO_CORRECTION
      END
