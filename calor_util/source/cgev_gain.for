      FUNCTION CGEV_GAIN(LAYERC,IPHIC,IETAC,SCALE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-        get a conversion of PH to Energy from CGEV bank
C-        E = C*PH
C-   Returned value  : C  (= 0 if there was a problem)
C-   Inputs  : LYRC,IPHIC,IETAC (note the index order) physics cell indices
C-             SCALE = 0 or 1 for X8, X1
C-   Outputs : the C factor
C-   Controls: none
C-
C-   Created  30-MAY-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER LAYERC,IPHIC,IETAC,SCALE
      INTEGER NCAD,NCRATE,IER,IER_SAVE
      INTEGER GZCADT,LCADT,CADT_LINK(2,0:5)
      INTEGER NRETURN
      REAL    CGEV_GAIN,CGEV(4)
      LOGICAL FIRST
      SAVE FIRST,IER_SAVE
      DATA FIRST /.TRUE./
      DATA IER_SAVE /0/
C----------------------------------------------------------------------
      IF(FIRST) THEN
C
C ****  SETUP CADT LOOK-UP TABLE
C
        FIRST = .FALSE.
        CALL CZLINI
        CALL STP_INZLNK
        LCADT = GZCADT ()
        DO NCAD = 1, 2
          DO NCRATE = 0, 5
C
C ****  RESERVE LINK IN STP_ZLINKA FOR LINK TO NCAD,NCRATE
C
            CALL STP_GSLINK('CUNPAK',CADT_LINK(NCAD,NCRATE) )
            STP_LSLINK(CADT_LINK(NCAD,NCRATE)) = LCADT
            IF (LCADT.LE.0) THEN
              CALL ERRMSG('CAL ADDRESSING TABLE','CAEPFL',
     &            'CADT CHAIN BAD','W')
              GOTO 999
            END IF
            LCADT = LC(LCADT)
            IF( LCADT.LE.0) GOTO 77
          END DO
        END DO
   77   LCADT = GZCADT ()
C
C ****  CHECK CAD VERSION TO FIX CADT TABLE IF NEEDED
C
        CALL CADT_FIX(CADT_LINK)
C
C ****  CREATE CGEV BANK FROM CALIB GAIN,PED and CSF_STPFILE
C
        CALL CGEVFL('CAHITS_RCP',IER_SAVE)
        IF (IER_SAVE.NE.0) THEN
          CALL ERRMSG('CGEV_BAD','CAL_GET_CGEV',
     &      'IER .NE. 0 from CGEVFL','F')
        ENDIF
      ENDIF
      CGEV_GAIN = 0
      IER = 1
      IF (IER_SAVE.EQ.0)
     &  CALL GTCGEV(LAYERC,IPHIC,IETAC,SCALE,NRETURN,CGEV,IER)
      IF (IER.EQ.0) CGEV_GAIN = CGEV(1)
  999 RETURN
      END
