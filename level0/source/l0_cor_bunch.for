      SUBROUTINE L0_COR_BUNCH(BUNCH_ID0)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the correct bunch id.
C-
C-   Inputs  : none
C-   Outputs : BUNCH_ID0
C-   Controls: none
C-
C-   Created  29-JUN-1992   Jeffrey Bantly
C-   Updated  25-JAN-1993   Jeffrey Bantly  check PLV0 bank first 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
C
      INTEGER BUNCH_ID0,BUNCH_ID1
      INTEGER CBUNCH, NBUNCH
      INTEGER LTRGR,LCRATE,LCRATE0
      INTEGER NDATA
      INTEGER VERSION,DETAILS(5,7)
      INTEGER ANDOR1,ANDOR2
      INTEGER GZFIND_CRATE
      EXTERNAL GZFIND_CRATE
      INTEGER ICONT(20)
C
      INTEGER IB,IC,BUNCH_BIT(32)
C
      LOGICAL FIRST
      LOGICAL PRODUC, PRODFL
      EXTERNAL PRODUC
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        PRODFL = PRODUC()
        FIRST=.FALSE.
      ENDIF
C
C  fetch correct bunch number from PLV0 bank if present
C
      CBUNCH = 0
      CALL VZERO(ICONT,20)
      CALL GTPLV0(ICONT)
      IF ( ICONT(1).NE.-1 ) CBUNCH = IBITS(ICONT(1),7,3)
      IF ( CBUNCH.GT.0 ) THEN
        BUNCH_ID0 = CBUNCH
        GOTO 999
      ENDIF
C
C  fetch correct bunch number from LV0H bank if present
C
      CBUNCH = 0
      NBUNCH = 0
      CALL GTLV0H(CBUNCH,NBUNCH)
      IF ( CBUNCH.GT.0 ) THEN
        BUNCH_ID0 = CBUNCH
        GOTO 999
      ENDIF
C
C  fetch location in TRGR bank of L0 crate, crate 01
C
      LTRGR = LQ(LHEAD-IZTRGR)
      IF (LTRGR .EQ. 0) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-no-TRGR1','L0_COR_BUNCH',
     &                                 'TRGR bank not found','W')
        GOTO 999
      ENDIF
      LCRATE = GZFIND_CRATE('TRGR',LTRGR,1)
      IF (LCRATE .LE. 0) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-no-Crt01','L0_COR_BUNCH',
     &                            'Crate 01 data not found','W')
        GOTO 999
      ENDIF
C
C  fetch crate 01 header info
C
      VERSION = 1
      LCRATE0 = LCRATE-1
      CALL L0_DECODE_HEADER(LCRATE0,VERSION,NDATA,DETAILS)
C
C  set correct bunch number
C
      BUNCH_ID0 = DETAILS(5,7)+1
C
C  fetch crate 11 link and correct bunch id
C
      LTRGR = LQ(LHEAD-IZTRGR)
      IF (LTRGR .EQ. 0) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-no-TRGR2','L0_COR_BUNCH',
     &                                 'TRGR bank not found','W')
        GOTO 999
      ENDIF
      LCRATE = GZFIND_CRATE('TRGR',LTRGR,11)
      IF (LCRATE .LE. 0) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-No-Crt11-cb',
     &    'L0_COR_BUNCH','Crate 11 not found','W')
        GOTO 999
      ENDIF
      ANDOR1 = IQ(LCRATE+197)
      ANDOR2 = IQ(LCRATE+213)
      IF ( ANDOR1.NE.ANDOR2 ) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-andors-donot-match',
     &    'L0_COR_BUNCH','ANDORs do not match in L1 crate in TRGR','W')
      ENDIF
      BUNCH_ID1 = -1
      DO IB=0,5
        BUNCH_BIT(IB+1) = IBITS(IQ(LCRATE+195+2),IB,1)
        IF ( BUNCH_BIT(IB+1).EQ.1 ) THEN
          IF ( BUNCH_ID1.EQ.-1 ) THEN
            BUNCH_ID1=IB+1
          ELSE
            IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-mul-correct-bunch',
     &          'L0_COR_BUNCH','Too many cor bunch','W')
          ENDIF
        ENDIF
      ENDDO
      IF ( BUNCH_ID0.NE.BUNCH_ID1 ) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-L0-L1-Bad-Cbunch',
     &      'L0_COR_BUNCH','L0 and L1 cor bunch donot match','W')
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
