      LOGICAL FUNCTION QMS_FIX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Restructure QMS DATA
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created 28-DEC-1995   Bob Hirosky
C-   Updated 12-MAR-1996   Andrew G. Brandt  QMS_FIX.INC to QMS.INC
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZCAEQ.LINK/LIST'
      INCLUDE 'D0$LINKS:IZPLV0.LINK/LIST'
      INCLUDE 'D0$LINKS:IZVERT.LINK/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INCLUDE 'D0$INC:QMS.INC'
      INTEGER LRECO,LPROC,LVERH,LHITS,LCAHT
      INTEGER LCAEQ,GZCAEQ,LPLV0,GZPLV0,GZPROC,LVERT,GZVERT,GZVERH
      INTEGER NVERT,NZBANK
      CHARACTER*4 PATH
      LOGICAL FIRST,SKIP
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C
      QMS_FIX=.TRUE.
C
C ****  check first evet to figure out if this is QMS data
C
      IF (FIRST) THEN
        QMS_DATA = .FALSE.
        SKIP = .TRUE.
        IF ((UPATH.EQ.'MDST').AND.(GZCAEQ().NE.0)) THEN
          CALL ERRMSG('QMS Data File','QMS_FIX',
     &        'Will rebuild structure for CAHITS','W')
          SKIP = .FALSE.
          QMS_DATA = .TRUE.
        ELSE
          CALL ERRMSG('Not QMS Data File','QMS_FIX',
     &        'No new structures for CAHITS','W')
        ENDIF
        First = .FALSE.
      ENDIF
      IF (SKIP) GOTO 999
C
C ****  set path to create new structure
C
      CALL PATHST('RECO')
C
      CALL BKRECO(LRECO)
      CALL BKPROC(LPROC)
      CALL BKVERH(LVERH)
      CALL BKHITS(LHITS)
      CALL BKCAHT(LCAHT)
C
C ****  Move CAEQ to standard place
C
      LCAEQ = GZCAEQ()
      IF ( LCAEQ .GT. 0 ) THEN
        CALL ZSHUNT( IXCOM, LCAEQ, GZPROC(), -IZCAEQ, 0 )
      ELSE
        CALL ERRMSG('No CAEQ','QMS_FIX',
     &      'No CAEQ bank in this event - Skip processing', 'W' )
        QMS_FIX =.FALSE.
      ENDIF
C
C ****  reset path
C
      CALL PATHST('MDST')
C
C ****  COPY PLV0 to PROC Tree
C
      LPLV0 = GZPLV0()
      IF ( LPLV0 .GT. 0 ) THEN
        CALL MZCOPY( IXMAIN, LPLV0, IXMAIN, LPROC, -IZPLV0, ' ' )
      ELSE
        CALL ERRMSG('No PLV0','QMS_FIX',
     &      'No PLV0 bank in this event - Skip Processing', 'W' )
        QMS_FIX =.FALSE.
      ENDIF
C
C ****  Fill DUMMY VERH BANK w/ nubmer of verticies
C
      NVERT = 0
      LVERT=GZVERT(1)
      IF (LVERT.NE.0) THEN
        NVERT = NZBANK(IXCOM,LVERT)
      ENDIF
      IQ(LVERH+2) = NVERT
C
C ****  COPY VERT chain to VERH Tree
C
      IF ( LVERT .NE. 0 ) THEN
        CALL MZCOPY( IXMAIN, LVERT, IXMAIN, LVERH, -IZVERT, ' ' )
      ELSE
        CALL ERRMSG('No VERT','QMS_FIX',
     &      'No VERT banks in this event', 'W' )
      ENDIF
C
C ****  Set Path to RECO for CAHITS
C
      CALL PATHST('RECO')
C
  999 RETURN
      END
