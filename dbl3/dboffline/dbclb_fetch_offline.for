      SUBROUTINE DBCLB_FETCH_OFFLINE(PATH,PEDRUN,CRATE,LDAT,LKY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read pedestal file from DBL3 data base
C-
C-   Inputs  : PEDRUN - Pedestal run number to read
C-             CRATE  - Crate number to read
C-   Outputs :
C-              LDAT  - Data bank address = 0 if error
C-              LKY   - Key bank address = 0 if error
C-   Controls:
C-
C-   Modified 09-APR-1992   S. ABACHI  For offline
C-   Modified 20-DEC-1993   S. ABACHI  Reading server added
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBDBL.INC'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INTEGER LKY, LDAT
C
      CHARACTER*(*) PATH
      INTEGER KEY(NKYS), KEYD(NKYS)
      INTEGER LBANK,PEDRUN,CRATE
      EQUIVALENCE (CALIB_LNK(1),LBANK)
      CHARACTER*72 MSGSTR
      INTEGER LENC,TRULEN,LSTPC,DETSEQ
      INTEGER LENTRY,JBIAS,LSUP,LINKC,NSEND,IER
      CHARACTER*3 DETYP
      CHARACTER*4 CALTYP,HBNK
C
C----------------------------------------------------------------------
C
      IF(RCSERVER) THEN
C&IF VAXVMS,VAXELN
        CALL INTMSG
     & ('DBCLB_FETCH_OFFLINE: RCSERVER NOT AVAILABLE ON THIS MACHINE.')
C&ELSE
C&        CALL D0DBL3_RCLIENT_GTDB(PATH,PEDRUN,CRATE,LDAT,LKY)
C&ENDIF
        GOTO 999
      ENDIF
C
   80 CONTINUE
      KEY(3) = PEDRUN
      KEY(4) = PEDRUN
      KEY(8) = CRATE
C
      CALL DBUSE(PATH,LKEYS(CRATE),LDATA(CRATE),PEDRUN,KEY,'S348')
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('DBUSE: Problem fetching the run requested ')
        LBANK = 0
      ENDIF
      KEYD(3) = IC(LKEYS(CRATE)+3)             ! start validity
      IF ( KEYD(3) .EQ. 1 ) THEN               ! dummy bank
        KEYD(4) = IC(LKEYS(CRATE)+4)           ! end validity
        IF ( KEYD(4) .EQ. 999999999 ) THEN     ! no real data
          WRITE (MSGSTR,1000) CRATE
 1000     FORMAT (' DBCLB_FETCH: no valid data for crate ', I3.3 )
          CALL INTMSG (MSGSTR)
        ELSE
          KEY(3) = KEYD(4) + 1           ! get first real data
          KEY(4) = KEYD(4) + 1
          WRITE (MSGSTR,1001) PEDRUN,KEY(3)
 1001     FORMAT(' DBCLB_FETCH:  No valid data for run ',I9,
     &       ' using run ',I9)
          CALL INTMSG(MSGSTR)
          CALL DBUSE(PATH,LKEYS(CRATE),LDATA(CRATE),PEDRUN,KEY,'S348')
        ENDIF
      ENDIF
C
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('DBUSE')
        LBANK = 0
        LDAT = 0
        LKY = 0
        CALL DBCLB_FINISH
      ELSE
        LBANK = LDATA(CRATE)
        LDAT = LDATA(CRATE)
        LKY = LKEYS(CRATE)
      ENDIF
C
  999 RETURN
      END
