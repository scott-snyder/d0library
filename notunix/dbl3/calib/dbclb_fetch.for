      SUBROUTINE DBCLB_FETCH(PATH,PEDRUN,CRATE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read pedestal file from DBL3 data base
C-
C-   Inputs  : PEDRUN - Pedestal run number to read
C-             CRATE  - Crate number to read
C-   Outputs : LBANK  - Pedestal bank address = 0 if error
C-   Controls:
C-
C-   Created  17-NOV-1989   S. Abachi, Jan Guida, S. Rajagopalan
C-   Updated  20-FEB-1992   Jan Guida   Add crate/mod no. no valid data mess.
C-   Updated  07-MAY-1992   S. Abachi   Check iquest(1) after first DBUSE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C
      CHARACTER*(*) PATH
      INTEGER KEY(NKYS), KEYD(NKYS)
      INTEGER LBANK,PEDRUN,CRATE
      EQUIVALENCE (CALIB_LNK(1),LBANK)
      CHARACTER*72 MSGSTR
C----------------------------------------------------------------------
C
      KEY(3) = PEDRUN
      KEY(4) = PEDRUN
      KEY(8) = CRATE
C
       CALL DBUSE(PATH,LKEYS(CRATE),LDATA(CRATE),PEDRUN,KEY,'S348')
       IF (IQUEST(1).NE.0) THEN
         CALL ERRDB('DBUSE')
         LBANK = 0
         CALL DBCLB_FINISH
         GOTO 999
       ENDIF
       KEYD(3) = IC(LKEYS(CRATE)+3)             ! start validity
       IF ( KEYD(3) .EQ. 1 ) THEN               ! dummy bank
         KEYD(4) = IC(LKEYS(CRATE)+4)           ! end validity
         IF ( KEYD(4) .EQ. 999999999 ) THEN     ! no real data
           WRITE (MSGSTR,1000) CRATE
 1000      FORMAT (' DBCLB_FETCH: no valid data for crate ', I3.3 )
           CALL INTMSG (MSGSTR)
         ELSE
           KEY(3) = KEYD(4) + 1           ! get first real data
           KEY(4) = KEYD(4) + 1
           WRITE (MSGSTR,1001) PEDRUN
 1001      FORMAT(' DBCLB_FETCH:  No valid data for run ',I9)
           CALL INTMSG(MSGSTR)
           WRITE (MSGSTR,1002) KEY(3),KEY(8)
 1002      FORMAT(' DBCLB_FETCH:  Using run ',I9,' crate/module ',I3)
           CALL INTMSG(MSGSTR)
           CALL DBUSE(PATH,LKEYS(CRATE),LDATA(CRATE),PEDRUN,KEY,'S348')
         ENDIF
       ENDIF
C
       IF (IQUEST(1).NE.0) THEN
         CALL ERRDB('DBUSE')
         LBANK = 0
         CALL DBCLB_FINISH
       ELSE
         LBANK = LDATA(CRATE)
       ENDIF
C
  999 RETURN
      END
