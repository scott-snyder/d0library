      SUBROUTINE DBCLB_GTKEY(PATH,CALRUN,CRATE,KEYC,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read pedestal file from DBL3 data base
C-
C-   Inputs  : PEDRUN - Pedestal run number to read
C-             CRATE  - Crate number to read
C-   Outputs : KEYC   - DBL3 keys for run CALRUN and crate CRATE
C-             IERR   - 0 if no error
C-   Controls:
C-
C-   Created  12-JUN-1990   Jan Guida
C-   Updated   2-APR-1991   Jan Guida  Add IERR 
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
      INTEGER KEY(NKYS), KEYC(NKYS)
      INTEGER CALRUN,CRATE,I,IERR
      CHARACTER*72 MSG
C----------------------------------------------------------------------
C
      IERR = 0
      CALL VZERO(KEYC,NKYS)
      KEY(3) = CALRUN
      KEY(4) = CALRUN
      KEY(8) = CRATE
      CALL DBUSE(PATH,LKEYS(CRATE),LDATA(CRATE),CALRUN,KEY,'KS348')
      IF(LKEYS(CRATE).EQ.0) THEN
        WRITE(MSG,14)CALRUN,CRATE
   14   FORMAT(1X,' Requested Data for ',' Run = ',
     &        I9.9,' Crate = ',I3.3,' not found')
        CALL INTMSG(MSG)
        IERR = 1
        GO TO 999
      ENDIF
      DO I = 1,NKYS
        KEYC(I) = IC(LKEYS(CRATE)+I)
      ENDDO

999   RETURN
      END
