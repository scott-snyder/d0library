      SUBROUTINE D0DBL3_CHK_CONT(PATH,NKEY,KEY,COPT,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check to see if a given data described by path
C-                         and keys and copt is in database.
C-
C-   Inputs  : PATH   data path
C-             NKEYS  number of keys
C-             KEY    Vector of keys
C-             COPT   Character option
C-
C-   Outputs : IOK    0=data found, 1=no data, -1=serious probelm
C-   Controls:
C-
C-   Created  16-DEC-1993   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PATH,COPT
      INTEGER NKEY,KEY(NKEY)
      INCLUDE      'D0$INC:D0DBL3_LNK.INC'
      INCLUDE      'D0$INC:D0DBL3_SRVR.INC'
      INCLUDE      'D0$INC:QUEST.INC'
      INTEGER NMKY
      PARAMETER (NMKY=99)
      INTEGER  IOK,KEY2(NMKY),LDAT,LKEY,RUN
      CHARACTER*80  MSG
      INTEGER CLEN,TRULEN
      CHARACTER*30 CUSE
C
      IOK = 0
      CALL VZERO(KEY2,NMKY)
      CALL UCOPY(KEY(1),KEY2(1),NKEY)
      CLEN = TRULEN(COPT)
      CUSE = COPT(1:CLEN)
C
      CALL RZCDIR(PATH, 'U')
C
      RUN = KEY2(3)
      KEY2(4) = KEY2(3)
      CALL DBUSE(PATH,LKEY,LDAT,RUN,KEY2,CUSE)
      IF(IQUEST(1) .EQ. 24) THEN
        IOK = 1
      ELSEIF(IQUEST(1) .NE. 0) THEN
        IOK = -1
        CALL ERRDB('D0DBL3_CHK_CONT: Serious problem.')
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
