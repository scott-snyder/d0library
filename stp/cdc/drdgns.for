      SUBROUTINE DRDGNS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : read gains from CDC_GAIN file, which
C-                         contains gains from offline calibration
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  21-JUN-1992   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LAYER, SECTOR, WIRE, PLDGNL, GZDGNL, POINT
      INTEGER LUN, IERR
      REAL    GAIN
      LOGICAL OK
      CHARACTER*11 FILNAM
      DATA  FILNAM/'CDC_GAIN'/
C----------------------------------------------------------------------
C
C  open the gain file
C
      CALL GTUNIT(51,LUN,IERR)
      CALL D0OPEN(LUN,FILNAM,'IF',OK)
      IF (.NOT. OK) THEN
        CALL ERRMSG('CDWSTP','DRDGNS',
     &    'Unable to open input file CDC_GAIN','W')
        GOTO 999
      ENDIF
C
      READ (LUN,1000)
C
  100 READ (LUN,1001,END=999) LAYER, SECTOR, WIRE, GAIN
      PLDGNL = GZDGNL(LAYER)
      POINT = PLDGNL + 4 + (SECTOR*IC(PLDGNL+4)+WIRE) * IC(PLDGNL+3)
      C(POINT+1) = GAIN
      GOTO 100
C
 1000 FORMAT (10X)
 1001 FORMAT (1X, 3I8, F12.6)
  999 RETURN
      END
