      SUBROUTINE GETOFS(OFFSET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads the local time differect from a file
C-                         and returns it to the user.
C-
C-   Inputs  : None
C-   Outputs : OFFSET      Time difference in seconds 
C-   Controls: None
C-
C-   Created  28-JUN-1989   Jason McCampbell (MSU)
C-   Updated  11-DEC-1991   Herbert Greenlee
C-       UNIX compatible version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
      INTEGER TMPOFF, OFFSET, LUN, USER, IERR
      LOGICAL FIRST
      CHARACTER*20 VARSTR
      DATA FIRST /.TRUE./
C&ELSE
C&      INTEGER OFFSET, D0_TZ_OFFSET
C&ENDIF
C----------------------------------------------------------------------
C&IF VAXVMS
      IF (FIRST) THEN
        CALL GTUNIT(USER, LUN, IERR)    ! Get next unit number
C       IF (IERR) THEN                  ! Optional error handler
C         WRITE(VARSTR,FMT=100) IERR
C         CALL ERRMSG('ERROR IN DEVICE NUMBER','COMPACK:GTUNIT',VARSTR,
C    +      'W')
C       ENDIF
        OPEN(UNIT=LUN, STATUS='OLD', FILE='D0$HOUR_OFFSET')
        READ (LUN,*) OFFSET
        TMPOFF=OFFSET*3600
        CLOSE(UNIT=LUN)
C
        OPEN(UNIT=LUN, STATUS='OLD', FILE='D0$SECOND_OFFSET')
        READ(LUN,*) OFFSET
        TMPOFF=TMPOFF+OFFSET
        CLOSE(UNIT=LUN)
        FIRST= .FALSE.

        CALL RLUNIT(USER,LUN,IERR)      ! Release unit number
C       IF (IERR) THEN                  ! Optional error handler
C         WRITE(VARSTR,FMT=100) IERR
C         CALL ERRMSG('ERROR IN RELEASING DEVICE','COMPACK:RLUNIT',VARSTR,
C    +      'W')
C       ENDIF
      ENDIF
C
      OFFSET = TMPOFF
C
C Formats
C
  100 FORMAT('Error number: ',I3)
C
C&ELSE
C&      OFFSET = 3600*D0_TZ_OFFSET()
C&ENDIF
  999 RETURN
      END
