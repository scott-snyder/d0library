       SUBROUTINE FCWRIT
C----------------------------------------------------------------------------
C
C      THIS SUBROUTINE WRITES OUT FZ FILE ON UNIT 17
C
C      AUTHOR:    S KAHN        20 APRIL 1987
C      REVISIONS:  ALTERED SO THAT IT CAN BE FIT INTO 'ZEBSTP'
C      Updated  13-Feb-1992   Herbert Greenlee
C         Changed OPEN to D0OPEN
C
C---------------------------------------------------------------------------
       IMPLICIT NONE
       INCLUDE 'D0$INC:ZEBSTP.INC'
       INCLUDE 'D0$INC:CLINKS.INC'
       INCLUDE 'D0$LINKS:IZSCAL.LINK'
       INTEGER LUN
       LOGICAL OK
C
       DATA LUN / 17/
C
       CALL D0OPEN (LUN, 'CALTOWER_STP', 'OU', OK)
       IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
       CALL FZFILE(LUN,0,'IO')
       CALL FZLOGL(LUN,4)
C
       CALL FZOUT(LUN,IDVSTP,LSCAL,1,' ',0,0,0)
       CALL FZENDO(LUN,' ')
       RETURN
       END

