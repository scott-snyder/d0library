      SUBROUTINE ASSIGN_NAME (NAME)
C----------------------------------------------------------------------
C
C   Purpose and Methods : Gets the current process name and retains it;
C                         sets a user-specified process name during the
C                         program execution. ENTRY RESTORE_NAME sets
C                         the process name back to the original value.
C
C   Inputs  : NONE
C   Outputs : NONE
C   Controls: NONE
C
C   Created  17-MAY-1989   K. Wyatt Merritt
C
C-   Updated   2-FEB-1990   Harrison B. Prosper
C-      Made into utility
C-   Updated  10-Mar-1992   Herbert Greenlee
C-      Added machine block to become dummy on UNIX.
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
C
C&IF VAXVMS
      INCLUDE '($JPIDEF)/LIST'
C
      INTEGER*2 IOS(4),ITMLST(8)
      INTEGER ITM(3),RETADR,ISTAT,SYS$GETJPIW
      INTEGER I,II,JJ,LL
      EQUIVALENCE (ITM(1),ITMLST(3))
C
      CHARACTER*15 STRNG
C
      LOGICAL CALLED
      DATA CALLED /.FALSE./
      SAVE I,STRNG,CALLED
C----------------------------------------------------------------------
      ITMLST(1)=15
      ITMLST(2)=JPI$_PRCNAM
      ITM(1)=%LOC(STRNG)
      ITM(2)=%LOC(RETADR)
      ISTAT=SYS$GETJPIW(,,,%REF(ITMLST),IOS,,,)
      IF(ISTAT.NE.1) CALL MSGSCR(ISTAT,' USER_PROCNAME--> ')
      I = RETADR
C
      CALL WORD (NAME,II,JJ,LL)
      CALL SYS$SETPRN(NAME(II:JJ))
      CALLED = .TRUE.
C&ELSE
C&ENDIF
      RETURN
C----------------------------------------------------------------------
      ENTRY RESTORE_NAME
C
C&IF VAXVMS
      IF ( CALLED ) THEN
        CALL SYS$SETPRN(STRNG(1:I))
      ENDIF
C&ELSE
C&ENDIF
  999 RETURN
      END
