      SUBROUTINE ECUNIQ(NELE,IAROLD,NOLD,IARNEW,NNEW,IARFLG,NUNIQ,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Look for events which appear both in new sample
C-     and in existing catalogs entries.  Flag them as duplicates in
C-     new sample, and return number of events in final merged set.
C-     The key is in field 1.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER NELE,NOLD,NNEW,NUNIQ,IAROLD(NELE,*),IARNEW(NELE,*),
     +   IARFLG(*),IERR
      INTEGER I,J
C-----------------------------------------------------------------------
      IERR=0
      I=1
      J=1
      NUNIQ=NNEW
      CALL VZERO(IARFLG,NNEW)
C
 10   CONTINUE
      IF( I.GT.NOLD ) GOTO 999
      IF( J.GT.NNEW ) GOTO 999
        IF( IAROLD(1,I).EQ.(IARNEW(1,J)) ) THEN
          IARFLG(J)=1
          NUNIQ=NUNIQ-1
        ENDIF
        IF( IAROLD(1,I).LT.IARNEW(1,J) ) THEN
          I=I+1
        ELSE
          J=J+1
        ENDIF
      GOTO 10
C
  999 CONTINUE
      RETURN
      END
