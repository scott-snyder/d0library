      FUNCTION ISGBEG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      ISAJET hook for begin run
C-
C-   Created   8-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PRIMAR.INC'
      INTEGER USNVRN
      LOGICAL FLGVAL,ISGDIA,ISGBEG
C----------------------------------------------------------------------
C
      NEVENT=USNVRN()
      ISGBEG=.TRUE.
      IF ( FLGVAL('ISA_DIAL') ) THEN
        CALL FLGSET('ISA_DIAL',.FALSE.)
      ELSE
        ISGBEG=ISGDIA()
      ENDIF
      CALL ISABFL    ! fill ISAJET begin record
  999 RETURN
      END
