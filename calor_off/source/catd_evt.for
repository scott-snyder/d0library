      FUNCTION CATD_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Call any subroutines to process for DST/MicroDST
C-
C-   Created  23-DEC-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      LOGICAL OK,CATD_EVT
      LOGICAL FIRST,DOCATD
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      OK = .TRUE.
      IF ( FIRST) THEN                     ! LOCAL INIT
        CALL EZPICK('CATD_RCP')
        CALL EZERR(IER)
        IF (IER .EQ. 0) THEN
          CALL EZGET('DO_CATDFL',DOCATD,IER)
          CALL EZRSET
        ELSE
          CALL ERRMSG('CATD','CATD_EVT',
     &        'NO CATD_RCP! SKIP PROCESSING','W')
          OK = .FALSE.
          GO TO 900
        END IF
        FIRST = .FALSE.
      ENDIF
C
      IF ( DOCATD ) THEN
        CALL CATDFL                     ! fill CATD
      ENDIF
C
  900 CATD_EVT = OK
  999 RETURN
      END
