      SUBROUTINE EZPICK_NOMSG (BKNAME,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Exactly the same as EZPICK, except that an error 
C-                          code is returned instead of generating a call to
C-                          ERRMSG when the bank can't be found
C-
C-   Inputs  : BKNAME      Bank name. Up to 32 characters.
C-
C-   Outputs : IER         Uses EZERROR to return error code
C-                         0 --- OK
C-                         See also EZGET_ERROR_TEXT
C-   Controls: None
C-
C-   Created  7-APR-1992   James T. Linnemann   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BKNAME
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INTEGER LOCAT,IER
      LOGICAL OK,EZERROR
C----------------------------------------------------------------------
      CALL EZLOC(BKNAME,LOCAT)
      IF  (LOCAT.GT.0) THEN         ! Bank was found
        CALL EZPICK(BKNAME)
        OK = .NOT.EZERROR(IER)
      ELSE
        IER = EZS_BANK_NOTFOUND     ! Bank not found
      ENDIF
  999 RETURN
      END
