      SUBROUTINE EZ_COPY_ARRAY(RCPIN,RCPOUT,ARNAME,ARRAY,LARRAY,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : COPY A CHARACTER ARRAY OF NAME
C-   ARNAME FROM RCPFILE RCPIN  TO RCPFILE RCPOUT
C-
C-   Inputs  : RCPIN = name of RCP input file
C-             RCPOUT = name of RCP output file
C-             ARNAME = name of array
C-   Outputs : ARRAY = contents of array
C-             LARRAY = length of array
C-             IER is non zero on error
C-   Controls:
C-
C-   Created   5-APR-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPIN,RCPOUT,ARNAME
      CHARACTER*(*) ARRAY
      INTEGER LARRAY,IER
      LOGICAL EZERR
C----------------------------------------------------------------------
      IER = 0
      CALL EZPICK(RCPIN)
      IF(.NOT.EZERR(IER))THEN
        CALL EZ_GET_CHARS(ARNAME,LARRAY,ARRAY,IER)
        CALL EZRSET
C
        CALL EZPICK(RCPOUT)
        IF(.NOT.EZERR(IER))THEN
          CALL EZ_REPLACE_ARRAY(ARNAME,ARRAY,LARRAY,IER)
          CALL EZRSET
        ENDIF
      ENDIF
  999 RETURN
      END
