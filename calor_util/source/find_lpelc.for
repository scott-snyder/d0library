      INTEGER FUNCTION FIND_LPELC(NBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returnd the link to the NBANK PELC in linear
C-   chain. Used for recovering links after a possible garbage collection.
C-
C-   Returned value  : Link to PELC
C-   Inputs  : Bank number in chain
C-   Outputs :
C-   Controls:
C-
C-   Created  10-JUL-1995   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'd0$inc:zebcom.inc'
      INTEGER NBANK, LPELC, GZPELC, NZBANK, NPELC, I
      CHARACTER*80 MSG
      LOGICAL Err
C----------------------------------------------------------------------
      FIND_LPELC = 0
      Err = .FALSE.
      LPELC = GZPELC()
      IF (LPELC.GT.0) THEN
        NPELC = NZBANK(IXCOM,LPELC)
        IF (NBANK.LE.NPELC) THEN
          DO 5 I = 1, (NBANK - 1)
            LPELC = LQ(LPELC)
    5     CONTINUE
        ELSE
          Err = .TRUE.
        ENDIF
      ELSE
        Err = .TRUE.
      ENDIF
      IF (.NOT.err) THEN
        FIND_LPELC = LPELC
      ELSE
        WRITE (UNIT=MSG,FMT=10) NBANK,NPELC
        CALL ERRMSG('NO PELC','FIND_LPELC',msg,'W')
   10   FORMAT ('Request for ',I5, ' PELC from a chain of ',I5)
      ENDIF
  999 RETURN
      END
