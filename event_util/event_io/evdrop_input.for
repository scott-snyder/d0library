      FUNCTION EVDROP_INPUT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-        Drops banks on INPUT - run this package before all others
C-
C-
C-   Created  28-MAR-1992 Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER IER
      INTEGER MAXDRP
      PARAMETER( MAXDRP = 40 )
      INTEGER BANKS(MAXDRP)
      LOGICAL EVDROP_INPUT,EVDROP_INI
      INTEGER I,NUMDRP,UPTO,INTNAME,OKOK,LBANK,LZFIDH
C----------------------------------------------------------------------
C
      EVDROP_INPUT = .TRUE.
C
      DO I=1,UPTO
   2    LBANK = LZFIDH(IXMAIN,BANKS(I),0)
        IF (LBANK.NE.0) THEN
          CALL MZDROP(IXCOM,LBANK,'L')
          GOTO 2
        ENDIF
      ENDDO
C
      RETURN
C
      ENTRY EVDROP_INI()
C
      EVDROP_INI = .TRUE.
C
      CALL INRCP('DROP_INPUT_RCP',IER)
      IF (IER.NE.0) THEN
        CALL ERRMSG('DROP_INPUT_RCP missing','EVDROP_INI',' ','F')
      ENDIF
      CALL EZPICK('DROP_INPUT_RCP')
C
C       Tell which banks must be dropped from standard input
C
      CALL EZGETA('DROP_LIST',0,0,0,NUMDRP,IER)   ! get number of banks
C
C       can only drop up to MAXDRP
C
      IF (NUMDRP.LE.MAXDRP) THEN
        CALL EZGET('DROP_LIST',BANKS,IER) ! get list of banks
        UPTO = NUMDRP
      ELSE
        CALL ERRMSG('INPUT','INPUT',
     &      'DROP request exceeds maximum allowed ','W')
        UPTO = MAXDRP
      ENDIF
C
      CALL EZRSET
C
      RETURN
      END
