      SUBROUTINE BKUINT(SIZE)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Make a UINT bank hanging from the ANLS 
C-     structure.
C-
C-   Inputs  : SIZE - length of bank in words.
C-   Outputs :
C-   Controls:
C-
C-   Created  14-Oct-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUINT.LINK'
      INTEGER LANLS,LUINT,LESUM
      INTEGER NS,SIZE
      LOGICAL LOK
      CHARACTER*80 ERRTXT
      INTEGER GZANLS,GZESUM,LENOCC
C-----------------------------------------------------------------------
C
      LANLS=GZANLS()
      IF( LANLS.LE.0 ) CALL BKANLS(LANLS)
      IF( LANLS.LE.0 ) THEN
        WRITE(ERRTXT,*) 'Could not find or book ANLS bank'
        CALL ERRMSG('NO_ANLS','BKUINT',ERRTXT(1:LENOCC(ERRTXT)),'W')
        GOTO 999
      ENDIF
C
C  Make sure there are enough links for UINT bank.  Drop an existing bank.
C
      NS=IQ(LANLS-2)
      IF( NS.LT.IZUINT ) THEN                   ! Not enough links. Push...
        CALL MZPUSH(IXMAIN,LANLS,IZUINT-NS,0,' ')
        IF( LANLS.LE.0 ) THEN
          WRITE(ERRTXT,*) 'Error pushing links in ANLS'
          CALL ERRMSG('ANLS_PUSH','BKUINT',ERRTXT,'W')
          GOTO 999
        ENDIF
      ENDIF
      IF( LQ(LANLS-IZUINT).NE.0 ) THEN         ! Already a UINT. Drop it...
        CALL MZDROP(IXMAIN,LQ(LANLS-IZUINT),'L')
      ENDIF
C
C  Make the bank...
C
      CALL MZBOOK(IXMAIN,LUINT,LANLS,-IZUINT,'UINT',0,0,SIZE,1,0)
      IF( LUINT.LE.0 ) THEN
        WRITE(ERRTXT,*) 'Error booking UINT bank'
        CALL ERRMSG('ANLS_PUSH','BKUINT',ERRTXT,'W')
        GOTO 999
      ENDIF
C
 999  RETURN
      END


