      LOGICAL FUNCTION MAKE_UINT
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Make a UINT bank hanging from the ANLS 
C-     structure and fill it with the packed ESUM banks..
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-Oct-1994   John D. Hobbs
C-   Modified 03-Oct-1995   JDH - Add TR15 bank and call BKUINT
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUINT.LINK'
      INTEGER LANLS,LUINT,LESUM
      INTEGER N(3),NL,NS,I,SIZE
      LOGICAL LOK
      CHARACTER*80 ERRTXT
C
      INTEGER GZANLS,GZESUM,LENOCC
C-----------------------------------------------------------------------
C
      MAKE_UINT=.TRUE.
C
      LANLS=GZANLS()
      IF( LANLS.LE.0 ) CALL BKANLS(LANLS)
      IF( LANLS.LE.0 ) THEN
        WRITE(ERRTXT,*) 'Could not find or book ANLS bank'
        CALL ERRMSG('NO_ANLS','MAKE_UINT',ERRTXT(1:LENOCC(ERRTXT)),'W')
        GOTO 999
      ENDIF
C
C  Check for the ESUM banks...
C
      LOK = GZESUM('TRGR').EQ.0 
     >    .AND. GZESUM('TR15').EQ.0
     >    .AND. GZESUM('FILT').EQ.0
      IF( LOK ) THEN
        WRITE(ERRTXT,*)'Could not find any TRGR, FILT or TR15 ESUM bank'
        CALL ERRMSG('NO_ESUM','MAKE_UINT',ERRTXT(1:LENOCC(ERRTXT)),'W')
        GOTO 999
      ENDIF
C
C  Make the bank...
C
      CALL VZERO(N,3)
      LESUM=GZESUM('TRGR')
      IF( LESUM.GT.0 ) CALL ESUM_COMPRESSED_SIZE(IQ(LESUM+1),N(1))
      LESUM=GZESUM('TR15')
      IF( LESUM.GT.0 ) CALL ESUM_COMPRESSED_SIZE(IQ(LESUM+1),N(2))
      LESUM=GZESUM('FILT')
      IF( LESUM.GT.0 ) CALL ESUM_COMPRESSED_SIZE(IQ(LESUM+1),N(3))
      SIZE=2+N(1)+N(2)+N(3)
      CALL BKUINT(SIZE)
C
      LUINT=LQ(GZANLS()-IZUINT)
      IF( LUINT.LE.0 ) GOTO 999
C
C  Fill it...
C
      LESUM=GZESUM('TRGR')
      IF( LESUM.GT.0 ) CALL ESUM_COMPRESS(IQ(LESUM+1),IQ(LUINT+3),LOK)
      IF( .NOT.LOK .OR. LESUM.LE.0 ) THEN
        CALL MZPUSH(IXMAIN,LUINT,0,-N(1),'I')
        N(1)=0
      ENDIF
C
      I=N(1)
      LESUM=GZESUM('TR15')
      IF(LESUM.GT.0)CALL ESUM_COMPRESS(IQ(LESUM+1),IQ(LUINT+3+I),LOK)
      IF( .NOT.LOK .OR. LESUM.LE.0 ) THEN
        CALL MZPUSH(IXMAIN,LUINT,0,-N(2),'I')
        N(2)=0
      ENDIF
C
      I=N(1)+N(2)
      LESUM=GZESUM('FILT')
      IF(LESUM.GT.0)CALL ESUM_COMPRESS(IQ(LESUM+1),IQ(LUINT+3+I),LOK)
      IF( .NOT.LOK .OR. LESUM.LE.0 ) THEN
        CALL MZPUSH(IXMAIN,LUINT,0,-N(3),'I')
        N(3)=0
      ENDIF
C
C  If the compression results are OK, save. Otherwise, drop.
C
      SIZE=N(1)+N(2)+N(3)
      IF( SIZE.EQ.0 ) THEN
        LANLS=GZANLS()
        CALL MZDROP(IXMAIN,LQ(LANLS-IZUINT),'L')
      ELSE
        IQ(LUINT+1)=ISHFT(N(2),16)+N(1)
        IQ(LUINT+2)=N(3)
      ENDIF
C
 999  RETURN
      END


