      SUBROUTINE D0DAD_TESTDF_DOALL(IERR)
C------------------------------------------------------------------------------
C-
C-  Read events using a d0dad file doing all opening/controlling directly.
C-
C-   OUTPUT:  IERR - I - IERR.ne.0 ==> Error during processing.
C------------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:quest.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INCLUDE 'D0$INC:d0dad.inc'
      INTEGER  IOLDFILE,IERR,IXWIPE,ICNTXT,IFLAG,IKEY
      INTEGER  LSTART,NEV,ICLEN,IRUN,IEVENT,IFILE,IREC,IOFFS,IDFDAT(5)
      EQUIVALENCE (IRUN,IDFDAT(1)),(IEVENT,IDFDAT(2))
      INTEGER  LENOCC
      EXTERNAL LENOCC
      LOGICAL  ZEBFOPEN,ZEBFOK,OK
      CHARACTER FZBTMP*255
C
      NEV=0
      CALL D0DAD_SYSOPEN(FDFNAM,IDADDF,IERR)
      IF( IERR.NE.0 ) GOTO 901
C
C  Main event reading loop
C
 20   CONTINUE
C
C     Read the event record from the d0dad file...
C
        CALL D0DAD_DFREAD(IDADDF,IERR)
        IF( IERR.EQ.3 ) GOTO 998
        IF( IERR.NE.0 ) GOTO 903

C     No error during FZIN...

         NEV=NEV+1
         IF( MOD(NEV,100).EQ.0 ) THEN
           WRITE(*,1003) NEV
 1003      FORMAT('     Read sequential event number ',I6)
C& IF VAXVMS
C& ELSE
C&         CALL FLUSH_STD
C& ENDIF
         ENDIF
         CALL D0DAD_LASTIN(IDFDAT)
         IF( IQ(LHEAD+6).NE.IRUN .OR. IQ(LHEAD+9).NE.IEVENT) THEN
            IF( LDDBG.GT.0 ) WRITE(*,1001) IRUN,IEVENT,
     +          IQ(LHEAD+6),IQ(LHEAD+9)
 1001       FORMAT(' DAD ERROR: Original R/E: ',2I12,' Read R/E:',2I12)
         ELSE IF( LDDBG.GT.4 ) THEN
            WRITE(*,1002) IRUN,IEVENT
 1002       FORMAT(' D0DAD_TESTDF: Read R/E: ',2I12)
         ENDIF

      GOTO 20
C
C     Done.  Clean up and exit...
C
 998  CONTINUE
      WRITE(*,1998) NEV
 1998 FORMAT('  End of d0dad file after ',I6,' events')
      IERR=0
      GOTO 999
C
C  Error reporting...
C
 901  CONTINUE
      IF( LDDBG.GT.0 ) WRITE(*,9901) IERR
 9901 FORMAT(' D0DAD_TESTDF: Error ',I5,' opening d0dad file.')
      IERR = -1
      GOTO 999

 903  CONTINUE
      IF( LDDBG.GT.0 ) WRITE(*,9903) IERR
 9903 FORMAT(' D0DAD_TESTDF: Error ',I5,' reading  d0dad file.')
      IERR = -3
      GOTO 999

 999  CONTINUE
      END
