      SUBROUTINE PRTSUM ( PRUNIT, LTSUMIN, NTSUM, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content
C-              of bank 'TSUM'. No options.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LTSUMIN [I]: unused
C-             NTSUM  [I] : unused
C-             CFL    [C*]: unused
C-             IFL    [I] : unused
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  20-MAR-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTSUM.LINK'
      INTEGER NR,NFIX,NTRIG,NFILT
C
      INTEGER PRUNIT, LTSUMIN, NTSUM, IFL
      CHARACTER*(*) CFL
      CHARACTER*32 TNAME
      INTEGER LTSUM, GZTSUM, POINT, I
C----------------------------------------------------------------------
C
      LTSUM = GZTSUM()
C
C  ***  Print the content of the bank pointed by LTSUM
C
      IF ( LTSUM.LE.0 ) THEN
        WRITE(PRUNIT,FMT=100)
        GOTO 999
      ELSE
        WRITE(PRUNIT,FMT=101) IQ(LTSUM+1)
        NFIX=IQ(LTSUM+2)
        NR  =IQ(LTSUM+3)
        NTRIG=IQ(LTSUM+4)
        NFILT=IQ(LTSUM+5)
        POINT=LTSUM+NFIX
        DO I=1,NTRIG
          CALL UHTOC(IQ(POINT+2),8,TNAME,32)
          WRITE(PRUNIT,FMT=102) IQ(POINT+1),TNAME
          POINT=POINT+NR
        ENDDO
        DO I=1,NFILT
          CALL UHTOC(IQ(POINT+2),8,TNAME,32)
          WRITE(PRUNIT,FMT=103) IQ(POINT+1),TNAME
          POINT=POINT+NR
        ENDDO
      ENDIF
      WRITE(PRUNIT,FMT=104)
  999 RETURN
C
  100 FORMAT(//,' ********* No TSUM bank *******',//)
  101 FORMAT(//,' Dump of TSUM bank, version ',I3,/)
  102 FORMAT(' Trigger bit ',I4,', trigger name ',A32)
  103 FORMAT(' Filter  bit ',I4,', filter  name ',A32)
  104 FORMAT(//)
      END
