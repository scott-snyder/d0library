      SUBROUTINE PRCHOT(PRUNIT,LCHOT,NCHOT,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the contents of the CHOT bank
C-
C-   Inputs  : PRUNIT [I] - Unit number for printout
C-             LCHOT  [I] - Pointer to the CHOT bank
C-             NCHOT  [I] - not used
C-             CFL    [C] - not used
C-             IFL    [I] - not used
C-   Outputs : none
C-   Controls: none
C-
C-   Created  26-JAN-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      CHARACTER*(*) CFL
      LOGICAL BTEST
      INTEGER PRUNIT,LCHOT,NCHOT,IFL
      INTEGER NH,NV,NTOT,IETA,IPHI,ILYR,IFLG,BNKFLG
      INTEGER IWORD,I,IRUN
      BYTE IBWORD(4)
      EQUIVALENCE (IWORD,IBWORD)
C----------------------------------------------------------------------
      NH = IC(LCHOT+1)
      NV = IC(LCHOT+2)
      NTOT = IC(LCHOT+3)
      BNKFLG = IC(LCHOT+4)
      IRUN = IC(LCHOT+5)
      WRITE (PRUNIT,100)
  100 FORMAT(' Output of the calorimeter hot channel bank')
      WRITE (PRUNIT,101)NH,NV,NTOT,IRUN
  101 FORMAT(' Header length:',T30,I9,/,
     &  ' Version number:',T30,I9,/,
     &  ' Number of hot/bad channels:',T30,I9,/,
     &  ' Run number:',T30,I9)
      IF (BTEST(BNKFLG,7)) THEN
        WRITE (PRUNIT,102)
  102   FORMAT(' The bank does not contain all the hot channels, ',
     &    'only the first 500',/)
      ENDIF
      IF (BTEST(BNKFLG,0)) THEN
        WRITE (PRUNIT,103)
  103   FORMAT(' Channels flagged by CALIB bad pedestals contained ',
     &    'in bank, Bit 0')
      ENDIF
      IF (BTEST(BNKFLG,1)) THEN
        WRITE (PRUNIT,104)
  104   FORMAT(' Channels flagged by CALIB bad gains contained ',
     &    'in bank, Bit 1')
      ENDIF
      IF (BTEST(BNKFLG,2)) THEN
        WRITE (PRUNIT,105)
  105   FORMAT(' Channels flagged by hot channel program contained ',
     &    'in bank, Bits 2-7')
      ENDIF
      WRITE (PRUNIT,106)
  106 FORMAT(' ')
C
      IF (NTOT.GT.0) THEN
        WRITE (PRUNIT,110)
  110   FORMAT(5X,' eta ',5X,' phi ',4X,' layer ',4X,
     &    ' hot channel flag')
        DO I=1,NTOT
          IWORD = IC(LCHOT+NH+I)
          IETA = IBWORD(BYTE4)
          IPHI = IBWORD(BYTE3)
          ILYR = IBWORD(BYTE2)
          IFLG = IBWORD(BYTE1)
          WRITE (PRUNIT,111) IETA,IPHI,ILYR,IFLG
  111     FORMAT(6X,I3,7X,I3,7X,I3,7X,Z8)
        ENDDO
      ENDIF
C
  999 RETURN
      END
