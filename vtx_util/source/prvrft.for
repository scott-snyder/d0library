      SUBROUTINE PRVRFT (PRUNIT, KVRFT, NVRFT, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : print drift volume geometry bank VRFT
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              LVRFT : bank address
C-              NVRFT : bank number
C-              CFL   : how many banks to print - not used
C-              IFL   : flag controlling type of printout - not used  
C-
C-   Created  13-OCT-1988   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT, KVRFT, LVRFT, NVRFT, IFL
      CHARACTER CFL*(*)
      INTEGER NBLAY, ILAY, J
C----------------------------------------------------------------------
      LVRFT = KVRFT
      IF ( LVRFT .LE. 0 ) THEN
        WRITE ( PRUNIT, 900 )
  900   FORMAT (/,10X,' Routine PRVRFT: ',
     &    '******** The bank VRFT does not exist ********')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C     PRINT OUT BANK
C----------------------------------------------------------------------
      NBLAY = IC(LVRFT + 1)
      WRITE (PRUNIT, 30) NBLAY
C
C  *** Print layer data
C
      DO 910 ILAY = 0, NBLAY-1
        WRITE (PRUNIT, 40) ILAY, (IC(LVRFT+1+7*ILAY+J), J=1,3),
     &     (C(LVRFT+1+7*ILAY+J), J=4,7)
  910 CONTINUE
C
C  *** Print wire positions     
C       
      WRITE (PRUNIT, 50) (C(LVRFT+1+7*NBLAY+J), J=1,16)
C----------------------------------------------------------------------
   30 FORMAT (/,' **** Vertex Chamber drift volume geometry bank',
     &  ' VRFT ****',/,
     &  ' **** Number of sensitive layers: ',I1,/, 
     &  11X,'SECTORS',4X,'WIRES',5X,'Zmax',7X,'HALF',5X,'HALF',
     &  8X,'R',8X,'PHI',/,                                              
     &  13X,'PER',7X,'PER',5X,'OFFSET',6X,'CELL',4X,'OPENING',5X,'CELL',
     &  5X,'CELL 0',/,
     &  2X,'LAYER',5X,'LAYER',5X,'SECTOR',3X,'IN VGEH',4X,'HEIGHT',4X,
     &  'ANGLE',5X,'CENTER',4X,'CENTER')
   40 FORMAT (4X,I1,5X,3(4X,I2,4X),4(1X,F7.4,2X))
   50 FORMAT (' **** Wire position relative to cell center(sense 
     &wires along X)',/,
     &  ' X(0,...,7):',8(1X,F7.4),/,
     &  ' Y(0,...,7):',8(1X,F7.4),/,
     &  ' **** (for odd sectors, multiply Y by -1)')
  999 RETURN
      END
