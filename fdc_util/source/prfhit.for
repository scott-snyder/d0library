      SUBROUTINE PRFHIT(PRUNIT,LJFHIT,MFHIT,CFL,IFL)
C----------------------------------------------------------------------
C-
C-  Purpose and Methods : Print out FHIT (compressed hit bank).
C-
C-   Inputs  : 
C-         PRUNIT= unit number for printout
C-         LJFHIT = bank address (not used)
C-         MFHIT = numerical bank identifier (not used)
C-         CFL   = not used
C-         IFL   = 0      no printout
C-         IFL   = 3      full printout of bank
C-
C-
C-   Created  22-AUG-1991   Robert E. Avery
C-   Updated  18-OCT-1991   Robert E. Avery  Use FHIT_DECODE. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Input:      
      INTEGER PRUNIT,LJFHIT,MFHIT,IFL
      CHARACTER CFL*(*)
C  Local:
      INTEGER IQFHIT(2),NHIT
C  Printout line:
      INTEGER IHIT,HALF,UNIT,QUAD,SECTOR,WIRE
      INTEGER DL,LR,ON_SEG,TRK_FDCT,TRK_ZTRK
      REAL    DRIFTD, DRIFTD_MIRROR, Z_POS, IONIZATION 
C----------------------------------------------------------------------
      IF ( IFL .LT. 3 ) GOTO 999
C
      CALL GTFHIT(0,NHIT)
      IF ( NHIT .LT. 0 ) GOTO 999
C
      WRITE(PRUNIT,*) ' '
      WRITE(PRUNIT,100) NHIT
      WRITE(PRUNIT,101)
C
      DO IHIT =  1, NHIT
C
        CALL GTFHIT(IHIT,IQFHIT)
        CALL FHIT_DECODE( IQFHIT,
     &    HALF,UNIT,QUAD,SECTOR,WIRE,
     &    DL,LR,ON_SEG,TRK_FDCT,TRK_ZTRK,
     &    DRIFTD, DRIFTD_MIRROR, Z_POS, IONIZATION )
C
        WRITE(PRUNIT,102) IHIT,HALF,UNIT,QUAD,SECTOR,WIRE,
     &      DL,LR,ON_SEG,TRK_FDCT,TRK_ZTRK,DRIFTD,Z_POS,IONIZATION 
C
      ENDDO
C
  100 FORMAT(1X,' FHIT, Compressed Hit Bank , Number of hits:',I7)
  101 FORMAT(1X,'    Hit   H   U   Q   S   W  DL  LR  SG TRK ZTRK',
     &   '    Drift    Y/X_DL Ionization' )
  102 FORMAT(1X,I7,5I4,L4,I4,L4,2I4,3F10.3)
C
  999 RETURN
      END
