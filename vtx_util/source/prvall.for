      SUBROUTINE PRVALL (PRUNIT, KVALL, NVALL, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank VALL:  VTX gains
C-                         one layer of wires
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              KVALL : bank address
C-              NVALL : bank number (0,1,2 for layers 0,1,2)
C-                      Note: if both bank address and number are given,
C-                      the bank address KVALL is used
C-              CFL   : flag to control printout - not used
C-              IFL   : flag to control partial printout - not used
C-
C-   Created  27-OCT-1988   Peter Grudberg
C-   Updated  14-JUN-1990   Peter Grudberg  IXSTP ==> IDVSTP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT, KVALL, LVALL, NVALL, IFL
      CHARACTER CFL*(*)
      INTEGER LZLOC, LAYER, NUMSEC(0:2), LOWRUN, HIGRUN
      DATA NUMSEC /16, 32, 32/
C----------------------------------------------------------------------
C       Print which banks (do they exist)?
C----------------------------------------------------------------------
      LVALL = KVALL
      IF ( LVALL .LE. 0 ) THEN
        IF ( NVALL .LT. 0 .OR. NVALL .GT. 2 ) THEN
          WRITE ( PRUNIT, 910 ) NVALL
  910     FORMAT (/,10X,' Routine PRVALL: ',
     &      '******** There is no VALL bank number',I2,' ********')
          GO TO 999
        ENDIF
        LVALL = LZLOC(IDVSTP, 'VALL', NVALL)
      ENDIF
C
C       Check for existence of bank VALL at LVALL
      IF ( LVALL .LE. 0 ) THEN
        WRITE ( PRUNIT, 920 ) LVALL
  920   FORMAT (/,10X,' Routine PRVALL: ',
     &    '**** Bank VALL does not exist, LVALL = ',I8,' ****')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C       Print out bank(s)
C----------------------------------------------------------------------
C       Print header, etc (note:bank number = layer number)        
      LAYER = IC(LVALL-5)
      LOWRUN = IC(LVALL+1)
      HIGRUN = IC(LVALL+2)
      WRITE ( PRUNIT, 30 ) LAYER, LOWRUN, HIGRUN, NUMSEC(LAYER)
C----------------------------------------------------------------------
   30 FORMAT (/,' **** Vertex Chamber wire alignment layer header bank',
     &  ' VALL, layer',I2,' ****',/,
     &  ' **** Lower/Upper valid run #: ',I6,4X,I6,/,
     &  ' **** Number of sectors: ',I2)
  999 RETURN
      END
