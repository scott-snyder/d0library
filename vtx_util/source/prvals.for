
      SUBROUTINE PRVALS (PRUNIT, LBANK, NVALS, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank VALS:  VTX wire alignment for
C-                         one sector of wires
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              LBANK : bank address -
C-                      For 'ONE', LBANK = LVALS
C-                      For 'ALL', LBANK = LVALL of desired layer
C-                      or LBANK = 0 for all layers
C-              NVALS : bank number 
C-              CFL   : flag to control printout
C-                      'ALL' for all banks, 'ONE' for one bank only
C-                      THIS IS NOT A LINEAR STRUCTURE!
C-                      For 'ONE' bank, KVALS or NVALS must be given.
C-                      For 'ALL' banks:
C-                      To print all banks for one layer, KVALS is
C-                      the address of VALL for that layer.
C-                      To print all banks for all layers, set KVALS=0
C-              IFL   : flag to control partial printout - not used
C-
C-   Created  27-OCT-1988   Peter Grudberg
C-   Updated  14-JUN-1990   Peter Grudberg  IXSTP ==> IDVSTP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT, LBANK, LVALL, LVALS, NVALS, IFL
      CHARACTER CFL*(*)
      INTEGER LAYER, SECTOR, NUMSEC(0:2), IWIR, J
      DATA NUMSEC /16, 32, 32/
      INTEGER LZLOC
      INTEGER LOWRUN, HIGRUN, NBWIRE, NWWIRE
      REAL COSINE, SINE
C----------------------------------------------------------------------
C       Print which banks (do they exist)?
C----------------------------------------------------------------------
      IF ( CFL .NE. 'ONE' .AND. CFL .NE. 'ALL' ) THEN
        WRITE ( PRUNIT, 900 ) 
  900   FORMAT (/,10X,' Routine PRVALS: ',
     &    '******** You must set CFL to ONE or ALL ********')
        GO TO 999
      ENDIF
C       For 'ONE' bank:
      IF ( CFL .EQ. 'ONE' ) THEN
        IF ( LBANK .LE. 0 ) THEN
          LVALS = LZLOC(IDVSTP, 'VALS', NVALS)
        ELSE
          LVALS = LBANK
        ENDIF
      ENDIF
C       For 'ALL' banks:
      IF ( CFL .EQ. 'ALL' ) THEN
        IF ( LBANK .EQ. 0 ) THEN
C       Find address of VALL for layer 0
          LVALL = LZLOC(IDVSTP, 'VALL', 0)
        ELSE
          LVALL = LBANK
        ENDIF
C       Find first VALS bank (sector 0) for existence check
        LVALS = LC(LVALL-1)
      ENDIF
C       Check for existence of bank VALS at LVALS
      IF ( LVALS .LE. 0 ) THEN
        WRITE ( PRUNIT, 910 ) LVALS
  910   FORMAT (/,10X,' Routine PRVALS: ',
     &    '**** Bank VALS does not exist, LVALS = ',I8,' ****')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C       Print out bank(s)
C----------------------------------------------------------------------
C       First print for 'ONE' bank
      IF ( CFL .EQ. 'ONE' ) THEN
        LAYER  = IBITS( IC(LVALS-5), 9, 7 )
        SECTOR = IBITS( IC(LVALS-5), 4, 5 )
        LOWRUN = IC(LVALS+1)
        HIGRUN = IC(LVALS+2)
        COSINE =  C(LVALS+3)
        SINE   =  C(LVALS+4)
        NBWIRE = IC(LVALS+5)
        NWWIRE = IC(LVALS+6)
        WRITE ( PRUNIT, 30 ) LAYER, SECTOR, LOWRUN,
     &    HIGRUN, NWWIRE, NBWIRE, COSINE, SINE 
        DO 100 IWIR = 0 , NBWIRE-1
          WRITE (PRUNIT,40) IWIR,(C(LVALS+6+IWIR*NWWIRE+J),J=1,NWWIRE)
  100   CONTINUE
      ENDIF
C       Now for 'ALL' banks
      IF ( CFL .EQ. 'ALL' ) THEN
        LAYER  = IC(LVALL-5)
  200   LOWRUN = IC(LVALL+1)
        HIGRUN = IC(LVALL+2)
        WRITE (PRUNIT, 50 ) LAYER, LOWRUN, HIGRUN
        DO 110 SECTOR = 0 , NUMSEC(LAYER)-1 
          LVALS  = LC(LVALL-SECTOR-1)
          COSINE =  C(LVALS+3)
          SINE   =  C(LVALS+4)
          NBWIRE = IC(LVALS+5)
          NWWIRE = IC(LVALS+6) 
          WRITE ( PRUNIT, 60 ) LAYER, SECTOR, NWWIRE, NBWIRE,
     &      COSINE, SINE
          DO 120 IWIR = 0 , NBWIRE-1
            WRITE (PRUNIT,40) IWIR,(C(LVALS+6+IWIR*NWWIRE+J),J=1,NWWIRE)
  120     CONTINUE
  110   CONTINUE
        IF ( LBANK .EQ. 0 ) THEN
          LAYER = LAYER + 1
          IF ( LAYER .LE. 2 ) THEN
            LVALL = LZLOC(IDVSTP, 'VALL', LAYER)
            GO TO 200
          ENDIF
        ENDIF
      ENDIF
C----------------------------------------------------------------------
   30 FORMAT (/,' **** Vertex Chamber wire alignment bank',
     &  ' VALS, layer',I2,' sector ',I2,' ****',/,
     &  ' **** Lower/Upper valid run #: ',I6,4X,I6,/,
     &  ' **** Words/wire: ',I2,'  Wires/sector: ',I2,
     &  '  Cosine/sine theta: ',F7.4,4X,F7.4,/,
     &  ' **** Theta = angle between +phi drift direction and the',
     &  ' D0 X axis',/,
     &  ' **** (Theta = phi(sector) + 90 degrees)',/,
     &  ' **** Wire coordinates in D0 frame: ',/,
     &  '  WIRE',6X,'X',9X,'Y',6X,'Z',7X,'dX/dZ',7X,'dY/dZ',6X,
     &  'd2X/dZ2',5X,'d2Y/dZ2')
   40 FORMAT (3X,I1,4X,F8.4,2X,F8.4,2X,F4.2,4(2X,E10.3))
   50 FORMAT (//,' **** Vertex Chamber wire alignment banks VALS',
     &  ' for layer ',I2,' ****',/,
     &  ' **** Lower/Upper valid run #: ',I6,4X,I6,/,
     &  ' **** NOTE: Theta is the angle between +phi drift direction',
     &  ' and the D0 X axis',/,
     &  ' **** (Theta = phi(sector) + 90 degrees)')
   60 FORMAT (/,' **** VALS bank for layer ',I2,' sector ',I2,/,
     &  ' **** Words/wire: ',I2,'  Wires/sector: ',I2,
     &  '  Cosine/sine theta: ',F7.4,4X,F7.4,/,
     &  ' **** Wire coordinates in D0 frame: ',/,
     &  '  WIRE',6X,'X',9X,'Y',6X,'Z',7X,'dX/dZ',7X,'dY/dZ',6X,
     &  'd2X/dZ2',5X,'d2Y/dZ2')
  999 RETURN
      END
