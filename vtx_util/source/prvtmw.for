      SUBROUTINE PRVTMW (PRUNIT, KVTMW, NVTMW, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank VTMW:  VTX time-to-space for
C-                         one layer of wires
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              KVTMW : bank address
C-              NVTMW : bank number (0,1,2 for layers 0,1,2)
C-              CFL   : flag to control printout
C-                      'ALL' for all banks, 'ONE' for one bank only
C-                      THIS IS NOT A LINEAR STRUCTURE!
C-                      KVTMW or NVTMW must be provided for 'ONE'
C-                      If both given, KVTMW is used
C-                      KVTMW and NVTMW ignored for 'ALL'
C-              IFL   : flag to control partial printout - not used
C-
C-   Created  21-OCT-1988   Peter Grudberg
C-   Updated  14-JUN-1990   Peter Grudberg  IXSTP ==> IDVSTP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVTMH.LINK'
      INTEGER PRUNIT, KVTMW, LVTMW, NVTMW, IFL
      CHARACTER CFL*(*)
      INTEGER LZLOC, J, LAYER, NUMSEC, ISEC, IWIR
      INTEGER LOWRUN, HIGRUN, NWWIRE, NBWIRE
C----------------------------------------------------------------------
C       Print which banks (do they exist)?
C----------------------------------------------------------------------
      LVTMW = KVTMW
      IF ( CFL .NE. 'ONE' .AND. CFL .NE. 'ALL' ) THEN
        WRITE ( PRUNIT, 900 ) 
  900   FORMAT (/,10X,' Routine PRVTMW: ',
     &    '******** You must set CFL to ONE or ALL ********')
        GO TO 999
      ENDIF
      IF ( CFL .EQ. 'ONE' ) THEN
        IF ( LVTMW .LE. 0 ) THEN
          IF ( NVTMW .LT. 0 .OR. NVTMW .GT. 2 ) THEN
            WRITE ( PRUNIT, 910 ) NVTMW
  910       FORMAT (/,10X,' Routine PRVTMW: ',
     &        '******** There is no VTMW bank number',I2,' ********')
            GO TO 999
          ENDIF
          LVTMW = LZLOC(IDVSTP, 'VTMW', NVTMW)
        ENDIF
      ENDIF
C
      IF ( CFL .EQ. 'ALL' ) THEN
C       Find pointer to layer zero VTMW bank 
        LVTMW = LZLOC(IDVSTP, 'VTMW', 0)
      ENDIF
C       Check for existence of bank VTMW at LVTMW
      IF ( LVTMW .LE. 0 ) THEN
        WRITE ( PRUNIT, 920 ) LVTMW
  920   FORMAT (/,10X,' Routine PRVTMW: ',
     &    '**** Bank VTMH does not exist, LVTMW = ',I8,' ****')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C       Print out bank(s)
C----------------------------------------------------------------------
C       First print for 'ONE' bank
      IF ( CFL .EQ. 'ONE' ) THEN
C       Print header, etc (note:bank number = layer number)        
        LAYER = IC(LVTMW-5)
        LOWRUN = IC(LVTMW+1)
        HIGRUN = IC(LVTMW+2)
        NWWIRE = IC(LVTMW+3)
        NBWIRE = IC(LVTMW+4)
        NUMSEC = IC(LVTMW+5)
        WRITE ( PRUNIT, 30 ) LAYER, LOWRUN, HIGRUN, NWWIRE,
     &    NBWIRE, NUMSEC
        DO 100 ISEC = 0, NUMSEC-2, 2
          WRITE ( PRUNIT, 40 ) ISEC, ISEC+1
          DO 110 IWIR = 0, 7
            WRITE ( PRUNIT, 50 ) IWIR,
     &       (C(LVTMW+5+(ISEC*NBWIRE+IWIR)*NWWIRE+J),J=1,2),
     &       IWIR+1,
     &       (C(LVTMW+5+((ISEC+1)*NBWIRE+IWIR)*NWWIRE+J),J=1,2)
  110     CONTINUE
  100   CONTINUE
      ENDIF
C       Now for 'ALL' banks
      IF ( CFL .EQ. 'ALL' ) THEN
        DO 120 LAYER = 0, 2
C       Find pointer to VTMW(LAYER)
          LVTMW = LZLOC(IDVSTP, 'VTMW', LAYER)
          LOWRUN = IC(LVTMW+1)
          HIGRUN = IC(LVTMW+2)
          NWWIRE = IC(LVTMW+3)
          NBWIRE = IC(LVTMW+4)
          NUMSEC = IC(LVTMW+5)
          WRITE ( PRUNIT, 30 ) LAYER, LOWRUN, HIGRUN, NWWIRE,
     &      NBWIRE, NUMSEC
          DO 130 ISEC = 0, NUMSEC-2, 2
            WRITE ( PRUNIT, 40 ) ISEC, ISEC+1
            DO 140 IWIR = 0, 7
              WRITE ( PRUNIT, 50 ) IWIR,
     &          (C(LVTMW+5+(ISEC*NBWIRE+IWIR)*NWWIRE+J),J=1,2),
     &          IWIR+1,    
     &          (C(LVTMW+5+(ISEC*NBWIRE+IWIR)*NWWIRE+J),J=1,2)
  140       CONTINUE  
  130     CONTINUE
  120   CONTINUE
      ENDIF
C----------------------------------------------------------------------
   30 FORMAT (/,' **** Vertex Chamber time-to-space conversion bank',
     &  ' VTMW, layer',I2,' ****',/,
     &  ' **** Lower/Upper valid run #: ',I6,4X,I6,/,
     &  ' **** Words per wire: ',I2,'  Wires per sector: ',I2,
     &  '  Number of sectors: ',I2,/,
     &  ' **** Time-to-space conversion constants: (T0=Time offset,',
     &  ' Vd=Drift time)',/,
     &  ' **** If T is the measured FADC time, the position is',
     &  ' X = (T - T0)*Vd')
   40 FORMAT (/,' **** SECTOR: ',I2,25X,'**** SECTOR: ',I2,/,
     & 4X,'WIRE',7X,'T0',9X,'Vd',16X,'WIRE',7X,'T0',9X,'Vd')
   50 FORMAT (5X,I1,7X,F6.1,3X,E10.3,13X,I1,7X,F6.1,3X,E10.3)
  999 RETURN
      END
