      SUBROUTINE PRVPDL (PRUNIT, KVPDL, NVPDL, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank VPDL:  VTX pedestals for
C-                         one layer of wires
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              KVPDL : bank address
C-              NVPDL : bank number (0,1,2 for layers 0,1,2)
C-              CFL   : flag to control printout
C-                      'ALL' for all banks, 'ONE' for one bank only
C-                      THIS IS NOT A LINEAR STRUCTURE!
C-                      KVPDL or NVPDL must be provided for 'ONE'
C-                      If both given, KVPDL is used
C-                      KVPDL and NVPDL ignored for 'ALL'
C-              IFL   : flag to control partial printout - not used
C-
C-   Created  26-OCT-1988   Peter Grudberg
C-   Updated  14-JUN-1990   Peter Grudberg  IXSTP ==> IDVSTP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT, KVPDL, LVPDL, NVPDL, IFL
      CHARACTER CFL*(*)
      INTEGER LZLOC, LAYER, NUMSEC, IFADC, ISEC, IWIR, IEND
      INTEGER I, J, LOWRUN, HIGRUN, NWFADC, NBFADC
C----------------------------------------------------------------------
C       Print which banks (do they exist)?
C----------------------------------------------------------------------
      LVPDL = KVPDL
      IF ( CFL .NE. 'ONE' .AND. CFL .NE. 'ALL' ) THEN
        WRITE ( PRUNIT, 900 ) 
  900   FORMAT (/,10X,' Routine PRVPDL: ',
     &    '******** You must set CFL to ONE or ALL ********')
        GO TO 999
      ENDIF
      IF ( CFL .EQ. 'ONE' ) THEN
        IF ( LVPDL .LE. 0 ) THEN
          IF ( NVPDL .LT. 0 .OR. NVPDL .GT. 2 ) THEN
            WRITE ( PRUNIT, 910 ) NVPDL
  910       FORMAT (/,10X,' Routine PRVPDL: ',
     &        '******** There is no VPDL bank number',I2,' ********')
            GO TO 999
          ENDIF
          LVPDL = LZLOC(IDVSTP, 'VPDL', NVPDL)
        ENDIF
      ENDIF
C
      IF ( CFL .EQ. 'ALL' ) THEN
C       Find pointer to layer zero VPDL bank 
        LVPDL = LZLOC(IDVSTP, 'VPDL', 0)
      ENDIF
C       Check for existence of bank VPDL at LVPDL
      IF ( LVPDL .LE. 0 ) THEN
        WRITE ( PRUNIT, 920 ) LVPDL
  920   FORMAT (/,10X,' Routine PRVPDL: ',
     &    '**** Bank VPDL does not exist, LVPDL = ',I8,' ****')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C       Print out bank(s)
C----------------------------------------------------------------------
C       First print for 'ONE' bank
      IF ( CFL .EQ. 'ONE' ) THEN
C       Print header, etc (note:bank number = layer number)        
        LAYER = IC(LVPDL-5)
        LOWRUN = IC(LVPDL+1)
        HIGRUN = IC(LVPDL+2)
        NWFADC = IC(LVPDL+3)
        NBFADC = IC(LVPDL+4)
        NUMSEC = IC(LVPDL+5)
        WRITE ( PRUNIT, 30 ) LAYER, LOWRUN, HIGRUN, NWFADC,
     &    NBFADC, NUMSEC
        DO 100 ISEC = 0, NUMSEC-2, 2
          WRITE ( PRUNIT, 40 ) ISEC, ISEC+1
          DO 110 IFADC = 0, NBFADC-2, 2 
            IWIR = IFADC/2
            WRITE ( PRUNIT, 50 ) IWIR, 
     &       (C(LVPDL+5+(ISEC*NBFADC+IFADC)*NWFADC+J),J=1,4),
     &       IWIR,
     &       (C(LVPDL+5+((ISEC+1)*NBFADC+IFADC)*NWFADC+J),J=1,4)
  110     CONTINUE
  100   CONTINUE
      ENDIF
C       Now for 'ALL' banks
      IF ( CFL .EQ. 'ALL' ) THEN
        DO 120 LAYER = 0, 2
C       Find bank VPDL(LAYER)
          LVPDL = LZLOC( IDVSTP, 'VPDL', LAYER)
          LOWRUN = IC(LVPDL+1)
          HIGRUN = IC(LVPDL+2)
          NWFADC = IC(LVPDL+3)
          NBFADC = IC(LVPDL+4)
          NUMSEC = IC(LVPDL+5)
          WRITE ( PRUNIT, 30 ) LAYER, LOWRUN, HIGRUN, NWFADC,
     &      NBFADC, NUMSEC
          DO 130 ISEC = 0, NUMSEC-2, 2
            WRITE ( PRUNIT, 40 ) ISEC, ISEC+1
            DO 140 IFADC = 0, NBFADC-2, 2
              IWIR = IFADC/2
              WRITE ( PRUNIT, 50 ) IWIR,
     &       (C(LVPDL+5+(ISEC*NBFADC+IFADC)*NWFADC+J),J=1,4),
     &       IWIR,
     &       (C(LVPDL+5+((ISEC+1)*NBFADC+IFADC)*NWFADC+J),J=1,4)
  140       CONTINUE
  130     CONTINUE
  120   CONTINUE 
      ENDIF
C----------------------------------------------------------------------
   30 FORMAT (/,' **** Vertex Chamber pedestal bank',
     &  ' VPDL, layer',I2,' ****',/,
     &  ' **** Lower/Upper valid run #: ',I6,4X,I6,/,
     &  ' **** Words per FADC: ',I2,'  FADCs per sector: ',I2,
     &  '  Number of sectors: ',I2,/,
     &  ' **** Pedestal mean values and sigmas:')
   40 FORMAT (/,' **** SECTOR: ',I2,25X,'**** SECTOR: ',I2,/,
     &  12X,'END 0',11X,'END 1',19X,'END 0',11X,'END 1',/,
     &  2X,'WIRE',3X,'MEAN',3X,'SIGMA',4X,'MEAN',3X,'SIGMA',
     &  5X,'WIRE',3X,'MEAN',3X,'SIGMA',4X,'MEAN',3X,'SIGMA')
   50 FORMAT (3X,I1,4X,F6.2,2X,F6.3,2X,F6.2,2X,F6.3,
     &        5X,I1,4X,F6.2,2X,F6.3,2X,F6.2,2X,F6.3)
  999 RETURN
      END
