      SUBROUTINE PRVGNL (PRUNIT, KVGNL, NVGNL, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank VGNL:  VTX gains
C-                         one layer of wires
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              KVGNL : bank address
C-              NVGNL : bank number (0,1,2 for layers 0,1,2)
C-              CFL   : flag to control printout
C-                      'ALL' for all banks, 'ONE' for one bank only
C-                      THIS IS NOT A LINEAR STRUCTURE!
C-                      KVGNL or NVGNL must be provided for 'ONE'
C-                      If both given, KVGNL is used
C-                      KVGNL and NVGNL ignored for 'ALL'
C-              IFL   : flag to control partial printout - not used
C-
C-   Created  26-OCT-1988   Peter Grudberg
C-   Updated  14-JUN-1990   Peter Grudberg  IXSTP ==> IDVSTP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT, KVGNL, LVGNL, NVGNL, IFL
      CHARACTER CFL*(*)
      INTEGER LZLOC, LAYER, NUMSEC, IFADC, ISEC, IWIR, IEND
      INTEGER I, J, LOWRUN, HIGRUN, NWFADC, NBFADC
C----------------------------------------------------------------------
C       Print which banks (do they exist)?
C----------------------------------------------------------------------
      LVGNL = KVGNL
      IF ( CFL .NE. 'ONE' .AND. CFL .NE. 'ALL' ) THEN
        WRITE ( PRUNIT, 900 ) 
  900   FORMAT (/,10X,' Routine PRVGNL: ',
     &    '******** You must set CFL to ONE or ALL ********')
        GO TO 999
      ENDIF
      IF ( CFL .EQ. 'ONE' ) THEN
        IF ( LVGNL .LE. 0 ) THEN
          IF ( NVGNL .LT. 0 .OR. NVGNL .GT. 2 ) THEN
            WRITE ( PRUNIT, 910 ) NVGNL
  910       FORMAT (/,10X,' Routine PRVGNL: ',
     &        '******** There is no VGNL bank number',I2,' ********')
            GO TO 999
          ENDIF
          LVGNL = LZLOC(IDVSTP, 'VGNL', NVGNL)
        ENDIF
      ENDIF
C
      IF ( CFL .EQ. 'ALL' ) THEN
C       Find pointer to layer zero VGNL bank 
        LVGNL = LZLOC(IDVSTP, 'VGNL', 0)
      ENDIF
C       Check for existence of bank VGNL at LVGNL
      IF ( LVGNL .LE. 0 ) THEN
        WRITE ( PRUNIT, 920 ) LVGNL
  920   FORMAT (/,10X,' Routine PRVGNL: ',
     &    '**** Bank VGNL does not exist, LVGNL = ',I8,' ****')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C       Print out bank(s)
C----------------------------------------------------------------------
C       First print for 'ONE' bank
      IF ( CFL .EQ. 'ONE' ) THEN
C       Print header, etc (note:bank number = layer number)        
        LAYER = IC(LVGNL-5)
        LOWRUN = IC(LVGNL+1)
        HIGRUN = IC(LVGNL+2)
        NWFADC = IC(LVGNL+3)
        NBFADC = IC(LVGNL+4)
        NUMSEC = IC(LVGNL+5)
        WRITE ( PRUNIT, 30 ) LAYER, LOWRUN, HIGRUN, NWFADC, NBFADC, 
     &    NUMSEC
        DO 100 ISEC = 0, NUMSEC-2, 2
          WRITE ( PRUNIT, 40 ) ISEC, ISEC+1
          DO 110 IFADC = 0, NBFADC-2, 2 
            IWIR = IFADC/2
            WRITE ( PRUNIT, 50 ) IWIR, 
     &       (C(LVGNL+5+(ISEC*NBFADC+IFADC)*NWFADC+J),J=1,2),
     &       IWIR,
     &       (C(LVGNL+5+((ISEC+1)*NBFADC+IFADC)*NWFADC+J),J=1,2)
  110     CONTINUE
  100   CONTINUE
      ENDIF
C       Now for 'ALL' banks
      IF ( CFL .EQ. 'ALL' ) THEN
        DO 120 LAYER = 0, 2
C       Find bank VGNL(LAYER)
          LVGNL = LZLOC( IDVSTP, 'VGNL', LAYER)
          LOWRUN = IC(LVGNL+1)
          HIGRUN = IC(LVGNL+2)
          NWFADC = IC(LVGNL+3)
          NBFADC = IC(LVGNL+4)
          NUMSEC = IC(LVGNL+5)
          WRITE ( PRUNIT, 30 ) LAYER, LOWRUN, HIGRUN, NWFADC,
     &      NBFADC, NUMSEC
          DO 130 ISEC = 0, NUMSEC-2, 2
            WRITE ( PRUNIT, 40 ) ISEC, ISEC+1
            DO 140 IFADC = 0, IC(LVGNL+4)-2, 2
              IWIR = IFADC/2
              WRITE ( PRUNIT, 50 ) IWIR,
     &         (C(LVGNL+5+(ISEC*NBFADC+IFADC)*NWFADC+J),J=1,2),
     &         IWIR,
     &         (C(LVGNL+5+((ISEC+1)*NBFADC+IFADC)*NWFADC+J),J=1,2)
  140       CONTINUE
  130     CONTINUE
  120   CONTINUE 
      ENDIF
C----------------------------------------------------------------------
   30 FORMAT (/,' **** Vertex Chamber gain bank',
     &  ' VGNL, layer',I2,' ****',/,
     &  ' **** Lower/Upper valid run #: ',I6,4X,I6,/,
     &  ' **** Words per FADC: ',I2,'  FADCs per sector: ',I2,
     &  '  Number of sectors: ',I2,/,
     &  ' **** FADC gain values: (MIPS per count)')
   40 FORMAT (/,' **** SECTOR: ',I2,25X,'**** SECTOR: ',I2,/,
     & 4X,'WIRE',5X,'END 0',5X,'END 1',16X,'WIRE',5X,'END 0',5X,'END 1')
   50 FORMAT (5X,I1,6X,F6.3,4X,F6.3,17X,I1,6X,F6.3,4X,F6.3)
  999 RETURN
      END
