      SUBROUTINE FLCAD(ECUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  BUILDS THE FAKE RAW CALORIMETER DATA BANKS
C-                          CAD1, CAD2 INSIDE D0GEANT.
C-                          uses CAEP bank built by D0GEANT
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   7-APR-1989   A.P. White
C-   Updated  27-JUN-1990   Richard V. Astur  and Marc Paterno
C-                            Altered so RSLINK is executed even if the CAEP
C-                            bank does not exist.
C-   Updated  17-MAR-1992   Chip Stewart  
C-                            Check for plates CAEP bank
C-
C----------------------------------------------------------------------
C-
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'       ! TEMPORARY LINK AREA
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:DCALOG.INC'       ! D0GEANT CAL SWITCHES 
C
      BYTE B(4)
      INTEGER PADDR
      EQUIVALENCE(PADDR,B)
      INTEGER GZCAEP
      INTEGER IETAC,IPHIC,ILYRC,BITS
      INTEGER CRATE,ADC,BLS,ROTOW,DEPTH,PCRATE
      INTEGER NCH,NC,LOC,NCAEP,NV,CAEP_TYPE
      INTEGER ICOND
      REAL ENERGY,ECUT
      LOGICAL OK
C-
C----------------------------------------------------------------------
C
C ****  Reserve a tempory link for LCAEP
      CALL GSLINK('FLCAD',NCAEP)
C
C--- LOCATE THE CAEP BANK
      LSLINK(NCAEP) = GZCAEP()
      IF ( LSLINK(NCAEP).EQ.0 ) GO TO 99
C
C ****  CHECK FOR PLATES CAEP BANK
C
      NV =  IQ(LSLINK(NCAEP)+1)
      CAEP_TYPE = NV / 1000
      IF(CAEP_TYPE.EQ.4) THEN
        CALL CAD_GAIN_PLATES
        IF (.NOT.PLATE_GEOM) THEN
          CALL ERRMSG('SET PLATE_GEOM TRUE','FLCAD',
     &      ' THIS IS POST_PROCESS TO GEANT','W')
          PLATE_GEOM = .TRUE.
        END IF
      ELSE IF(PLATE_GEOM.AND.(LOAD1.OR.LOAD2)) THEN
        CALL CAD_GAIN_PLATES
        CALL ERRMSG('FIND PLATE_GEOM TRUE','FLCAD',
     &      ' CAD BANKS USE PLATE GEOMETRY SAMPLING WEIGHTS','I')
      END IF
C
      NCH = IQ(LSLINK(NCAEP)+3)
      IF ( NCH.LE.0 ) GO TO 99
      NC = 0
      LOC = 4
C
C--- LOOP OVER CAEP BANK
C
   50 PADDR = IQ(LSLINK(NCAEP)+LOC)
      IETAC = B(BYTE4)
      IPHIC = B(BYTE3)
      ILYRC = B(BYTE2)
      BITS = B(BYTE1)
      ENERGY = Q(LSLINK(NCAEP)+LOC+1)
C
C ****  CUT CELL IF ENERGY LESS THAN ECUT ( ECUT = SCAL(5) )
C
      IF ( ABS(ENERGY).LT.ECUT .AND.    ! Zero suppress
     &          ILYRC.NE.9 .AND.        ! NOT ICD
     &          ILYRC.NE.8 .AND.        ! NOT MSG
     &          ILYRC.NE.10 ) GOTO 70
C
C--- CONVERT TO READOUT SYSTEM INDICES
C
      CALL CPHAD(IETAC,IPHIC,ILYRC,CRATE,ADC,BLS,ROTOW,DEPTH,ICOND)
      IF ( ICOND.NE.0 ) THEN
        IF ( ILYRC.LE.17 ) THEN
          WRITE(LOUT, 101) IETAC,IPHIC,ILYRC,CRATE,ADC,BLS,ROTOW,
     C                    DEPTH,ICOND
  101     FORMAT(1X,'FLCAD - BAD CPHAD CONVERSION',9I7)
        ENDIF
        GO TO 70
      ENDIF
C
C--- SAVE DATA IN CAD1 OR CAD2 BANK
C
      CALL CADFL(CRATE,ADC,BLS,ROTOW,DEPTH,IETAC,IPHIC,ILYRC,ENERGY)
C
   70 NC = NC + 1
      LOC = LOC + 2
      IF ( NC.LT.NCH ) GO TO 50
C
C--- NOW COMPRESS OUT THE "ZEROES" IN THE CAD1 AND CAD2 BANKS
C--- IF REQUESTED.
      IF ( ECUT.GT.0 ) THEN
        CALL CADZS(1,OK)
        CALL CADZS(2,OK)
      ENDIF
C
   99 CALL RSLINK('FLCAD',NCAEP)        ! Release temporary link
C
      RETURN
      END
