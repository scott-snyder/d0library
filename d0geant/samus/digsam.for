      LOGICAL FUNCTION DIGSAM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Digitize hits in SAMUS drift tubes
C-
C-   Inputs  : Geant hit bank obtained through S/R GFHITS.
C-
C-        NHITS       - I - Number of hits in each SAMUS tube type.
C-        HITS(1,NHI) - F - Station number
C-        HITS(2,NHI) - F - Section number in station
C-        HITS(3,NHI) - F - Tube number in section
C-        HITS(4,NHI) - F - Drift distance (in cm)
C-        HITS(5,NHI) - F - Type of secondary particle made this hit (zero for
C-                          primary particle) - only for filling GSAT bank
C-
C-   Outputs : - Digitization bank for SAMUS (MUD1) stored in S/R SAFILL
C-             - bank GSAT with GEANT information about tracks    
C-               (if SSAM(1) does not equal zero)
C-               
C-
C-   Created  29-SEP-1990   A.Kiryunin
C-   Updated  19-OCT-1990   V. Glebov & V. Podstavkov
C-   Updated  26-MAR-1991   V. GLEBOV     ! MUD1 format from 3-7-91
C-   Updated   5-APR-1991   Andrei Kiryunin: create Zebra bank GSAT (under 
C-   GMUH - GHIT - GEAN - HEAD) with information about tracks in GEANT   
C-   Updated   8-APR-1991   V. GLEBOV  ! Add S/R SACHAN 
C-   Updated  24-APR-1991   Andrei Kiryunin: add fifth word in hit
C-   Updated  30-APR-1991   Vladimir Glebov: add S/R SATOPM   
C-   Updated  24-MAY-1991   Vladimir Glebov: set limits for too many hits   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER NVDIM,NHMAX,NHDIM
      PARAMETER( NVDIM = 1 )
      PARAMETER( NHDIM = 5 )
      PARAMETER( NHMAX = 2000 )
      INTEGER NSTMAX, NSEMAX, NTUMAX
      PARAMETER( NSTMAX = 6 )
      PARAMETER( NSEMAX = 6 )
      PARAMETER( NTUMAX = 310 )         ! Allow 100 hits/section
      INTEGER NUMVS(NVDIM),NUMBV(NVDIM,NHMAX),ITRA(NHMAX),NHITS
      CHARACTER*4 TUBE_NAME,VOLU_NAME
      REAL HITS(NHDIM,NHMAX), DRFLEN
C&IF VAXVMS,IBMAIX,ULTRIX,ALFOSF
      INTEGER SAICMP
C&ENDIF
C&IF SIUNIX
C&      INTEGER*2 SAICMP
C&ENDIF
      EXTERNAL SAICMP
      INTEGER INDX(NSEMAX,NSTMAX), L, SAMADC
      INTEGER I, J, IFL, IPED, NWD, NCHAN
      INTEGER SAHITS(NTUMAX,NSEMAX,NSTMAX)    ! Index order to save CPU
C
      INTEGER MACADR
      INTEGER NST,NSE,NTU, NTYPE,NTY, NHI,PM
      DATA NUMVS/0/, IPED/300/
C----------------------------------------------------------------------
      DIGSAM = .TRUE.
      IF ( DSAM.LT.3 ) GOTO 999
C
C ****  Initialize SAFILL subroutine.
C
      CALL SAFILL(1,0,0,0,0)
C
C ****  Clear index array
C
        DO 810 I = 1, NSTMAX
          DO 820 J = 1, NSEMAX
            INDX(J,I) = 0               
  820     CONTINUE
  810   CONTINUE
C
C ****  Receive number of drift tube types in SAMUS
C
      CALL SAGTYP ( NTYPE )
C
C ****  Loop over all drift tube types
C
      DO 900 NTY = 1,NTYPE
        CALL SANAME (NTY, TUBE_NAME, VOLU_NAME)  ! Receive volume name
C
C ****  Fetch hits for this drift tube type
C
        CALL GFHITS ('SDRF',VOLU_NAME,NVDIM,NHDIM,NHMAX,0,NUMVS,
     +               ITRA,NUMBV,HITS,NHITS)
        IF (NHITS.EQ.0) GOTO 900
C
C ****  Loop over all hits in drift tubes of this type
C
        DO 800  NHI = 1,NHITS
          NST=HITS(1,NHI)               ! No. of station
          NSE=HITS(2,NHI)               ! No. of section in station
          NTU=HITS(3,NHI)               ! No. of tube in section
          DRFLEN=HITS(4,NHI)            ! Drift length (in cm)
C
C ****  Pack track information in GSAT
C
          IF ( SSAM(1).NE.0.0 ) THEN
            CALL SAMGT1 (ITRA(NHI),HITS(1,NHI))
          ENDIF
C
C ****  Digitization
C
          CALL SATOPM(NST,NSE,PM)         ! Convert NST and NSE to PM
          MACADR = PM*256                 ! Form MAC ID for PM 
          CALL SACHAN(NST,NSE,NTU,NCHAN)  ! Calculate channel number
          IFL = MOD(NCHAN,2)              ! Check even or odd
          IF(IFL .EQ. 1) GO TO 700
C
C ****  Fill for even channel number
C
          L = INDX(NSE,NST)
          IF (L .GE. 300) GO TO 800            ! Check boundary
          L = L + 1
          SAHITS(L,NSE,NST) = MACADR + NCHAN   ! Form "Address"
          L = L + 1
          SAHITS(L,NSE,NST) = SAMADC(NST,NSE,DRFLEN)   ! Calc. ADC count
          L = L + 1
          SAHITS(L,NSE,NST) = IPED
          INDX(NSE,NST) = L
          GO TO 800
C
C ****  Fill for odd channel number
C
  700     CONTINUE        
          L = INDX(NSE,NST)
          IF (L .GE. 300) GO TO 800            ! Check boundary
          L = L + 1
          SAHITS(L,NSE,NST) = MACADR + NCHAN - 1  ! Form "Address"
          L = L + 1
          SAHITS(L,NSE,NST) = IPED
          L = L + 1
          SAHITS(L,NSE,NST) = SAMADC(NST,NSE,DRFLEN)      ! Calc. ADC count
          INDX(NSE,NST) = L
C
  800   CONTINUE
  900 CONTINUE
C
C ****  Sort hits in array. NB QSORT is the routine from C run-time lib.
C ****  So, you should link SYS$LIBRARY:VAXCRTL.OLB!
C
      DO 910 I =  1, NSTMAX
        DO 920 J =  1, NSEMAX
C&IF SIUNIX,IBMAIX
C&          CALL QSORT(SAHITS(1,J,I), INDX(J,I)/3, 12, SAICMP)
C&ENDIF
C&IF VAXVMS,ULTRIX,ALFOSF
          CALL QSORT(SAHITS(1,J,I), %VAL(INDX(J,I)/3), %VAL(12), SAICMP)
C&ENDIF
  920   CONTINUE
  910 CONTINUE
C
C ****  Compress array SAHITS. Since we have one hit electronics 
C ****  program remain only one hit/tube with maximum ADC count. 
C
      DO 600 NST = 1, NSTMAX
        DO 610 NSE = 1, NSEMAX
          L = INDX(NSE,NST)
          IF(L .GT. 3) THEN 
          J = 0
          DO 620 I = 1, L, 3
            IF( I .EQ. L-2) GO TO 630
            IF(SAHITS(I,NSE,NST) .EQ. SAHITS(I+3,NSE,NST)) GO TO 640
  630       CONTINUE
            J = J + 1
            SAHITS(J,NSE,NST) = SAHITS(I,NSE,NST)
            J = J + 1
            SAHITS(J,NSE,NST) = SAHITS(I+1,NSE,NST)
            J = J + 1
            SAHITS(J,NSE,NST) = SAHITS(I+2,NSE,NST)
            GO TO 620
  640       CONTINUE
            IF(SAHITS(I+4,NSE,NST) .LT. SAHITS(I+1,NSE,NST)) 
     *            SAHITS(I+4,NSE,NST) = SAHITS(I+1,NSE,NST)
            IF(SAHITS(I+5,NSE,NST) .LT. SAHITS(I+2,NSE,NST)) 
     *            SAHITS(I+5,NSE,NST) = SAHITS(I+2,NSE,NST)
  620     CONTINUE
          INDX(NSE,NST) = J
        ENDIF  
  610   CONTINUE
  600 CONTINUE
C
C ****  Loop over crates.
C
      DO 911 NST = 1, NSTMAX       
C       Create crate header block.
        CALL SAFILL(2,NST,0,0,0)
        DO 912 NSE = 1, NSEMAX
          L = INDX(NSE,NST)
          IF (L .GT. 0) THEN
C -- fill hit information in MUD1 bank...
            CALL SAFILL(3,NST,NSE,L,SAHITS(1,NSE,NST))
          ENDIF
  912   CONTINUE
  911 CONTINUE
C
C ****  Store data in MUD1 bank.
C
      CALL SAFILL(4,0,0,0,0)
C
C ****  Truncate free space in GSAT banks
C
      IF ( SSAM(1).NE.0.0 ) THEN
        CALL SAMGT2 ()
      ENDIF
C
  999 RETURN
      END
