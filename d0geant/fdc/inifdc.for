      FUNCTION INIFDC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initializations for the Forward Drift Chamber.
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  dd-mmm-198y   ????????????????
C-   Updated  23-MAY-1988   Ghita Rahal-Callot  : Put the structure to read
C-                          the geometry from a file in FDISTP
C-   Updated  19-OCT-1988   Jeffrey Bantly   put in good 0-sup thresholds
C-                          for the FDC
C-   Updated  26-JUN-1989   Jeffrey Bantly   put in pulse shape parameters 
C-   Updated  17-JUL-1989   Harrison B. Prosper :Made into pbd logical function 
C-   Updated  18-MAR-1990   Jeffrey Bantly  general cleanup 
C-   Updated   7-JAN-1992   Jeffrey Bantly  Add GEANT/link area flag set 
C-   Updated  20-AUG-1992   Robert E. Avery  Use latest versions of STP banks. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:ZPULPR.INC/LIST'
      INCLUDE 'D0$INC:ZZEROP.INC/LIST'
      INCLUDE 'D0$LINKS:IZFGEH.LINK'
      INCLUDE 'D0$LINKS:IZFALH.LINK'
      INCLUDE 'D0$LINKS:IZFTMH.LINK'
C
      INTEGER IERR
      CHARACTER*80 VARMSG
      LOGICAL INIFDC
      INTEGER LKFGEH,LKFALH,LKFTMH
      INTEGER LZFIND
      INTEGER VERSION,VERSION_CURRENT
C----------------------------------------------------------------------
      INIFDC = .TRUE.
      IF ( DFDC .LE. 0 ) GOTO 999
C
C ****  Set flag for link area check
C
      CALL FGEAN_SET()
C
C ****  Take the geometry from the file FDC_STPFILE
C
      IERR = 0
      CALL FDISTP ( 'FDC_STPFILE', IERR )
      IF (IERR .NE. 0 ) THEN
        WRITE(VARMSG,10)
   10   FORMAT(' **INIFDC** Error initializing geometry  ')
        CALL ERRMSG('FDC-NO STPF','INIFDC',VARMSG,'F')
      ENDIF
C
C  Select correct version of FDC STP banks. Look for V2.
C
      VERSION = 2
C
      LKFGEH = LC(LSFDC-IZFGEH)   
      IF ( LKFGEH.GT.0 ) THEN
        VERSION_CURRENT = IC(LKFGEH-5)
        IF ( VERSION.NE.VERSION_CURRENT ) THEN
          LKFGEH = LZFIND(IXSTP,LKFGEH,VERSION,-5)
          IF ( LKFGEH.GT.0 ) THEN
            CALL ZSHUNT(IXSTP,LKFGEH ,LSFDC,-IZFGEH,0)
          ENDIF
        ENDIF
      ENDIF
      LFGEH = LC(LSFDC-IZFGEH)   
C
      LKFALH = LC(LSFDC-IZFALH)
      IF ( LKFALH.GT.0 ) THEN
        VERSION_CURRENT = IC(LKFALH-5)
        IF ( VERSION.NE.VERSION_CURRENT ) THEN
          LKFALH = LZFIND(IXSTP,LKFALH,VERSION,-5)
          IF ( LKFALH.GT.0 ) THEN
            CALL ZSHUNT(IXSTP,LKFALH ,LSFDC,-IZFALH,0)
          ENDIF
        ENDIF
      ENDIF
C
      LKFTMH = LC(LSFDC-IZFTMH)
      IF ( LKFTMH.GT.0 ) THEN
        VERSION_CURRENT = IC(LKFTMH-5)
        IF ( VERSION.NE.VERSION_CURRENT ) THEN
          LKFTMH = LZFIND(IXSTP,LKFTMH,VERSION,-5)
          IF ( LKFTMH.GT.0 ) THEN
            CALL ZSHUNT(IXSTP,LKFTMH ,LSFDC,-IZFTMH,0)
          ENDIF
        ENDIF
      ENDIF
      LFTMH = LC(LSFDC-IZFTMH)
C
C
C ****   Parameters for pulse shape simulation: 
C                 (x=0: VTX; x=1: CDC; x=2: FDC)
C 
C PARTR and PARTW are fitted parameters from real data
C
C   Rise time = PARTR(1,x) * SQRT(Tdrift) + PARTR(2,x)  
C               (for uniform drift region: Tdrift > TRLIMT)
C             = PARTR(3,x) * Tdrift ** 2 + PARTR(4,x) * Tdrift + PARTR(5,x)
C               (for the non-uniform drift region: 0 < Tdrift < TRLIMT)
C
C Total width = PARTW(1,x) * SQRT(Tdrift) + PARTW(2,x) 
C               (for uniform drift region: Tdrift > TWLIMT)
C             = PARTW(3,x) * Tdrift ** 2 + PARTW(4,x) * Tdrift + PARTW(5,x)
C               (for the non-uniform drift region: 0 < Tdrift < TWLIMT)
C
      PARTR(1,2) = 0.476
      PARTR(2,2) = 31.9
      PARTR(3,2) = -0.00097
      PARTR(4,2) = 0.45
      PARTR(5,2) = -7.39
C
      PARTW(1,2) = 0.029
      PARTW(2,2) = 11.3
      PARTW(3,2) = -0.000097
      PARTW(4,2) = 0.083
      PARTW(5,2) = -2.2
C
      TRLIMT(2) = 300.0                 ! ns
      TWLIMT(2) = 220.0                 ! ns
      MINRS(2)  = 20.0                  ! minimum rise time (ns)
      MINWID(2) = 10.0                  ! minimun width (FADC channels)
      PULWEI(2) = 1.2                   ! weight for center of gravity
      SCALE(2)  = 3.0                   ! Scale of pulse height
      DLSCAL(2) = 1.2                   ! Delay line / Sense wire (width)
C
C ****  Fill the common ZZEROP
C ****  contains the thresholds for the Zero-suppression in the FADC for
C ****  the 3 chambers VTX, CDC, FDC.  DIFTHn(x) and BINTHn(x)
C                 (x=0: VTX; x=1: CDC; x=2: FDC)
C
      IF( SFDC(5) .EQ. 0. ) THEN
        DIFTH1(2) = 5
        DIFTH2(2) = 5
        DIFTH3(2) = 5
        BINTH1(2) = 25
        BINTH2(2) = 25
        BINTH3(2) = 25
        BINTH4(2) = 25
      ELSE
        DIFTH1(2) = 0
        DIFTH2(2) = 0
        DIFTH3(2) = 0
        BINTH1(2) = 0
        BINTH2(2) = 0
        BINTH3(2) = 0
        BINTH4(2) = 0
      ENDIF
C
C--------------------------------------------------------------------------
  999 RETURN
      END
