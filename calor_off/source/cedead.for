      SUBROUTINE CEDEAD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Accumulates Dead material Energies from
C-                         Geant CAEP bank
C-
C-   Inputs  :
C-   Outputs : DEADM.INC
C-   Controls:
C-
C-   Created  24-MAY-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:DEAD_MATERIALS.PARAMS'
      INCLUDE 'D0$INC:DEADM.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'     ! Main D0 Data Store
      INTEGER LCAEP,GZCAEP            ! Zebra links and routines
      INTEGER NDATA,NCELL             ! Data lengths
      INTEGER IADDR                   ! Packed Cell Address
      INTEGER IETA,IPHI,ILYR,ITAG     ! Unpacked Cess Address
      INTEGER I,NREP,POINTER,JBYT,JBIT
      INTEGER IENFL
C----------------------------------------------------------------------
      CALL PATHST('GEAN')               ! Set path to Gean
      LCAEP = GZCAEP()                ! Get link to CAEP bank
      LCAEP = LQ(LCAEP)               ! Get second CAEP bank
      IF ( LCAEP.LE.0 ) GOTO 999      ! Error - No data
      DO 50 I = DEADLO , DEADHI
        EDEADM(I) = 0.                  ! ZEROING IT.
   50 CONTINUE
      EDEADT = 0.                       ! total dead material energy
      NDEADM = 0                        ! Number of dead material layers
      NDATA = IQ(LCAEP-1)             ! Number of data words
      NCELL = IQ(LCAEP+3)             ! Number of cells with data
      NREP = IQ(LCAEP+2)              ! Repetition number
      DO 100 I = 1, NCELL             ! Loop over cells
        POINTER = NREP*(I-1)+LCAEP    ! Pointer
        IADDR = IQ(POINTER+4)         ! Packed Addr in Physics Indices
        IETA = JBYT(IADDR,25,8)
        IF(IETA.GE.128)IETA = IETA -256 ! 2'S COMPLEMENT
        IPHI = JBYT(IADDR,17,8)
        ILYR = JBYT(IADDR,9,8)
        IENFL = JBIT(IADDR,6)           !Flag to see if Energy in Gev.
        IF(IENFL.NE.0)THEN
          CALL ERRMSG('CALORIMETER','CDEAD',
     &      'ENERGY NOT IN GEV ','W')
        ENDIF
        IF(ILYR.GE.DEADLO.AND.ILYR.LE.DEADHI)THEN
          EDEADM(ILYR) = EDEADM(ILYR)+Q(POINTER+5)        ! ENERGY
          NDEADM = NDEADM + 1
          EDEADT = EDEADT + Q(POINTER+5)
        ENDIF
  100 CONTINUE
  999 CONTINUE
      CALL PATHRS                       ! Reset to default path
      RETURN
      END
