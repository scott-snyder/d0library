      LOGICAL FUNCTION INICAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in SRCP geometry and set-defintion
C-                         ZEBRA banks for Calorimeter and Cryostat.
C-                         Also set up CC and EC GEANT rotation matrices
C-                         conditional on the value of the flags DUCA
C-                         and DECA. The flags are kept in D0LOG.INC.
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  23-JUL-1987   A.M.Jonckheere
C-   Updated  18-OCT-1988   Rajendran Raja
C-   Updated  22-NOV-1988   Harrison B. Prosper
C-                          Set up new SRCP technology
C-   Updated  29-Nov-1988   Elliott A. Treadwell, Harrison B. Prosper
C-                          Set up the non specific geometry bank (REST)
C-   Updated  20-DEC-1988   Harrison B. Prosper
C-                          Changed calling sequence slightly
C-   Updated   4-JAN-1989   A.M.Jonckheere - Added INIRST call
C-   Updated  16-JAN-1989   Harrison B. Prosper
C-                          Use CAISTP to read in Cal geometry banks
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-   Updated  27-JUL-1989   John Womersley  shower library initialization 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:GCLIST.INC/LIST'
      INCLUDE 'D0$INC:CALTRK.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
C
      INTEGER INIT,IUCOMP
      INTEGER IZ,LBANK
C
      INTEGER IERR
      DATA INIT/4hINIT/
C----------------------------------------------------------------------
      INICAL = .TRUE.
      IF ( DCAL .LE. 0 ) GOTO 999
C
C ****  Read in geometry banks for the Calorimeter into store /ZEBSTP/
C
      CALL CAISTP ('CAL_STPFILE',IERR)
      IF ( IERR.NE.0 ) THEN
        CALL ERRMSG
     &    ('D0GEANT','INICAL','Error opening geometry file','F')
      ENDIF
C
C ****  Check if Calorimeter setup is required
C
      IF( IUCOMP(INIT,LGET,NGET).EQ.0 ) THEN
C
C ****  No GET 'INIT'; geometry banks are to be re-built
C
C ****  CENTRAL
C
        IF (DUCA.GE.1) THEN
          CALL EZPICK ('SRCP_UCAL')
          CALL SETROT ('CC_ROT_MATRICES')    !Setup the CC rotation matrices
        ENDIF
C
C ****  ENDCAPS
C
        IF (DECA.GE.1) THEN
          CALL EZPICK ('SRCP_ECAL')
          CALL SETROT ('EC_ROT_MATRICES')    !Setup the EC rotation matrices
        ENDIF
C
      ENDIF
C
C&IF ETA10
C&C each of the mother volumes below contains identical shapes
C&C hence they are vectorizable
C&      CALL GSVECT(NUCEMM)       !CC(UC) ElectroMagnetic mother volume
C&      CALL GSVECT(NUCFHM)       !CC(UC) Fine Hadronic mother volume
C&      CALL GSVECT(NUCCHM)       !CC(UC) Coarse Hadronic mother volume
C&C
C&      DO 10 IZ=1,2              !2 Z copies
C&        CALL GSVECT(NECEMM(IZ))   !EC Electro Magnetic mother volume
C&        CALL GSVECT(NECIPM(IZ))   !EC Insert Plug mother volume
C&        CALL GSVECT(NECMHM(IZ))   !EC Middle Hadronic mother volume
C&        CALL GSVECT(NECOHM(IZ))   !EC Outer Hadronic mother volume
C&   10 CONTINUE
C&C
C&ENDIF
C
C ****  Shower Library Initialization
C
      IF(SHWG.EQ.3) CALL SHLINI
C ****  initialize calorimeter primary track flag
      CAL_PRIMARY = 0
C
  999 RETURN
      END
