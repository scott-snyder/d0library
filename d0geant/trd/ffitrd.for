      LOGICAL FUNCTION FFITRD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TRD FFREAD cards defined here
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created   8-JUL-1987   A.M.Jonckheere
C-   Updated  17-JUL-1989   Harrison B. Prosper
C-   Made into a pbd interface function; moved TRD code from FFICEN
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER I
      LOGICAL PRT_FFITRD
C----------------------------------------------------------------------
      FFITRD = .TRUE.
      DTRD = DCEN
      PTRD = 1
C
      IF ( DTRD .LE. 0 ) GOTO 999
C
      DO I = 1, 10
        SCEN(I) = 0.
      ENDDO
C
      CALL FFKEY_i1('DTRD',DTRD,1,'INTEGER')
      CALL FFKEY_i1('PTRD',PTRD,1,'INTEGER')
C
      CALL VZERO(STRD,10)
      STRD(6)=10. !Choose Garibian' parametrization  with d(gap)/gap=1.
C
      CALL FFKEY('STRD',STRD,10,'REAL')
C
C ****  Initialize the working flags
C ****                   STRD(1) = 0  (default): read from XRSPECT.DAT
C ****                           = 1 recreate TRD cluster tables XRSPECT.DAT
C ****                           = 2 No clusters - only total energy
C ****                                       Read DELDIST.DAT and XRAYDIST.DAT
C ****                   STRD(2) = 0 (default) cathode + anode : the banks kept
C ****                   in the output is given by STRD(3)
C ****                           = 1 No treatment of the cathodes
C ****                   STRD(3) = 0 Digitization ( CDD4 ) + Geant specific
C ****                               banks GTLY
C ****                             1 CDD4 only
C ****                   STRD(4) = 0 No  Hits written ( TRDH )
C ****                           STRD(3) = Data         ( DCDA )
C ****                   STRD(5) = Acts on FADC trailer word 3
C****                    STRD(6) =0  Generation of X ray with regular radiator
C****                            >0      "            "   "    Garibian method:
C****                      d Xgap=10*STRD(6)*X gap  d Xfoil=.1 X foil
C****                      Example:if STRD(6)=5, d Xgap =.5*X gap
C ****                   STRD(7) =0  No histograms for TRD
C ****                           =1  Fill and print histo. for TRD
C
      ENTRY PRT_FFITRD
      PRT_FFITRD = .TRUE.
C
      WRITE (LOUT,9000) DTRD,PTRD,STRD
 9000 FORMAT(
     & ' FFITRD ** DTRD ',I3,' PTRD ',I3/
     & '           STRD ',10F6.1)
C
  999 RETURN
      END
