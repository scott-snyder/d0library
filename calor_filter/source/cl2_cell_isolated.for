      LOGICAL FUNCTION CL2_CELL_ISOLATED (IETA, IPHI, ILYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given the address (ieta, iphi, ilyr) of a hit in the
C-   calorimeter, determine if it is a hot cell.
C-
C-   N.B. -- this routine should only be called for those cells which are
C-   already hot cell candidate, that is, those cell that pass the ET cut.
C-
C-   Inputs  : IETA, IPHI, ILYR [I] the address of the hit in question
C-   Outputs : none
C-   Controls: none
C-
C-   Created  28-SEP-1992   Marc Paterno
C-   Modified  8_OCT-1992   Amber Boehnlein, skip FH1
c-   Modified 22-march-1994 Amber Boehnlein use FH1
c-   Modified  6-JUL-1994   Amber Boehnlein, set FH1 cut to 30 gev
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INTEGER  IETA, IPHI, ILYR
C----------------------------------------------------------------------
      REAL     RATIO
      REAL     ETUP, ETDN, EHIT, R
      INTEGER  N, LCAEP, GZCAEP, NREP, PTR
      INTEGER  FH1
      LOGICAL  OK
      DATA RATIO/.05/
      DATA FH1/11/
C----------------------------------------------------------------------
C
C ****  Default is to NOT call the cell hot
C
      CL2_CELL_ISOLATED = .FALSE.

C ****  Find the CAEP
C
      LCAEP = GZCAEP()
      IF ( LCAEP .LE. 0 ) GOTO 999      ! no bank found
      NREP = IQ(LCAEP + 2)
C
C ****  Get the energy of the hit
C
      PTR = PTCAEP2 (ILYR, IPHI, IETA)
      IF ( PTR .LE. 0 ) GOTO 999        ! no hit found

      PTR = (PTR-1)*NREP + LCAEP + 5
      EHIT = Q(PTR)
      
C **** handle FH1 differently
      IF(ILYR.EQ.FH1 .AND. EHIT.LT.30.0) GOTO 999
C
C ****  Get the ET of the cell above and below the candidate.  If the call is
C ****  not successful, then DO NOT flag the cell as hot.
C
      CALL CL2_NEIGHBOR_HITS (IETA, IPHI, ILYR, ETUP, ETDN, N, OK)
      IF ( .NOT. OK ) GOTO 999
C
C ****  Compare the average neighbor hit to the cut ratio, and if it is SMALLER
C ****  flag the cell as hot.
C
      IF ( N .EQ. 0 ) THEN              ! no neighbors exist; don't cut
        CL2_CELL_ISOLATED = .FALSE.
        GOTO 999
      ELSE                              ! neighbors were found; determine ratio
        R = (ETUP + ETDN) / (N * EHIT)
      ENDIF

      IF ( R .LE. RATIO ) THEN
        CL2_CELL_ISOLATED = .TRUE.
      ENDIF                           ! if r .le. ratio


  999 CONTINUE
      RETURN
      END
