      FUNCTION TEVCAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called before each event by ====> GUTREV
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  23-JUL-1987   A.M.Jonckheere
C-   Updated   6-JUL-1989   Rajendran Raja  Put in Timer can GOSEAR calls.
C-   Updated  14-JUL-1989   Harrison B. Prosper  
C-   Made into logical PBD function 
C-   Updated  13-JAN-1992   K. Wyatt Merritt  Shift zeroing of calorimeter
C-                          energy totals here from HCFILL  
C-   Updated   ?-???-1992   C. Stewart Skipped zeroing of cal totals for
C-                          SHWG>2
C-   Updated   1-JUL-1992   K. Wyatt Merritt  Change Chip's test to RAJA_SHLB
C-                          to allow use of SHWG values > 3 (tree-pruning,
C-                          selective Linn parametrization, etc. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:DCALOG.INC'
      INCLUDE 'D0$INC:HCAL.INC'
C
      LOGICAL TEVCAL
C----------------------------------------------------------------------
      TEVCAL = .TRUE.
      IF ( DCAL .LT. 1 ) GO TO 999
C
C ****  Zero energy totals for the event.  They are accumulated in CALBLD
C ****   (or TBMBLD for test beam).
C
C UNSMEARED TOTALS
      TOTECA = 0.
      TOTUCA = 0.
      TOTCRK = 0.
      TOTMSG = 0.
      TOTSCN = 0.
      TOTMRG = 0.
C SMEARED TOTALS
      TOTUCS = 0.
      TOTECS = 0.
      TOTMSS = 0.
C
C ****  Init Ideal Hits
      IF ( (SCAL(1).EQ.1.) .AND. (SCAL(4).NE.0.) ) THEN
        CALL JETINI
      ENDIF
C
  999 RETURN
      END
