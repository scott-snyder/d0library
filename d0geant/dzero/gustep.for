      SUBROUTINE GUSTEP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine to process the tracking  information
C-                         after each step..
C-                         It performs the following tasks:
C-                         - Call Detector Routines which store the hits
C-                         - Call STPD0 which stores tracks created in the
C-                          CD in JKINE, for later transfer to ISPx,ISVx
C-                         - Stores secondary tracks, unless preempted by
C-                          GOKING (Linn parametrization), pruning them
C-                          if requested (TREE_PRUNE)
C-                         - Draw or print trajectory if IDEBUG .NE. 0
C-
C-   Created   ??
C-   Updated   1-OCT-1987   G Rahal-Callot - save secondaries in JKINE
C-   Updated   8-OCT-1987   A.M.Jonckheere - get ready to save in /ZEBCOM/
C-   Updated  28-NOV-1988   Alan M. Jonckheere  add 6th and 7th UBUF parameters
C-                               - Parent Parton and Jet numbers
C-   Updated  15-DEC-1988   A.M.Jonckheere  Add store JXYZ option, set
C-                              SD0(1).NE.0 to store
C-   Updated   1-MAR-1989   Chip Stewart  ADDED LV0
C-   Updated   2-JUL-1989   Rajendran Raja  DTRK=3 added
C-   Updated  14-JUL-1989   Harrison B. Prosper
C-   Added hooks LUSTEP and L0STEP for PBD
C-   Updated   8-AUG-1991   K. Wyatt Merritt   Added option to use N. Amos'
C-                          tree-pruning algorithm to store only a fraction
C-                          of the secondaries generated. 
C-   Updated  10-DEC-1991   K. Wyatt Merritt   Call STPD0 directly and
C-                          eliminate the PBD hook L0STEP (new program
C-                          builder will not include this hook)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:CALTRK.INC'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:DCALOG.INC/LIST'
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC'
      INCLUDE 'D0$INC:GCKING.INC'
      INCLUDE 'D0$INC:GCTRAK.INC'
C
      INTEGER IPNT,ISKP
      INTEGER ITYPA,JENER,I
C
      REAL UPWSTO,PARTMASS,PARTKIN,WGHT
C
      DATA IPNT/0/,ISKP/4/
C----------------------------------------------------------------------
C
C **** This section for histogramming or accumulating GEANT
C **** quantities - use GEANT variable ISWIT(5) to turn it on.
C
      IF (ISWIT(5) .NE. 0) CALL PRINT_STEP  
C
      IF (TREE_PRUNE) THEN
C
C    Rescale the energy by the particle's weight!! This should make the
C    pruned tree approximate the real tree in energy deposition.
C
        DESTEP = DESTEP*UPWGHT
C
      ENDIF
C
C ************************
C ****  USER HOOK LUSTEP
C ************************
      CALL LUSTEP                       ! Calls STPxxx
C
C       Handle tracks created in MCEN separately (if not testbeam)
C       for tracks with p > SSEC
C
      IF (SCAL(10).EQ.0. .AND. SSEC.GT.0. .AND. ISTAK.EQ.0) THEN
        CALL STORE_MCEN
      ENDIF
C
C ****  Prune the secondary tree if requested, before storing, and
C       reset DESTEP to be the WEIGHTED energy for use in the detector
C       routines
C
      IF (TREE_PRUNE) CALL PRUNE_GSKINE
C
C       Store any particles left on the JSTAK stack - the selection algorithms
C       in PRUNE_GSKINE or in GOKING will preempt this storage if the
C       variables TREE_PRUNE or LINN_PARAM are .TRUE. 
C
      IF (RAJA_SHLB .AND. IPART.LE.3) GO TO 10 ! Do not store secondaries on
                                               ! stack for e/gamma's traced
                                               ! through when SHWG=3
      IF (NGKINE .GT. 0) CALL GSKING(0)
   10 CONTINUE
C
C ****  Drawing or print section
C
      IF (IDEBUG .NE. 0) THEN
        IF (DTRK.EQ.1.OR.DTRK.EQ.3) CALL GUPXYZ
C&IF VAXVMS
        IF ( (DTRK.EQ.2.OR.DTRK.EQ.3) .AND. (CHARGE.NE.0.) ) THEN
          IF ( (SD0(1).EQ.0.) ) THEN
            CALL GDCXYZ
          ELSE
            IF ( (MOD(IPNT,ISKP).EQ.1) ) CALL GSXYZ
          ENDIF
        ENDIF
C&ENDIF
C&IF SIUNIX,IBMAIX,ULTRIX,SUNOS,ALFOSF
C&        IF (DTRK.EQ.2.OR.DTRK.EQ.3) THEN
C&          IF ( (SD0(1).EQ.0.) .AND. (CHARGE.NE.0) )  CALL GDCXYZ
C&          IF ( (SD0(1).EQ.1.) .AND. (CHARGE.NE.0) )  CALL GSXYZ
C&          IF (  SD0(1).EQ.2.)  CALL GSXYZ
C&        ENDIF
C&ENDIF
      ENDIF
C
  999 RETURN
      END
