      FUNCTION DIGCAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Digitizes the Calorimeters
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  30-APR-1987   A.M.Jonckheere
C-   Updated  14-SEP-1988   Rajendran Raja
C-   Updated  18-OCT-1988   John Womersley  for Shower Library
C-   Updated   7-APR-1989   Chip Stewart  ZERO SUPPRESION THRESHOLD = SCAL(5)
C-   Updated  28-JUL-1989   Harrison B. Prosper - Make into PBD function
C-   Updated   8-AUG-1989   Alan M. Jonckheere  Add 2d CAEP bank
C-                                              (dead material)
C-   Updated   9-AUG-1989   Alan M. Jonckheere  Change to LOGICAL FUNCTION
C-                              AGAIN, 3rd time is a charm?
C-   Updated   9-AUG-1989   Alan M. Jonckheere  Add finish of ICD
C-   Updated  10-FEB-1992   K. Wyatt Merritt  Delete call to HCFILL
C-                           which zeroed HCAL variables and made histograms:
C-                           the variables are now zeroed in TEVCAL, and
C-                           the user should provide histograms in a separate
C-                           package
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:DCALOG.INC'
      INCLUDE 'D0$INC:HCAL.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      LOGICAL DIGCAL
C
      INTEGER LCAEP,GZCAEP
      INTEGER NDCAEP,NCCAEP
      INTEGER LICDH,GZICDH
C
C----------------------------------------------------------------------
C
      DIGCAL = .TRUE.
      IF ( DCAL.LT.3 ) GOTO 999
C
      IF ( .NOT. CALTOW ) THEN
        IF ( DUCA.GE.3 ) CALL DIGUCA
        IF ( DECA.GE.3 ) CALL DIGECA
        IF ( DUCA.GE.3 .OR. DECA.GE.3 ) CALL DIGSCN
        IF ( DEAD.GE.3 ) CALL DIGDED
C
      ELSE IF ( CALTOW ) THEN
C
C ****  Already digitized. Done as each primary track is finished.
C
C?  not implemented yet      IF ( SHWG.GE.2 ) CALL CALBLD(0)
C?                                       ! Call CALBLD for sum of all
C?                                       ! tracks for Shower Library
C
        IF ( PCAL.GE.1 .AND. DHIT.EQ.1 ) THEN
          WRITE (LOUT,8000)TOTUCA,TOTECA,TOTMSG,TOTCRK,
     &        TOTUCS,TOTECS,TOTMRG,TOTSCN
 8000     FORMAT(/' CALORIMETER ENERGY TOTALS:'/
     &        ' CENTRAL: ',F9.4,' END CAP: ',F9.4,
     &        ' MASSLESS GAPS: ',F9.4,' CRACK: ',F9.4/
     &        ' SMEARED: ',F9.4,10X,F9.4,' MR CHANNELS   :',F9.4/
     &        ' LIGHT IN SCINTILLATOR: ',F9.4)
        ENDIF
C
C ****  Finish up ideal hits
        IF ( SCAL(4).NE.0 ) THEN
          CALL JETEND
        ENDIF
C
C ****  Create CADn raw data banks
        IF ( SCAL(5).GE.0 ) THEN
          CALL MKCAD(SCAL(5))       ! SCAL(5)= ZERO SUPPRESSION ENERGY THRESH
        ENDIF
C
C ****  Finish up ICD hits
        IF ( SCAL(6).GT.0 ) THEN
          CALL ENICDH
        ELSE
          LICDH = GZICDH()
          IF ( LICDH.GT.0 ) CALL MZDROP(IXCOM,LICDH,'L')
        ENDIF
C
C ****  Push CAEP bank to minimum size
        LCAEP = GZCAEP()
  100   IF ( LCAEP.GT.0 ) THEN
          IF ( SCAL(3).NE.0 ) THEN
            NDCAEP = IQ(LCAEP-1)            ! Number of data words
            NCCAEP = IQ(LCAEP+3)            ! Number of channels used
            CALL MZPUSH(IXCOM,LCAEP,0,2*NCCAEP+3-NDCAEP,'I')
            LCAEP = LQ(LCAEP)
            GOTO 100
          ELSE
            CALL MZDROP(IXCOM,LCAEP,'L')
          ENDIF
          IF ( (PCAL.GT.1) .AND.
     &          (DHIT.EQ.1) .AND.
     &             (IDEBUG.NE.0) ) THEN
            CALL PRCAEP(LOUT,0,0,'ALL',1)    ! PRINT DATA
          ENDIF
        ENDIF
        IF ( (SCAL(3).NE.0) .AND.
     &            (PCAL.GT.1) .AND.
     &                 (DHIT.EQ.1) .AND.
     &                      (IDEBUG.NE.0) ) THEN
          CALL PRGCAH(LOUT,0,0,'ALL',0)    ! PRINT DATA
        ENDIF
C
C
      ENDIF   ! SCAL(1)
C
  999 END
