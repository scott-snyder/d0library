C DEC/CMS REPLACEMENT HISTORY, Element CAJETS_ADDON.FOR
C *14    2-DEC-1993 10:57:20 MEENA "Richard V. Astur: Move jet_vertex_word filling to CAJETS_ADDON"
C *13   22-NOV-1993 22:00:37 MEENA "Richard V. Astur: Move jet_vertex_word filling to CAJETS_ADDON"
C *12   19-NOV-1993 18:57:20 MEENA "astur : Remove filling of JETS bank word 20"
C *11   14-AUG-1993 22:45:13 MEENA "Norman A. Graf: fixed integer NC and N90"
C *10    6-AUG-1993 17:27:44 MEENA "Norman A. Graf: fixed integer NC and N90"
C *9    17-JUL-1993 16:59:28 MEENA "Norman A. Graf: fixed integer NC and N90"
C *8     6-JUL-1993 18:49:13 MEENA "Norman A. Graf: fixed integer NC and N90"
C *7     8-MAR-1993 11:05:07 NGRAF "fixed dr calculation for subcones"
C *6    11-FEB-1993 23:59:13 MEENA "Norman A. Graf: fixed some bugs"
C *5    20-JAN-1993 14:46:50 NGRAF "corrected some problems in jtcs filling"
C *4    14-JAN-1993 09:48:24 HARRY "Norman A. Graf: added code for JETS version 3"
C *3     6-OCT-1992 13:43:10 NGRAF "Fixed error handling and overwrite bug"
C *2     5-OCT-1992 23:30:42 RAJA "Norman A. Graf: added code for JETS version 3"
C *1    30-SEP-1992 21:06:18 RAJA "Norman A. Graf: added code for JETS version 3"
C DEC/CMS REPLACEMENT HISTORY, Element CAJETS_ADDON.FOR
      SUBROUTINE CAJETS_ADDON
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Adds additional information to the JETS
C-                         banks, which may be useful in eliminating
C-                         hot cells and other "bad" jets from DST.
C-                         Present form is temporary in nature.
C-
C-   Inputs  : None, loops over all jets
C-   Outputs : Adds information to JETS banks
C-   Controls: CAJETS_RCP
C-
C-   Created  28-SEP-1992   Norman A. Graf
C-   Updated  16-JAN-1993   Norman A. Graf Added QCD jet sub cones code
C-                          and calculation of number of CDC tracks in
C-                          jet cone (NC) and number of towers in jet which
C-                          comprise 90% of the jet Et (N90).
C-   Updated   3-JUN-1993   Chip Stewart - Loop over all CAPH, skipping
C-                          CAPHEL and/or already processed JETS banks.
C-
C-   Updated  15-JUL-1993   N. Graf, R. Astur - ensure creation of JTCS and
C-                          calculation of extra JETS variables by looping
C-                          over CAPH bank directly,get rid of SET_CAPH.
C-
C-   Updated  18-JUL-1993   sss - protect against walking off the ends
C-                          of arrays.  remove some unused local vars.
C-
C-   Updated  12-AUG-1993   Norman A. Graf   ETOETA has been fixed,
C-                          remove "fix" for negative energy cells
C-
C-   Update   19-NOV-1993   R. Astur - stop filling word 20 (# of cdc tracks)
C                           and start filling of word 27
C-
C-   Updated  24-NOV-1993   R. Astur - replace ZVERTE call with VERTEX_INFO
C-
C-   Updated  18-SEP-1995   Bob Hirosky/Brad Abbott Ncones =
C-                          Abs(int (SUBCONES_JETRAD*10)) to handle D0
C-                          angle selection/ Calculate number of cells in each
C-                          calorimeter layer
C-
C-   Updated  27-SEP-1995   Dhiman Chakraborty   
C-                          Added the eta-phi covariance word (COVEP*)
C-                          as word 40 in the JETS bank 
C-                          + some general cleanup
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER,II,JJ,KK
      INCLUDE 'D0$LINKS:IZJPTS.LINK'
      INCLUDE 'D0$LINKS:IZJTSH.LINK'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      INTEGER MAX_CONES, MAX_JETS, NCELL_MAX
      PARAMETER (MAX_CONES = 15,
     &           MAX_JETS  = 20,
     &           NCELL_MAX = 2000)
      INTEGER NSTORE
      PARAMETER( NSTORE = 10000 )
      INTEGER LPTRS(NSTORE)
C
      LOGICAL FIRST,QCD_CONES,USE_CELL_E,CALCULATE_N90,BOOK_JTCS
      DATA FIRST/.TRUE./
      INTEGER JET_VERTEX_WORD
      INTEGER NUMCELLS_EM,NUMCELLS_ICD,NUMCELLS_FH,NUMCELLS_CH
      INTEGER NREP_CAEH, NREP_CAEP, CELLPTR_CAEH, CELLPTR_CAEP
      INTEGER NCELL, NUMCELLS
      INTEGER LJETS, LCAEH, GZCAEH, LCAEP, GZCAEP
      INTEGER LPROC, GZPROC
      INTEGER NCONES
      REAL CELL_ETMIN, CELL_ETMAX, CELL_ETNXT, CELL_ET, CELL_E
      REAL COVEP,COVEP_CELL_ETMIN
      REAL ETJET, ETICD, ETCH, ECONE(MAX_CONES, MAX_JETS)
      REAL PHIJT, ETAJT, THETAJT, PHICL, ETACL, THETACL, E(4)
      REAL DPHI, DIFF_PHI, DR, JETRAD, SUBCONES_JETRAD
      REAL JET_QUAN(3, MAX_JETS)
C
      INTEGER IADDR, IETA, IPHI, ILYR
      INTEGER PTRCATE(NCELL_MAX)
      INTEGER LJTCS,NCJ,NDAT,N90
C
      INTEGER LCAPH, ND_NEEDED,ND
      PARAMETER(ND_NEEDED = 26)  ! NEEDS 26 words in JETS bank
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
C----------------------------------------------------------------------
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL EZPICK('CAJETS_RCP')       ! SELECT JETS RCP BANK
        CALL EZGET('CELL_ETMIN',CELL_ETMIN,IER)
        CALL EZGET_l('CALCULATE_N90',CALCULATE_N90,IER)
        CALL EZGET_l('BOOK_JTCS',BOOK_JTCS,IER)
        CALL EZGET('SUBCONES_JETRAD',SUBCONES_JETRAD,IER)
        CALL EZGET_l('SUBCONES_CELLE',USE_CELL_E,IER)
        CALL EZGET('COVEP_CELL_ETMIN',COVEP_CELL_ETMIN,IER)
        CALL EZRSET
        NCONES = ABS(INT(SUBCONES_JETRAD*10))
        IF (NCONES .GT. MAX_CONES) THEN
          CALL ERRMSG ('CAJETS_ADDON', 'subcones_jetrad too large',
     &                 ' Truncated', 'W')
          ncones = MAX_CONES
        ENDIF
      ENDIF
C
      LCAEH = GZCAEH()
      IF (LCAEH .LE. 0) THEN
        CALL ERRMSG('CALORIMETER','CAJETS_ADDON',
     &      ' LCAEH = 0','W')
        GOTO 999
      END IF
      NREP_CAEH = IQ(LCAEH+2)
      LCAEP = GZCAEP()
      IF (LCAEP .LE. 0) THEN
        CALL ERRMSG('CALORIMETER','CAJETS_ADDON',
     &      ' LCAEP = 0','W')
        GOTO 999
      END IF
      NREP_CAEP = IQ(LCAEP+2)
C
C ****  LOOP OVER ALL CAPH BANKS...
C
      II = 1
      LCAPH = 0
      LPROC = GZPROC()
      IF (LPROC .GT. 0) LCAPH = LQ(LPROC-IZCAPH)
      DO WHILE(LCAPH .GT. 0)
        JETRAD = 0.
        IF(IQ(LCAPH+4).EQ. 1) GOTO 86   ! CAPHEL
        IF(IQ(LCAPH+4).EQ. 2) JETRAD = Q(LCAPH+6)
        QCD_CONES = (JETRAD .EQ. SUBCONES_JETRAD)
        CALL UZERO(ECONE, 1, MAX_CONES * MAX_JETS)
C
C ****  LOOP OVER RECO JETS
C
        NCJ = 0
        LJETS = LQ(LCAPH-IZJETS)
        NUMCELLS = IQ(LJETS+16)
        IF(NUMCELLS.GT.0) GOTO 86      !ADDED WORDS ALREADY ADDED
        DO WHILE (LJETS .GT. 0)
          ND = IQ(LJETS-1)
          IF(ND.LT.ND_NEEDED)CALL MZPUSH(IXCOM,LJETS,0,ND_NEEDED-ND,' ')
          CELL_ETMAX = 0.
          CELL_ETNXT = 0.
          ETICD      = 0.
          ETCH       = 0.
          NUMCELLS = 0
          NUMCELLS_EM=0
          NUMCELLS_ICD=0
          NUMCELLS_FH=0
          NUMCELLS_CH=0
          ETJET   = Q(LJETS+6)
          THETAJT = Q(LJETS+7)
          PHIJT   = Q(LJETS+8)
          ETAJT   = Q(LJETS+9)
C
          NCJ = NCJ + 1
          IF(QCD_CONES.AND.BOOK_JTCS .AND. NCJ .LE. MAX_JETS) THEN
            JET_QUAN(1,NCJ) = ETAJT
            JET_QUAN(2,NCJ) = PHIJT
            JET_QUAN(3,NCJ) = ETJET
          ENDIF
C
          CALL GTJPTS(LJETS,LPTRS,NCELL_MAX,NCELL,IER)
          IF (NCELL .GT. NCELL_MAX) THEN
            CALL ERRMSG ('CAJETS_ADDON', 'Too many cells',
     &                   ' Truncated', 'W')
            NCELL = NCELL_MAX
          ENDIF
C
        CALL COVEP_JETFIX_INI(LJETS)  
C
          DO 310 II = 1 , NCELL
C
            CELLPTR_CAEP = LCAEP + NREP_CAEP*(LPTRS(II)-1)
C
            CELLPTR_CAEH = LCAEH + NREP_CAEH*(LPTRS(II)-1)
            CALL COVEP_JETFIX_LOOP(LJETS,CELLPTR_CAEH)
C
            IADDR = IQ(CELLPTR_CAEP+4)         ! Packed Addr in Physics Indices
            CALL CAEP_INDICES(IADDR,IETA,IPHI,ILYR)
            PTRCATE(II) = PTCATE(IETA,IPHI,2)
C
            CELL_ET = Q(CELLPTR_CAEH+8)
            CELL_E  = Q(CELLPTR_CAEH+7)
C
C ****  CALCULATE ENERGY FLOW WITHIN JET FOR JETS WITH R=1.0
C
            IF(QCD_CONES .AND. BOOK_JTCS .AND. NCJ .LE. MAX_JETS) THEN
C
C ****  FIX FOR ETOETA (NEGATIVE ENERGIES GIVE WRONG LOCATION)
C
              E(4) = Q(CELLPTR_CAEH+7)
              DO 20 KK = 1 ,3
                E(KK) = Q(CELLPTR_CAEH+3+KK)
   20         CONTINUE
              CALL ETOETA(E,PHICL,THETACL,ETACL)
              DPHI = DIFF_PHI(PHIJT,PHICL)
              DR =  SQRT(DPHI**2 + (ETAJT - ETACL)**2)
              DO JJ=1,NCONES
                IF (DR.GT.FLOAT(JJ-1)*.1 .AND.
     +            DR.LE.FLOAT(JJ)*.1) THEN
                  IF(USE_CELL_E) THEN
                    ECONE(JJ,NCJ)=ECONE(JJ,NCJ)+CELL_E
                  ELSE
                    ECONE(JJ,NCJ)=ECONE(JJ,NCJ)+CELL_ET
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
C
            IF(IQ(CELLPTR_CAEH+14) .GE. MNLYEM .AND.
     &        IQ(CELLPTR_CAEH+14).LE. MXLYEM)
     &        NUMCELLS_EM=NUMCELLS_EM+1
            IF(IQ(CELLPTR_CAEH+14) .GE. MNLYMG .AND.
     &        IQ(CELLPTR_CAEH+14) .LE. MXLYMG)
     &        THEN
              ETICD = ETICD + CELL_ET
              NUMCELLS_ICD=NUMCELLS_ICD+1
            ENDIF
            IF(IQ(CELLPTR_CAEH+14) .GE. MNLYFH .AND.
     &        IQ(CELLPTR_CAEH+14) .LE. MXLYFH)
     &        NUMCELLS_FH=NUMCELLS_FH+1
            IF(IQ(CELLPTR_CAEH+14) .GE. MNLYCH .AND.
     &        IQ(CELLPTR_CAEH+14) .LE. MXLYCH)
     &        THEN
              ETCH = ETCH + CELL_ET
              NUMCELLS_CH=NUMCELLS_CH+1
            ENDIF
C
            IF(CELL_ET .GE. CELL_ETMAX) THEN
              CELL_ETNXT = CELL_ETMAX
              CELL_ETMAX = CELL_ET
            ELSEIF (CELL_ET .GE. CELL_ETNXT) THEN
              CELL_ETNXT = CELL_ET
            ENDIF
            IF(CELL_ET .GE. CELL_ETMIN) NUMCELLS = NUMCELLS + 1
  310     CONTINUE
C
          CALL COVEP_JETFIX_FIN(LJETS,COVEP)
          Q(LJETS+40) = COVEP
C
C ****  NOW GET N90
C
          IF(CALCULATE_N90) CALL CJETN90(NCELL,ETJET,PTRCATE,N90)
C
C ****  FIND NUMBER OF CDC TRACKS IN CONE...
C
C          IF(JETRAD .NE. 0.) THEN
C            DELTA  = JETRAD
C            PHIMIN = PHIJT   - DELTA
C            PHIMAX = PHIJT   + DELTA
C            DTHETA = 2.*(ATAN(EXP(-ETAJT+DELTA))-
C     &                 ATAN(EXP(-ETAJT-DELTA)))
C            DTHETA = ABS(DTHETA)
C            THEMIN = THETAJT - DTHETA
C            THEMAX = THETAJT + DTHETA
C            CALL NCROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NC,IDC)
C          ENDIF
C
C ****  Now fill in words in JETS bank
C
          IQ(LJETS+16) = NUMCELLS
          Q(LJETS+17)  = ETICD/ETJET
          Q(LJETS+18)  = ETCH/ETJET
          IF(CELL_ETNXT .GT. 0.) THEN
            Q(LJETS+19)  = CELL_ETMAX/CELL_ETNXT
          ELSE
            Q(LJETS+19)  = 9999.
          ENDIF
C          IQ(LJETS+20)  = NC
          IQ(LJETS+21)  = N90

          IQ(LJETS+42)=NUMCELLS_EM
          IQ(LJETS+43)=NUMCELLS_CH
          IQ(LJETS+44)=NUMCELLS_FH
          IQ(LJETS+45)=NUMCELLS_ICD
C
C ****  JET VERTEX MATCH WORD
C
          IF (IQ(LJETS + 1) .GE. 5) THEN
            IQ(LJETS+27) = JET_VERTEX_WORD(LJETS)
          ELSE
            CALL ERRMSG('Old JETS bank','CAJETS_ADDON',
     &        'Pre version 5 of JETS bank is used','W')
          ENDIF
C
          LJETS = LQ(LJETS)
        ENDDO
C
        IF (NCJ .GT. MAX_JETS) THEN
          CALL ERRMSG ('CAJETS_ADDON', 'Too many jets',
     &                 ' Truncated', 'W')
          NCJ = MAX_JETS
        ENDIF
C
C ****  If QCD subcones, book and fill JTCS (Jet Cone Size) bank
C
        IF(QCD_CONES .AND. BOOK_JTCS) THEN
          NDAT = NCJ*(3+NCONES) + 3
          CALL BKJTCS(LCAPH,NDAT,LJTCS)
          IQ(LJTCS+2) = 3+NCONES
          IQ(LJTCS+3) = NCJ
          CELLPTR_CAEP = 4
          DO II = 1,NCJ
            CALL UCOPY(JET_QUAN(1,II),Q(LJTCS+CELLPTR_CAEP),3)
            CALL UCOPY(ECONE(1,II),Q(LJTCS+CELLPTR_CAEP+3),NCONES)
            CELLPTR_CAEP = CELLPTR_CAEP + 3+NCONES
          ENDDO
        ENDIF
C
   86   LCAPH = LQ(LCAPH)
      ENDDO
  998 CONTINUE   !CALL RESET_CAPH
  999 RETURN
      END
