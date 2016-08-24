      SUBROUTINE MULIB_MAKE_ANALYZE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ANALYSE AS WE MAKE THE MUON HIT LIBRARY
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-MAY-1993   Jasbir Singh
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MULCON.INC'
      INCLUDE 'D0$INC:MULDAT.INC'
      INTEGER CYC,CYC_MAX,IETAM,IPHIM
      REAL    RNDM,MIN_RESIDUAL,RESIDUAL,XYZ_ISP1(3),VTX(3),PP
      REAL    PLIB(4)
      REAL DIFF_PHI
C
      INTEGER RKEY(NKEY)
C
      INTEGER I,J,K,N,ID
      INTEGER NVTXG,JQ
      INTEGER PRD_RKEY,INDX_RKEY
      INTEGER II,IJ
      LOGICAL FIRST,OK
      CHARACTER*40 A
      REAL    DIR(3)
      INTEGER NUM_gmuh,NUM_HITS,PT_gmuh,NUMHITS
      INTEGER GEAN_ID
      INTEGER NTUPLE_SIZE,NSIZE2,NSIZE1
      PARAMETER(NTUPLE_SIZE=13)
      PARAMETER(NSIZE2=6)
      PARAMETER(NSIZE1=1)
      REAL XTUPLE(NTUPLE_SIZE),XTUPLE1(NSIZE1)
      CHARACTER MSG*80,LABELS(NTUPLE_SIZE)*5,LABEL2(NSIZE2)*5
      CHARACTER LABEL1(NSIZE1)*5
      DATA LABELS /'IDI','PI','ETAP','PHIP','IETA','IPHI','NGMUH'
     &  ,'PP','NH','IDG','KPHI','KETA','KP'/
      DATA LABEL2 /'KPHI','KETA','KP','PHI','ETA','P'/
      DATA LABEL1 /'NGMUH'/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C BOOK HISTOGRAMS HERE
C
        CALL EZPICK('MUONLIBRARY_RCP')
        CALL DO_HBOOK('ANALYZE_HISTOGRAMS')
        CALL HBOOKN(901,'PNCH MULIB_HAD$',NTUPLE_SIZE,' ',1024,LABELS)
        CALL HBOOKN(902,'pnch MULIB_EM$',NTUPLE_SIZE,' ',1024,LABELS)
        CALL HBOOKN(903,'pnch MULIB_MUON$',NTUPLE_SIZE,' ',1024,LABELS)
        CALL HBOOKN(801,'UNBIAS MULIB_HAD$',NSIZE2,' ',1024,LABEL2)
        CALL HBOOKN(802,'unb MULIB_EM$',NSIZE2,' ',1024,LABEL2)
        CALL HBOOKN(803,'unb MULIB_MUON$',NSIZE2,' ',1024,LABEL2)
        CALL EZRSET
C
      ENDIF
      CALL UCOPY ( KEY,RKEY,NKEY)
      PT_GMUH = MULIB(2)                 ! POINTER TO 1ST GMUH AREA IN SHLB
      XTUPLE(1) = RKEY(1)      ! PHI RKEY
      XTUPLE(2) = RKEY(2)      ! ETA RKEY
      XTUPLE(3) = RKEY(3)      ! MOMENTUM RKEY
      XTUPLE(4) = PHIM_PRIMARY
      XTUPLE(5) = ETAM_PRIMARY
      XTUPLE(6) = P4_PRIMARY(4)
      ID = 800+RKEY(4)
      CALL HFN(ID,XTUPLE)
      IF(PT_GMUH.EQ.0) GO TO 999
      CALL VZERO(XTUPLE(1),12)
C
C ****  SKIP NEXT IF NO HITS
C
      NUM_GMUH = 0
 1000 CONTINUE
      NUM_GMUH = NUM_GMUH + 1
      NUM_HITS=MULIB(PT_GMUH+6)
      IF(NUM_HITS.EQ.0) GOTO 990
      IF(NUM_GMUH.EQ.1) THEN
        CALL UCOPY(MULIB(13),VTX(1),3)
        CALL UCOPY(MULIB(6),PLIB(1),4)
        PP = SQRT(PLIB(1)*PLIB(1)+PLIB(2)*PLIB(2)+PLIB(3)*PLIB(3))
        DIR(1) = PLIB(1)/PP
        DIR(2) = PLIB(2)/PP
        DIR(3) = PLIB(3)/PP
        CALL MUON_CLINPH_FAST(VTX,DIR,ESTO,PSTO,IETAM,IPHIM,OK)
        XTUPLE(1) = MULIB(5)      ! ISAJET ID
        XTUPLE(2) = MULIB(9)      ! ISAJET MOMENTUM
        XTUPLE(3) = ESTO          !  ETA
        XTUPLE(4) = PSTO          !  PHI
        XTUPLE(5) = MULIB(10)          ! IETA KEY SHOULD BE SAME AS IETAM
        XTUPLE(6) = MULIB(11)          ! IPHI KEY SHOULD BE SAME AS IPHIM
        XTUPLE(11) = RKEY(1)      ! PHI RKEY
        XTUPLE(12) = RKEY(2)      ! ETA RKEY
        XTUPLE(13) = RKEY(3)      ! MOMENTUM RKEY
      END IF
      XTUPLE(7) = NUM_GMUH
      XTUPLE(8) = XTUPLE(8)+ MULIB(PT_GMUH+4)      ! PUNCH MOMENTUM
      XTUPLE(9) = XTUPLE(9)+ MULIB(PT_GMUH+6)     ! NUMBER OF MOD-HITS
      XTUPLE(10) = MULIB(PT_GMUH+5)      ! GEANT ID
 990  CONTINUE
      PT_GMUH = MULIB(PT_GMUH)
      NUM_HITS=MULIB(PT_GMUH+6)
      IF(PT_GMUH.NE.0)GO TO 1000        ! MORE GMUH STUFF
      ID = 900+RKEY(4)
      CALL HFN(ID,XTUPLE)
  999 RETURN
      END