      SUBROUTINE MU_SUPERVISOR(L1_ANDOR_STATES, L15_TERM_STATES,
     &  DELTA_WORDS, TRGR_MUON_DATA,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the muon trigger information for
C-                         both level 1 and level 1.5 to be passed to
C-                         the trigger framework
C-
C-   Created                        18-NOV-1991   Kamel A. Bazizi
C-   Updated to 7 Trigger Regions   08-OCT-1992   Kamel A. Bazizi
C-   Updated   3-DEC-1992   Guilherme Lima  Add call to MU_HITS_UNPACK
C-   Moved the call to MU_OTC_MGR from MU_TRIG_CRATE to this routine
C-                                  1-27-1993  Kamel Bazizi
C-   Decouple l1 and l1.5           jan-94     Jussara
C-
C-  The 7 trigger regions TRIG_REG :
C-                    1 = Central                    (CF)
C-                    2 = End WAMUS North            (WN)
C-                    3 = End WAMUS South            (WS)
C-                    4 = SAMUS-WAMUS Overlap North  (ON)
C-                    5 = SAMUS-WAMUS Overlap South  (OS)
C-                    6 = SAMUS North                (SN)
C-                    7 = SAMUS South                (SS)
C-
C-   Inputs  : None
C-   Outputs : L1_ANDOR_STATES
C-             L15_TERM_STATES
C-             DELTA_WORDS
C-             TRGR_MUON_DATA
C-             IERR (=0 OK;=1 FATAL; >1 WARNINGS)
C-
C-   Controls: None
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
C..   Unpacking
      INTEGER TRIG_LEVEL,DET_REGION,SKIP_LEVEL
      INTEGER IERR,IERR2
      DATA TRIG_LEVEL,DET_REGION,SKIP_LEVEL /0,4,7/
C..   Truncation falgs
      LOGICAL CENT_TRUNC,TOO_MANY_HITS,MAC_SKIP
C..   L1 supervisor variables
      LOGICAL MU_RAW_CCT(16),CCT_LATCH(7)
C..   L1 supervisor variables
      INTEGER DELTA_WORDS,MU_RAW_OTC(16),TRGR_MUON_DATA(500)
C..   TRIG_MON outputs
      LOGICAL L1_ANDOR_STATES(16),L15_TERM_STATES(16)

C..   ENTRY points and auxiliar variables
      INTEGER IBIT,IWORD,MWORD,D_WORDS
      INTEGER MUON_DATA(500),TRGR_DATA(500)
      LOGICAL L1_BITS(16) 
      LOGICAL L1R_BITS(16),L15R_BITS(16),L1P_BITS(16),L15P_BITS(16)
      INTEGER CCT_WORD(7),IOCT
      LOGICAL L1OCT(40)
      INTEGER CCTLAT(7),CCT_LAT(2,5)
      INTEGER TRMASK
      LOGICAL TREG(7)
      DATA TREG /7*.TRUE./

C..   To initialize hits info
      INTEGER MACHIT(26,4)
      LOGICAL LPROD
      DATA LPROD/.FALSE./

C-------------------------------------------------------------------------------

C-- Just unpack raw data and fill MTRG
C-- this fills the appropriate zebra banks used by the trigger,
C-- MUHT, MUOF and SAHH
      IERR = 0
      CALL MUANLZ(IERR2,SKIP_LEVEL,TRIG_LEVEL,DET_REGION)
      IERR = IERR2
      IF(IERR2.EQ.1.OR.IERR2.EQ.2) RETURN

C.. Initialize hits info
      CALL MUMLAT(0,LPROD,IERR,MACHIT)
      IF(IERR.NE.0)RETURN

C.. Checks if any module was not read out
      CALL MU_CHK_MAC_FLAGS(CENT_TRUNC,TOO_MANY_HITS,MAC_SKIP)
      IF(MAC_SKIP) THEN
        IERR = 1
        RETURN
      ENDIF

C.. Get l1 supervisor information, for all regions
      CALL MU_SUP_L1(TREG,CCT_LATCH,MU_RAW_CCT)

C.. Get l1.5 supervisor information. Input CCT_LATCH.
      CALL MU_SUP_L15(TREG,CCT_LATCH,MU_RAW_OTC,DELTA_WORDS,
     &  TRGR_MUON_DATA)


C..  Call MU_TRIG_MON to convert the raw muon trigger information
C    to physics bits.
      CALL MU_TRIG_MON(MU_RAW_CCT,MU_RAW_OTC,
     &                    L1_ANDOR_STATES,L15_TERM_STATES)

C.. Fill intermediate variables to be passed to ENTRY points
      DO IBIT=1,16
        L1R_BITS(IBIT)=MU_RAW_CCT(IBIT)
        L15R_BITS(IBIT)=MU_RAW_OTC(IBIT)
        L1P_BITS(IBIT)=L1_ANDOR_STATES(IBIT)
        L15P_BITS(IBIT)=L15_TERM_STATES(IBIT)
      ENDDO

      DO IBIT=1,7
        CCT_WORD(IBIT)=CCT_LATCH(IBIT)
      ENDDO

      D_WORDS = DELTA_WORDS
      IF(DELTA_WORDS .GT. 0) THEN
        DO IWORD=1,DELTA_WORDS
          TRGR_DATA(IWORD)=TRGR_MUON_DATA(IWORD)
        ENDDO
      ENDIF

  999 RETURN
C
C----------------------------------------------------------------------
C-- entry point to tell the supervisor crate which trigger regions
C-- to be processed
C
      ENTRY MU_TRIG_REG_MASK(TRMASK)
      TREG(1)=.TRUE.
      TREG(2)=.TRUE.
      TREG(3)=.TRUE.
      TREG(4)=.TRUE.
      TREG(5)=.TRUE.
      TREG(6)=.TRUE.
      TREG(7)=.TRUE.
      IF(MOD(TRMASK/1000000,   2).EQ.0) TREG(1)=.FALSE.   ! CF
      IF(MOD(TRMASK/100000,    2).EQ.0) TREG(2)=.FALSE.   ! WN
      IF(MOD(TRMASK/10000,     2).EQ.0) TREG(3)=.FALSE.   ! WS
      IF(MOD(TRMASK/1000,      2).EQ.0) TREG(4)=.FALSE.   ! ON
      IF(MOD(TRMASK/100,       2).EQ.0) TREG(5)=.FALSE.   ! OS
      IF(MOD(TRMASK/10,        2).EQ.0) TREG(6)=.FALSE.   ! SN
      IF(MOD(TRMASK,           2).EQ.0) TREG(7)=.FALSE.   ! SS
      RETURN
C----------------------------------------------------------------------
C-- entry point to retrieve the level 1 muon RAW bits calculated earlier
C
      ENTRY MU_L1_RAW_BITS(L1_BITS)
      DO IBIT=1,16
        L1_BITS(IBIT)=.FALSE.
        IF (L1R_BITS(IBIT)) L1_BITS(IBIT)=.TRUE.
      ENDDO
      RETURN
C----------------------------------------------------------------------
C-- entry point to retrieve the level 1 muon PHYSICS bits calculated earlier
C
      ENTRY MU_L1_PHYS_BITS(L1_BITS)
      DO IBIT=1,16
        L1_BITS(IBIT)=.FALSE.
        IF (L1P_BITS(IBIT)) L1_BITS(IBIT)=.TRUE.
      ENDDO
      RETURN
C----------------------------------------------------------------------
C-- entry point to extract the level 1 trigger OCTANT fired
C-- for all the 7 trigger regions
C
      ENTRY MU_L1_TRIG_OCT(L1OCT)
      DO IOCT=1,40
        L1OCT(IOCT)=.FALSE.
      ENDDO
      CALL MU_L1BITS_TO_OCT(CCT_WORD,L1OCT)
      RETURN
C----------------------------------------------------------------------
C-- TEMPORARY entry point to pass CCT Latch info for Level 2.0 studies
C
      ENTRY MU_CCT_LATCH(CCTLAT)
      DO IBIT=1,7
        CCTLAT(IBIT)=CCT_WORD(IBIT)
      ENDDO
      RETURN
C----------------------------------------------------------------------
      END

