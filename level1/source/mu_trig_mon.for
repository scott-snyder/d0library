      SUBROUTINE MU_TRIG_MON(L1_MUON_BITS,L15_MUON_BITS,
     &  			L1_ANDOR_STATES,L15_TERM_STATES)
C-------------------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts the level 1 and level 1.5 muon
C-                         raw bits to ANDOR inputs to the level 1
C-                         framework.
C-
C-   Inputs  : L1_MUON_BITS   muon level 1 trigger bits from CCT
C-             L15_MUON_BITS  muon level 1.5 trigger bits from OTC
C-
C-   Outputs : L1_ANDOR_STATES  muon level 1 input ANDOR terms to the
C-                              framework
C-             L15_TERM_STATES  muon level 1.5 input terms to the
C-                              framework
C-
C-   Controls: None
C-
C-   Created   4-DEC-1991   Kamel A. Bazizi
C-   Updated   5-NOV-1992   Guilherme Lima   - Implemented init entry point
C-   
C-------------------------------------------------------------------------------
        IMPLICIT NONE
        LOGICAL L1_MUON_BITS(16), L15_MUON_BITS(16)
        INTEGER L1_TABLE(0:15,8), L15_TABLE(0:15,8)
        LOGICAL L1_ANDOR_STATES(16), L15_TERM_STATES(16)
        INTEGER IC,IBIT, I, TRMASK,ICCCT(7), ICOTC(7)
	INTEGER IC_LO, IC_HI, ICOTC_LO(7), ICOTC_HI(7)
	INTEGER L15_TABLE_LOW(0:15,8), L15_TABLE_HIGH(0:15,8)
	INTEGER RUN, EVT
        LOGICAL FIRST, OTCFLG
        SAVE FIRST
        DATA FIRST /.TRUE./

        INTEGER OUTBIT, NUMBITS, MUCOUNT, NUML15_BITS
	INTEGER REQ_MASK
C-------------------------------------------------------------------------------

	CALL EVNTID(RUN,EVT)

      DO IBIT = 1, 16
         L1_ANDOR_STATES(IBIT) = .FALSE.
         L15_TERM_STATES(IBIT) = .FALSE.
      ENDDO

      DO I = 1, 7
         ICCCT(I) = 0
         ICOTC_HI(I) = 0
         ICOTC_LO(I) = 0
      ENDDO

C.. Counts CCT triggers in each region

      IC=0
      IF(L1_MUON_BITS(2)) IC=IC+1
      IF(L1_MUON_BITS(3)) IC=IC+2
      ICCCT(1)=IC

      IC=0
      IF(L1_MUON_BITS(4)) IC=IC+1
      IF(L1_MUON_BITS(5)) IC=IC+2
      ICCCT(2)=IC
	
      IC=0
      IF(L1_MUON_BITS(6)) IC=IC+1
      IF(L1_MUON_BITS(7)) IC=IC+2
      ICCCT(3)=IC

      IC=0
      IF(L1_MUON_BITS(8)) IC=IC+1
      IF(L1_MUON_BITS(9)) IC=IC+2
      ICCCT(4)=IC

      IC=0
      IF(L1_MUON_BITS(10)) IC=IC+1
      IF(L1_MUON_BITS(11)) IC=IC+2
      ICCCT(5)=IC

      IC=0
      IF(L1_MUON_BITS(12)) IC=IC+1
      IF(L1_MUON_BITS(13)) IC=IC+2
      ICCCT(6)=IC

      IC=0
      IF(L1_MUON_BITS(14)) IC=IC+1
      IF(L1_MUON_BITS(15)) IC=IC+2
      ICCCT(7)=IC

C.. Use counters and mapping to evaluate L1 ANDOR terms
C   Caution:  The muon bits are numbered 1..16, while the ANDOR terms
C             are numbered 0..15.
	
        DO IBIT=0,NUMBITS-1
          IC=0
          DO I=1,7
             IC=IC+L1_TABLE(IBIT,I)*ICCCT(I)
          ENDDO
          IF(IC.GE.L1_TABLE(IBIT,8)) L1_ANDOR_STATES(IBIT+1)=.TRUE.
        ENDDO

C  Look for high and low Pt OTC triggers in each region.
C  CF	- p level 13

	IF((RUN.EQ.1).OR.(RUN.GE.74190))THEN
	  IC_LO = 0
	  IC_HI = 0
     	  IF(L15_MUON_BITS(1))IC_LO = 1		!# of CF L1.5 P0 Trigs = 1 
      	  IF(L15_MUON_BITS(2))IC_LO = 2		!# of CF L1.5 P0 Trigs > 1 
      	  IF(L15_MUON_BITS(3))IC_HI = 1		!# of CF L1.5 P1 Trigs > 0
	  ICOTC_LO(1) = IC_LO
	  ICOTC_HI(1) = IC_HI

C EF and overlap North.  Plevel 13 does not distinguish between EF and overlap
C   triggers in crate 31.

	  IC_LO = 0
	  IC_HI = 0
      	  IF(L15_MUON_BITS(4))IC_LO = 1	  !# of EFN and/or OVN L1.5 P0 Trigs = 1 
      	  IF(L15_MUON_BITS(5))IC_LO = 2	  !# of EFN and/or OVN L1.5 P0 Trigs > 1
      	  IF(L15_MUON_BITS(6))IC_HI = 1   !# of EFN and/or OVN L1.5 P1 Trigs > 0
	  ICOTC_LO(2) = IC_LO	
	  ICOTC_HI(2) = IC_HI
          ICOTC_LO(4) = IC_LO
          ICOTC_HI(4) = IC_HI

C EF and overlap South.  Plevel 13

	  IC_LO = 0
	  IC_HI = 0
      	  IF(L15_MUON_BITS(7))IC_LO = 1	!# of EFS and/or OVS L1.5 P0 Trigs = 1
       	  IF(L15_MUON_BITS(8))IC_LO = 2	!# of EFS and/or OVS L1.5 P0 Trigs > 1 
          IF(L15_MUON_BITS(9))IC_HI = 1 !# of EFS and/or OVS L1.5 P1 Trigs > 0 
 	  ICOTC_LO(3) = IC_LO
  	  ICOTC_HI(3) = IC_HI
          ICOTC_LO(5) = IC_LO
          ICOTC_HI(5) = IC_HI

	ELSE 

C  CF - p level 9

 	  IC_LO = 0
	  IC_HI = 0
      	  IF(L15_MUON_BITS(1))IC_LO = 1	!# of CF L1.5 P0 Trigs > 0 
      	  IF(L15_MUON_BITS(2))IC_HI = 1	!# of CF L1.5 P1 Trigs > 0
	  ICOTC_LO(1) = IC_LO
	  ICOTC_HI(1) = IC_HI

C EF North.  Plevel 11 

	  IC_LO = 0
	  IC_HI = 0
      	  IF(L15_MUON_BITS(5))IC_LO = 1	!# of EFN L1.5 P0 Trigs > 0
      	  IF(L15_MUON_BITS(6))IC_HI = 1   !# of EFN L1.5 P1 Trigs > 0
	  ICOTC_LO(2) = IC_LO	
	  ICOTC_HI(2) = IC_HI

C OVERLAP North.  Plevel 11 

	  IC_LO = 0
	  IC_HI = 0
      	  IF(L15_MUON_BITS(4))IC_LO = 1	!# of OVN L1.5 P0 Trigs > 0
	  ICOTC_LO(4) = IC_LO	

C EF South.  Plevel 11 

	  IC_LO = 0
	  IC_HI = 0
      	  IF(L15_MUON_BITS(8))IC_LO = 1	!# of EFS L1.5 P0 Trigs > 0
      	  IF(L15_MUON_BITS(9))IC_HI = 1   !# of EFS L1.5 P1 Trigs > 0
	  ICOTC_LO(3) = IC_LO	
	  ICOTC_HI(3) = IC_HI

C Overlap South.  Plevel 11 
	  IC_LO = 0
	  IC_HI = 0
      	  IF(L15_MUON_BITS(7))IC_LO = 1	!# of OVS L1.5 P0 Trigs > 0
	  ICOTC_LO(5) = IC_LO	

	ENDIF

C SAMUS North.

	IC_LO = 0
	IC_HI = 0
      	IF(L15_MUON_BITS(10))IC_LO = 1		! # of SN L1.5 P0 Trigs = 1 
      	IF(L15_MUON_BITS(11))IC_LO = 2		! # of SN L1.5 P0 Trigs > 1
	ICOTC_LO(6) = IC_LO
	ICOTC_HI(6) = IC_HI

C SAMUS South.

	IC_LO = 0
	IC_HI = 0
      	IF(L15_MUON_BITS(13))IC_LO = 1		! # of SS L1.5 P0 Trigs = 1 
      	IF(L15_MUON_BITS(14))IC_LO = 2		! # of SS L1.5 P0 Trigs > 1 
	ICOTC_LO(7) = IC_LO
	ICOTC_HI(7) = IC_HI

C  Use counters and mapping to evaluate L15 terms. IBIT loops over the
C  trigger bits. The index I loops over the 7 geographic regions (CF,EFN,EFS,
C  OVN,OVS,SN,SS).  L15_TABLE_LOW(IBIT,I), L15_TABLE_HIGH(IBIT,I) contains 
C  a '1' if region I is participates in trigger IBIT; a '0' if not.

	DO IBIT = 0, NUML15_BITS-1
           IC_LO = 0
           IC_HI = 0
           DO I = 1, 7
              IC_LO = IC_LO + L15_TABLE_LOW(IBIT,I)*ICOTC_LO(I)
              IC_HI = IC_HI + L15_TABLE_HIGH(IBIT,I)*ICOTC_HI(I)
           ENDDO

C Compare the number of high pt and low pt muons found to the number 
C    required for trigger # IBIT.

           IF( (IC_LO.GE.L15_TABLE_LOW(IBIT,8) ) .AND.
     &	       (IC_HI.GE.L15_TABLE_HIGH(IBIT,8) ) ) 
     &				L15_TERM_STATES(IBIT+1)=.TRUE.

C Do you need a L15_TERM_STATES_LOW(IBIT+1), L15_TERM_STATES_HIGH(IBIT+1)?

99	ENDDO

999	RETURN

C-------------------------------------------------------------------------------
C  Entry point to initialize the TRIG MON table for L1.5
C
	ENTRY MU_TRIG_MON_INIT_L15(OUTBIT,MUCOUNT,REQ_MASK)

C  Clear both high and low Pt mapping arrays in the first call.

        IF( FIRST )THEN
           FIRST = .FALSE.
           NUML15_BITS=0
           DO IBIT=0,15
             DO I=1,7
               L15_TABLE_LOW(IBIT,I)=0
               L15_TABLE_HIGH(IBIT,I)=0
             ENDDO
             L15_TABLE_LOW(IBIT,8)=-1
             L15_TABLE_HIGH(IBIT,8)=-1
           ENDDO
        ENDIF

C Fill Level 1.5 table. Bit 8 of REQ_MASK indicates whether L1.5 confirm is
C   high pt or low pt.  The total number of L1.5 muons required by trigger
C  bit OUTBIT is MU_COUNT.

          IBIT = REQ_MASK
	  IF(BTEST(REQ_MASK,8))THEN
            DO I = 1, 7
               IBIT = ISHFT(IBIT,-1)
               IF(MOD(IBIT,2).EQ.1) L15_TABLE_HIGH(OUTBIT,I)=1
            ENDDO
            L15_TABLE_HIGH(OUTBIT,8) = MUCOUNT  
	  ELSE
            DO I = 1, 7
               IBIT = ISHFT(IBIT,-1)
               IF(MOD(IBIT,2).EQ.1) L15_TABLE_LOW(OUTBIT,I)=1
            ENDDO
            L15_TABLE_LOW(OUTBIT,8) = MUCOUNT   
	  ENDIF
          NUML15_BITS=NUML15_BITS+1

        RETURN
C------------------------------------------------------------------------------
C  Entry point to initialize the TRIG MON table for L1.0

	ENTRY MU_TRIG_MON_INIT_L1(OUTBIT,MUCOUNT,REQ_MASK)

C  Clear mapping array in the first call.

        IF( FIRST )THEN
           FIRST = .FALSE.
           NUMBITS=0
           DO IBIT=0,15
             DO I=1,7
               L1_TABLE(IBIT,I)=0
             ENDDO
             L1_TABLE(IBIT,8)=-1
           ENDDO
        ENDIF

C.. Level 1.0 table
C   Caution:  The muon bits are numbered 1..16, while the ANDOR terms
C             are numbered 0..15.

          IBIT=REQ_MASK
          DO I=1,7
            IBIT=ISHFT(IBIT,-1)
            IF(MOD(IBIT,2).EQ.1) L1_TABLE(OUTBIT,I)=1
          ENDDO
          L1_TABLE(OUTBIT,8) = MUCOUNT
          NUMBITS=NUMBITS+1

        RETURN
C------------------------------------------------------------------------------
C  Entry point to disable trigger regions not used in this run
C
	ENTRY MU_TRREG_DISBL

        TRMASK=0
        DO I=1,7
          IC=0
          DO IBIT=0,NUMBITS-1
            IC=IC+L1_TABLE(IBIT,I)
          ENDDO
          IF(IC.NE.0) TRMASK=TRMASK+10**(7-I)
        ENDDO

        PRINT *,' *** Trigger region mask:',TRMASK
        CALL MU_TRIG_REG_MASK(TRMASK)

        RETURN
        END
