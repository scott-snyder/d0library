C===============================================================================
      SUBROUTINE MU_OTC_MGR(IREG,NOTC,MGRDAT,NMGRDAT,CCT_LAT_WRD,JBOTC,
     &                  OTCMGR_DATA,NWORDS)
C==============================================================================
C
C  SUBROUTINE MU_OTC_MGR : SIMULATE OTC MANAGER CARD
C
C       INPUTS :        NOTC        = # of Kin OTC'S in this region.
C                       MGRDAT      = Kinematic data from OTC's.
C                       NMGRDAT     = Number of OTC triggers (in each OTC)
C                       CCT_LAT_WRD = CCT Latch word 
C
C       OUTPUTS :       JBOTC       = 3-BIT Output to trigger monitor.
C                       OTCMGR_DATA = OTC Manager Data array.
C                       NWORDS      = number of elements in that array.
C
C       AUTHOR: Leigh Markosky
C
C       Dec 20, 1993 - added code to switch between plevels 9/13 in CF, 
C                       11/13 in EF and overlap.
C------------------------------------------------------------------------------
	IMPLICIT NONE
        INCLUDE 'D0$INC:ZEBCOM.INC'
C
        INTEGER MGRDAT(5,130,4,2), NMGRDAT(5)
        INTEGER I, J, DUM1, DUM2, NOTC, IREG
        INTEGER NWORDS, OTCMGR_DATA(294)
        LOGICAL JBOTC(3)
	INTEGER L15_CONFIRM_L1
	INTEGER NT, NP0_CONFIRM, NP1_CONFIRM
      	INTEGER TOKEN_PASS_STATUS, CHECKSUM, REC_TYPE
      	INTEGER VBD_WRD, TOT_WRD_COUNT, K, CCTMAT
      	INTEGER BEGIN_DATA_MARKER, MGR_OVRFL(16)
      	INTEGER LAYER_FLAG1(5), LAYER_FLAG2(5)
	INTEGER SAVE_BITS(130), OTC_DIP, P1_CUT, EXTRA_WORD
	INTEGER NEF_P0, NEF_P1, NSW_P0, NSW_P1
      	INTEGER CONTROL_WRD, CRATE_ID(5), N_OTC_IN_CRATE(5)
      	INTEGER VERS, VERSION_NUM, VRSN_NUM, LOC1, LOC2, SIM, D0
      	INTEGER OTC_ERR_WRD1, OTC_ERR_WRD2, OTC_INFO_WRD
      	INTEGER MGR_LTO_VAL, OFFSET, TOTAL_WORD_COUNT
      	INTEGER DOWN_LOAD_FILE, CCT_LAT_WRD(2), NP0, NP1
      	INTEGER OTC_STAT_WRD(16), OTC_ID_NUM(1:5,1:16)
      	INTEGER SYNC_WRD, TRIG_NUM, DUMMY, GZHEAD
      	INTEGER DATA_WORD(1:131,1:2), N_TRIG_TOT
      	INTEGER TRAILER_WORD(1:4), MONTH, DAY, YEAR
      	INTEGER OFF_SET, TAB_TYP, TAB_VER, P0TAB_ERR
      	INTEGER MTAB_ERR, KTAB_ERR, BCMAP_ERR, P1TAB_ERR
	INTEGER CCTLAT_TAB_ERR, RUN1B
	INTEGER OTC_BUSY(16), DATA_READ(16), L15_PROC(16)
        INTEGER TRGLAT(16), MGRDY(16), INOVRFL(16), DATRD_TIMOUT(16)
        INTEGER CHOKE(16), LPAGE(16), LT_OUT(16), MEM_FULL(16)
        INTEGER SPARE, RUN, EVT, STATE_WORD, TAB_ID_WRD
	INTEGER TAB_ID_WORD, TAB_ID_1, TAB_ID_2

        DATA D0/0/
        DATA SIM/0/
        DATA LOC1/0/
        DATA LOC2/0/
        DATA TOKEN_PASS_STATUS/0/
        DATA MGR_OVRFL/16*0/
        DATA CHECKSUM/0/
        DATA MGR_LTO_VAL/0/
        DATA DATRD_TIMOUT/16*0/
        DATA LT_OUT/16*0/
        DATA TRGLAT/16*0/
        DATA KTAB_ERR/0/
        DATA MTAB_ERR/0/
	DATA BCMAP_ERR/0/
	DATA P1TAB_ERR/0/
	DATA CCTLAT_TAB_ERR/0/
        DATA MEM_FULL/16*0/
        DATA CHOKE/16*0/
        DATA LPAGE/16*0/
        DATA MGRDY/16*0/
        DATA INOVRFL/16*0/
        DATA OTC_BUSY/16*0/
        DATA L15_PROC/16*0/
        DATA DATA_READ/16*0/
        DATA OTC_STAT_WRD/16*0/
        DATA CRATE_ID/21,31,41,51,61/
        DATA N_OTC_IN_CRATE/8,12,12,16,16/
        DATA DATA_WORD/262*0/
        DATA LAYER_FLAG1/5*0/
        DATA LAYER_FLAG2/1,0,0,0,0/

C  Below array contains the numbers of all OTC's, region by region, and in the
C   order they appear in the actual crate slots.

      DATA ((OTC_ID_NUM(I,J),J=1,16),I=1,5)/  
     1  707,700,701,702,703,704,705,706,0,0,0,0,0,0,0,0,
     2  730,720,710,716,726,736,732,722,712,714,724,734,0,0,0,0,
     3  760,750,740,746,756,766,762,752,742,744,754,764,0,0,0,0,
     4  770,776,666,664,662,660,616,614,612,610,606,604,602,600,772,774,
     5  780,786,676,674,672,670,646,644,642,640,636,634,632,630,782,784/

C-------------------------------------------------------------------------------
 
C  Get the trigger number (i.e. here called the event number).  

        CALL EVNTID(RUN,TRIG_NUM)
        LHEAD = GZHEAD(0)
        REC_TYPE = IQ(LHEAD+1)

C  SIM indicates whether you have real data or monte carlo.

        IF(REC_TYPE.GE.1005.AND.REC_TYPE.LE.1999)THEN
 	  SIM = 1			! monte carlo
        ELSE
          SIM = 0			! real data
        ENDIF

C Initialization.

	SPARE = 0
        SYNC_WRD    = 0
	TAB_ID_WRD = 0
        CONTROL_WRD = 0
        DOWN_LOAD_FILE = 0
        VERSION_NUM  = 0
        OTC_INFO_WRD = 0
        TOTAL_WORD_COUNT = 0
        VBD_WRD  = 0
	OTC_ERR_WRD1 = 0
	OTC_ERR_WRD2 = 0

        DO I = 1, 3
          JBOTC(I) = .FALSE.
        ENDDO

	NT  = 0                              ! L1.5 trigger counter
        IF(NMGRDAT(IREG).GT.0)THEN           ! Check if any triggers in Ith OTC.
           NT = NMGRDAT(IREG)  
	   IF(NT.GT.130)NT=130		     ! protect against overwrites
        ENDIF
      

C  Fill the OTCMGR_DATA array.   NWORDS = Number of OTC data words =
C   29 header words + (2N + 1 words for N triggers )+ 4 trailer words.
  
C  Pack the "sync' word. (Longword NW1+2)
	
        DUMMY = 65535                                   !FFFF
        SYNC_WRD = IOR(SYNC_WRD,ISHFT(DUMMY,0))
        SYNC_WRD = IOR(SYNC_WRD,ISHFT(TRIG_NUM,16))

C  Pack the version number (Longword NW1+4).

c       VERS = 11301				!old mgr
C	VERS = 12549				!new mgr I
	VERS = 15139				!new mgr II
	IF((SIM.EQ.1) .OR. (RUN.GE.74190))VERS = 16680

        VERSION_NUM = IOR(VERSION_NUM,ISHFT(VERS,0))
	IF(RUN.GT.70944)VERSION_NUM = IBSET(VERSION_NUM,20)
	
C  Pack the OTC information word. (Longword NW1+5).

        OTC_INFO_WRD = IOR(OTC_INFO_WRD,ISHFT(MGR_LTO_VAL,0))
        OTC_INFO_WRD = IOR(OTC_INFO_WRD,ISHFT(N_OTC_IN_CRATE(IREG),16))

C  Pack the TAB_ID_WORD. (Longword + 6)

	TAB_ID_WORD = IOR(TAB_ID_WORD,ISHFT(TAB_ID_2,0))
	TAB_ID_WORD = IOR(TAB_ID_WORD,ISHFT(TAB_ID_1,16))

C  Pack OTC_ERR_WRD1. (Longword NW1+7)
	
        OTC_ERR_WRD1 = IOR(OTC_ERR_WRD1,ISHFT(KTAB_ERR,0))
        OTC_ERR_WRD1 = IOR(OTC_ERR_WRD1,ISHFT(P0TAB_ERR,16))

C  Pack the 16 OTC status words. (Longwords NW1+13 through NW1+28)

      DO I = 1, 16
        OTC_STAT_WRD(I) = IOR(OTC_STAT_WRD(I),ISHFT(OTC_BUSY(I),0))
        OTC_STAT_WRD(I) = IOR(OTC_STAT_WRD(I),ISHFT(DATA_READ(I),1))
        OTC_STAT_WRD(I) = IOR(OTC_STAT_WRD(I),ISHFT(L15_PROC(I),2))
        OTC_STAT_WRD(I) = IOR(OTC_STAT_WRD(I),ISHFT(TRGLAT(I),3))

        OTC_STAT_WRD(I) = IOR(OTC_STAT_WRD(I),ISHFT(MGRDY(I),4))
        OTC_STAT_WRD(I) = IOR(OTC_STAT_WRD(I),ISHFT(MGR_OVRFL(I),5))
        OTC_STAT_WRD(I) = IOR(OTC_STAT_WRD(I),ISHFT(CHOKE(I),6))
        OTC_STAT_WRD(I) = IOR(OTC_STAT_WRD(I),ISHFT(LPAGE(I),7))
        OTC_STAT_WRD(I) = IOR(OTC_STAT_WRD(I),
     &                                  ISHFT(DATRD_TIMOUT(I),8))
        OTC_STAT_WRD(I) = IOR(OTC_STAT_WRD(I),ISHFT(INOVRFL(I),9))
        OTC_STAT_WRD(I) = IOR(OTC_STAT_WRD(I),ISHFT(LT_OUT(I),10))
        OTC_STAT_WRD(I) = IOR(OTC_STAT_WRD(I),ISHFT(MEM_FULL(I),11))
        OTC_STAT_WRD(I) = IOR(OTC_STAT_WRD(I),ISHFT(OTC_ID_NUM(IREG,I),
     &								   16))
      ENDDO

      BEGIN_DATA_MARKER = 0
	
C  Pack the trigger data words. 

        K = 0
	NSW_P0 = 0
	NSW_P1 = 0
	NEF_P0 = 0
	NEF_P1 = 0	
	NP0 = 0
	NP1 = 0
	NP0_CONFIRM = 0		
	NP1_CONFIRM = 0

        DO J = 1, NMGRDAT(IREG)           !Loop over kinematic triggers in OTC.
	  CCTMAT = 0
          K = K + 1                   !Counts total number of triggers.

C  Save the OTC dip #, CCT match, and P1 trigger flag from the Nth trigger.
C   Will be stored in the upper 24 bits of the N+1th trigger.

	  SAVE_BITS(J) = 0
	  CALL MVBITS(MGRDAT(IREG,J,4,1),0,6,SAVE_BITS(J),0)

C  Word 1.
          DATA_WORD(K,1) = 0
          CALL MVBITS(MGRDAT(IREG,J,1,1),0,8,DATA_WORD(K,1),0)
          CALL MVBITS(MGRDAT(IREG,J,2,1),0,8,DATA_WORD(K,1),8)
          CALL MVBITS(MGRDAT(IREG,J,3,1),0,8,DATA_WORD(K,1),16)

C  Check OTC dip # in MGRDAT to count EF and overlap triggers separately.
C  P1 trig flag in bit 4.

	  P1_CUT = 0 
	  CALL MVBITS(MGRDAT(IREG,J,4,1),0,4,OTC_DIP,0)
	  CALL MVBITS(MGRDAT(IREG,J,4,1),4,1,P1_CUT,0)
	  CALL MVBITS(MGRDAT(IREG,J,4,1),5,1,CCTMAT,0)
	  IF(CCTMAT.EQ.1)THEN
	     NP0_CONFIRM = NP0_CONFIRM + 1
             IF(IREG.LT.4.AND.P1_CUT.GT.0)NP1_CONFIRM = NP1_CONFIRM + 1
	  ENDIF

C  For real data with run numbers<74190, there was no CCT match required
C    in the OTC state bits.  Just count triggers 

	  IF(SIM.EQ.0.AND.RUN.LT.74190)THEN
	     NP0 = NP0 + 1
	     IF(IREG.LT.4.AND.P1_CUT.GT.0)NP1 = NP1  + 1
	  ENDIF
	  	  
C PLEVEL 11
	
	  IF(IREG.EQ.2.OR.IREG.EQ.3)THEN
	     IF(OTC_DIP.EQ.2.OR.OTC_DIP.EQ.8.OR.OTC_DIP.EQ.9.OR.
     &			OTC_DIP.EQ.3)THEN
		IF(CCTMAT.EQ.1)NEF_P0 = NEF_P0+1
	     	IF(CCTMAT.EQ.1.AND.P1_CUT.GT.0)NEF_P1 = NEF_P1 + 1
	     ELSE
		 IF(CCTMAT.EQ.1)NSW_P0 = NSW_P0 + 1
c 	         IF(P1_CUT.GT.0)NSW_P1 = NSW_P1 + 1
	     ENDIF
	  ENDIF

C  Word 2.

          DATA_WORD(K,2) = 0
          CALL MVBITS(MGRDAT(IREG,J,1,2),0,6,DATA_WORD(K,2),0)  !A centroid #
          CALL MVBITS(MGRDAT(IREG,J,2,2),0,6,DATA_WORD(K,2),8)  !B centroid #
          CALL MVBITS(MGRDAT(IREG,J,3,2),0,6,DATA_WORD(K,2),16) !C centroid #

        ENDDO

C Now pack the saved bits of the jth trigger into the upper 8 bits 
C   of the j+1th trigger.
 
        N_TRIG_TOT = K          !total OTC triggers in this trigger region.
	DO J = 1, N_TRIG_TOT
	   CALL MVBITS(SAVE_BITS(J),0,6,DATA_WORD(J+1,1),24) 
	ENDDO

C Set the OTC manager state bits. 

        STATE_WORD = 0         

C For all real data with run numbers<74190: Plevel 9 for crate 21, Plevel 13 
C  for crates 31 and 41. Plevel 14 for crates 51 and 61.  No CCT match required
C  in these states.

	IF(SIM.EQ.0.AND.RUN.LT.74190)THEN
          IF(IREG.EQ.1)THEN
 	    IF (NP0.EQ.0) STATE_WORD=0
	    IF (NP0.GT.0) STATE_WORD=1
	    IF (NP1.GT.0) STATE_WORD=3
            IF (IBITS(STATE_WORD,0,1).EQ.1) JBOTC(1) = .TRUE.
            IF (IBITS(STATE_WORD,1,1).EQ.1) JBOTC(2) = .TRUE.
	  ENDIF
C Plevel 11 
	  IF(IREG.EQ.2.OR.IREG.EQ.3)THEN
	    IF(NSW_P0.EQ.0.AND.NEF_P0.EQ.0.AND.NEF_P1.EQ.0)STATE_WORD=0
	    IF(NSW_P0.GT.0.AND.NEF_P0.EQ.0.AND.NEF_P1.EQ.0)STATE_WORD=1
	    IF(NSW_P0.EQ.0.AND.NEF_P0.GT.0.AND.NEF_P1.EQ.0)STATE_WORD=2
	    IF(NSW_P0.GT.0.AND.NEF_P0.GT.0.AND.NEF_P1.EQ.0)STATE_WORD=3
	    IF(NSW_P0.EQ.0.AND.NEF_P0.GT.0.AND.NEF_P1.GT.0)STATE_WORD=6
	    IF(NSW_P0.GT.0.AND.NEF_P0.GT.0.AND.NEF_P1.GT.0)STATE_WORD=7

            IF(IBITS(STATE_WORD,0,1).EQ.1)JBOTC(1) = .TRUE.
            IF(IBITS(STATE_WORD,1,1).EQ.1)JBOTC(2) = .TRUE.
            IF(IBITS(STATE_WORD,2,1).EQ.1)JBOTC(3) = .TRUE.
	  ENDIF
C PLEVEL 9
C Plevel 14 for SAMUS, but no CCT match required.

	  IF(IREG.GE.4)THEN
            IF (NP0.EQ.0) STATE_WORD = 0
            IF (NP0.EQ.1) STATE_WORD = 1
            IF (NP0.GT.1) STATE_WORD = 2
            IF (IBITS(STATE_WORD,0,1).EQ.1) JBOTC(1) = .TRUE.
            IF (IBITS(STATE_WORD,1,1).EQ.1) JBOTC(2) = .TRUE.
	 ENDIF
	ENDIF

C All monte carlo, and real data with run # >= 74190, Plevel 13 with
C  CCT confirm for WAMUS, Plevel 14 with cct confirm for SAMUS.

	IF(SIM.EQ.1 .OR. RUN.GE.74190)THEN
	  IF(IREG.LT.4)THEN
	    IF (NP0_CONFIRM.EQ.0.AND.NP1_CONFIRM.EQ.0) STATE_WORD=0
	    IF (NP0_CONFIRM.EQ.1.AND.NP1_CONFIRM.EQ.0) STATE_WORD=1
	    IF (NP0_CONFIRM.GT.1.AND.NP1_CONFIRM.EQ.0) STATE_WORD=2
	    IF (NP0_CONFIRM.EQ.1.AND.NP1_CONFIRM.GT.0) STATE_WORD=5
	    IF (NP0_CONFIRM.GT.1.AND.NP1_CONFIRM.GT.0) STATE_WORD=6
C
            IF (IBITS(STATE_WORD,0,1).EQ.1) JBOTC(1) = .TRUE.
            IF (IBITS(STATE_WORD,1,1).EQ.1) JBOTC(2) = .TRUE.
            IF (IBITS(STATE_WORD,2,1).EQ.1) JBOTC(3) = .TRUE.
	  ENDIF
          IF( IREG.GE.4 )THEN
            IF (NP0_CONFIRM.EQ.0) STATE_WORD = 0
            IF (NP0_CONFIRM.EQ.1) STATE_WORD = 1
            IF (NP0_CONFIRM.GT.1) STATE_WORD = 2
            IF (IBITS(STATE_WORD,0,1).EQ.1) JBOTC(1) = .TRUE.
            IF (IBITS(STATE_WORD,1,1).EQ.1) JBOTC(2) = .TRUE.
	  ENDIF
	ENDIF

C  Now that you finally have STATE_WORD, pack OTC_ERR_WRD2. (Longword NW1+8)

        OTC_ERR_WRD2 = IOR(OTC_ERR_WRD2,ISHFT(P1TAB_ERR,16))
        OTC_ERR_WRD2 = IOR(OTC_ERR_WRD2,ISHFT(BCMAP_ERR,17))
        OTC_ERR_WRD2 = IOR(OTC_ERR_WRD2,ISHFT(CCTLAT_TAB_ERR,18))
        OTC_ERR_WRD2 = IOR(OTC_ERR_WRD2,ISHFT(STATE_WORD,0))

C  Pack the "controller" word. (Longword NW1+3).

        CONTROL_WRD = IOR(CONTROL_WRD,ISHFT(STATE_WORD,0))
        CONTROL_WRD = IOR(CONTROL_WRD,ISHFT(N_OTC_IN_CRATE(IREG),16))
        CONTROL_WRD = IOR(CONTROL_WRD,ISHFT(CRATE_ID(IREG),24))

C Total number of words in bank is 29 header words, 2*N_TRIG_TOT + 1 OTC
C  trigger words, plus 4 trailer words.

	IF(N_TRIG_TOT.GT.0)THEN	
           TOTAL_WORD_COUNT = 29 + 2*N_TRIG_TOT + 1 + 4
	ELSE
	   TOTAL_WORD_COUNT = 33
	ENDIF

C  Begin packing the OTCMGR_DATA array.

        OTCMGR_DATA(1) = TOTAL_WORD_COUNT - 1           ! long word NW1+1
        OTCMGR_DATA(2) = SYNC_WRD
        OTCMGR_DATA(3) = CONTROL_WRD
        OTCMGR_DATA(4) = VERSION_NUM
        OTCMGR_DATA(5) = OTC_INFO_WRD
        OTCMGR_DATA(6) = TAB_ID_WRD
        OTCMGR_DATA(7) = OTC_ERR_WRD1
	OTCMGR_DATA(8) = OTC_ERR_WRD2
	OTCMGR_DATA(9) = SPARE
	OTCMGR_DATA(10) = SPARE
        OTCMGR_DATA(11) = CCT_LAT_WRD(1)
        OTCMGR_DATA(12) = CCT_LAT_WRD(2)

        DO I = 1, 16
          OTCMGR_DATA(I+12) = OTC_STAT_WRD(I)
        ENDDO

        OTCMGR_DATA(29) = BEGIN_DATA_MARKER

C  Pack the OTC trigger words into OTC_MGR_DATA, two by two.

	OFF_SET = 29			!NEW MGR
        DO J = 1, N_TRIG_TOT
          OTCMGR_DATA( OFF_SET+2*J-1 ) = DATA_WORD(J,1)
          OTCMGR_DATA( OFF_SET+2*J )   = DATA_WORD(J,2)
        ENDDO

C Pack in the 'EXTRA_WORD' from the last trigger.

	EXTRA_WORD=0
	IF(N_TRIG_TOT.GT.0)THEN
	   EXTRA_WORD = OFF_SET + 2*N_TRIG_TOT + 1
	   CALL MVBITS(DATA_WORD(N_TRIG_TOT+1,1),24,6,
     &			OTCMGR_DATA(EXTRA_WORD),24)
  	ENDIF

C  Pack the VBD word.

        VBD_WRD = IOR(VBD_WRD,ISHFT(CRATE_ID(IREG),0))
        VBD_WRD = IOR(VBD_WRD,ISHFT(TRIG_NUM,16))

C  Fill the 4 trailer words.
	
        TRAILER_WORD(1) = TOTAL_WORD_COUNT
        TRAILER_WORD(2) = VBD_WRD
        TRAILER_WORD(3) = TOKEN_PASS_STATUS
        TRAILER_WORD(4) = CHECKSUM

C  Pack the 4 trailer words into last 4 spots of OTCMGR_DATA.

        NWORDS = TOTAL_WORD_COUNT

        DO J = 1, 4
          OTCMGR_DATA(NWORDS-4+J) = TRAILER_WORD(J)
        ENDDO
C-------------------------------------------------------------------------------

        RETURN
        END
