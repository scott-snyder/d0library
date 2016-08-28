C-------------------------------------------------------------------------------
	SUBROUTINE MU_SUP_L15(TREG,CCTLAT,MU_RAW_OTC,DELTA_WORDS,
     &  	              TRGR_MUON_DATA)
C-------------------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the muon trigger information for
C-                         both level 1 and level 1.5 to be passed to
C-                         the trigger framework
C-
C-  The 7 muon trigger regions TRIG_REG :
C-                    1 = Central                    (CF)
C-                    2 = End WAMUS North            (WN)
C-                    3 = End WAMUS South            (WS)
C-                    4 = SAMUS-WAMUS Overlap North  (ON)
C-                    5 = SAMUS-WAMUS Overlap South  (OS)
C-                    6 = SAMUS North                (SN)
C-                    7 = SAMUS South                (SS)
C-
C-   Inputs  : TREG(1:7) - Logical array which indicates which of the 7
C-                         muon trigger regions are active.
C-             CCTLAT(7) - 32 bit cct latch word for each region. 
C-
C-   Outputs : MU_RAW_OTC - OTC trigger bits for 5 crates passed to TRIGMON.
C-
C-             TRGR_MUON_DATA - Array of OTC crate data to be packed in TRGR.
C-
C-             DELTA_WORDS - Number of words in above array. 
C-
C-----------------------------------------------------------------------------
        IMPLICIT NONE
        INTEGER IBIT,IWORD,MWORD,D_WORDS,DELTA_WORDS,NWORDS
        INTEGER OTCMGR_DATA(294),MUON_DATA(500),TRGR_DATA(500)
        INTEGER TRGR_MUON_DATA(500), SKIP_LEVEL, TREG(7)
        INTEGER TRIG_REG,TRMASK,IOCT,CCT_WORD(7),TRREG
        INTEGER CCTLAT(7),CCT_LAT(2,5),IREG,CCT_LAT_WRD(2)
        INTEGER NOTC(7),NMGRDAT(5),MGRDAT(5,130,4,2)
        INTEGER IMGRDAT(7),JMGRDAT(7,130,4,2)
        INTEGER I,II,III,IIII,IJ,J,II1,II2,IERR

        LOGICAL MU_RAW_OTC(16), L15_BITS(16), JOTC(3)
	LOGICAL L15R_BITS(16), L15P_BITS(16)
	DATA NOTC/8,4,4,8,8,4,4/
C-------------------------------------------------------------------------------
C  Initialization

        DO IBIT = 1, 16
          MU_RAW_OTC(IBIT)      = .FALSE.
        ENDDO

        DO I = 1, 7
           IMGRDAT(I) = 0
	   IF(I.LE.5)NMGRDAT(I) = 0
        ENDDO

        DO I = 1, 7
          DO II = 1, 130
            DO III = 1, 4
              DO IIII = 1, 2
                JMGRDAT(I,II,III,IIII) = 0
		IF(I.LE.5)MGRDAT(I,II,III,IIII) = 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO

        DO IWORD=1,500
          TRGR_DATA(IWORD)=0
          TRGR_MUON_DATA(IWORD)=0
        ENDDO
        DELTA_WORDS=0

C  Compute the muon triggers for all trigger regions.
	
        DO TRIG_REG = 1, 7
           DO IBIT = 1, 3
             JOTC(IBIT) = .FALSE.
           ENDDO

           IF(TREG(TRIG_REG).ne.0)THEN
             CALL MU_TRIG_CRATE_L15(TRIG_REG,CCTLAT(TRIG_REG),
     &					JMGRDAT,IMGRDAT)

C Pack OTC manager data array, MGRDAT.

	     I = TRIG_REG
	     IF(I.LE.3)NMGRDAT(I) = IMGRDAT(I)
	     IF(I.GE.6)NMGRDAT(I-2) = IMGRDAT(I)

C Add overlap triggers in with EF.  Make sure total triggers .le. 130

             IF(I.GE.4.AND.I.LE.5)THEN
               IF( (NMGRDAT(I-2)+IMGRDAT(I)).LT.130 )THEN
                  NMGRDAT(I-2) = NMGRDAT(I-2) + IMGRDAT(I)
               ELSE
                  NMGRDAT(I-2) = 130
               ENDIF
             ENDIF
	    	    
C Start filling MGRDAT from JMGRDAT. 

	     J = 0
             DO II = 1, 130
               DO III = 1, 4
                  DO IIII = 1, 2
C WAMUS CF & EF.
		    IF(I.LE.3)MGRDAT(I,II,III,IIII) = 
     &				JMGRDAT(I,II,III,IIII)

C Overlap.  Put overlap data in with EF.

                    IF(I.GE.4.AND.I.LE.5)THEN
                      J = IMGRDAT(I-2)+II
                      IF(J.LE.130)MGRDAT(I-2,J,III,IIII) =
     &                   MGRDAT(I-2,J,III,IIII)+JMGRDAT(I,II,III,IIII)
                    ENDIF
C SAMUS
		    IF(I.GE.6)MGRDAT(I-2,II,III,IIII) = 
     &				JMGRDAT(I,II,III,IIII)
		    
                  ENDDO
               ENDDO
	     ENDDO					! End Packing MGRDAT
           ENDIF
        ENDDO				!End loop over TRIG_REG = 1,7
	
        CCT_LAT(1,1)=CCTLAT(1)
        CCT_LAT(2,1)=0
        CCT_LAT(1,2)=CCTLAT(2)
        CCT_LAT(2,2)=CCTLAT(4)
        CCT_LAT(1,3)=CCTLAT(3)
        CCT_LAT(2,3)=CCTLAT(5)
        CCT_LAT(1,4)=CCTLAT(6)
        CCT_LAT(2,4)=0
        CCT_LAT(1,5)=CCTLAT(7)
        CCT_LAT(2,5)=0

C  Call OTC manager to pack the muon trigger data for region IREG

	DO IREG = 1, 5          
           DO I = 1, 2
             CCT_LAT_WRD(I) = CCT_LAT(I,IREG)
           ENDDO
           CALL MU_OTC_MGR(IREG,NOTC(IREG),MGRDAT,NMGRDAT,CCT_LAT_WRD,
     &                     JOTC,OTCMGR_DATA,NWORDS)

C Pack OTC bits to the TRIGMON.  There are 3 bits for each the 5 OTC crates.

           DO IBIT = 1, 3
              MU_RAW_OTC((IREG-1)*3+IBIT) = JOTC(IBIT)
           ENDDO

C- Pack data for TRGR bank.

           IF(NWORDS.GT.0)THEN
              DO IWORD = 1, NWORDS
                 DELTA_WORDS = DELTA_WORDS + 1
                 TRGR_MUON_DATA(DELTA_WORDS) = OTCMGR_DATA(IWORD)
              ENDDO
           ENDIF
        ENDDO			!End loop over IREG = 1, 5

        D_WORDS = DELTA_WORDS
        IF(DELTA_WORDS .GT. 0) THEN
          DO IWORD=1,DELTA_WORDS
            TRGR_DATA(IWORD)=TRGR_MUON_DATA(IWORD)
          ENDDO
        ENDIF

999 	RETURN
C-------------------------------------------------------------------------------
C  Entry point to retrieve the level 1.5 muon RAW bits calculated earlier

      ENTRY MU_L15_RAW_BITS(L15_BITS)
      DO IBIT=1,16
        L15_BITS(IBIT)=.FALSE.
        IF (L15R_BITS(IBIT)) L15_BITS(IBIT)=.TRUE.
      ENDDO
      RETURN
C-------------------------------------------------------------------------------
C  Entry point to retrieve the level 1.5 muon PHYSICS bits calculated earlier

      ENTRY MU_L15_PHYS_BITS(L15_BITS)

      DO IBIT=1,16
        L15_BITS(IBIT)=.FALSE.
        IF (L15P_BITS(IBIT)) L15_BITS(IBIT)=.TRUE.
      ENDDO
      RETURN
C----------------------------------------------------------------------
C Entry point to fill the TRGR bank with the muon trigger data
C calculated by MU_SUP_L15.

      ENTRY MU_TRIG_DATA(MWORD,MUON_DATA)

      MWORD = D_WORDS
      DO IWORD = 1, 500
        MUON_DATA(IWORD)=TRGR_DATA(IWORD)
      ENDDO
      RETURN
C----------------------------------------------------------------------
      END

