C===============================================================================
	SUBROUTINE MU_WAM_OTC(III,AFINE,NAFINE,BFINE,NBFINE,CFINE,NCFINE,
     &                   MUNIT,KUNIT,NTRIG,KDAT,OTCNUM)
C==============================================================================
C
C 	PURPOSE : Simulate the Level 1.5 fine centroid trigger logic.
C
C	INPUTS  :	III    = octant number.
C			AFINE  = Centroids and module #'S for A-layer mdoules
C		   	NAFINE = Number of A-layer centroids.
C			Similarly for BFINE, NBFINE, CFINE, and NCFINE.
C			MUNIT  = M Table
C			KUNIT  = K Table
C
C	OUTPUTS :	NTRIG  = Number of L1.5 triggers found.
C			KDAT   = Kinematic data for found L1.5 triggers.
C			OTCNUM = OTC Phil Martin number.
C
C	REVISION HISTORY: 11/05/91  Original Creation. (Dave Fein)
C 			   4/07/92  New M table format.
C	   		   6/10/92  New K table format.
C	   		  10/18/92  Added .RCP switch between data and test
C					mode. (LAM)
C
C------------------------------------------------------------------------------
	IMPLICIT NONE
C
	INTEGER III,I,J,K, L, Z, OTCNUM, NTRIG
	INTEGER AFINE(32),NAFINE,BFINE(32),NBFINE,CFINE(32),NCFINE
	INTEGER MUNIT, M1TRIG, M2TRIG, TEMP
	INTEGER M1ADD, M2ADD, K1ADD, K2ADD, K3ADD, KADD,KUNIT
	INTEGER KDAT(1000,3,2), MODNO, MODULE, IERR
	INTEGER SLOT_BITS0, SLOT_BITS1
	INTEGER OTC_MODE, IER
C<<
      CHARACTER*72 STRING
C<<
	LOGICAL FIRST
	DATA FIRST/.TRUE./
C-------------------------------------------------------------------------------
C<<
C-- Get OTC mode from RCP file (to be used in packing the kinematic data).
C<<
	IF(FIRST)THEN
	  FIRST = .FALSE.
          CALL EZPICK('MUSIM_RCP')
          CALL EZERR(IER)     ! Check if error
          IF(IER.NE.0) THEN
            CALL EZGET_ERROR_TEXT(IER,STRING)
            CALL ERRMSG(' EZPICK ERR','MU_WAM_OTC',STRING,'F')
            GOTO 999
          ENDIF
C
          CALL EZGET('OTC_DATA_TEST_MODE',OTC_MODE,IER)
          IF(IER.NE.0) THEN
            CALL EZGET_ERROR_TEXT(IER,STRING)
            CALL ERRMSG(' OTC_DATA_TEST_MODE','MU_WAM_OTC',STRING,'F')
            GOTO 999
          ENDIF
C
          CALL EZRSET()
C
  	ENDIF
	
C  Find OTC #'s and slots for kinematic tables.
C<<
	IF (III.LT.9) THEN 			! CENTRAL OTC'S
	  OTCNUM = 700 + III -1
	  CALL MU_OTC_SLOT(OTCNUM,Z)		! Z = SLOT #
	ELSE
	  IF (MOD(III,2).EQ.1) THEN		! NORTH
            OTCNUM = 710 + III - 11
	    CALL MU_OTC_SLOT(OTCNUM,Z)		
	  END IF
	  IF (MOD(III,2).EQ.0) THEN		! SOUTH
	    OTCNUM = 740 + III - 12
            CALL MU_OTC_SLOT(OTCNUM,Z)	
	  END IF
	END IF
C<<
	SLOT_BITS0 = 0
	SLOT_BITS1 = 0
C<<
	CALL MVBITS(Z,0,4,SLOT_BITS0,15)	! MASK = 0 Bits 14-18
	CALL MVBITS(Z,0,4,SLOT_BITS1,15)	! MASK = 1 Bits 15-18
	CALL MVBITS(1,0,1,SLOT_BITS1,14)	! MASK = 1 Bit 14
C<<
C  Loop through all the centroid combinations.
C<<
	DO I = 1,NAFINE 		! # A-CENTROIDS
	  DO J = 1,NBFINE		! # B-CENTROIDS
	    DO K = 1,NCFINE     	! # C-CENTROIDS
C<<
C  Calculate M1/M2 addresses.  Fine centroid numbers are in bits 0-7;
C	Module ID's in bits 11-13.
C<<
	      CALL MVBITS(IAND(CFINE(K),127),0,6,M2ADD,0)
	      CALL MVBITS(IAND(BFINE(J),127),0,6,M2ADD,6)
	      CALL MVBITS(IAND(AFINE(I),127),0,6,M2ADD,12)
              CALL MVBITS(IAND(CFINE(K),127),3,3,M1ADD,0)
              CALL MVBITS((CFINE(K)/2048),0,1,M1ADD,3)
              CALL MVBITS(IAND(BFINE(J),127),3,3,M1ADD,4)
              CALL MVBITS((BFINE(J)/2048),0,1,M1ADD,7)
              CALL MVBITS(IAND(AFINE(I),127),3,3,M1ADD,8)
              CALL MVBITS((AFINE(I)/2048),0,1,M1ADD,11)
              CALL MVBITS((CFINE(K)/2048),1,2,M1ADD,12)
              CALL MVBITS((BFINE(J)/2048),1,2,M1ADD,14)
              CALL MVBITS((AFINE(I)/2048),1,2,M1ADD,16)
C<<
C  Do look-up.
C<<
	      READ(MUNIT'M1ADD,ERR=990) M1TRIG		
	      READ(MUNIT'M2ADD,ERR=990) M2TRIG
990	      CONTINUE
C<<
C  Make L1.5 trigger decision.	M1 in lower 16 bits.  M2 in upper 16 bits.
C<<
	      IF(IBITS(M1TRIG,Z,1).EQ.1.AND.IBITS(M2TRIG,(Z+16),1)
     &			.EQ.1) THEN	
C<<
	        NTRIG = NTRIG + 1       !Total # of L1.5 trigs for this octant.
C<<
C  Fill KDAT array according to whether in TEST or DATA mode. See
C	D0 Note #1451 for definitions.
C<<
	        IF(OTC_MODE.EQ.1)THEN	!DATA mode.
C<<
C  Phil Martin number for module 1.  'Phil Martin number' refers to the
C	Arizona/D0 convention of numbering muon modules and OTCs.  I have no
C 	idea who Phil Martin is/was.
C<<
	          K1ADD = (AFINE(I)/2048)
	  	  KADD = IOR(SLOT_BITS0,K1ADD)
	          READ(KUNIT'KADD,ERR=991) TEMP ! COMP A-LAYER MOD #
		  TEMP = IAND(TEMP,255) ! $FF
		  CALL MVBITS(TEMP,0,8,KDAT(NTRIG,1,1),0)
991		  CONTINUE
		  CALL MU_KTAB2MOD(KDAT(NTRIG,1,1),MODNO) ! REAL PHIL MARTIN #
		  KDAT(NTRIG,1,1) = MODNO
C<<
C  Phil Martin number for module 2.
C<<
	          K2ADD = (BFINE(J)/2048)
		  KADD = IOR(SLOT_BITS0,K2ADD)
	          READ(KUNIT'KADD,ERR=992) TEMP ! COMP B-LAYER MOD #
		  TEMP = IAND(TEMP,65280) ! $FF00
		  CALL MVBITS(TEMP,8,8,KDAT(NTRIG,2,1),0)
992		  CONTINUE
		  CALL MU_KTAB2MOD(KDAT(NTRIG,2,1),MODNO) ! REAL PHIL MARTIN #
		  KDAT(NTRIG,2,1) = MODNO
C<<
C  Phil Martin number for module 3.
C<<
	          K3ADD = (CFINE(K)/2048)
		  KADD = IOR(SLOT_BITS0,K3ADD)
	          READ(KUNIT'KADD,ERR=993) TEMP ! COMP C-LAYER MOD #
                  TEMP = IAND(TEMP,16711680) ! $FF0000
                  CALL MVBITS(TEMP,16,8,KDAT(NTRIG,3,1),0)
993		  CONTINUE
		  CALL MU_KTAB2MOD(KDAT(NTRIG,3,1),MODNO) ! REAL PHIL MARTIN #
		  KDAT(NTRIG,3,1) = MODNO
C K table Theta
	          K1ADD = 0
	          CALL MVBITS(IAND(AFINE(I),127),0,6,K1ADD,0)
	          CALL MVBITS((AFINE(I)/2048),0,3,K1ADD,6)
	 	  KADD = IOR(SLOT_BITS1,K1ADD)
	          READ(KUNIT'KADD,ERR=994) TEMP
		  TEMP = IAND(TEMP,255)  		! $FF
		  CALL MVBITS(TEMP,0,8,KDAT(NTRIG,1,2),0)
994		  CONTINUE
C<<
C  K table momentum.
C<<
 	          K2ADD = 0
 	          CALL MVBITS(IAND(CFINE(K),127),0,6,K2ADD,0)
	          CALL MVBITS((CFINE(K)/2048),0,1,K2ADD,6)
	          CALL MVBITS(IAND(BFINE(J),127),0,6,K2ADD,7)
	          CALL MVBITS((BFINE(J)/2048),0,1,K2ADD,13)
	  	  KADD = IOR(SLOT_BITS1,K2ADD)
	          READ(KUNIT'KADD,ERR=995) TEMP
		  TEMP = IAND(TEMP,65280)		! $FF00
		  CALL MVBITS(TEMP,8,8,KDAT(NTRIG,2,2),0)
995 		  CONTINUE
C<<
C K table octant.
	          K3ADD = 6
		  KADD = IOR(SLOT_BITS1,K3ADD)
	          READ(KUNIT'KADD,ERR=996) TEMP
		  TEMP = IAND(TEMP,16711680)	! $FF0000
		  CALL MVBITS(TEMP,16,8,KDAT(NTRIG,3,2),0)
 996		  CONTINUE
C<<
	       ELSE			! Test mode.
		
C  Pack in module numbers 1-5 for each layer.
C<<
	         CALL MVBITS(AFINE(I),11,3,KDAT(NTRIG,1,1),0)
        	 CALL MVBITS(BFINE(J),11,3,KDAT(NTRIG,2,1),0)
       		 CALL MVBITS(CFINE(K),11,3,KDAT(NTRIG,3,1),0)
C<<
C  Pack in fine centroid numbers (1-47) for each layer.
C<<
	         KDAT(NTRIG,1,2) = FLOAT(IAND(AFINE(I),127))
        	 KDAT(NTRIG,2,2) = FLOAT(IAND(BFINE(J),127))
        	 KDAT(NTRIG,3,2) = FLOAT(IAND(CFINE(K),127))
C<<
	       ENDIF 		! Trigger or Test mode.
	      ENDIF 		! Good Trigger?
	    ENDDO   		! Loop over C-layer centroids.
	  ENDDO     		! Loop over B-layer centroids.
	ENDDO	     		! Loop over A-layer centroids.
C<<
 999	RETURN
	END
