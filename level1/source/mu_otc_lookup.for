C===============================================================================
	SUBROUTINE MU_OTC_LOOKUP(NDX,JOTC,AFINE,NAFINE,BFINE,NBFINE,
     &			CFINE,NCFINE,ODX,OTRIG,OTRIG2,OCENT,ODAT)
C===============================================================================
C
C ****** THIS VERSION OF MU_OTC_LOOKUP IS FOR RUN 1B NEW MGR DATA ONLY *********
C
C   Subroutine to model the lookups done by the various types of OTC
C    The index NDX specifies type of look-up to be done.  Code will 
C    return up to 31 triplet or road triggers, and up to 130 kinematic.
C	
C  Procedure: For all OTC's:
C	 - Open all tables.
C	 - Form M1, M2 address.
C	 - Check for trigger.
C	 - Form K table address.
C	 - Look up and write out K table data.
C	 - Where applicable, form centroids for second stage lookup.

C	NDX = 1 = Do pure WAMUS trigger look-up.
C	    = 2 = Do SAMUS A + WAMUS B + WAMUS C trigger look-up.
C	    = 3 = Do SAMUS A + SAMUS B + WAMUS C trigger look-up.
C           = 4 = Do SAMUS overlap triplet A look-up.
C	    = 5 = Do SAMUS overlap triplet B look-up.
C           = 6 = Do pure SAMUS triplet B look-up.(?)
C           = 7 = Do look-up for SAMUS X roads.
C           = 8 = Do look-up for SAMUS Y roads. 
C           = 9 = Do pure SAMUS trigger look-up.

C	INPUTS:
C	------
C	OTC = otc number           
C	NAFINE, AFINE(32) = number and list of a fine centroids 
C	NBFINE, BFINE(32) = number and list of b fine centroids 
C	NCFINE, CFINE(32) = number and list of c fine centroids 
C	ODX = array index = 1,8

C	OUTPUTS:
C	-------
C	OTRIG(ODX) = number of otc triggers found this otc
C	OCENT(ODX,32) = list of otc centroids sent to next stage of otc's
C	ODAT(ODX,2,3,130) = k table DATA for OTRIG(ODX) triggers
              
C	LOCAL DECLARATIONS
C	------------------

	IMPLICIT NONE
	INTEGER I, J, J1, J2, K, L, P1
	INTEGER IREG, IERW, P0UNIT(5), KUNIT(5)
        INTEGER K_UNIT, BC
	INTEGER BCUNIT(5), P1UNIT(5), RFKUNIT, PFAKUNIT, OVKUNIT(3)
	INTEGER SKKUNIT, SBKUNIT, PFAUNIT, OVBKUNIT, CMUNIT(5)
	INTEGER LUN_CR21(5), LUN_CR31(6), LUN_CR41(6)
        INTEGER LUN_CR51(8), LUN_CR61(8)

	INTEGER M1ADD, M2ADD, K1ADD, K2ADD, K3ADD
	INTEGER M1DATA, M2DATA, K1DATA, K2DATA, K3DATA
	INTEGER JOTC, JREG, JQUAD
	INTEGER BCADD, BCDATA, P1ADD, P1DATA, OTRIG2(8)
	INTEGER JOCTANT, JOTC_CRATE,JOTC_SLOT, MOD_OTC(3,5)
	INTEGER NDX, ODX, IER, JOTC_DIP
	INTEGER NAFINE, NBFINE, NCFINE
	INTEGER AFINE(32), BFINE(32), CFINE(32)
	INTEGER OTRIG(12), OCENT(12,130), ODAT(12,2,3,130)

	LOGICAL FIRST, WAMONLY
	CHARACTER*72 STRING
        CHARACTER*32 MESSID,CALLER
        CHARACTER*132 MESSAG

	DATA FIRST/.TRUE./

C-------------------------------------------------------------------------------

	IF(FIRST)THEN
	  FIRST = .FALSE.
          CALL EZPICK('MUSIM_RCP')
	  CALL EZERR(IER)     ! Check if error
	  IF(IER.NE.0) THEN
            CALL EZGET_ERROR_TEXT(IER,STRING)
            CALL ERRMSG(' CANNOT PICK MUSIM_RCP','MU_OTC_LOOKUP',
     &          STRING,'F')
            GOTO 999
          ENDIF

          CALL EZGET('WAMONLY',WAMONLY,IERW)
          IF(IERW.NE.0)THEN
             CALL EZGET_ERROR_TEXT(IERW,STRING)
             CALL ERRMSG(' EZPICK ERR','MU_OTC_LOOKUP',STRING,'F')
             GOTO 999
          ENDIF
	  IF(WAMONLY)THEN 
	     CALLER = 'MU_OTC_LOOKUP'
	     MESSID ='WAMONLY = .TRUE.' 
	     MESSAG=
     &'*** NOTICE: YOU HAVE TURNED OFF OTC TRIGGER LOOKUPS IN OVERLAP 
     &AND SAMUS.  CHECK TRIGGERS TO VERIFY!! ****'
	   CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
	  ENDIF
	ENDIF

C For triggers not using muons beyond y2, turn off overlap and SAMUS.

	IF(WAMONLY.AND.NDX.GT.3)RETURN

C  Get the region (WAMUS or SAMUS) of the particular OTC, and its slot #.
            
	JREG = 0
        CALL MOTNUM(JREG,JOTC_DIP,JOTC,JQUAD)

C  The OTC's input overflow when any of the three inputs receive > 31 centroids.

	IF(NAFINE.GT.31)NAFINE=31
	IF(NBFINE.GT.31)NBFINE=31
	IF(NCFINE.GT.31)NCFINE=31
	
C-------------------------------------------------------------------------------
C  *** NDX = 1 *** Pure WAMUS kinematic look-up.
                                 
	IF(NDX .EQ. 1)THEN
	  DO 311 I = 1, NCFINE
	    DO 211 J = 1, NBFINE
	      BC = 0
	      DO 111 K = 1, NAFINE
	         IF(BC.GT.0.AND.JOTC.EQ.706)GOTO 111
	         IF(BC.GT.0.AND.JOTC.EQ.705)GOTO 111

C  Form M1, M2 addresses.

	         M1ADD = 0
             	 CALL MVBITS(CFINE(I),3,3,M1ADD,0)
             	 CALL MVBITS(CFINE(I),11,1,M1ADD,3)
             	 CALL MVBITS(BFINE(J),3,3,M1ADD,4)
             	 CALL MVBITS(BFINE(J),11,1,M1ADD,7)
             	 CALL MVBITS(AFINE(K),3,3,M1ADD,8)
             	 CALL MVBITS(AFINE(K),11,1,M1ADD,11)
             	 CALL MVBITS(CFINE(I),12,2,M1ADD,12)
             	 CALL MVBITS(BFINE(J),12,2,M1ADD,14)
             	 CALL MVBITS(AFINE(K),12,2,M1ADD,16)
                    
	     	 M2ADD = 0
             	 CALL MVBITS(CFINE(I),0,6,M2ADD,0)
             	 CALL MVBITS(BFINE(J),0,6,M2ADD,6)
             	 CALL MVBITS(AFINE(K),0,6,M2ADD,12)

C  Do the P0 lookup, and make low Pt trigger decision.
       
		 IF(M1ADD.EQ.0.or.M2ADD.EQ.0)GOTO 111

	     	 READ (P0UNIT(JREG)'M1ADD) M1DATA
	     	 READ (P0UNIT(JREG)'M2ADD) M2DATA
	     	 IF(BTEST(M1DATA,JOTC_DIP).AND.
     &			  	BTEST(M2DATA,JOTC_DIP+16))THEN
		    BC=1
	    	    OTRIG(ODX) = OTRIG(ODX) + 1

		    IF(OTRIG(ODX).GT.130)THEN
			OTRIG(ODX) = 130
			GOTO 999
		    ENDIF

C  Good otc trigger, form the Ktable addresses, fill K table arrays.
C   Mask = 0.
	     	    K1ADD = 0
	 	    K2ADD = 0
	     	    K3ADD = 0
		    K1DATA = 0
		    K2DATA = 0
		    K3DATA = 0
	     	    CALL MVBITS(AFINE(K),0,6,K1ADD,0)
	     	    CALL MVBITS(AFINE(K),11,3,K1ADD,11)
	     	    CALL MVBITS(BFINE(J),11,3,K2ADD,0)
	     	    CALL MVBITS(CFINE(I),11,3,K2ADD,3)
	     	    CALL MVBITS(CFINE(I),11,3,K3ADD,0)
	
C Do K table look-up. 

	     	    IF(K1ADD.GT.0)READ (KUNIT(JREG)'K1ADD) K1DATA
	     	    IF(K2ADD.GT.0)READ (KUNIT(JREG)'K2ADD) K2DATA
	     	    IF(K3ADD.GT.0)READ (KUNIT(JREG)'K3ADD) K3DATA

	     	    CALL MVBITS(K1DATA,0,8,ODAT(ODX,1,1,OTRIG(ODX)),0)
	     	    CALL MVBITS(K2DATA,8,8,ODAT(ODX,1,2,OTRIG(ODX)),0)
	     	    CALL MVBITS(K3DATA,16,8,ODAT(ODX,1,3,OTRIG(ODX)),0)
C  Mask = 1
	     	    K1ADD = 0
		    K2ADD = 0
	     	    K3ADD = 0

                    CALL MVBITS(AFINE(K),11,3,K1ADD,11)
                    CALL MVBITS(BFINE(J),11,3,K2ADD,6)
                    CALL MVBITS(CFINE(I),11,3,K3ADD,6)
                    CALL MVBITS(AFINE(K),0,6,K1ADD,0)
                    CALL MVBITS(BFINE(J),0,6,K2ADD,0)
                    CALL MVBITS(CFINE(I),0,6,K3ADD,0)

                    K1ADD = IBSET(K1ADD,14)
                    K2ADD = IBSET(K2ADD,14)
                    K3ADD = IBSET(K3ADD,14)

		    K1DATA = 0
		    K2DATA = 0
		    K3DATA = 0
	     	    IF(K1ADD.GT.0)READ (KUNIT(JREG)'K1ADD) K1DATA
  	     	    IF(K2ADD.GT.0)READ (KUNIT(JREG)'K2ADD) K2DATA
	     	    IF(K3ADD.GT.0)READ (KUNIT(JREG)'K3ADD) K3DATA

	     	    CALL MVBITS(K1DATA,0,8,ODAT(ODX,2,1,OTRIG(ODX)),0)
	     	    CALL MVBITS(K2DATA,8,8,ODAT(ODX,2,2,OTRIG(ODX)),0)
	     	    CALL MVBITS(K3DATA,16,8,ODAT(ODX,2,3,OTRIG(ODX)),0)

C  Feed k2data and k3data through BC lookup.

                    BCADD = 0
                    CALL MVBITS(K2DATA,8,8,BCADD,0)
                    CALL MVBITS(K3DATA,16,8,BCADD,8)

                    READ (BCUNIT(JREG)'BCADD) BCDATA

C  Now feed all of this information into the manager P1 lookup (high Pt).

                    P1ADD = 0
                    CALL MVBITS(BCDATA,0,12,P1ADD,0)
                    CALL MVBITS(K1DATA,0,8,P1ADD,12)

                    READ (P1UNIT(JREG)'P1ADD) P1DATA

                    ODAT(ODX,1,3,OTRIG(ODX)) =
     &                          IBCLR(ODAT(ODX,1,3,OTRIG(ODX)),24)
                    IF(BTEST(P1DATA,JOTC_DIP))THEN
                       OTRIG2(ODX) = OTRIG2(ODX) + 1
	     	       ODAT(ODX,1,3,OTRIG(ODX)) =
     &			IBSET(ODAT(ODX,1,3,OTRIG(ODX)),24)
                    ENDIF                  ! P1 ENDIF
                    
	     	 ENDIF		        ! P0 ENDIF

111	      CONTINUE		      ! NAFINE 
211	    CONTINUE		   ! NBFINE
311 	  CONTINUE		! NCFINE
          
	ENDIF         		! End of pure WAMUS look-up

C-------------------------------------------------------------------------------
C *** NDX = 2 ***  SAMUS A + WAMUS B + WAMUS C Trigger look-up for overlap.

	IF(NDX .EQ. 2)THEN             

	  DO 300 I = 1, NCFINE
	    DO 200 J = 1, NBFINE
	      DO 100 K = 1, NAFINE
		M1ADD = 0
        	CALL MVBITS(CFINE(I),3,3,M1ADD,0)
        	CALL MVBITS(CFINE(I),11,1,M1ADD,3)
        	CALL MVBITS(BFINE(J),3,3,M1ADD,4)
        	CALL MVBITS(BFINE(J),11,1,M1ADD,7)
                CALL MVBITS(AFINE(K),3,3,M1ADD,8)
                CALL MVBITS(AFINE(K),11,1,M1ADD,11)
        	CALL MVBITS(CFINE(I),12,2,M1ADD,12)
        	CALL MVBITS(BFINE(J),12,2,M1ADD,14)
        	CALL MVBITS(AFINE(K),12,2,M1ADD,16)
                    
		M2ADD = 0
        	CALL MVBITS(CFINE(I),0,6,M2ADD,0)
        	CALL MVBITS(BFINE(J),0,6,M2ADD,6)
       		CALL MVBITS(AFINE(K),0,6,M2ADD,12)

C  Do the P0 lookup and make low Pt trigger decision.
       
	        IF(M1ADD.EQ.0.or.M2ADD.EQ.0)GOTO 100

		READ (P0UNIT(JREG)'M1ADD) M1DATA
		READ (P0UNIT(JREG)'M2ADD) M2DATA
		IF(BTEST(M1DATA,JOTC_DIP) .AND. 
     &      	   BTEST(M2DATA,JOTC_DIP+16)) THEN
	  	   OTRIG(ODX) = OTRIG(ODX) + 1
		   IF(OTRIG(ODX).GT.130)THEN
		      OTRIG(ODX)=130
		      GOTO 999
		   ENDIF

C  Good otc trigger so fill K table arrays.
C   Mask = 0
		   K1ADD = 0
		   K2ADD = 0
		   K3ADD = 0
		   CALL MVBITS(AFINE(K),0,6,K1ADD,0)
		   CALL MVBITS(AFINE(K),11,3,K1ADD,11)

		   CALL MVBITS(BFINE(J),11,3,K2ADD,0)
		   CALL MVBITS(CFINE(I),11,3,K2ADD,3)

		   CALL MVBITS(CFINE(I),11,3,K3ADD,0)

C Read back Mask 0 Ktable info.

		   K1DATA = 0
		   K2DATA = 0
		   K3DATA = 0
	     	   IF(K1ADD.GT.0)READ(OVKUNIT(JREG)'K1ADD) K1DATA
  	     	   IF(K2ADD.GT.0)READ(OVKUNIT(JREG)'K2ADD) K2DATA
	     	   IF(K3ADD.GT.0)READ(OVKUNIT(JREG)'K3ADD) K3DATA

		   CALL MVBITS(K1DATA,0,8,ODAT(ODX,1,1,OTRIG(ODX)),0)
		   CALL MVBITS(K2DATA,8,8,ODAT(ODX,1,2,OTRIG(ODX)),0)
		   CALL MVBITS(K3DATA,16,8,ODAT(ODX,1,3,OTRIG(ODX)),0)
C  Mask = 1
		   K1ADD = 0
		   K2ADD = 0
		   K3ADD = 0

		   CALL MVBITS(AFINE(K),0,6,K1ADD,0)
		   CALL MVBITS(AFINE(K),11,3,K1ADD,11)

   	 	   CALL MVBITS(BFINE(J),0,6,K2ADD,0)
	 	   CALL MVBITS(BFINE(J),11,3,K2ADD,6)

		   CALL MVBITS(CFINE(I),0,6,K3ADD,0)
		   CALL MVBITS(CFINE(I),11,3,K3ADD,6)

		   K1ADD = IBSET(K1ADD,14)
		   K2ADD = IBSET(K2ADD,14)
		   K3ADD = IBSET(K3ADD,14)

C Read back mask 1 Ktable info.                

		    K1DATA = 0
		    K2DATA = 0
		    K3DATA = 0
	     	   IF(K1ADD.GT.0)READ(OVKUNIT(JREG)'K1ADD) K1DATA
  	     	   IF(K2ADD.GT.0)READ(OVKUNIT(JREG)'K2ADD) K2DATA
	     	   IF(K3ADD.GT.0)READ(OVKUNIT(JREG)'K3ADD) K3DATA

		   CALL MVBITS(K1DATA,0,8,ODAT(ODX,2,1,OTRIG(ODX)),0)
		   CALL MVBITS(K2DATA,8,8,ODAT(ODX,2,2,OTRIG(ODX)),0)
		   CALL MVBITS(K3DATA,16,8,ODAT(ODX,2,3,OTRIG(ODX)),0)

C  Feed k2data and k3data through BC lookup.

                    BCADD = 0
                    CALL MVBITS(K2DATA,0,8,BCADD,0)
                    CALL MVBITS(K3DATA,0,8,BCADD,8)

                    READ (BCUNIT(JREG)'BCADD) BCDATA

C  Now feed all of this information into manager P1 lookup (high Pt).

                    P1ADD = 0
                    CALL MVBITS(BCDATA,0,12,P1ADD,0)
                    CALL MVBITS(K1DATA,0,8,P1ADD,12)

                    READ (P1UNIT(JREG)'P1ADD) P1DATA

                   ODAT(ODX,1,3,OTRIG(ODX)) =
     &                          IBCLR(ODAT(ODX,1,3,OTRIG(ODX)),24)
                    IF(BTEST(P1DATA,JOTC_DIP))THEN
                       OTRIG2(ODX) = OTRIG2(ODX) + 1
		       ODAT(ODX,1,3,OTRIG(ODX)) = 
     &			IBSET(ODAT(ODX,1,3,OTRIG(ODX)),24)
                    ENDIF                  ! P1 ENDIF
  
	        ENDIF		        ! P0 ENDIF
100	      CONTINUE		      ! NAFINE 
200	     CONTINUE		    ! NBFINE
300  	  CONTINUE                ! NCFINE

	ENDIF         		! NDX = 2

C------------------------------------------------------------------------------
C **** NDX = 3 *** SAMUS A + SAMUS B + WAMUS C trigger look-up for overlap.

	IF(NDX .EQ. 3)THEN
	   DO 301 I = 1, NCFINE
	     DO 201 J = 1, NBFINE
	       DO 101 K = 1, NAFINE

		 M1ADD = 0
        	 CALL MVBITS(CFINE(I),3,3,M1ADD,0)
        	 CALL MVBITS(CFINE(I),11,1,M1ADD,3)
                 CALL MVBITS(BFINE(J),3,4,M1ADD,4)
                 CALL MVBITS(AFINE(K),3,4,M1ADD,8)
       	 	 CALL MVBITS(CFINE(I),12,2,M1ADD,12)
        	 CALL MVBITS(BFINE(J),12,2,M1ADD,14)
        	 CALL MVBITS(AFINE(K),12,2,M1ADD,16)

		 M2ADD = 0
        	 CALL MVBITS(CFINE(I),0,6,M2ADD,0)
        	 CALL MVBITS(BFINE(j),0,6,M2ADD,6)
        	 CALL MVBITS(AFINE(K),0,6,M2ADD,12)

C   Do the P0 (low Pt) lookup.
       
	         IF(M1ADD.EQ.0.or.M2ADD.EQ.0)GOTO 101

		 READ (P0UNIT(JREG)'M1ADD) M1DATA
		 READ (P0UNIT(JREG)'M2ADD) M2DATA

		 IF(BTEST(M1DATA,JOTC_DIP) .AND. 
     &      		BTEST(M2DATA,JOTC_DIP+16)) THEN
	  	   OTRIG(ODX) = OTRIG(ODX) + 1
		   IF(OTRIG(ODX).GT.130)THEN
		      OTRIG(ODX)=130
		      GOTO 999
		   ENDIF

C  Good otc trigger so fill k table arrays.
C    Mask = 0
		   K1ADD = 0
		   K2ADD = 0
		   K3ADD = 0
		   CALL MVBITS(AFINE(K),0,6,K1ADD,0)
		   CALL MVBITS(AFINE(K),11,3,K1ADD,11)

		   CALL MVBITS(BFINE(J),11,3,K2ADD,0)
		   CALL MVBITS(CFINE(I),11,3,K2ADD,3)

		   CALL MVBITS(CFINE(I),11,3,K3ADD,0)

		   K1DATA = 0
		   K2DATA = 0
		   K3DATA = 0
	     	   IF(K1ADD.GT.0)READ(OVKUNIT(JREG)'K1ADD) K1DATA
  	     	   IF(K2ADD.GT.0)READ(OVKUNIT(JREG)'K2ADD) K2DATA
	     	   IF(K3ADD.GT.0)READ(OVKUNIT(JREG)'K3ADD) K3DATA

		   CALL MVBITS(K1DATA,0,8,ODAT(ODX,1,1,OTRIG(ODX)),0)
		   CALL MVBITS(K2DATA,8,8,ODAT(ODX,1,2,OTRIG(ODX)),0)
		   CALL MVBITS(K3DATA,16,8,ODAT(ODX,1,3,OTRIG(ODX)),0)
C  Mask = 1
		   K1ADD = 0
		   K2ADD = 0
		   K3ADD = 0

		   CALL MVBITS(AFINE(K),0,7,K1ADD,0)
		   CALL MVBITS(AFINE(K),11,3,K1ADD,11)

		   CALL MVBITS(BFINE(J),0,6,K2ADD,0)
		   CALL MVBITS(BFINE(J),11,3,K2ADD,6)

		   CALL MVBITS(CFINE(I),0,6,K3ADD,0)
		   CALL MVBITS(CFINE(I),11,3,K3ADD,6)

		   K1ADD = IBSET(K1ADD,14)
		   K2ADD = IBSET(K2ADD,14)
		   K3ADD = IBSET(K3ADD,14)

		   K1DATA = 0
		   K2DATA = 0
		   K3DATA = 0
	     	   IF(K1ADD.GT.0)READ(OVKUNIT(JREG)'K1ADD) K1DATA
  	     	   IF(K2ADD.GT.0)READ(OVKUNIT(JREG)'K2ADD) K2DATA
	     	   IF(K3ADD.GT.0)READ(OVKUNIT(JREG)'K3ADD) K3DATA

		   CALL MVBITS(K1DATA,0,8,ODAT(ODX,2,1,OTRIG(ODX)),0)
		   CALL MVBITS(K2DATA,8,8,ODAT(ODX,2,2,OTRIG(ODX)),0)
		   CALL MVBITS(K3DATA,16,8,ODAT(ODX,2,3,OTRIG(ODX)),0)

C  Feed k2data and k3data through BC lookup.

                    BCADD = 0
                    CALL MVBITS(K2DATA,0,8,BCADD,0)
                    CALL MVBITS(K3DATA,0,8,BCADD,8)

                    READ (BCUNIT(JREG)'BCADD) BCDATA

C  Now feed all of this information into manager P1 lookup (high Pt).

                    P1ADD = 0
                    CALL MVBITS(BCDATA,0,12,P1ADD,0)
                    CALL MVBITS(K1DATA,0,8,P1ADD,12)

                    READ (P1UNIT(JREG)'P1ADD) P1DATA

                   ODAT(ODX,1,3,OTRIG(ODX)) =
     &                          IBCLR(ODAT(ODX,1,3,OTRIG(ODX)),24)
                    IF(BTEST(P1DATA,JOTC_DIP))THEN
                       OTRIG2(ODX) = OTRIG2(ODX) + 1
	     	       ODAT(ODX,1,3,OTRIG(ODX)) = 
     &			IBSET(ODAT(ODX,1,3,OTRIG(ODX)),24)
                    ENDIF                   ! P1 ENDIF

		ENDIF			  ! P0 ENDIF

101	      CONTINUE			! NAFINE 
201	    CONTINUE                  ! NBFINE
301	  CONTINUE                  ! NCFINE

	ENDIF         		  ! NDX = 3
C-------------------------------------------------------------------------------
C  *** NDX = 4, 5. **** SAMUS overlap A and B triplet look-up.

	IF(NDX.EQ.4. OR .NDX.EQ.5)THEN
                             
	  DO 302 I = 1, NCFINE               
	    DO 202 J = 1, NBFINE
	      DO 102 K = 1, NAFINE
                            
		M1ADD = 0
        	CALL MVBITS(CFINE(I),2,4,M1ADD,0)
        	CALL MVBITS(BFINE(J),2,4,M1ADD,4)
        	CALL MVBITS(AFINE(K),2,4,M1ADD,8)
        	CALL MVBITS(CFINE(I),6,1,M1ADD,12)
        	CALL MVBITS(CFINE(I),11,1,M1ADD,13)
        	CALL MVBITS(BFINE(J),6,1,M1ADD,14)
        	CALL MVBITS(BFINE(J),11,1,M1ADD,15)
        	CALL MVBITS(AFINE(K),6,1,M1ADD,16)
        	CALL MVBITS(AFINE(K),11,1,M1ADD,17)
                    
		M2ADD = 0
        	CALL MVBITS(CFINE(I),0,6,M2ADD,0)
        	CALL MVBITS(BFINE(J),0,6,M2ADD,6)
        	CALL MVBITS(AFINE(K),0,6,M2ADD,12)

C  Do the M1,M2 lookups.
       
	        IF(M1ADD.EQ.0.or.M2ADD.EQ.0)GOTO 102

		READ (P0UNIT(JREG)'M1ADD) M1DATA
		READ (P0UNIT(JREG)'M2ADD) M2DATA

		IF(BTEST(M1DATA,JOTC_DIP) .AND. 
     &      		BTEST(M2DATA,JOTC_DIP+16))THEN
	  	   OTRIG(ODX) = OTRIG(ODX) + 1

C Once you have 31 triggers, stop processing.

		   IF(OTRIG(ODX).GT.31)THEN
		      OTRIG(ODX) = 31
		      GOTO 999
		   ENDIF

C  Good otc trigger so fill k table arrays.
C  Mask = 0
		  K1ADD = 0
		  K2ADD = 0
 		  K3ADD = 0
                  CALL MVBITS(AFINE(K),0,7,K1ADD,0)
		  CALL MVBITS(AFINE(K),11,3,K1ADD,11)

		  CALL MVBITS(BFINE(J),1,6,K2ADD,0)
		  CALL MVBITS(BFINE(J),11,1,K2ADD,6)
		  CALL MVBITS(CFINE(I),1,6,K2ADD,7)
		  CALL MVBITS(CFINE(I),11,1,K2ADD,13)

		  CALL MVBITS(CFINE(I),0,7,K3ADD,0)
		  CALL MVBITS(BFINE(J),0,7,K3ADD,7)

		  IF(NDX.EQ.4)K_UNIT = PFAKUNIT
		  IF(NDX.EQ.5)K_UNIT = OVBKUNIT

		  K1DATA = 0
		  K2DATA = 0
		  K3DATA = 0
	 	  IF(K1ADD.GT.0)READ(K_UNIT'K1ADD) K1DATA
		  IF(K2ADD.GT.0)READ(K_UNIT'K2ADD) K2DATA
		  IF(K3ADD.GT.0)READ(K_UNIT'K3ADD) K3DATA

		  CALL MVBITS(K1DATA,0,8,ODAT(ODX,1,1,OTRIG(ODX)),0)
		  CALL MVBITS(K2DATA,8,8,ODAT(ODX,1,2,OTRIG(ODX)),0)
		  CALL MVBITS(K3DATA,16,8,ODAT(ODX,1,3,OTRIG(ODX)),0)
C  Mask = 1.
		  K1ADD = 0
		  K2ADD = 0
		  K3ADD = 0

                  CALL MVBITS(AFINE(K),0,7,K1ADD,0)
		  CALL MVBITS(AFINE(K),11,3,K1ADD,11)

		  CALL MVBITS(BFINE(J),0,7,K2ADD,0)
                  CALL MVBITS(BFINE(J),11,1,K2ADD,7)

		  CALL MVBITS(CFINE(I),0,7,K3ADD,0)
                  CALL MVBITS(CFINE(I),11,1,K3ADD,7)

		  K1ADD = IBSET(K1ADD,14)
		  K2ADD = IBSET(K2ADD,14)
		  K3ADD = IBSET(K3ADD,14)

                  K1DATA = 0
		  K2DATA = 0
		  K3DATA = 0
	 	  IF(K1ADD.GT.0)READ(K_UNIT'K1ADD) K1DATA
		  IF(K2ADD.GT.0)READ(K_UNIT'K2ADD) K2DATA
		  IF(K3ADD.GT.0)READ(K_UNIT'K3ADD) K3DATA

		  CALL MVBITS(K1DATA,0,8,ODAT(ODX,2,1,OTRIG(ODX)),0)
		  CALL MVBITS(K2DATA,8,8,ODAT(ODX,2,2,OTRIG(ODX)),0)
		  CALL MVBITS(K3DATA,16,8,ODAT(ODX,2,3,OTRIG(ODX)),0)

C  Send centroids here.

		  CALL MVBITS(ODAT(ODX,1,3,OTRIG(ODX)),0,7,
     &				OCENT(ODX,OTRIG(ODX)),0)
		  CALL MVBITS(ODAT(ODX,1,2,OTRIG(ODX)),0,7,
     &				OCENT(ODX,OTRIG(ODX)),7)

	        ENDIF			! BTEST ENDIF
102	      CONTINUE		    ! NAFINE 
202	    CONTINUE              ! NBFINE
302	  CONTINUE		! NCFINE
                                        
	ENDIF        

C------------------------------------------------------------------------------
C  *** NDX = 6 *** SAMUS B triplet look-up for pure SAMUS.
	
	IF (NDX .EQ. 6) THEN

	DO 303 I = 1,NCFINE               
	  DO 203 J = 1,NBFINE
	    DO 103 K = 1,NAFINE
		M1ADD = 0
        	CALL MVBITS(CFINE(I),2,4,M1ADD,0)
        	CALL MVBITS(BFINE(J),2,4,M1ADD,4)
        	CALL MVBITS(AFINE(K),2,4,M1ADD,8)
        	CALL MVBITS(CFINE(I),6,1,M1ADD,12)
        	CALL MVBITS(CFINE(I),11,1,M1ADD,13)
        	CALL MVBITS(BFINE(J),6,1,M1ADD,14)
        	CALL MVBITS(BFINE(J),11,1,M1ADD,15)
        	CALL MVBITS(AFINE(K),6,1,M1ADD,16)               
        	CALL MVBITS(AFINE(K),11,1,M1ADD,17)
                    
		M2ADD = 0
        	CALL MVBITS(CFINE(I),0,6,M2ADD,0)
        	CALL MVBITS(BFINE(J),0,6,M2ADD,6)
        	CALL MVBITS(AFINE(K),0,6,M2ADD,12)

C  Do the M table lookups.
       
	        IF(M1ADD.EQ.0.or.M2ADD.EQ.0)GOTO 103
		READ (P0UNIT(JREG)'M1ADD) M1DATA
		READ (P0UNIT(JREG)'M2ADD) M2DATA
		IF(BTEST(M1DATA,JOTC_DIP).AND. 
     &      		BTEST(M2DATA,JOTC_DIP+16)) THEN
	  	  OTRIG(ODX) = OTRIG(ODX) + 1
                  IF(OTRIG(ODX).GT.31)THEN
		     OTRIG(ODX) = 31
		     GOTO 999
		  ENDIF

C  Good otc trigger so fill k table arrays.
C    Mask = 0
		  K1ADD = 0
		  K2ADD = 0
 		  K3ADD = 0
		  CALL MVBITS(AFINE(K),0,7,K1ADD,0)
		  CALL MVBITS(AFINE(K),11,3,K1ADD,11)
		  CALL MVBITS(BFINE(J),0,7,K2ADD,0)
		  CALL MVBITS(BFINE(J),11,1,K2ADD,11)
		  CALL MVBITS(CFINE(I),0,7,K3ADD,0)
		  CALL MVBITS(CFINE(I),11,1,K3ADD,11)
		
		  K1DATA = 0
		  K2DATA = 0
		  K3DATA = 0
		  IF(K1ADD.GT.0)READ (SBKUNIT'K1ADD) K1DATA
		  IF(K2ADD.GT.0)READ (SBKUNIT'K2ADD) K2DATA
		  IF(K3ADD.GT.0)READ (SBKUNIT'K3ADD) K3DATA

		  CALL MVBITS(K1DATA,0,8,ODAT(ODX,1,1,OTRIG(ODX)),0)
		  CALL MVBITS(K2DATA,8,8,ODAT(ODX,1,2,OTRIG(ODX)),0)
		  CALL MVBITS(K3DATA,16,8,ODAT(ODX,1,3,OTRIG(ODX)),0)
C  Mask = 1.
		  K1ADD = 0
		  K2ADD = 0
		  K3ADD = 0

		  CALL MVBITS(AFINE(K),0,7,K1ADD,0)
		  CALL MVBITS(AFINE(K),11,3,K1ADD,11)

                  CALL MVBITS(BFINE(J),11,1,K2ADD,11)
                  CALL MVBITS(CFINE(I),11,1,K3ADD,11)

		  K1ADD = IBSET(K1ADD,14)
		  K2ADD = IBSET(K2ADD,14)
		  K3ADD = IBSET(K3ADD,14)
                       
		  K1DATA = 0
		  K2DATA = 0
		  K3DATA = 0
		  IF(K1ADD.GT.0)READ (SBKUNIT'K1ADD) K1DATA
		  IF(K2ADD.GT.0)READ (SBKUNIT'K2ADD) K2DATA
		  IF(K3ADD.GT.0)READ (SBKUNIT'K3ADD) K3DATA

		  CALL MVBITS(K1DATA,0,8,ODAT(ODX,2,1,OTRIG(ODX)),0)
		  CALL MVBITS(K2DATA,8,8,ODAT(ODX,2,2,OTRIG(ODX)),0)
		  CALL MVBITS(K3DATA,16,8,ODAT(ODX,2,3,OTRIG(ODX)),0)

C  Send centroids here.
                          
	          CALL MVBITS(ODAT(ODX,1,3,OTRIG(ODX)),0,7,
     &			OCENT(ODX,OTRIG(ODX)),0)
		  CALL MVBITS(ODAT(ODX,1,2,OTRIG(ODX)),0,7,
     &			OCENT(ODX,OTRIG(ODX)),7)

	      ENDIF		! BTEST ENDIF

103	    CONTINUE	    ! NAFINE 
203	  CONTINUE        ! NBFINE
303	CONTINUE	! NCFINE
                                        
	ENDIF        

C-------------------------------------------------------------------------------
C  *** NDX = 7,8 ***  SAMUS X and Y roads.

 	IF (NDX .EQ. 7 .or. NDX .EQ. 8) THEN

	  DO 304 I = 1,NCFINE               
	    DO 204 J = 1,NBFINE
	      DO 104 K = 1,NAFINE
                            
		M1ADD = 0
        	CALL MVBITS(CFINE(I),2,4,M1ADD,0)
        	CALL MVBITS(BFINE(J),2,4,M1ADD,4)
        	CALL MVBITS(AFINE(K),2,4,M1ADD,8)
        	CALL MVBITS(CFINE(I),6,1,M1ADD,12)
        	CALL MVBITS(CFINE(I),11,1,M1ADD,13)
       	 	CALL MVBITS(BFINE(J),6,1,M1ADD,14)
        	CALL MVBITS(BFINE(J),11,1,M1ADD,15)
        	CALL MVBITS(AFINE(K),6,1,M1ADD,16)
        	CALL MVBITS(AFINE(K),11,1,M1ADD,17)
                    
		M2ADD = 0
       	 	CALL MVBITS(CFINE(I),0,6,M2ADD,0)
        	CALL MVBITS(BFINE(J),0,6,M2ADD,6)
        	CALL MVBITS(AFINE(K),0,6,M2ADD,12)

C  Do the M table lookups.
       
	        IF(M1ADD.EQ.0.or.M2ADD.EQ.0)GOTO 104
		READ (P0UNIT(JREG)'M1ADD) M1DATA
		READ (P0UNIT(JREG)'M2ADD) M2DATA

		IF(BTEST(M1DATA,JOTC_DIP) .AND. 
     &      	  		BTEST(M2DATA,JOTC_DIP+16)) THEN
	  	  OTRIG(ODX) = OTRIG(ODX) + 1
		  IF(OTRIG(ODX).GT.31)THEN
		     OTRIG(ODX) = 31
                     GOTO 999
		  ENDIF

C  Good otc trigger so fill k table arrays.
C    Mask = 0.
		  K1ADD = 0
		  K2ADD = 0
 		  K3ADD = 0
		  CALL MVBITS(AFINE(K),0,7,K1ADD,0)
		  CALL MVBITS(AFINE(K),11,3,K1ADD,11)
		  CALL MVBITS(CFINE(I),1,6,K2ADD,0)	
		  CALL MVBITS(CFINE(I),11,1,K2ADD,6)	
		  CALL MVBITS(BFINE(J),1,6,K2ADD,7)	
		  CALL MVBITS(BFINE(J),11,1,K2ADD,13)	
		  CALL MVBITS(BFINE(J),0,7,K3ADD,0)	
		  CALL MVBITS(BFINE(J),11,1,K3ADD,11)	

		  K1DATA = 0
		  K2DATA = 0
		  K3DATA = 0
		  IF(K1ADD.GT.0)READ (RFKUNIT'K1ADD) K1DATA
		  IF(K2ADD.GT.0)READ (RFKUNIT'K2ADD) K2DATA
		  IF(K3ADD.GT.0)READ (RFKUNIT'K3ADD) K3DATA

		  CALL MVBITS(K1DATA,0,8,ODAT(ODX,1,1,OTRIG(ODX)),0)
		  CALL MVBITS(K2DATA,8,8,ODAT(ODX,1,2,OTRIG(ODX)),0)
		  CALL MVBITS(K3DATA,16,8,ODAT(ODX,1,3,OTRIG(ODX)),0)
C  Mask = 1.
		  K1ADD = 0
		  K2ADD = 0
		  K3ADD = 0

		  CALL MVBITS(AFINE(K),0,7,K1ADD,0)
		  CALL MVBITS(AFINE(K),11,3,K1ADD,11)
		  CALL MVBITS(BFINE(J),0,7,K2ADD,0)
                  CALL MVBITS(BFINE(J),11,1,K2ADD,11)	
	  	  CALL MVBITS(CFINE(I),0,7,K3ADD,0)
                  CALL MVBITS(CFINE(I),11,1,K3ADD,11)

		  K1ADD = IBSET(K1ADD,14)
		  K2ADD = IBSET(K2ADD,14)
		  K3ADD = IBSET(K3ADD,14)
                       
		  K1DATA = 0
		  K2DATA = 0
		  K3DATA = 0
		  IF(K1ADD.GT.0)READ (RFKUNIT'K1ADD) K1DATA
		  IF(K2ADD.GT.0)READ (RFKUNIT'K2ADD) K2DATA
		  IF(K3ADD.GT.0)READ (RFKUNIT'K3ADD) K3DATA

		  CALL MVBITS(K1DATA,0,8,ODAT(ODX,2,1,OTRIG(ODX)),0)
		  CALL MVBITS(K2DATA,8,8,ODAT(ODX,2,2,OTRIG(ODX)),0)
		  CALL MVBITS(K3DATA,16,8,ODAT(ODX,2,3,OTRIG(ODX)),0)

c  Send centroids here.
                          
		  CALL MVBITS(ODAT(ODX,1,3,OTRIG(ODX)),0,7,
     &				OCENT(ODX,OTRIG(ODX)),0)
		  CALL MVBITS(ODAT(ODX,1,2,OTRIG(ODX)),0,7,
     &				OCENT(ODX,OTRIG(ODX)),7)

		ENDIF		      ! BTEST ENDIF

104	      CONTINUE 		    ! NAFINE 
204	    CONTINUE	 	  ! NBFINE
304	  CONTINUE		! NCFINE
	ENDIF        	      ! NDX = 6, 7

C-------------------------------------------------------------------------------
C *** NDX = 9 *** SAMUS kinematic trigger look-up.   
C There is only the low Pt, P0 lookup for SAMUS.

	IF (NDX .EQ. 9) THEN
	  DO 305 I = 1, NCFINE               
	    DO 205 J = 1, NBFINE
	      DO 105 K = 1, NAFINE
 		M1ADD = 0
        	CALL MVBITS(CFINE(I),0,4,M1ADD,0)
	        CALL MVBITS(BFINE(J),6,1,M1ADD,4)
        	CALL MVBITS(AFINE(K),6,1,M1ADD,5)
	        CALL MVBITS(AFINE(K),7,4,M1ADD,8)
        	CALL MVBITS(CFINE(I),4,3,M1ADD,12)
	        CALL MVBITS(AFINE(K),11,3,M1ADD,15)
                    
		M2ADD = 0
	        CALL MVBITS(BFINE(J),0,6,M2ADD,6)
        	CALL MVBITS(AFINE(K),0,6,M2ADD,12)

C  Do the M1,M2 lookups.
       
	        IF(M1ADD.EQ.0.or.M2ADD.EQ.0)GOTO 105
		READ (P0UNIT(JREG)'M1ADD) M1DATA
		READ (P0UNIT(JREG)'M2ADD) M2DATA

		IF(BTEST(M1DATA,JOTC_DIP) .AND. 
     &			   BTEST(M2DATA,JOTC_DIP+16)) THEN
		   OTRIG(ODX) = OTRIG(ODX) + 1
		   IF(OTRIG(ODX).GT.130)THEN
		      OTRIG(ODX)=130
		      GOTO 999
		   ENDIF

C  Good otc trigger so fill k table arrays.
C  Mask = 0
		   K1ADD = 0
		   K2ADD = 0
	 	   K3ADD = 0
	   	   CALL MVBITS(AFINE(K),0,13,K1ADD,0)
		   CALL MVBITS(BFINE(J),0,7,K2ADD,0)
		   CALL MVBITS(CFINE(I),0,7,K3ADD,0)

		   K1DATA = 0
		   K2DATA = 0
		   K3DATA = 0
		   IF(K1ADD.GT.0)READ (SKKUNIT'K1ADD) K1DATA
		   IF(K2ADD.GT.0)READ (SKKUNIT'K2ADD) K2DATA
		   IF(K3ADD.GT.0)READ (SKKUNIT'K3ADD) K3DATA

		   CALL MVBITS(K1DATA,0,8,ODAT(ODX,1,1,OTRIG(ODX)),0)
		   CALL MVBITS(K2DATA,8,8,ODAT(ODX,1,2,OTRIG(ODX)),0)
		   CALL MVBITS(K3DATA,16,8,ODAT(ODX,1,3,OTRIG(ODX)),0)
C  Mask = 1.
		   K1ADD = 0
		   K2ADD = 0
		   K3ADD = 0

		   CALL MVBITS(AFINE(K),0,13,K1ADD,0)
		   CALL MVBITS(BFINE(J),7,2,K2ADD,0)
		   CALL MVBITS(CFINE(I),7,2,K2ADD,7)
		   CALL MVBITS(BFINE(J),6,1,K3ADD,0)
		   CALL MVBITS(CFINE(I),6,1,K3ADD,1)

		   K1ADD = IBSET(K1ADD,14)
		   K2ADD = IBSET(K2ADD,14)
		   K3ADD = IBSET(K3ADD,14)

		   K1DATA = 0
		   K2DATA = 0
		   K3DATA = 0
		   IF(K1ADD.GT.0)READ (SKKUNIT'K1ADD) K1DATA
		   IF(K2ADD.GT.0)READ (SKKUNIT'K2ADD) K2DATA
		   IF(K3ADD.GT.0)READ (SKKUNIT'K3ADD) K3DATA

		   CALL MVBITS(K1DATA,0,8,ODAT(ODX,2,1,OTRIG(ODX)),0)
		   CALL MVBITS(K2DATA,8,8,ODAT(ODX,2,2,OTRIG(ODX)),0)
		   CALL MVBITS(K3DATA,16,8,ODAT(ODX,2,3,OTRIG(ODX)),0)

		ENDIF		      ! BTEST ENDIF

105           CONTINUE		    ! NAFINE 
205	    CONTINUE              ! NBFINE
305	  CONTINUE		! NCFINE
	ENDIF         	      ! NDX = 9

999	RETURN
C------------------------------------------------------------------------------
C-- entry point to set the logical unit numbers for reading
C-- the M and K OTC tables
      ENTRY MU_OTC_LUN_SET(LUN_CR21,LUN_CR31,LUN_CR41,LUN_CR51,
     &				LUN_CR61)

	P0UNIT(1) = LUN_CR21(1)		!P0 or M table for CF Kin
	P0UNIT(2) = LUN_CR31(1)		!P0 or M table for EFN and OVN 
	P0UNIT(3) = LUN_CR41(1)		!P0 or M table for EFS and OVS
	P0UNIT(4) = LUN_CR51(1)		!P0 or M table for SAMUS North
	P0UNIT(5) = LUN_CR61(1)		!P0 or M table for SAMUS South

	P1UNIT(1) = LUN_CR21(4)		!P1 table for CF Kin
	P1UNIT(2) = LUN_CR31(5)		!P1 table for EFN Kin
	P1UNIT(3) = LUN_CR41(5)		!P1 table for EFS Kin
C	P1UNIT(4) = LUN_CR51(9)		!P1 table for SAMUS North Kin
C	P1UNIT(5) = LUN_CR61(9)		!P1 tabel for SAMUS South Kin

	BCUNIT(1) = LUN_CR21(3)
	BCUNIT(2) = LUN_CR31(4)
	BCUNIT(3) = LUN_CR41(4)
	BCUNIT(4) = LUN_CR51(2)
	BCUNIT(5) = LUN_CR61(2)
 
	KUNIT(1)  = LUN_CR21(2)		!Ktable for CF Kin
	KUNIT(2)  = LUN_CR31(2)		!Ktable for EFN Kin
	KUNIT(3)  = LUN_CR41(2)		!Ktable for EFS Kin

	OVKUNIT(2) = LUN_CR31(3)	!Ktable for OVN kin
	OVKUNIT(3) = LUN_CR41(3)	!Ktable for OVS Kin

	PFAKUNIT = LUN_CR51(3)		!Ktable for overlap SAMUS A PF
	OVBKUNIT = LUN_CR51(4)		!Ktable for overlap SAMUS B PF
	SBKUNIT  = LUN_CR51(5)		!Ktable for pure SAMUS B PF
	RFKUNIT  = LUN_CR51(6)		!Ktable for SAMUS road find
	SKKUNIT  = LUN_CR51(7)		!Ktable for pure SAMUS kin
C-------------------------------------------------------------------------
	END
