      SUBROUTINE CL2_ROTOW_ETNOM(ETAMIN,ETAMAX,PHIMIN,PHIMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert specified channels to nominal ET
C-      put results into level2 CAEP bank in the FILT path, and set pointer
C-      array PTCAEP2 so that it is zero if the cell does not exist, or was
C-      zero supressed
C-
C-      The conversion is made fast by
C-              a) limited precision guarantee (1-2%)
C-              b) using precomputed tables
C-              c) doing limited areas at a time
C-
C-      CL2_ROTOW_ETNOM and CL2_ICDMG_ETNOM operate cooperatively.  They both
C-      convert one trigger tower at a time, and record that the conversion has
C-      taken place in TTDONE.  They both put converted data into CAEP, and
C-      point to it with PTCAEP2.  Thus, both may initialize PTCAEP2 (which
C-      they do a trigger tower at a time).  Therefore, they check whether
C-      they are the first to put data into PTCAEP2 for this tower, and only
C-      zero it if the other routine has not zeroed the tower already.
C-
C-      Further, CL2_CAEPFL also manages PTCAEP2.
C-
C-      The 32 trigger towers in phi are encoded by the 32 bits in a word of
C-      TTDONE.
C-
C-   Inputs  : ETAMIN,ETAMAX,PHIMIN,PHIMAX
C-              range of readout towers to be converted
C-   Outputs : CAEP,PTCAEP2
C-   Controls: (Shared among CL2_ROTOW_ETNOM,CL2_ICDMG_ETNOM,CL2_CAEPFL)
C-              CL2CAEP_EVT is the event for which the PTCAEP2 pointers are
C-                  valid (but only in the area converted!!)
C-              TTDONE shows the trigger towers for which conversion has been
C-                  done
C-
C-   Created   1-NOV-1990   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ETAMIN,ETAMAX,PHIMIN,PHIMAX       ! bounds in OFFLINE coords
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF' ! pointer array, 
                                                   !perhaps zero'd here
      INCLUDE 'D0$INC:PHTT.INC'         ! trigger towers containing rotows
      INCLUDE 'D0$INC:TTDONE.INC'       ! status info
      INCLUDE 'D0$INC:TTMGETA.INC'      ! whether TT has MG or ICD in it
      INCLUDE 'D0$INC:TTEDGE.INC'       ! 1st location of TT in PTCAEP2
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! location of current event number
      LOGICAL ZEROED                    ! whether MG did zeroing already
      INTEGER TETAMIN,TETAMAX,TPHIMIN,TPHIMAX   ! bounds in TT coords
      INTEGER TTETA,TTPHI               ! loop indices in trigger towers
      INTEGER INRANGE,IX,LO,IHI         ! statement function, see below
      INTEGER IETAC,IPHIC               ! location to zero from
      INTEGER NZERO                     ! how much of pointer array to zero
      PARAMETER( NZERO = 2*NLYRL )      ! 1 TT-wide in phi
      INTEGER NOT,IBSET                 ! functions
      LOGICAL BTEST
C&IF VAXVMS,VAXELN
C&ELSE
C&      EXTERNAL NOT,IBSET,BTEST
C&ENDIF
C
C...STATEMENT FUNCTION: put IX into range [LO,IHI]
C----------------------------------------------------------------------
      INRANGE(IX,LO,IHI) = MIN0(MAX(LO,IX),IHI)
C----------------------------------------------------------------------
C
C...check inputs and convert to trigger towers
      TETAMIN = PHTTETA(INRANGE(ETAMIN,-NETAL,NETAL))
      TETAMAX = PHTTETA(INRANGE(ETAMAX,-NETAL,NETAL))
      TPHIMIN = PHTTPHI(INRANGE(PHIMIN,1,NPHIL))
      TPHIMAX = PHTTPHI(INRANGE(PHIMAX,1,NPHIL))
C
C...restart record keeping on new event
      IF (CL2CAEP_EVT.NE.IQ(LHEAD+7)) THEN
        CALL VZERO(TTDONE,2*NETAL11+1)  ! bits which say if converted
                                        ! tower has been converted this event
        TTDONE(0) = NOT(TTDONE(0))      ! no need to do eta = 0
        CL2CAEP_EVT = IQ(LHEAD+7)
      ENDIF
      DO TTETA = TETAMIN,TETAMAX
        DO TTPHI = TPHIMIN,TPHIMAX
          IF (.NOT.BTEST(TTDONE(TTETA),TTPHI-1)) THEN
            IF (ABS(TTETA).EQ.NETAL11) THEN
C
C...conversion of end tower implies ICD/MG conversion: in same trig tower
              CALL CL2_ICDMG_ETNOM(ETAMIN,ETAMAX,PHIMIN,PHIMAX)
            ELSE
C
C...convert each tower on an as-needed basis; first zero pointer array
              ZEROED = TTMGETA(TTETA)     ! zeroing possibly done already
                                          ! IF corresponding MG's converted
              IF (ZEROED)
     &          ZEROED = BTEST(TTDONE(SIGN(NETAL11,TTETA)),TTPHI-1)
              IF (.NOT.ZEROED) THEN
                IPHIC = 2*TTPHI-1
                IETAC = TTEDGE(TTETA)
                CALL VZERO( PTR2( 1,IPHIC,IETAC) ,NZERO)
C
C...coarse trigger towers are only one ROTOW wide in eta
                IF (ABS(TTETA).LT.MNCTTE)
     &            CALL VZERO( PTR2( 1,IPHIC,IETAC+1) ,NZERO)
              ENDIF
              CALL CL2_TTOW_ETNOM(TTETA,TTPHI)  !do actual conversion of 1 tt
              TTDONE(TTETA) = IBSET(TTDONE(TTETA),TTPHI-1) !remember it's done
            ENDIF
          ENDIF
        ENDDO
      ENDDO
  999 RETURN
      END
