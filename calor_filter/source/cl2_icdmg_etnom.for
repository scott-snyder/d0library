      SUBROUTINE CL2_ICDMG_ETNOM(ETAMIN,ETAMAX,PHIMIN,PHIMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert raw PH to ETnominal for ICD's and Massless
C-      gaps in level 2
C-      Put results into level2 CAEP bank, and set pointer array PTCAEP2 so
C-      that it is zero if the cell does not exist, or was zero supressed
C-
C-      The conversion is made fast by
C-              a) limited precision guarantee (1-2%)
C-              b) using precomputed tables
C-              c) doing limited areas at a time
C-
C-
C-      CL2_ROTOW_ETNOM and CL2_ICDMG_ETNOM operate cooperatively.  They both
C-      convert one trigger tower at a time, and record that the conversion has
C-      taken place in TTDONE.  They both put converted data into CAEP, and
C-      point to it with PTCAEP2.  Thus, both may initialize PTCAEP2 (which
C-      they do a trigger tower at a time).  Therefor, they check whether
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
      INTEGER ETAMIN,ETAMAX,PHIMIN,PHIMAX
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF' ! pointer array
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! holds current event info
      INCLUDE 'D0$INC:PHTT.INC'         ! TTowers containing ROTOW
      INCLUDE 'D0$INC:TTDONE.INC'       ! status info
      INCLUDE 'D0$INC:TTMGETA.INC'      ! whether TT has MG or ICD in it
      INCLUDE 'D0$INC:TTEDGE.INC'       ! 1st location of TT in PTCAEP2
      INTEGER TETAMIN,TETAMAX,TPHIMIN,TPHIMAX   ! bounds in TT coords
      INTEGER INRANGE,IX,LO,IHI         ! statement function, see below
      INTEGER IETAC,IPHIC               ! indices of ROTOW to zero from
      INTEGER TTLO(2),TTHI(2)           ! boundaries of this MG/ICD region
      INTEGER TTEND(2)                  ! end TT containing MG info
      INTEGER I                         ! loop index for the 2 ends
      INTEGER TTPHI,TTETA               ! TT's in requested IPHIC range
      INTEGER NZERO                     ! how much of pointer array to zero
      PARAMETER( NZERO = 2*NLYRL )      ! # channels in 2 adjacent rotows
      INTEGER NOT,IBSET                 ! functions
      LOGICAL BTEST
C&IF VAXVMS,VAXELN
C&ELSE
C&      EXTERNAL NOT,IBSET,BTEST
C&ENDIF
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C
C...STATEMENT FUNCTION: put IX into range [LO,IHI]
C----------------------------------------------------------------------
      INRANGE(IX,LO,IHI) = MIN0(MAX(LO,IX),IHI)
C----------------------------------------------------------------------
      IF(FIRST)THEN                     ! LOCAL INIT
C
C...(1) is the - eta end; (2) is +:
        TTEND(2) = NETAL11
        TTLO(2) = PHTTETA(MNETAMG)
        TTHI(2) = PHTTETA(MXETAMG)
        TTEND(1) = - TTEND(2)
        TTLO(1) = - TTHI(2)
        TTHI(1) = - TTLO(2)
        FIRST = .FALSE.
      ENDIF
C
C...check inputs and convert to trigger towers
      TETAMIN = PHTTETA(INRANGE(ETAMIN,-NETAL,NETAL))
      TETAMAX = PHTTETA(INRANGE(ETAMAX,-NETAL,NETAL))
      TPHIMIN = PHTTPHI(INRANGE(PHIMIN,1,NPHIL))
      TPHIMAX = PHTTPHI(INRANGE(PHIMAX,1,NPHIL))
C
C...restart record keeping on new event
      IF (CL2CAEP_EVT.NE.IQ(LHEAD+7)) THEN
        CALL VZERO(TTDONE,2*NETAL11+1)  ! bits which say if trigger
        ! tower has been converted this event
        TTDONE(0) = NOT(TTDONE(0))      ! no need to do eta = 0
        CL2CAEP_EVT = IQ(LHEAD+7)
      ENDIF
C
C...the loop is over the two end "towers" where MG/ICD info is stored
      DO I = 1,2
C
C...want info if 1) request end tower or 2) request tower in MG region
        IF ( (TETAMIN.EQ.TTEND(I)).OR.(TETAMAX.EQ.TTEND(I)).OR.
     &         ((TETAMIN.LE.TTHI(I)).AND.(TETAMAX.GE.TTLO(I)))) THEN
C
C...check if conversion done already
          DO TTPHI = TPHIMIN,TPHIMAX
            IF (.NOT.BTEST(TTDONE(TTEND(I)),TTPHI-1)) THEN
              IPHIC = 2*TTPHI-1
C
C... zero the pointer array :  first, the end tower itself
              CALL VZERO( PTR2(1,IPHIC,SIGN(NETAL,TTEND(I))), NZERO)
C
C...then the individual towers with MG/ICD info, provided the calorimeter has
C...not zero'd them already
              DO TTETA = TTLO(I),TTHI(I)
                IF (.NOT.BTEST(TTDONE(TTETA),TTPHI-1)) THEN
                  IETAC = TTEDGE(TTETA)
                  CALL VZERO( PTR2(1,IPHIC,IETAC), NZERO)
                  CALL VZERO( PTR2(1,IPHIC,IETAC+1), NZERO)
                ENDIF
              END DO
C...convert the whole end "tower", which contains both the last readout
C...tower and the MG/ICD info;
              CALL CL2_TTOW_ETNOM(TTEND(I),TTPHI)  !do actual conversion of 1 tt
C
C...set the bit to remember it has been done
              TTDONE(TTEND(I)) = IBSET(TTDONE(TTEND(I)),TTPHI-1)
            ENDIF
          END DO      ! TTPHI
        ENDIF
      ENDDO     !I 
  999 RETURN
      END
