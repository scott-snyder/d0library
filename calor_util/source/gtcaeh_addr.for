      SUBROUTINE GTCAEH_ADDR(IETA,IPHI,ILYR,Ex,Ey,Ez,E,Et,SEX,SEY,
     &  CWEIGHT,STATUS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns energy for a given PHYSICS address
C-                         from CAEH bank. Routine works with PTCAEP array 
C-                         in D0$INC:PTCAEH.INC common.  If this array is not 
C-                         already filled, then this routine will fill it and
C-                         then set the PTZFLG to .FALSE. 
C-                         NOTE: PTCAEP depends on the program reseting the 
C-                         array at the end of every event with a call to 
C-                         CPTCAZ. 
C-
C-   Inputs  : IETA     [I]     Eta index
C-             IPHI     [I]     Phi index
C-             ILYR     [I]     Layer number
C-             
C-   Outputs : Ex       [R]     Ex=E cosx= XC/Distance
C-             Ey       [R]     Ey=E cosy= YC/Distance
C-             Ez       [R]     Ez=ZC/Distance
C-             E        [R]     Energy in GeV
C-             Et       [R]     Transverse Energy
C-             SEx      [R]     sig**2(Ex)
C-             SEy      [R]     sig**2(Ey)
C-             CWEIGHT  [R]     Cell weight
C-             STATUS   [I]     Status
C-                              Least significant 16 bits=cluster tags
C-                                                       =0 not in a cluster
C-                              Most significant 16 bits=bad channel tags
C-                                                       =0 good channel
C-             IER      [I]     Error code; 0 --- OK
C-                              -1 --- Rreached end of CAEH bank
C-                              -2 --- No CAEH bank address
C-                              -3 --- IETA,IPHI out of bounds 
C-                                  -- from D0$INC:CAL_OFFLINE.PARAMS
C-                              -4 --- ILYR for DEAD ENERGY not coded yet
C-                              -5 --- No energy in cell
C-
C-   Controls: NONE
C-   
C-   Created   27-FEB-1990   W.G.D.Dharmaratna, Chip Stewart
C-   Updated   14-APR-1993   Norman A. Graf  added check on ptcaep=0 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IETA,IPHI,ILYR,STATUS,IER
      REAL Ex,Ey,Ez,E,Et,SEx,SEy,CWEIGHT
C
      INTEGER GZCAEH,POINT
      INTEGER ICHANNEL,NCHANNEL,NV,NR,I
C
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
C----------------------------------------------------------------------
C
      IER = 0
      CALL CZLINI
      LCAEH=GZCAEH()
      IF ( LCAEH .LT. 0 ) THEN
        IER= -2
        GOTO 999
      ENDIF
      IF(ILYR.GT.NLYRL) THEN
C
C ****  LOOK FOR GEAN CAEH FOR DEAD ENERGY
C
        IER = -4
        GOTO 999
      END IF
C
      IF (PTZFLG) THEN
        CALL CPTCAF
      ENDIF
C ****  READ IN BANK HEADER AND THE FIRST CHANNEL
C
      NV       = IQ(LCAEH+1)
      NR       = IQ(LCAEH+2)
      NCHANNEL = IQ(LCAEH+3)
      IF (IABS(IETA).LE.NETAL .AND. IETA.NE.0 
     &      .AND. IPHI.LE.NPHIL ) THEN
        ICHANNEL = PTCAEP(IETA,IPHI,ILYR)
      ELSE
        IER= -3
        GOTO 999
      END IF
      IF ( ICHANNEL.GT.NCHANNEL ) THEN
C
C ****  pointing beyond the bank
C
            IER = -1 
            GOTO 999
      ENDIF
      IF ( ICHANNEL.LE.0 ) THEN
C
C ****  no energy in requested cell
C
            IER = -5
            GOTO 999
      ENDIF

C
      POINT  = 3+ICHANNEL*NR-NR
      Ex     = Q(LCAEH+POINT+1)
      Ey     = Q(LCAEH+POINT+2)
      Ez     = Q(LCAEH+POINT+3)
      E      = Q(LCAEH+POINT+4)
      Et     = Q(LCAEH+POINT+5)
      SEx    = Q(LCAEH+POINT+6)
      SEy    = Q(LCAEH+POINT+7)
      CWEIGHT= Q(LCAEH+POINT+8)
      STATUS = IQ(LCAEH+POINT+12)
C
  999 RETURN
      END
