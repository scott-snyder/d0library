      SUBROUTINE GTCAEH(START,EX,EY,EZ,E,ET,SEX,SEY,CWEIGHT,IETA,IPHI,
     & ILYR,STATUS,ICHAN,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns data for the next channel from CAEH bank.
C-                         First call returns data for the first channel.
C-                         START *MUST* be set to true when getting first
C-                         cell's data, and START *MUST* be false when getting
C-                         subsequent cells' data.
C-
C-   Inputs  : START    [L]    TRUE if starting over to unpack.
C-                             FALSE if getting subsequent data.
C-
C-   Outputs : Ex       [R]     Ex=E cosx= XC/Distance
C-             Ey       [R]     Ey=E cosy= YC/Distance
C-             Ez       [R]     Ez=ZC/Distance
C-             E        [R]     Energy in GeV
C-             Et       [R]     Transverse Energy
C-             SEx      [R]     sig**2(Ex)
C-             SEy      [R]     sig**2(Ey)
C-             CWEIGHT  [R]     Cell weight
C-             IETA     [I]     Eta index
C-             IPHI     [I]     Phi index
C-             ILYR     [I]     Layer number
C-             STATUS   [I]     Status
C-                              Least significant 16 bits=cluster tags
C-                                                       =0 not in a cluster
C-                              Most significant 16 bits=bad channel tags
C-                                                       =0 good channel
C-             ICHAN    [I]     Channel number
C-             IER      [I]     Error code; 0 --- OK
C-                              -1 --- Reached end of CAEH bank
C-                              -2 --- No CAEH bank address
C-                              -3 --- Expecting dead material but not found
C-   Controls: NONE
C-
C-   Related routines:
C-            GTCAEH_HEADER(NV,NR,NCH,IER)
C-            to return version number, repetition number and total
C-            number of channels in the bank.
C-
C-            GTCAEH_ADDR(IETA,IPHI,ILYR,Ex,Ey,Ez,E,Et,SEx,SEy,Cweight,IER)
C-            to return energy for a given address.
C-
C-
C-   Created   27-FEB-1990   W.G.D.Dharmaratna, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IETA,IPHI,ILYR,STATUS,ICHAN,IER,NR,NV
      REAL    EX,EY,EZ,E,ET,SEX,SEY,CWEIGHT

      LOGICAL START
      CHARACTER*4 PATH

C
      INTEGER GZCAEH,POINT
      INTEGER ICHANNEL,NCHANNEL
C
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      SAVE POINT,ICHANNEL,NCHANNEL

C----------------------------------------------------------------------
C
      IER = 0
C
      IF (START) THEN
C
C ****  READ IN BANK HEADER AND THE FIRST CHANNEL
C
        CALL CZLINI
        LCAEH=GZCAEH()
        IF ( LCAEH .LE. 0 ) THEN
          IER = -2
          GOTO 999
        ENDIF
        ICHANNEL = 0
        NV       = IQ(LCAEH+1)
        NR       = IQ(LCAEH+2)
        NCHANNEL = IQ(LCAEH+3)
        POINT = 4-NR
      ENDIF
C
      ICHANNEL = ICHANNEL + 1
      IF ( ICHANNEL.GT.NCHANNEL ) THEN
C
C ****  REACHED THE END OF THE BANK
C ****  CHECK IF THERE IS A GEAN DEAD MATERIAL CAEH TO UNPACK
C
        CALL PATHGT(PATH)
        IF(PATH.EQ.'GEAN' .AND. NV.LT. 1000) THEN
          LCAEH=LQ(LCAEH)
          IF ( LCAEH .LE. 0 ) THEN
            IER = -3
            GOTO 999
          ENDIF
          ICHANNEL = 1
          NV       = IQ(LCAEH+1)
          NR       = IQ(LCAEH+2)
          NCHANNEL = IQ(LCAEH+3)
          POINT = 2
        ELSE
          IER = -1
          GOTO 999
        END IF
      ENDIF
C
      POINT = POINT + NR
      EX      = Q(LCAEH+POINT)
      EY      = Q(LCAEH+POINT+1)
      EZ      = Q(LCAEH+POINT+2)
      E       = Q(LCAEH+POINT+3)
      ET      = Q(LCAEH+POINT+4)
      SEX     = Q(LCAEH+POINT+5)
      SEY     = Q(LCAEH+POINT+6)
      CWEIGHT = Q(LCAEH+POINT+7)
      IETA    = IQ(LCAEH+POINT+8)
      IPHI    = IQ(LCAEH+POINT+9)
      ILYR    = IQ(LCAEH+POINT+10)
      STATUS  = IQ(LCAEH+POINT+11)
      ICHAN = ICHANNEL
C
  999 RETURN
      END
