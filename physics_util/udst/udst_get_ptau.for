      SUBROUTINE UDST_GET_PTAU(LPTAU,XDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill tau parameters into array XDATA
C-
C-   Inputs  : LPTAU - pointer to PTAU bank
C-   Outputs :
C-       XDATA(1)  - UDST link to 0.7 cone jet
C-       XDATA(2)  - RMS = Q(LPTAU+11)
C-       XDATA(3)  - Et of hottest tower
C-       XDATA(4)  - Et of next-hottest tower
C-       XDATA(5)  - energy in 1x1 CAL window
C-       XDATA(6)  - energy in 3x3 CAL window
C-       XDATA(7)  - energy in 5x5 CAL window
C-       XDATA(8)  - number of associated tracks
C-       XDATA(9)  - UDST link to track 1
C-       XDATA(10) - UDST link to track 2
C-       XDATA(11) - UDST link to track 3
C-       XDATA(12) - UDST link to track 4
C-       XDATA(13) - UDST link to track 5
C-       XDATA(14) - UDST link to track 6
C-       XDATA(15) - UDST link to track 7
C-       XDATA(16) -  # of CDC tracks reconstructed in 10 degree cone 
C-       XDATA(17) -  # of CDC tracks reconstructed in 20 degree cone 
C-       XDATA(18) -  # of CDC tracks reconstructed in 30 degree cone 
C-       XDATA(19) - Et of 3rd hottest tower
C-       XDATA(20) - Et of 4th hottest tower
C-       XDATA(21) - iphi, ieta for hottest tower
C-       XDATA(22) -            "
C-       XDATA(23) - iphi, ieta for 2nd hottest tower
C-       XDATA(24) -            " 
C-       XDATA(25) - iphi, ieta for 3rd hottest tower
C-       XDATA(26) -            "
C-       XDATA(27) - iphi, ieta for 4th hottest tower
C-       XDATA(28) -            "
C-       XDATA(29) - Chi square for signal tau_Hmatrix
C-       XDATA(30) - Chi square for background tau_Hmatrix
C-       XDATA(31) - new_rms (only use cells above the threshold)
C-       XDATA(32) - threshold for calculating new_rms
C-       XDATA(33) - status word - bits 0-15 of IQ(LPTAU+0)      
C-
C-   Controls:
C-
C-   Created  22-NOV-1993   Ian Adam
C-   Updated   4-JAN-1994   Ian Adam  use SET_CAPH, add 2 more tracks
C-   Updated  27-MAR-1995   Ulrich Heintz  add words 19-30
C-   Updated   2-OCT-1995   Ian Adam  add words 31-32, VZERO
C---------------------------------------------------------------------- 
      IMPLICIT NONE
      INCLUDE   'D0$INC:ZEBCOM.INC'

      INTEGER   LPTAU,KPTAU,KPTAU1,ID_PTAU,INDEX
      PARAMETER (KPTAU=33)
      REAL      XDATA(KPTAU),XX(KPTAU),RWORD(2)
      INTEGER   NUMTRACKS,NUMTRACKS_MAX
      PARAMETER (NUMTRACKS_MAX=7)
      INTEGER   I,LZTRK,UDST_TRACK_LINK,IER,IDATA

      INTEGER   LJETS,GZJETS
      INTEGER   UDST_PTAU_JETS_LINK,COUNT,PTAU_JETS_LINK

      CHARACTER*8 PTAU_TAGS(KPTAU)
      DATA PTAU_TAGS/
     &  'LJETST','RMSTAU','TWR1ET','TWR2ET','CAL1E' ,'CAL3E' ,'CAL5E' ,
     &  'NTKTAU','TK1TAU','TK2TAU','TK3TAU','TK4TAU','TK5TAU','TK6TAU',
     &  'TK7TAU','NTRK10','NTRK20','NTRK30','TWR3ET','TWR4ET','ITWR1_',
     &  'ITWR1 ','ITWR2_','ITWR2 ','ITWR3_','ITWR3 ','ITWR4_','ITWR4 ',
     &  'CHISIG','CHIBKG','NEWRMS','NRTHRS','TAUSTAT' /
      REAL 
     &  LJETST,RMSTAU,TWR1ET,TWR2ET,CAL1E ,CAL3E ,CAL5E ,
     &  NTKTAU,TKTAU(7),NTRK10,NTRK20,NTRK30,TWR3ET,TWR4ET,ITWR1_,
     &  ITWR1 ,ITWR2_,ITWR2 ,ITWR3_,ITWR3 ,ITWR4_,ITWR4 ,
     &  CHISIG,CHIBKG,NEWRMS,NRTHRS,TAUSTAT
      COMMON/PTAU_OBJECT/
     &  LJETST,RMSTAU,TWR1ET,TWR2ET,CAL1E ,CAL3E ,CAL5E ,
     &  NTKTAU,TKTAU,NTRK10,NTRK20,NTRK30,TWR3ET,TWR4ET,ITWR1_,
     &  ITWR1 ,ITWR2_,ITWR2 ,ITWR3_,ITWR3 ,ITWR4_,ITWR4 ,
     &  CHISIG,CHIBKG,NEWRMS,NRTHRS,TAUSTAT

      EQUIVALENCE(XX,LJETST)

      REAL TEMPLATE(5,4)
      DATA TEMPLATE/ 1.,6.,0.7,0.,0.,      ! CONE R=0.7
     &               1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &               1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &               2.,7.,2.,8.,2./       ! NN 2x2
C----------------------------------------------------------------------
      CALL VZERO(XX,KPTAU)

      INDEX=0
      IDATA=IQ(LPTAU-1)
C
C find link to jets (cone=0.7)
C
      IER=0
      CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
      IF(IER.NE.0)CALL ERRMSG('SET_CAPH','UDST_GET_PTAU',
     &  'ERROR GETTING LINK TO 0.7 CONE JETS ','W')
      LJETS=GZJETS() 
      PTAU_JETS_LINK=LQ(LPTAU-2)
      UDST_PTAU_JETS_LINK=0
      COUNT=0
      DO WHILE (LJETS.GT.0)
        COUNT=COUNT+1
        IF (LJETS.EQ.PTAU_JETS_LINK) UDST_PTAU_JETS_LINK=COUNT
        LJETS=LQ(LJETS)
      ENDDO
      CALL RESET_CAPH
      LJETST = FLOAT(UDST_PTAU_JETS_LINK)
C
C find links to ZTRK
C
      NUMTRACKS=IQ(LPTAU-3)-2
      NTKTAU = FLOAT(NUMTRACKS)
      IF (NUMTRACKS.GT.NUMTRACKS_MAX) THEN
        NUMTRACKS=NUMTRACKS_MAX
        CALL ERRMSG('PTAU','UDST_GET_PTAU','TOO MANY TRACKS','W')
      ENDIF
      DO I=1,NUMTRACKS
        LZTRK=LQ(LPTAU-2-I)
        IF (LZTRK.GT.0) THEN
          CALL UDST_GET_TRACK_LINK(LZTRK,UDST_TRACK_LINK)
          TKTAU(I) = FLOAT(UDST_TRACK_LINK)
        ELSE
          CALL ERRMSG('PTAU','UDST_GET_PTAU','BAD ZTRK LINK','W')
        ENDIF
      ENDDO
      DO I=NUMTRACKS+1,NUMTRACKS_MAX
        TKTAU(I) = 0.0
      ENDDO
C
      RMSTAU = Q(LPTAU+11)
      IF(IDATA.GE.18)THEN
        TWR1ET = Q(LPTAU+12)
        TWR2ET = Q(LPTAU+13)
        CAL1E  = Q(LPTAU+16)
        CAL3E  = Q(LPTAU+17)
        CAL5E  = Q(LPTAU+18)
      ENDIF
      IF(IDATA.GE.21)THEN
        NTRK10 = FLOAT(IQ(LPTAU+19))
        NTRK20 = FLOAT(IQ(LPTAU+20))
        NTRK30 = FLOAT(IQ(LPTAU+21))
      ENDIF
      IF(IDATA.GE.32)THEN
        TWR3ET = Q(LPTAU+22)
        TWR4ET = Q(LPTAU+23)
        CALL SPLIT_BITMASK(IQ(LPTAU+24),RWORD)
        ITWR1_ = RWORD(1)
        ITWR1  = RWORD(2)
        CALL SPLIT_BITMASK(IQ(LPTAU+25),RWORD)
        ITWR2_ = RWORD(1)
        ITWR2  = RWORD(2)
        CALL SPLIT_BITMASK(IQ(LPTAU+26),RWORD)
        ITWR3_ = RWORD(1)
        ITWR3  = RWORD(2)
        CALL SPLIT_BITMASK(IQ(LPTAU+27),RWORD)
        ITWR4_ = RWORD(1)
        ITWR4  = RWORD(2)
        CHISIG = Q(LPTAU+28)
        CHIBKG = Q(LPTAU+29)
        NEWRMS = Q(LPTAU+31)
        NRTHRS = Q(LPTAU+32)
      ENDIF
      CALL SPLIT_BITMASK(IQ(LPTAU),RWORD)
      TAUSTAT = RWORD(2)

      DO I=1,KPTAU
        XDATA(I) = XX(I)
      ENDDO

  999 RETURN
C----------------------------------------------------------------------
      ENTRY UDST_PTAU_TAGS(KPTAU1,ID_PTAU)

      KPTAU1=KPTAU
      ID_PTAU=14
      CALL UDST_BOOK_GROUP(ID_PTAU,'PTAU',PTAU_TAGS,KPTAU)

      RETURN
      END
