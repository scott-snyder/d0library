      SUBROUTINE MUSIM_FILL_L15
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in L1.5 related histograms 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-OCT-1992   Kamel A. Bazizi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCF,NWN,NWS 
      LOGICAL L1_BITS(16),L15_BITS(16),L15_OCT(40)
      INTEGER IBIT,LSB,MSB
      INTEGER Y1,Y2
      LOGICAL L15_CF,L15_WN,L15_WS
      INTEGER IHOFF
      DATA IHOFF/0/
C
C-- initialization

      NCF=0
      NWN=0
      NWS=0
C
      Y1=0
      Y2=0
C
      L15_CF=.FALSE.
      L15_WN=.FALSE.
      L15_WS=.FALSE.
C
      DO IBIT=1,16
        L1_BITS(IBIT)=.FALSE.
        L15_BITS(IBIT)=.FALSE.
      ENDDO
C
      DO IBIT=1,39
        L15_OCT(IBIT)=.FALSE.
      ENDDO
C
C      LSB = LEAST SIGNIFICANT BIT
C      MSB = MOST  SIGNIFICANT BIT
C
C- get the 16 level 1 trigger bits
      CALL MU_L1_RAW_BITS(L1_BITS)
      
C- get the 16 level 1.5 trigger bits
      CALL MU_L15_RAW_BITS(L15_BITS)

C- get L1 trigger octant info
      CALL MU_L15_TRIG_OCT(L15_OCT)

C-- L1_BITS(1) is a pulser bit
C-- L1_BITS(16) is an UNUSED bit

C-- calculate CF triggers
C-- L1.0
      LSB=0
      MSB=0
      IF(L1_BITS(2)) LSB=1
      IF(L1_BITS(3)) MSB=1
      NCF=MSB*2+LSB
C-- L1.5
      IF(L15_BITS(1).OR.L15_BITS(2).OR.L15_BITS(3)) L15_CF=.TRUE.

C-- calculate WN triggers
C-- L1.0
      LSB=0
      MSB=0
      IF(L1_BITS(4)) LSB=1
      IF(L1_BITS(5)) MSB=1
      NWN=MSB*2+LSB
C-- L1.5
      IF(L15_BITS(4).OR.L15_BITS(5).OR.L15_BITS(6)) L15_WN=.TRUE.

C-- calculate WS triggers
C-- L1.0
      LSB=0
      MSB=0
      IF(L1_BITS(6)) LSB=1
      IF(L1_BITS(7)) MSB=1
      NWS=MSB*2+LSB
C-- L1.5
      IF(L15_BITS(7).OR.L15_BITS(8).OR.L15_BITS(9)) L15_WS=.TRUE.

C-- sum up regions
      Y1=NCF
      Y2=Y1+NWN+NWS

C-- Go to MUSIM directory in memory
      CALL HCDIR('//PAWC/MUSIM_L15',' ')

C-- fill histograms
      DO IBIT=1,16
        IF(L15_BITS(IBIT)) CALL HFILL(IHOFF+2,FLOAT(IBIT),0.,1.)
      ENDDO
C-- fill Trigger Octant info
      DO IBIT=1,39
        IF(L15_OCT(IBIT)) CALL HFILL(IHOFF+4,FLOAT(IBIT),0.,1.)
      ENDDO


C-- level 1.5 confiramtion (MU_1_CENT)
      IF(Y1.NE.0) THEN
        IF(L15_CF) THEN
          CALL HFILL(IHOFF+9,1.,0.,1.)  ! CONFIRMED
        ELSE
          CALL HFILL(IHOFF+9,0.,0.,1.)  ! REJECTED
        ENDIF
      ENDIF

C-- level 1.5 confiramtion (MU_1_HIGH)
      IF(Y2.NE.0) THEN
        IF(L15_CF.OR.L15_WN.OR.L15_WS) THEN
          CALL HFILL(IHOFF+10,1.,0.,1.)  ! CONFIRMED
        ELSE
          CALL HFILL(IHOFF+10,0.,0.,1.)  ! REJECTED
        ENDIF
      ENDIF

C-- Go back to Root Zebra directory
      CALL HCDIR('//PAWC',' ')

C----------------------------------------------------------------------
  999 RETURN
      END
