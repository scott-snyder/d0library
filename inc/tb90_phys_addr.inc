C----------------------------------------------------------------------
C-
C-   Created  20-MAR-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      INTEGER TB90_PHYS_ADDR(2,0:7000)
      COMMON /TB_SEQ_PHY/ TB90_PHYS_ADDR
C EACH element contains packed TB90 address info
C   TB90_PHYS_ADDR (1,N)....Pointer to next address (4608-5700) in this ganging
C   TB90_PHYS_ADDR (2,N)....Packed Physics Address 
C   MOST SIG Byte(4)    ....not used
C            Byte(3)    ....ETA
C            Byte(2)    ....PHI
C  Least Sig Byte(1)    ....LAYER 
