C----------------------------------------------------------------------
C-
C-   ** Common Block L1OL **
C-
C-   Created  24-JAN-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
C
C    This common block is used to keep the lookup table used in converting 
C    level-1 trigger data block index values into (IETAC,IPHIC) index
C    values, hex addresses and to signal detector cracks. The first index 
C    in this array is given by the DBI; The locations of the second index 
C    are assigned as follows:
C
C                         L1OL(*,1) <-- IETAC
C                         L1OL(*,2) <-- IPHIC
C                         L1OL(*,3) <-- CRATE
C                         L1OL(*,4) <-- first calorimeter IADDR
C                         L1OL(*,5) <-- last calorimeter IADDR
C                         L1OL(*,6) <-- CRACK (to be used as a logical, where
C                                       any nonzero number is true and 0
C                                       is false.
C
C    FINDBI (FINal Data Block Index) marks the last good entry in the table.
C
      INTEGER    L1OL(0:(NETAL1*NPHIL1*2),6),FINDBI
      COMMON /L1OL/L1OL,FINDBI
