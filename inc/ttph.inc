C----------------------------------------------------------------------
C-
C-   Created  12-SEP-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
      COMMON /TTPH/ TTPH
      INTEGER     TTPH(-NETAL11:NETAL11,NPHIL1,5)
C
C                   TTPH(l1eta,l1phi,1) <-- IETAC
C                   TTPH(l1eta,l1phi,2) <-- IPHIC
C                   TTPH(l1eta,l1phi,3) <-- CRATE
C                   TTPH(l1eta,l1phi,4) <-- IADDRL
C                   TTPH(l1eta,l1phi,5) <-- IADDRH
C 
C     -- where IADDRL and IADDRH are the Low and High hex addresses -- 
C        that define the range of hex address for the ROT.             
