C----------------------------------------------------------------------
C-
C-   Created   1-MAY-1994   A. Zylberstejn
C-  Contains the information per layer
C----------------------------------------------------------------------
      COMMON /TPRL_UNPACK/ rw(nword),intw(nword)
      real rw
      integer intw
      equivalence (rw,real_word),(intw,integer_word)
