      SUBROUTINE BKCTTR (EM_TT, HD_TT, LCTTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books the CTTR bank, assumes that CAEP should
C-                         exist.
C-
C-   Inputs  : EM_TT [I] : Number of EM Trigger Towers;
C-             HD_TT [I] : Number of HD Trigger Towers.
C-
C-   Outputs : LCTTR [I] : Zebra pointer to CTTR bank : 0 if CAEP bank
C-                         does not exist.
C-
C-   Controls: None.
C-
C-   Created  08-AUG-1990   Sylvain Tisserant (MSU)
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCTTR.LINK'
C
      INTEGER GZCTTR, GZCAEP
C
      INTEGER  EM_TT, HD_TT, LCTTR
C
      LOGICAL  FIRST
      INTEGER  IOCTTR, LCAEP, ND
C
      INTEGER    VERSION,     NR
      PARAMETER (VERSION = 1, NR = 7)
C
      DATA FIRST /.TRUE./
C
C----------------------------------------------------------------------
C
      LCTTR = GZCTTR()
      IF(LCTTR.NE.0) RETURN
C                             Defines the CTTR bank data format : only once
C                             ---------------------------------------------
      IF (FIRST) THEN
        CALL MZFORM ('CTTR','4I/4F3I',IOCTTR)
        FIRST = .FALSE.
      ENDIF
C                                                          CTTR bank booking
C                                                          -----------------
      LCAEP = GZCAEP()
      IF(LCAEP.EQ.0) RETURN
      ND = 4 + (NR * (EM_TT + HD_TT))
      CALL MZBOOK (IXMAIN,LCTTR,LCAEP,-IZCTTR,'CTTR',1,1,ND,IOCTTR,-1)
      IQ(LCTTR+1) = VERSION
      IQ(LCTTR+2) = NR
      IQ(LCTTR+3) = (1000*EM_TT) + HD_TT
      RETURN
      END
