      SUBROUTINE BKEMSV(I,LEMSV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :    Book em candidate memory bank EMSV
C-                            The EMSV bank number is set to I
C-
C-   Inputs  : I
C-   Outputs : LEMSV - pointer to the bank just booked
C-
C-   Created  30-SEP-1994   Lewis Taylor Goss
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZEMSV.LINK'
      INCLUDE 'D0$PARAMS:EMSV.PARAMS'
      INTEGER I,INTEGER,IOEMSV,LEMSV,GZEMSV,GZFRES,LFRES,NSIZE
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL MZFORM('EMSV','3I/4I39F3I9F',IOEMSV)
      ENDIF
C
      LEMSV=GZEMSV(I)
      IF(LEMSV.NE.0) GOTO 999   ! bank for this candidate already exists
C
C--- Get supporting link
      LFRES = GZFRES()
      IF (LFRES.LE.0) GOTO 999
C
C   Create new EMSV bank
C
      NSIZE = NMAIN_EMSV + NREP_EMSV
      CALL MZBOOK(IXMAIN,LEMSV,LFRES,-IZEMSV,'EMSV',0,0,NSIZE,IOEMSV,0)
C
      IQ(LEMSV-5) = I             ! bank no. set to pass number
      IQ(LEMSV+1) = 1             ! version number
      IQ(LEMSV+2) = NREP_EMSV     ! number of entries per candidate
      IQ(LEMSV+3) = 1             ! number of candidates per bank
  999 RETURN
      END
