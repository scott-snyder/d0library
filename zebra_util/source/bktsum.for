      SUBROUTINE BKTSUM(NTRIG,NFILT,LTSUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     book TSUM, bank for trigger bit names
C-   Inputs  : 
C-    NTRIG= number of trigger bits
C-    NFILT= number of filter bits
C-   Output:
C-    LTSUM= pointer to TSUM bank
C-
C-   Created  20-MAR-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NTRIG,NFILT
      INTEGER NR
      PARAMETER (NR = 9 )  ! per trigger bit
      INTEGER NFIX
      PARAMETER( NFIX = 5 )  !fixed header
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTSUM.LINK'
      INTEGER  LTSUM, LSUP, NSIZ
      INTEGER IOH, GZHSUM,GZTSUM
      LOGICAL FIRST
      SAVE FIRST, IOH
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      LTSUM=GZTSUM()
      IF(LTSUM.LE.0) THEN
        IF(FIRST) THEN
          CALL MZFORM('TSUM','5I /1I 8H',IOH)
          FIRST=.FALSE.
        ENDIF
C...get support if necessary
        LSUP=GZHSUM()
        IF(LSUP.LE.0) CALL BKHSUM(LSUP)
        IF (LSUP .LE. 0) THEN
          CALL ERRMSG('Cannot book','BKTSUM','Cannot book HSUM','E')
          GO TO 999
        END IF
C...initial booking; set fixed part of bank to zero
        NSIZ = NR*(NTRIG+NFILT) + NFIX
        CALL MZBOOK(IXMAIN,LTSUM,LSUP,-IZTSUM,'TSUM',0,0,NSIZ,IOH,-1)
        IQ( LTSUM + 1 ) = 1       ! version number
        IQ( LTSUM + 2 ) = NFIX    ! fixed length part
        IQ( LTSUM + 3 ) = NR      ! Repetition size
        IQ( LTSUM + 4 ) = NTRIG
        IQ( LTSUM + 5 ) = NFILT
      ENDIF
  999 RETURN
      END
