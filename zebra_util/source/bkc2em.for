      SUBROUTINE BKC2EM(LC2EM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book C2EM bank
C-            multiple calls result in a linear chain
C-
C-   Input   : None
C-   Outputs : Link to C2EM bank
C-   Controls: None
C-
C-   Created  27-APR-1993  James T. McKinley
C-   Modified  7-JUN-1993  James T. McKinley - change fatal errors to 
C-                         warnings, skip out if errors.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZC2EM.LINK/LIST'
C
      INTEGER LC2EM,IOC2EM,NR,NFIX,FILTVER
      INTEGER GZFILT,LFILT,NDATA,NP
      PARAMETER (FILTVER = 5)
      PARAMETER (NFIX = 30)
      PARAMETER (NR = 8)
      PARAMETER (NP = 128)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      LC2EM = 0
      IF(FIRST)THEN
        CALL MZFORM('C2EM','9I 21F/4I 4F',IOC2EM)      ! bank format
        FIRST=.FALSE.
      ENDIF
C
C get link to supporting bank
C
      LFILT = GZFILT()
      IF(LFILT.LE.0)THEN
        CALL BKFILT(LFILT)                ! book FILT bank if not there
        IF(LFILT.LE.0)THEN
          CALL ERRMSG('BKC2EM','FILT BANK PROBLEM',
     &      'Cannot book FILT bank, so no C2EM bank booked.','W')
          GOTO 999
        ENDIF
      ENDIF
C
      IF(IQ(LFILT+1).LT.5)THEN            ! can C2EM hang from FILT?
        CALL ERRMSG('BKC2EM','FILT BANK VERSION',
     &'FILT bank version less than 5, cannot support C2EM bank','W')
        GOTO 999
      ENDIF
C
      NDATA = NFIX + NR*NP                ! book to max size and push it later
      CALL MZBOOK
     &  (IXMAIN,LC2EM,LFILT,-IZC2EM,'C2EM',1,1,NDATA,IOC2EM,0)
      IQ(LC2EM+1) = 1       ! version number
      IQ(LC2EM+2) = NFIX    ! number of fixed words in each C2EM bank
      IQ(LC2EM+3) = NR      ! repetition words
      IQ(LC2EM+4) = NP      ! max number of parameter sets = 128
C
  999 RETURN
      END
