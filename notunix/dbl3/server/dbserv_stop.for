      PROGRAM DBSERV_STOP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To stop the dbl3 server. The logical
C-                         DBL3$CLIENT should be defined such that it
C-                         points to the subdetector of interest. For
C-                         instance CALOR for calorimeter and CDC for
C-                         muon detector,... etc.
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-AUG-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C
      CHARACTER*13 CACHENAME_S
      REAL    TIMEOUT
      INTEGER ICACHE(4)
      INTEGER IERR
      BYTE    BCACHE(16)
      EQUIVALENCE  (BCACHE, ICACHE)
C
      CALL DB_SERVER_NAMES
C
      TIMEOUT = 100.
      CACHENAME_S = DB_CURRENT_SNAME(1:2)//'_STOP_CACHE'
C
      CALL IC_BOOK (CACHENAME_S, ICACHE, 'C', IERR)
      IF (IERR.NE.0) THEN
        IQUEST(1) = -6
        IQUEST(11)= IERR
        PRINT *, ' Problem in ic_book'
        GO TO 999
      ENDIF
C
C      BCACHE(1) = ICHAR(SRVR_IMGNAME(8:8))
C      BCACHE(2) = ICHAR(SRVR_IMGNAME(9:9))
      ICACHE(3) = -9
      ICACHE(4) = -1
C
C
      CALL IC_SIGNAL (CACHENAME_S, ICACHE, TIMEOUT, ' ', IERR)
      IF (IERR.NE.0) THEN
        IQUEST(1) = -223
        IQUEST(11)= IERR
        PRINT *, 'Server - Stop request did not finish'
        GO TO 999
      ENDIF
C
      PRINT *, 'Server - Stop requested by ', SRVR_IMGNAME(8:9)
C
      IQUEST(1) = ICACHE(3)
      IQUEST(2) = ICACHE(2)
C
  999 STOP
      END
