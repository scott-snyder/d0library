      SUBROUTINE ZFDCHT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : unpack sense wire 0 hits and
C-                         their associated delay line hits
C-                         for finding vertex's Z position
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  13-SEP-1990   Jeffrey Bantly
C-   Updated  29-OCT-1991   Susan K. Blessing  Call to FDPULS was changed 
C-    a while ago and ZFDCHT wasn't changed.  Remove FMXPUL since MAXPUL
C-    is no longer part of the call.  Call to FDGETD changed even longer 
C-    ago, remove LUNDBG.
C-   Updated  20-FEB-1995   Norman A. Graf  Added NUM_DL to FDGETD 
C-                          argument list
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$INC:FDEVNT.INC'
C
      INTEGER HALF,QUAD,SECTOR,WIRE
      INTEGER NHITS, ERR, INDEX, SHIFTT
      INTEGER LKFTSC, LKFTDA, LKFDCH
      INTEGER NPULSE(0:NBTSEN+1)
      INTEGER MAXSEC,NUM_DL
C
      LOGICAL FIRST
      LOGICAL EZERROR
      EXTERNAL EZERROR
C
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
C        CALL EZPICK('VERTEX_RCP')
C        IF( EZERROR(ERR) ) THEN
C          CALL ERRMSG('ZTRAKS','ZFDCHT','VERTEX_RCP not found.','W')
C          FMXPUL=10
C        ELSE
C          CALL EZGET('FMXPUL',FMXPUL,ERR)
C          CALL EZRSET
C        ENDIF
        CALL EZPICK('FTRAKS_RCP')
        IF( EZERROR(ERR) ) THEN
          CALL ERRMSG('ZTRAKS','ZFDCHT','FTRAKS_RCP not found.','W')
          SHIFTT=0
        ELSE
          CALL EZGET('SHIFTT',SHIFTT,ERR)
          CALL EZRSET
        ENDIF
      ENDIF
C
C ****  Initialise the bank structure
C
      CALL BKFDCH(LKFDCH)
      TMPUBN = SHIFTT                   ! works for MC only
C
      DO 2 HALF = 0, MXHALF
        DO 5 QUAD = 0, MXQUAD
          MAXSEC = MXSECT
          IF(QUAD.LE.3) MAXSEC = MXSECT - 1
          DO 10 SECTOR = 0, MAXSEC
            NHITS  = 0
C
C    look at sense wire 0 then the delay line
C
            DO 20 INDEX = 0, 2
              WIRE = INDEX
              IF (INDEX .NE. 0) WIRE = INDEX + NBTSEN - 1
              IF( NHITS .GE. MXHTOT ) GOTO 10
              NPULSE(WIRE) = 0
              CALL FDPULS(HALF,0,QUAD,SECTOR,WIRE,NPULSE(WIRE))
              IF (NPULSE(0) .LE. 0) GOTO 10
              NHITS = NHITS + NPULSE(WIRE)
   20       CONTINUE
            CALL BKFTSC(HALF, QUAD, SECTOR, NHITS, LKFTSC)
            CALL BKFTDA(HALF, QUAD, SECTOR, NHITS, LKFTDA)
C
            CALL ZFFTDA(HALF, QUAD, SECTOR, NPULSE)
            CALL ZFFTSC(HALF, QUAD, SECTOR)
            IF(LFGEH.GT.0) CALL FDGETD(HALF, QUAD, SECTOR, NUM_DL)
C
   10     CONTINUE
C
    5   CONTINUE
C
    2 CONTINUE
C
c      FDCLNK(1) = 0            ! This is really a permanent link area.
      CALL FCLRLNK                      ! Zeroes all links in FDCLNK.INC
C---------------------------------------------------------------------------
  999 RETURN
      END
