C VAX/DEC CMS REPLACEMENT HISTORY, Element GTGLOB.FOR
C *1    16-NOV-1993 00:11:12 MEENA "Richard V. Astur: Modify GT routines to be MDST compatible"
C VAX/DEC CMS REPLACEMENT HISTORY, Element GTGLOB.FOR
      SUBROUTINE GTGLOB(IVERS,IQUAL,NTRK,NCEL,ETSUMS,ESUMS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get data from GLOB bank.
C-
C-   Inputs  : None
C-   Outputs : IVERS     -  [I] Version number
C-             IQUAL     -  [I] Bit-mapped event quality
C-             NTRK      -  [I] Total number of CD tracks
C-             NCEL      -  [I] Number of CC cells E>0.3 EM+FH
C-             ETSUMS(3) -  [R] Q(LGLOB+5) - Q(LGLOB+7) (see GLOB.ZEB)
C-             ESUMS(9)  -  [R] Q(LGLOB+8) - Q(LGLOB+16) (ditto)
C-             IER       -  [I] IER = 0:OK, -1: Bank not found
C-   Controls: 
C-
C-   Created  24-JAN-1993   Andrew J. Milder
C-   Modified 11-SEP-1993   R. Astur "Protect against GZGLOB = 0"
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C      INCLUDE 'D0$LINKS:IZJUTL.LINK'
      CHARACTER*4 PATH
      INTEGER IVERS,IQUAL,NTRK,NCEL,LANLS,GZANLS,LMDST,GZMDST,LJUTL
      INTEGER IER,LGLOB,GZGLOB
      INTEGER IVERSION,IERR
      REAL ETSUMS(3),ESUMS(9)
C----------------------------------------------------------------------
      IER = 0
      CALL PATHGT(PATH)
      IF (PATH.EQ.'MDST') THEN
        LANLS = GZANLS()
        IF (LANLS.LE.0) THEN
          IER = -1
          GOTO 999
        ENDIF
C        LJUTL = LQ(LANLS-IZJUTL)
        LMDST = GZMDST()
        IF (LMDST.LE.0) THEN
          IER = -1
          GOTO 999
        ENDIF
        CALL GTMDST_VERSION( LMDST, IVERSION,IERR)
        IF ( IERR .NE. 0 .OR. IVERSION .LE. 1 ) THEN
          IER = -1
          GOTO 999
        ENDIF
        LGLOB = IQ(LMDST+22)+LMDST-1
C        IQUAL = IQ(LJUTL+3)
        IVERS = NINT(Q(LGLOB+1))
        NTRK = NINT(Q(LGLOB+3))
        NCEL = NINT(Q(LGLOB+4))
      ELSE
        LGLOB = GZGLOB()
        IF ( LGLOB .LE. 0 ) THEN
          IER = -1
          GOTO 999
        ENDIF
        IQUAL = IQ(LGLOB+2)
        IVERS = IQ(LGLOB+1)
        NTRK = IQ(LGLOB+3)
        NCEL = IQ(LGLOB+4)
      ENDIF
      CALL UCOPY(Q(LGLOB+5),ETSUMS(1),3)
      CALL UCOPY(Q(LGLOB+8),ESUMS(1),9)
  999 RETURN
      END
