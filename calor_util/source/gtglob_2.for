C VAX/DEC CMS REPLACEMENT HISTORY, Element GTGLOB_2.FOR
C *1    16-NOV-1993 00:24:48 MEENA "Richard V. Astur: GT routine to get additional GLOB words"
C VAX/DEC CMS REPLACEMENT HISTORY, Element GTGLOB_2.FOR
      SUBROUTINE GTGLOB_2(TIME29,MRBITS,MAGPOL,L1ANOR,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get extra data from GLOB bank (VERS>=3).
C-
C-   Inputs  : None
C-   Outputs : 
C-             TIME29    -  [R] Q(LGLOB+17)
C-             MRBITS    -  [I] IQ(LGLOB+18)
C-             MAGPOL    -  [I] IQ(LGLOB+19,20)
C-             L1ANOR    -  [I] IQ(LGLOB+21-28)
C-
C-             IER       -  [I] IER = 0:OK, -1: Bank not found
C-   Controls: 
C-
C-   Created  27-SEP-1993   A. Brandt add LGLOB(17-28)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER*4 PATH
      INTEGER LANLS,GZANLS,LMDST,GZMDST
      INTEGER MRBITS,MAGPOL(2),L1ANOR(8)
      REAL TIME29
      INTEGER IER,LGLOB,GZGLOB,I
      INTEGER IERR, IVERSION
C----------------------------------------------------------------------
      IER = 0
      CALL PATHGT(PATH)
      IF (PATH.EQ.'MDST') THEN
        LANLS = GZANLS()
        IF (LANLS.LE.0) THEN
          IER = -1
          GOTO 999
        ENDIF
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
C
C Get words from MDST GLOB
C
        TIME29    = Q(LGLOB+17)
        MRBITS    = NINT(Q(LGLOB+18))
        MAGPOL(1) = NINT(Q(LGLOB+19))
        MAGPOL(2) = NINT(Q(LGLOB+20))
        DO I=1,8
          L1ANOR(I)=NINT(Q(LGLOB+20+I))
        ENDDO
      ELSE
        LGLOB = GZGLOB()
        IF ( LGLOB .LE. 0 ) THEN
          IER = -1
          GOTO 999
        ENDIF
C
C Get words from standard GLOB
C
        TIME29=Q(LGLOB+17)
        MRBITS    = IQ(LGLOB+18)
        MAGPOL(1) = IQ(LGLOB+19)
        MAGPOL(2) = IQ(LGLOB+20)
        DO I=1,8
          L1ANOR(I)=IQ(LGLOB+20+I)
        ENDDO
      ENDIF
  999 RETURN
      END
