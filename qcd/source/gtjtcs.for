      SUBROUTINE GTJTCS(JETNUM,JET_ETA,JET_PHI,JET_ET,CONE_ET,KERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :JETNUM - jet number in bank (i.e. 1 thru 20)
C-   Outputs :JET_ETA  - eta of jet
C-            JET_PHI  - phi of jet
C-            JET_ET   - Et of jet
C-            CONE_ET  - Et of each subcone
C-            KERR - = 0,1 if JTCS bank is there,missing
C-                   = -14, No MDST bank
C-   Controls:
C-
C-   Created  08-NOV-1993   Kathy Streets
C-   Modified 15-NOV-1993   Richard Astur "Add MDST capability"
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJTCS.LINK'
C
C..FUNCTIONS
      INTEGER LZFIDH
C
      INTEGER JETNUM
      INTEGER LJTCS
      INTEGER KERR,J
      INTEGER NUM_3_SUBC,NUM_SUBC,POINTER
C
      REAL JET_ETA,JET_PHI,JET_ET,CONE_ET(20)
      CHARACTER*4 PATH
      INTEGER LMDST, GZMDST
C
C
C----------------------------------------------------------------------
C
      KERR=0
      CALL PATHGT(PATH)
C
      IF ( PATH .EQ. 'MDST' ) THEN

        LMDST = GZMDST()
        IF ( LMDST .LE. 0 ) THEN
          KERR = -14
          RETURN
        ENDIF
        LJTCS = LMDST + IQ(LMDST+25) - 1
        NUM_3_SUBC = Q( LJTCS + 2 )

      ELSE
        LJTCS=LZFIDH(IXMAIN,'JTCS',0)
        IF (LJTCS.EQ.0)THEN
          KERR=1
          GO TO 999
        ENDIF
        NUM_3_SUBC= IQ(LJTCS+2)
      ENDIF
C
      NUM_SUBC  = NUM_3_SUBC - 3
C
      POINTER = 4 + (3 + NUM_SUBC)*(JETNUM-1)
C
      JET_ETA = Q(LJTCS+POINTER)
      JET_PHI = Q(LJTCS+POINTER+1)
      JET_ET  = Q(LJTCS+POINTER+2)
      DO 100 J = 1,NUM_SUBC
        CONE_ET(J) = Q(LJTCS+POINTER+2+J)
  100 CONTINUE
C
  999 CONTINUE
      RETURN
      END
C
C*******************************************************************

