      SUBROUTINE GTJTCS_TOTAL(CONESIZE,NUM_JETS,NUM_SUBC,KERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs : CONESIZE - cone size JTCS bank is made for
C-             NUM_JETS - number jets in JTCS bank
C-             NUM_SUBC - number subcones for each jet
C-             KERR - = 0,1 if JTCS bank is there,missing
C-                      -14 :No MDST bank
C-   Controls:
C-
C-   Created  08-NOV-1993   Kathy Streets
C-   Modified 15-NOV-1993   Richard Astur "Added MDST capability"
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJTCS.LINK'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INCLUDE 'D0$LINKS:IZJPTS.LINK'
      INCLUDE 'D0$LINKS:IZJTSH.LINK'
C
C..FUNCTIONS:
      INTEGER LZFIDH
C
      INTEGER LJTCS,LCAPH
      INTEGER KERR
      INTEGER LMDST, GZMDST
      INTEGER NUM_JETS,NUM_3_SUBC,NUM_SUBC
      CHARACTER*4 PATH
C
      REAL CONESIZE
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
        LJTCS      = LMDST + IQ(LMDST+25) - 1 
        CONESIZE   = Q( LJTCS + 0 )
        NUM_3_SUBC = Q( LJTCS + 2 )
        NUM_JETS   = Q( LJTCS + 3 )
      ELSE
        LJTCS=LZFIDH(IXMAIN,'JTCS',0)
        IF (LJTCS.NE.0)THEN
          LCAPH = LQ(LJTCS+2)
          CONESIZE = Q(LCAPH+6)
        ELSE
          KERR=1
        ENDIF
        NUM_3_SUBC = IQ(LJTCS+2)
        NUM_JETS   = IQ(LJTCS+3)
      ENDIF
C

      NUM_SUBC   = NUM_3_SUBC - 3
C
      RETURN
      END
C
C*******************************************************************
