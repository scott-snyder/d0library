      FUNCTION ISMG_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      merge ISAJET events
C-   Returned value  : true
C-
C-  ENTRY ISMG_DIA
C-      dialog to define 2nd file and number of merges
C-
C-   Created   2-APR-1991   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      LOGICAL ISMG_EVT, ISMG_DIA
      LOGICAL NOIN2,FLGCHK
      INTEGER UNIT2,NUM,N,LHEADN,LISAE,LISAEN,LOOP,I,IOS
C----------------------------------------------------------------------
      N=ABS(NUM)
      ISMG_EVT=.TRUE.
      IF ( N.LT.0 ) THEN
C       not yet implemented
        LOOP=0
      ELSE
        LOOP=N
      ENDIF
      IF(LOOP.GT.0) THEN
        DO I=1,LOOP
    1     CALL EVTIN2(UNIT2,IOS)
          IF(IOS.EQ.1) GOTO 1
          IF(IOS.NE.0) THEN
            ISMG_EVT=.FALSE.
            GOTO 999
          ENDIF
C           add new ISAE bank to old ISAE linear structure
          LHEADN=LQ(LHEAD)
          LISAEN=LQ(LHEADN-IZISAE)
          LISAE=LQ(LHEAD-IZISAE)
          CALL ZSHUNT(IXCOM,LISAEN,LISAE,0,0)
          CALL MZDROP(IXCOM,LHEADN,' ')
        ENDDO
      ENDIF
  999 RETURN
C
C
      ENTRY ISMG_DIA()
C
      IF(.NOT.FLGCHK('ISA_DIAL')) CALL FLGBK('ISA_DIAL',1)
      CALL FLGSET('ISA_DIAL',.TRUE.)
      CALL ZBINF2(UNIT2,NOIN2)
      ISMG_DIA=.NOT.NOIN2
      CALL OUTMSG(' Give number of events from 2nd file to be added to'
     &  //' each event.')
c      CALL OUTMSG(' If n is <0, events will be added to give')
c      CALL OUTMSG(' Poisson statistics with average -n.')
      NUM=1
      CALL GETPAR(1,' n [1]?','I',NUM)
      RETURN
      END
