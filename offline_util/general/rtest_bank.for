      LOGICAL FUNCTION RTEST_BANK(LBANK,IWORD,COMPA,LIST,NLIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO TEST WORD IWORD IN BANK AT LBANK (ZEBCOM) 
C-                         COMPARING IT TO LIST OF LENGTH NLIST AND SETTING
C-                         RETURN VALUE OF ITEST_BANK TRUE IF CONDITION IS MET
C-                              
C-   Inputs  : LBANK [I]    = BANK ADDRESS
C-             IWORD [I]    = WORD NUMBER WITH LBANK
C-             COMPA [I]    = TYPE OF COMPARISON TO BE MADE EQ,NE,GT,LT,GE,LE
C-                            1=LT,2=LE,3=EQ,4=NE,5=GE,6=GT
C-             LIST(*) [R]  = LIST OF VALUES TO BE COMPAIRED WITH 
C-             NLIST  [I]   = NUMBER OF VALUES IN LIST
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  12-FEB-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LBANK,IWORD,COMPA,NLIST,I
      REAL LIST(*),RDATA
      LOGICAL TEST
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      TEST = .FALSE.
    1 RDATA = Q(LBANK+IWORD)
      DO I = 1, NLIST
        GOTO (11,12,13,14,15,16 ) COMPA
   11   IF (RDATA.LT.LIST(I)) TEST=.TRUE.
        GOTO 20
   12   IF (RDATA.LE.LIST(I)) TEST=.TRUE.
        GOTO 20
   13   IF (RDATA.EQ.LIST(I)) TEST=.TRUE.
        GOTO 20
   14   IF (RDATA.NE.LIST(I)) TEST=.TRUE.
        GOTO 20
   15   IF (RDATA.GE.LIST(I)) TEST=.TRUE.
        GOTO 20
   16   IF (RDATA.GT.LIST(I)) TEST=.TRUE.
        GOTO 20
   20   CONTINUE
        IF (TEST) GOTO 21
      END DO
   21 CONTINUE
      RTEST_BANK = TEST
      IF (TEST) GOTO 999
      LBANK = LQ(LBANK)
      IF (LBANK.GT.0) GOTO 1
  999 RETURN
      END
