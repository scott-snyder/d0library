      SUBROUTINE GET_STORE_DIV(ISTOR,IDIV,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : gets store and division. Experiment
C-                         dependent part is factorized into this routine
C-
C-   Inputs  :
C-   Outputs : ISTOR,IDIV STORE AND DIVISION NEEDED. IER.NE.0 BAD INPUT
C-   Controls:
C-
C-   Created  23-OCT-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C-- The ZEBWRK bank is being handled thusly so as to avoid a big 
C-- space allocation in jobs that do not use the common block.
      COMMON/ZEBWRK/IXWRK,IDVWRK
      INTEGER IXWRK,IDVWRK
C--------------The bank below has had its variables renamed so
C--------------as not to conflict with ZEBCOM.
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(9000)
      EQUIVALENCE (QG(1),IQG(1),LQG(9)),(LQG(1),LMAIN)
C
      INTEGER   NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,IQG(1),LQG(8000)
      REAL      GVERSN,ZVERSN,FENDQ,WS,QG(1)
      INTEGER ISTOR,IDIV,ZCOM,ZDIV
      INTEGER IER
C----------------------------------------------------------------------
C
      IER = 0
      CALL GETPAR(1,'ZEBCOM(0),ZEBSTP(1),GEANT(2),ZEBWRK(3) >'
     &    ,'I',ZCOM)
      IF (ZCOM.EQ.0)THEN
        ISTOR = IXCOM
        IDIV = IXMAIN
      ELSEIF(ZCOM.EQ.1)THEN
        ISTOR = IXSTP
        IDIV = IDVSTP
      ELSEIF (ZCOM.EQ.2) THEN
        ISTOR = IXSTOR
        CALL GETPAR(1,'DIVISION IXDIV(0) OR IXCONS(1) >','I',ZDIV)
        IF ( ZDIV.EQ.0 ) THEN
          IDIV = IXDIV
        ELSEIF(ZDIV.EQ.1)THEN
          IDIV = IXCONS
        ENDIF
      ELSEIF ( ZCOM.EQ.3 ) THEN
        ISTOR = IXWRK
        IDIV = IDVWRK
      ELSE
        IER = 1
      ENDIF
C
  999 RETURN
      END
