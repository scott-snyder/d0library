      SUBROUTINE PRSTRD(PRUNIT,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump TRD static parameters  head bank
C-        IFL = 0   Print bank STRD + validity runs of all static TRD banks
C-        IFL = 1   Print bank STRD
C-
C-   Inputs  : PRUNIT = output unit, IFL control flag
C-   Outputs : Printout
C-
C-   Created  19-OCT-1987   MANSOULIE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER PRUNIT,IFL
      INTEGER K,IBK,LBK,NBK
C
      WRITE(PRUNIT,1001)
      WRITE(PRUNIT,1002) (IC(LSTRD+K),K=1,4)      
C
      IF ( IFL.EQ.0 ) THEN
        WRITE (PRUNIT,1003)
      NBK = IC(LSTRD-2) 
      DO 10 IBK = 1,NBK
        LBK=LC(LSTRD-IBK)
        WRITE (PRUNIT,1004) IC(LBK-4),IC(LBK+1),IC(LBK+2)
   10 CONTINUE
      ENDIF
  999 RETURN
 1001 FORMAT(///,'    TRD STATIC PARAMETERS BANK ')
 1002 FORMAT(/,'  MIN RELEVANT RUN NUMBER ',I8,'       MAX ',I8,
     >         '  MIN RELEVANT DATE ',I8,'       MAX ',I8)
 1003 FORMAT(//,'        BANK           MIN RUN           MAX RUN')
 1004 FORMAT(//,8X,A4,10X,I8,10X,I8)
      END
