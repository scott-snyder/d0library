
      SUBROUTINE PRTRLE(PRUNIT,LBK,NBK,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print TRD total energy likelihood bank
C-
C-   Inputs  : PRUNIT=  unit number for output
C-             LBK,NBK,CFL dummy arguments for routine call uniformity
C-             IFL   = 0   print everyhing
C-                   = 1   print only relevant run numbers
C-
C-
C-   Created  19-OCT-1987   MANSOULIE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTLIK.LINK'
      INCLUDE 'D0$LINKS:IZTRLE.LINK'
C
      INTEGER PRUNIT,LBK,NBK,IFL
      CHARACTER CFL *(*)
      INTEGER K,LAUX,NW
      IF(LTGEN.EQ.0) THEN
        WRITE(PRUNIT,1000)
        GOTO 999
      ENDIF
      LTLIK=LC(LTGEN-IZTLIK)
      IF(LTLIK.EQ.0) THEN
        WRITE(PRUNIT,1000)
        GOTO 999
      ENDIF
      WRITE (PRUNIT,1001) IC(LTLIK+1),IC(LTLIK+2)
      IF ( IFL.NE.0 ) GOTO 999
C
      LAUX = LC(LTLIK-IZTRLE)
      WRITE (PRUNIT,1002) IC(LAUX+1),C(LAUX+2)
      NW = IC(LAUX-1)-2
      LAUX=LAUX+2
      WRITE(PRUNIT,1003) (C(LAUX+K),K=1,NW)
C
  999 RETURN
 1000 FORMAT(///,'   TRD TOTAL ENERGY LIKELIHOOD BANK IS ABSENT')
 1001 FORMAT(///,'  TRD TOTAL ENERGY LIKELIHOOD BANK ,  MIN RELEVANT',
     >' RUN NUMBER ',I8,'      MAX  ',I8)
 1002 FORMAT(/,'   NUMBER  OF ENERGY BINS ',I8,/,
     >  '    ENERGY BIN WIDTH (KEV)  ',F10.2)
 1003 FORMAT(4X,10G12.3) 
      END
