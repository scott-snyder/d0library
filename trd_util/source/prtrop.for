      SUBROUTINE PRTROP(PRUNIT,LBK,NBK,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print TRD operating conditions bank
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
      INCLUDE 'D0$LINKS:IZTROP.LINK'
C
      INTEGER PRUNIT,LBK,NBK,IFL
      CHARACTER CFL *(*)
      INTEGER K,LTROP,LAUX,NW
      IF(LTGEN.EQ.0) THEN
        WRITE(PRUNIT,1000)
        GOTO 999
      ENDIF
      LTROP=LC(LTGEN-IZTROP)
      IF(LTROP.EQ.0) THEN
        WRITE(PRUNIT,1000)
        GOTO 999
      ENDIF
      WRITE (PRUNIT,1001) IC(LTROP+1),IC(LTROP+2)
      IF ( IFL.NE.0 ) GOTO 999
C
      WRITE (PRUNIT,1002) C(LTROP+3),C(LTROP+4)
      NW = IC(LTROP-1)-2
      LAUX=LTROP+2
      WRITE(PRUNIT,1003) (C(LAUX+K),K=1,NW)
C
  999 RETURN
 1000 FORMAT(///,'   TRD OPERATING CONDITIONS BANKS ARE ABSENT')
 1001 FORMAT(///,'  TRD OPERATING CONDITIONS BANK ,  MIN RELEVANT',
     >' RUN NUMBER ',I8,'      MAX  ',I8)
 1002 FORMAT(/,'   GAS GAIN (FOR 0 DRIFT LENGTH) (PC/KEV) ',G12.3,/,
     >  '    GAS ATTACHMENT COEFFICIENT AT 1 CM  ',F10.4)
 1003 FORMAT(4X,10G12.3) 
      END
