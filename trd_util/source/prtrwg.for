      SUBROUTINE PRTRWG(PRUNIT,LBK,NBK,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print TRD electronic gains bank(s)
C-
C-   Inputs  : PRUNIT=  unit number for output
C-             LBK =  bank address(for the case 'SINGLE' is asked)
C-             CFL   =  'ALL'    print all electronics banks (3)
C-                      'SINGLE' print only bank with address LBK
C-             IFL   =  0  print everything
C-                      1  print only relevant min and max run numbers
C-
C-   Created  19-OCT-1987   MANSOULIE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTWGH.LINK'
C
      INTEGER PRUNIT,LBK,NBK,IFL
      CHARACTER CFL *(*)
      INTEGER IBK,K,NW,LTWGH,LAUX
      IF(LTGAI.EQ.0) THEN
        WRITE(PRUNIT,1000)
        GOTO 999
      ENDIF
      LTWGH=LC(LTGAI-IZTWGH)
      IF(LTWGH.EQ.0) THEN
        WRITE(PRUNIT,1000)
        GOTO 999
      ENDIF
      WRITE (PRUNIT,1001) IC(LTWGH+1),IC(LTWGH+2)
      IF( IFL.NE.0 ) GOTO 999
C
      IF(CFL.EQ.'SINGLE'.AND.(LBK.NE.0.OR.NBK.NE.0)) THEN
        IF ( LBK.EQ.0 ) THEN
          LAUX = LC(LTWGH-NBK)
        ELSE
          LAUX=LBK
        ENDIF
        NW=IC(LAUX-1)
        WRITE (PRUNIT,1002) (C(LAUX+K),K=1,NW)
      ELSE
        DO 10 IBK = 1,3
          WRITE(PRUNIT,1003) IBK
          LAUX=LC(LTWGH-IBK)
          NW=IC(LAUX-1)
          WRITE(PRUNIT,1002) (C(LAUX+K),K=1,NW) 
   10   CONTINUE
      ENDIF
C
  999 RETURN
 1000 FORMAT(///,'   TRD WIRE GAINS BANKS ARE ABSENT')
 1001 FORMAT(///,'  TRD WIRE GAINS BANK ,  MIN RELEVANT',
     >' RUN NUMBER ',I8,'      MAX  ',I8)
 1002 FORMAT(1X,16F7.2)
 1003 FORMAT(/,'   TRD WIRE GAINS BANK NUMBER ',I6)
      END
