      SUBROUTINE PRESVC(PRUNIT,LESVC,NESVC,CFL,IFL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    PRINT OUT ESVC - CALIBRATION END TASK SAVE    CRITERIA
CC
CC    PRUNIT - UNIT NUMBER FOR PRINTOUT
CC    LESVC - BANK ADDRESS
CC    NESVC - BANK NUMBER
CC    CFL - FLAG TO CONTROL PRINT OUT
CC    IFL - HOW MUCH TO PRINT
CC
CC               SHOULD ONLY BE ONE ESVC BANK
CC    DH 1-87                                          
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSCPH.LINK'
      INCLUDE 'D0$LINKS:IZESVC.LINK'
      INTEGER PRUNIT,LESVC,NESVC,IFL,LSCPH,I,KESVC
      CHARACTER CFL*(*)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(CFL.EQ.'ALL') THEN               
        KESVC=0
        LSCPH=LC(LSTPH-IZSCPH)
        IF(LSCPH.NE.0) THEN
          KESVC=LC(LSCPH-IZESVC)
        ENDIF
        IF(KESVC.EQ.0) THEN
          WRITE(PRUNIT,100)
 100      FORMAT('0   NO ESVC BANK  '/)
        ELSE
          WRITE(PRUNIT,101) (IC(KESVC+I),I=1,15)                   
 101  FORMAT('0       BANK ESVC -- CALIBRATION SAVE    CRITERIA '//
     A ' Type ',I8,'      Status ',I5,'      Spare ',I5/
     A ' Minimum and Maximum Runs this set is valid for = ',2I6/     
     A ' Generated for run type                           ',I6/
     A ' Date and Time generated          ',2I12//
     A ' MASK 1 (turns on/off quality criteria)         ',O12/
     A ' MASK 2 (turns on/off old/new criteria)         ',O12/
     A '                               Spares           ',2I5/
     A ' Failure limit 1 (no. quality failures allowed) ',I5/   
     A ' Failure limit 2 (no. old/new failures allowed) ',I5/
     A ' Failure limit 3 (total no. failures allowed)   ',I5//)
        ENDIF
      ENDIF
      RETURN
      END
