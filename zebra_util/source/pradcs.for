
      SUBROUTINE PRADCS(PRUNIT,LADCS,NADCS,CFL,IFL)
C---------------------------------------------------------
C-
C-  Print out for ADCS (Calibration working SUMMARY bank)
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LADCS = bank address 
C-  NADCS = bank number
C-  CFL   = character switch
C-  IFL   = step number
C-
C---------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBWRK.INC'
      INTEGER PRUNIT,LADCS,NADCS,ISTP,IFL,MAX_ADCS,I
      INTEGER NDATA,IA,LOC
      CHARACTER CFL*(*)
C                                                          
      ISTP=IFL
      IF(LADCS.LE.0) THEN
        WRITE(PRUNIT,100) LADCS,NADCS,ISTP
        GO TO 999
      ENDIF
C
      WRITE(PRUNIT,101) ISTP
      MAX_ADCS=IW(LADCS-1)/6
      IF(CFL.EQ.'ONE') THEN
        NDATA=MIN0(MAX_ADCS,384)
      ELSE 
        NDATA=MAX_ADCS
      ENDIF
C 
      DO 10 IA = 1 , NDATA 
        LOC=LADCS+(IA-1)*6
        WRITE(PRUNIT,102) (IW(LOC+I),I=1,3),(W(LOC+I),I=4,6)
 10   CONTINUE
C
 100  FORMAT(/,' Wrong Address ADCS: LADCS= ',I8,' ; NADCS =',I8
     $,' ; STEP# = ',I4/)
 101  FORMAT(//
     $' ============================================'/
     $'     ADCS:  Working Summary Bank for ADCS    '/
     $'     STEP# = ',I4/
     $' ============================================'//
     $'     ADC_ID   ; BAD_FLAG  ;   NOENT   ;    AVR     ;    SIG    '
     $,'; AVR-PEAK '/)
 102  FORMAT(3I12,3F12.2)
C
 999  RETURN      
      END
