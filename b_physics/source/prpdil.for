      SUBROUTINE PRPDIL ( PRUNIT,LPDIL,NPDIL,CFL,IFL )
C----------------------------------------------------------------------
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LPDIL= bank address 
C-  NPDIL = bank number
C-  CFL   = flag to control printout
C-          'ALL' for all banks 
C-          'ONE' for one bank only
C-  IFL     not used
C-
C-   Created  4-DEC-1991   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPDIL.LINK'
      INTEGER PRUNIT,LPDIL,NPDIL,IFL,KPDIL,GZPDIL,LZLOC,K
      CHARACTER*(*) CFL
      KPDIL = LPDIL
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LPDIL .LE. 0 ) GOTO 998
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LPDIL .LE. 0 ) THEN
          IF( NPDIL .EQ. 0 ) GOTO 998          
          KPDIL = LZLOC( IXMAIN,'PDIL',NPDIL )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
        KPDIL = GZPDIL(0)
      ENDIF
      IF(KPDIL.LE.0) GOTO 999
    1 CONTINUE
C
C  *  Print the content of the bank pointed by KPDIL
C
      WRITE( PRUNIT,101 ) IQ(KPDIL+1)
  101 FORMAT(//,'  PDIL bank  Version #',I3)
      WRITE( PRUNIT,102 ) 
  102 FORMAT(/,'  Dilepton_ID  Lepton_ID    NDF   Fit_quality  NSTEP ',
     1       '  CHISQ    PROB')
      WRITE( PRUNIT,103 ) (IQ(KPDIL+K),K=2,6),Q(KPDIL+26),Q(KPDIL+27)
  103 FORMAT(5I10,2E10.2)
      WRITE( PRUNIT,104 ) 
  104 FORMAT(/,'     MASS      PT_MIN1    PT_MIN2    PT_MIN  ',
     1         '     E1         E2         E3         E4         E5')
      WRITE( PRUNIT,105 ) (Q(KPDIL+K),K=7,15) 
  105 FORMAT(9E11.3)
      WRITE( PRUNIT,106 ) 
  106 FORMAT(/,'       P1        THE1       PHI1       P2    ',
     1         '     THE2      PHI2         P3        THE3       PHI3 ')
      WRITE( PRUNIT,107 ) (Q(KPDIL+K),K=16,24) 
  107 FORMAT(3(E11.3,2F11.4))
      IF( CFL .EQ. 'ONE' ) GOTO 999
      KPDIL = LQ( KPDIL )
      IF( KPDIL .NE. 0) GOTO 1
      GO TO 999
  998 WRITE( PRUNIT,200) 
  200 FORMAT(/'  PRPDIL  called for invalid bank ',
     &        'pointer,LPDIL =',I10/)
  999 RETURN
      END
