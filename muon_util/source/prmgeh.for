      SUBROUTINE PRMGEH(PRUNIT,LMGEHI,NMGEH,CFL,IFL)
C.
C-    PRMGEH  - PRINT MGEH BANK                 NO  1.00 (29/04/87)
C.
C.    INPUT:
C.          PRUNIT - Unit Number for Printout
C.          LMGEHI - Bank Address
C.        * NMGEH  - Bank Number(if 0, all modules)
C.        * CFL    - Flag to control Printout(ONE/ALL/OLD/NEW/CURRENT)
C.        * IFL    - How many data want to print
C.
C.       (*)... Dummy Argument
C.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INTEGER PRUNIT,LMGEHI,NMGEH,IFL, JP,JPP
      CHARACTER*4 JNAME
      CHARACTER*36 CTITL(20)
      CHARACTER   CFL*(*)
C-
      DATA CTITL /'Type','Status','Quality','Low_Run#','High_Run#'
     6, 'Generated Run','Generated Date','Generated Type of Run'
     9, 'Average Resolution(drift)','         "        (wire)'
     1, 'Aluminium Wall Thickness','Density of Aluminium(g/cm**3)'
     3, 'Radiation Length of Al Wall(cm)'
     4, 'Interaction Length of Al Wall(cm)','Pad Pattern Number'
     6, 'Half Length of Pad Pattern(cm)','Half Width      "       (cm)'
     8, 'Minimum Width of inner Pad(cm)'
     9, 'Maximum          "        (cm)'
     +, 'Gap between inner and outer Pad'/
C-
      LMGEH = LMGEHI
      IF (LMGEH .EQ. 0)       GO TO 888
C-
C---  Print MGEH Bank
      CALL UHTOC (IC(LMGEH-4), 4, JNAME, 4)
C-
      WRITE (PRUNIT,1000)
      WRITE (PRUNIT,1005) JNAME
      DO 100 JP=1,10
          JPP = JP + 10
        IF (JP .LE. 8) THEN
          WRITE (PRUNIT,1010) JP, CTITL(JP), IC(LMGEH+JP)
     +,                       JPP,CTITL(JPP),C(LMGEH+JPP)
        ELSE
          WRITE (PRUNIT,1015) JP, CTITL(JP), C(LMGEH+JP)
     +,                       JPP,CTITL(JPP),C(LMGEH+JPP)
        ENDIF
  100 CONTINUE
C---  Format Seq.
 1000 FORMAT(1H1)
 1005 FORMAT(1H0,6X,'******************************',/
     +,          7X,'*  Contents of Bank :  ',A4,'  *',/
     +,          7X,'******************************')
 1010 FORMAT(1X,I3,'.',2X,A36,' :',I10,10X,I3,'.',2X,A36,' :',F10.2)
 1015 FORMAT(1X,I3,'.',2X,A36,' :',F10.2,10X,I3,'.',2X,A36,' :',F10.2)
C-
      RETURN
  888 CONTINUE
      WRITE (PRUNIT,1200)
 1200 FORMAT(1H1,' === @PRMGEH. NO MGEH BANK !!! ===')
      RETURN
      END
