      SUBROUTINE PRMMAH(PRUNIT,LMMAHI,NMMAH,CFL,IFL)
C.
C-    PRMMAH  - PRINT MMAH BANK                 NO  1.00 (08/05/87)
C.
C.    INPUT:
C.          PRUNIT - Unit Number for Printout
C.          LMMAHI - Bank Address
C.        * NMMAH  - Bank Number(if 0, all modules)
C.        * CFL    - Flag to control Printout(ONE/ALL/OLD/NEW/CURRENT)
C.        * IFL    - How many data want to print
C.
C.       (*)... Dummy Argument
C.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INTEGER PRUNIT,LMMAH,LMMAHI,NMMAH,IFL, JP,JPP
      CHARACTER*4 JNAME
      CHARACTER*36 CTITL(10)
      CHARACTER   CFL*(*)
C-
      DATA CTITL /'Type','Status','Quality','Low_Run#','High_Run#'
     6, 'Generated Run','Generated Date','Generated Type of Run'
     9, 2*'Spare'/
C-
      LMMAH = LMMAHI
      IF (LMMAH .EQ. 0)       GO TO 888
C-
C---  Print MMAH Bank
      CALL UHTOC (IC(LMMAH-4), 4, JNAME, 4)
C-
      WRITE (PRUNIT,1000)
      WRITE (PRUNIT,1005) JNAME
      DO 100 JP=1,10
          WRITE (PRUNIT,1010) JP, CTITL(JP), IC(LMMAH+JP)
  100 CONTINUE
C---  Format Seq.
 1000 FORMAT(1H1)
 1005 FORMAT(1H0,6X,'******************************',/
     +,          7X,'*  Contents of Bank :  ',A4,'  *',/
     +,          7X,'******************************')
 1010 FORMAT(1X,I3,'.',2X,A36,' :',I10,10X,I3,'.',2X,A36,' :',F10.2)
C-
      RETURN
  888 CONTINUE
      WRITE (PRUNIT,1200)
 1200 FORMAT(1H1,' === @PRMMAH. NO MMAH BANK !!! ===')
      RETURN
      END
