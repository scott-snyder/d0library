      SUBROUTINE PRMSRH(PRUNIT,LMSRHI,NMSRH,CFL,IFL)
C.
C-    PRMSRH  - PRINT MSRH BANK                 NO  1.00 (27/06/90)
C.
C.    INPUT:
C.          PRUNIT - Unit Number for Printout
C.          LMSRHI - Bank Address
C.        * NMSRH  - Bank Number(if 0, all modules)
C.        * CFL    - Flag to control Printout(ONE/ALL/OLD/NEW/CURRENT)
C.        * IFL    - How many data want to print
C.
C.       (*)... Dummy Argument
C.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INTEGER PRUNIT,LMSRHI,NMSRH,IFL, JP,JPP,LMSRH
      CHARACTER*4 JNAME
      CHARACTER*36 CTITL(10)
      CHARACTER   CFL*(*)
C-
      DATA CTITL /'Bank VRSN','Status','Quality','Low_Run#','High_Run#'
     6, 'Generated Run','Generated Date','Generated Type of Run'
     9, 'Spare'
     1, 'spare' /
C-
      LMSRH = LMSRHI
      IF (LMSRH .EQ. 0)       GO TO 888
C-
C---  Print MSRH Bank   !   REPLACE -4   BY -5!!!   STR 27JUN90
      CALL UHTOC (IC(LMSRH-4), 4, JNAME, 4)
C-
      WRITE (PRUNIT,1000)
      WRITE (PRUNIT,1005) JNAME
      DO 100 JP=1,10
          WRITE (PRUNIT,1010) JP, CTITL(JP), IC(LMSRH+JP)
  100 CONTINUE
C---  Format Seq.
 1000 FORMAT(1H1)
 1005 FORMAT(1H0,6X,'******************************',/
     +,          7X,'*  Contents of Bank :  ',A4,'  *',/
     +,          7X,'******************************')
 1010 FORMAT(1X,I3,'.',2X,A36,' :',I10,10X)
C-
      RETURN
  888 CONTINUE
      WRITE (PRUNIT,1200)
 1200 FORMAT(1H1,' === @PRMSRH. NO MSRH BANK !!! ===')
      RETURN
      END
