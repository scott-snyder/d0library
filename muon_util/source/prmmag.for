      SUBROUTINE PRMMAG(PRUNIT,LMMAGI,NMMAG,CFL,IFL)
C.
C-    PRMMAG  - PRINT MMAG BANK                 NO  1.00 (14/05/87)
C.
C.    INPUT:
C.          PRUNIT - Unit Number for Printout
C.          LMMAGI - Bank Address
C.        * NMMAG  - Bank Number(if 0, all modules)
C.        * CFL    - Flag to control Printout(ONE/ALL/OLD/NEW/CURRENT)
C.        * IFL    - How many data want to print
C.
C.       (*)... Dummy Argument
C.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INTEGER PRUNIT,LMMAGI,NMMAG,IFL,I,J
      INTEGER LMMAG,LMMAH,NUMIDB(9),ICOL,LLS,NBNKS,JBK
      REAL     BUFC(29,9)
      INTEGER IBUFC(29,9)
      EQUIVALENCE (IBUFC(1,1),BUFC(1,1))
      CHARACTER*4 JNAME,SNAME(9)
      CHARACTER*8 FMFLO,FMINT,FMA
      CHARACTER*32 CTITL(29), FMCHAR
      CHARACTER   CFL*(*)
      INTEGER NZBANK
C-
      DATA FMCHAR(1:22)    /'(1X,I3,1H.,2X,A32,1H:,'/
      DATA FMINT,FMFLO,FMA /'9I10)','9F10.2)','9A10)'/
      DATA CTITL /'Type','Status','Quality','Low_Run#','High_Run#'
     6, 'Generated Run','Generated Date','Generated Type of Run'
     9, 'Field Map Number',2*'Spare'
     2, 'Iron Slab Number','Iron Slab Name'
     4, 'Scale Params for Magnetic Fields','Hight(in local x)'
     6, 'Width(    "    y)','Length(    "    z)'
     8, 'X of Center of Slab','Y       "          '
     +, 'Z       "          '
     1, 'cos(x,X)',' " (x,Y)',' " (x,Z)',' " (y,X)',' " (y,Y)'
     6, ' " (y,Z)',' " (z,X)',' " (z,Y)',' " (z,Z)'/
       DATA ICOL /0/
C-
      IF (LMMAGI .EQ. 0)       GO TO 888
      LMMAH = LC (LMMAGI+1)
      LLS   = LMMAH - 1
      NBNKS = NZBANK(IXSTP,LLS)
      LMMAG = LC (LMMAH-1)
C-
      DO 500 JBK = 1,NBNKS
      IF (ICOL .EQ. 0) THEN      
        CALL VZERO(BUFC(1,1),261)
        CALL UHTOC (IC(LMMAG-4), 4, JNAME, 4)
      ENDIF
C-
C---  Store BUFC
      ICOL = ICOL + 1
      NUMIDB(ICOL) = IC(LMMAG-5)
      DO 100 J=1,29
        IF (J .LE. 12) THEN
          IBUFC(J,ICOL) = IC(LMMAG+J)
        ELSE IF (J .EQ. 13) THEN
          CALL UHTOC (IC(LMMAG+J), 4, SNAME(ICOL), 4)
        ELSE
           BUFC(J,ICOL) =  C(LMMAG+J)
        ENDIF
  100 CONTINUE
        LMMAG = LC (LMMAG)
        IF (LMMAG .EQ. 0) GO TO 200
        IF (ICOL .LT. 9)  GO TO 500
C-
C---  Print MMAG Bank
C-
  200 WRITE (PRUNIT,1000)
      WRITE (PRUNIT,1005) JNAME
      WRITE (PRUNIT,1015)
      WRITE (PRUNIT,1010) (NUMIDB(I), I=1,ICOL)
      WRITE (PRUNIT,1015)
      DO 300 J=1,29
      IF (J .LE. 12) THEN
        FMCHAR(23:28) = FMINT
        WRITE (PRUNIT,FMCHAR) J,CTITL(J),(IBUFC(J,I), I=1,ICOL)
      ELSE IF (J .EQ. 13) THEN
        FMCHAR(23:28) = FMA
        WRITE (PRUNIT,FMCHAR) J,CTITL(J),(SNAME(I),   I=1,ICOL)
      ELSE
        FMCHAR(23:30) = FMFLO
        WRITE (PRUNIT,FMCHAR) J,CTITL(J),( BUFC(J,I), I=1,ICOL)
      ENDIF
  300 CONTINUE
      ICOL = 0
  500 CONTINUE
C---  Format Seq.
 1000 FORMAT(1H1)
 1005 FORMAT(1H0,6X,'==============================',/
     +,          7X,'I  Contents of Bank :  ',A4,'  I',/
     +,          7X,'==============================')
 1010 FORMAT(22X,'Bank ID          =',9I10)
 1015 FORMAT(3X,130(1H-))
C-
      RETURN
  888 CONTINUE
      WRITE (PRUNIT,1200)
 1200 FORMAT(1H1,'=== @PRMMAG. NO MMAG BANK !!! ===')
      RETURN
      END
