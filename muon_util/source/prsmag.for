      SUBROUTINE PRSMAG ( PRUNIT, LSMAG, NSMAG, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the content of SMAG bank(s)
C-
C-   Inputs  : PRUNIT - Unit number for printout
C-             LSMAG  - Address of the one bank (CFL='ONE') or of the 
C-                      first bank in a linear structure (CFL='LINEAR')
C-             NSMAG  - Bank number, used only if CFL='ONE' and LSMAG=0
C-             CFL    - Flag to control printout (ONE/LINEAR)
C-             IFL    - Amount of printing: 0 means full printout, 
C-   Outputs : on unit PRUNIT
C-   Controls: 
C-
C-   Created  02-MAY-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT,LSMAG,NSMAG,IFL
      CHARACTER*(*) CFL
      CHARACTER*4 NAME
      INTEGER I, LSMAG1, LZLOC
      CHARACTER*36 TITLE(25),TITL1(13),TITL2(12)
      EQUIVALENCE (TITLE(1),TITL1(1)),(TITLE(14),TITL2(1))
      DATA TITL1/'Bank version number                 ',  
     &           'Number of the SAMUS magnet          ',  
     &           'Name of the SAMUS magnet            ',  
     &           'Tracking medium number of the magnet',  
     &           'Mother volume for the magnet        ',  
     &           'X half size of the magnet           ',  
     &           'Y half size of the magnet           ',  
     &           'Z half size of the magnet           ',  
     &           'X half size of the magnet hole      ',  
     &           'Y half size of the magnet hole      ',  
     &           'Z half size of the magnet hole      ',  
     &           'X position in Mother volume         ',  
     &           'Y position in Mother volume         '/
      DATA TITL2/'Z position in mother volume         ',  
     &           'Rotation matrix number              ',  
     &           'Rotaion matrix, cos(X",X)           ',
     &           '    :           cos(X",Y)           ',
     &           '    :           cos(X",Z)           ',
     &           '    :           cos(Y",X)           ',
     &           '    :           cos(Y",Y)           ',
     &           '    :           cos(Y",Z)           ',
     &           '    :           cos(Z",X)           ',
     &           '    :           cos(Z",Y)           ',
     &           '    :           cos(Z",Z)           ',
     &           'Rotaion flag (zero if no rotation)  '/  
C----------------------------------------------------------------------
C
      LSMAG1=LSMAG
      IF ( CFL.EQ.'LINEAR' ) THEN
        IF (LSMAG1.EQ.0) GOTO 990
      ELSEIF ( CFL.EQ.'ONE' ) THEN
        IF (LSMAG1.EQ.0) THEN
          IF (NSMAG.EQ.0) GOTO 990
          LSMAG1=LZLOC(IDVSTP,'SMAG',NSMAG)
        ENDIF
      ELSE
        WRITE (PRUNIT,1000) CFL
 1000   FORMAT (1H1/1X,'*** PRSMAG: illegal value of CFL = ',A/)
        GOTO 999
      ENDIF
C
C ****  Print bank
C
      CALL UHTOC (IC(LSMAG1-4),4,NAME,4)
   10 WRITE (PRUNIT,1010) NAME
 1010 FORMAT (1H1/25X,30(1H*)/25X,'*  Contents of Bank :  ',A4,'  *'/
     &25X,30(1H*)//)
      DO 50 I=1,25
        IF (I.GE.6.AND.I.LE.24.AND.I.NE.15) THEN
          WRITE (PRUNIT,1020) I,TITLE(I),C(LSMAG1+I)
 1020     FORMAT (1X,I4,2H. ,A36,2H :,F10.5)
        ELSEIF (I.EQ.3.OR.I.EQ.5) THEN
          WRITE (PRUNIT,1030) I,TITLE(I),IC(LSMAG1+I)
 1030     FORMAT (1X,I4,2H. ,A36,2H :,6X,A4)
        ELSE
          WRITE (PRUNIT,1040) I,TITLE(I),IC(LSMAG1+I)
 1040     FORMAT (1X,I4,2H. ,A36,2H :,I10)
        ENDIF
   50 CONTINUE
C
C ****  Look for another bank if needed
C
  100 IF ( CFL.EQ.'ONE' ) GOTO 999
      IF ( CFL.EQ.'LINEAR' ) THEN
        LSMAG1 = LC(LSMAG1)
        IF (LSMAG1.NE.0) GOTO 10
      ENDIF
      GOTO 999
C
C ****  Errors: bank is absent
C
  990 WRITE (PRUNIT,1060)
 1060 FORMAT (1H1/1X,'*** PRSMAG: bank SMAG is absent'/)
C
  999 RETURN
      END
