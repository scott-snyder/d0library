      SUBROUTINE PRSBPG ( PRUNIT, LSBPG, NSBPG, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the content of SBPG bank(s)
C-
C-   Inputs  : PRUNIT - Unit number for printout
C-             LSBPG  - Address of the one bank (CFL='ONE') or of the 
C-                      first bank in a linear structure (CFL='LINEAR')
C-             NSBPG  - Bank number, used only if CFL='ONE' and LSBPG=0
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
      INTEGER PRUNIT,LSBPG,NSBPG,IFL
      CHARACTER*(*) CFL
      CHARACTER*4 NAME
      INTEGER I, LSBPG1, NPAR, L, LZLOC
      CHARACTER*38 TITLE(12)
      DATA TITLE/'Bank version number                   ',
     &           'Name of Volume                        ',
     &           'Shape of Volume                       ',
     &           'GEANT tracking medium number          ',
     &           'Mother volume from which this hangs   ',
     &           'Keyword to position (e.g POS)         ',
     &           'Rotation matrix number                ',
     &           'Copy number of this volume            ',
     &           'X position in Mother volume           ',
     &           'Y position in Mother volume           ',
     &           'Z position in mother volume           ',
     &           'Number of parameters describing volume'/
C----------------------------------------------------------------------
C
      LSBPG1=LSBPG
      IF ( CFL.EQ.'LINEAR' ) THEN
        IF (LSBPG1.EQ.0) GOTO 990
      ELSEIF ( CFL.EQ.'ONE' ) THEN
        IF (LSBPG1.EQ.0) THEN
          IF (NSBPG.EQ.0) GOTO 990
          LSBPG1=LZLOC(IDVSTP,'SBPG',NSBPG)
        ENDIF
      ELSE
        WRITE (PRUNIT,1000) CFL
 1000   FORMAT (1H1/1X,'*** PRSBPG: illegal value of CFL = ',A/)
        GOTO 999
      ENDIF
C
C ****  Print main part of bank
C
      CALL UHTOC (IC(LSBPG1-4),4,NAME,4)
   10 WRITE (PRUNIT,1010) NAME
 1010 FORMAT (1H1/25X,30(1H*)/25X,'*  Contents of Bank :  ',A4,'  *'/
     &25X,30(1H*)//)
      DO 50 I=1,12
        GOTO (20,30,30,20,30,30,20,20,40,40,40,20), I
   20   WRITE (PRUNIT,1020) I,TITLE(I),IC(LSBPG1+I)
 1020   FORMAT (1X,I4,2H. ,A38,2H :,I10)
        GOTO 50
   30   WRITE (PRUNIT,1030) I,TITLE(I),IC(LSBPG1+I)
 1030   FORMAT (1X,I4,2H. ,A38,2H :,6X,A4)
        GOTO 50
   40   WRITE (PRUNIT,1040) I,TITLE(I),C(LSBPG1+I)
 1040   FORMAT (1X,I4,2H. ,A38,2H :,F10.5)
   50 CONTINUE
C
C ****  Print volume parameters
C
      NPAR=IC(LSBPG1+12)
      DO 60 I=1,NPAR
        L=12+I
        WRITE (PRUNIT,1050) L,I,C(LSBPG1+L)
 1050   FORMAT (1X,I4,2H. ,'Parameter #',I2,25X,2H :,F10.5)
   60 CONTINUE        
C
C ****  Look for another bank if needed
C
  100 IF ( CFL.EQ.'ONE' ) GOTO 999
      IF ( CFL.EQ.'LINEAR' ) THEN
        LSBPG1 = LC(LSBPG1)
        IF (LSBPG1.NE.0) GOTO 10
      ENDIF
      GOTO 999
C
C ****  Errors: bank is absent
C
  990 WRITE (PRUNIT,1060)
 1060 FORMAT (1H1/1X,'*** PRSBPG: bank SBPG is absent'/)
C
  999 RETURN
      END
