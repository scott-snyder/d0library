      SUBROUTINE PRGSAT ( PRUNIT, LGSAT, NGSAT, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the content of GSAT bank(s)
C-
C-   Inputs  : PRUNIT - Unit number for printout
C-             LGSAT  - Address of the one bank (CFL='ONE') or of the 
C-                      first bank in a linear structure (CFL='LINEAR')
C-             NGSAT  - Bank number, used only if CFL='ONE' and LGSAT=0
C-             CFL    - Flag to control printout (ONE/LINEAR)
C-             IFL    - Amount of printing: 0 means full printout, 
C-                      1 is without hits
C-   Outputs : on unit PRUNIT
C-   Controls: 
C-
C-   Created  10-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LGSAT,NGSAT,IFL
      CHARACTER*(*) CFL
      CHARACTER*4 NAME
      INTEGER I, J, LGSAT1, NHITS, LAD, LZLOC
      INTEGER NST,NSE,NTU,NCH,NSP
      REAL    DRL
      CHARACTER*38 TITLE(10)
      DATA TITLE/'Track number in GEANT', 
     &'Particle type in GEANT',
     &'x-coordinate of track"s origin', 
     &'y-coordinate of track"s origin',
     &'z-coordinate of track"s origin',
     &'Px (x projection of momentum)',
     &'Py (y projection of momentum)',
     &'Pz (z projection of momentum)',
     &'Energy of the particle',
     &'Number of hits in SAMUS per this track'/
C----------------------------------------------------------------------
C
      LGSAT1=LGSAT
      IF ( CFL.EQ.'LINEAR' ) THEN
        IF (LGSAT1.EQ.0) GOTO 990
      ELSEIF ( CFL.EQ.'ONE' ) THEN
        IF (LGSAT1.EQ.0) THEN
          IF (NGSAT.EQ.0) GOTO 990
          LGSAT1=LZLOC(IXMAIN,'GSAT',NGSAT)
        ENDIF
      ELSE
        WRITE (PRUNIT,1000) CFL
 1000   FORMAT (1H1/1X,'*** PRGSAT: illegal value of CFL = ',A/)
        GOTO 999
      ENDIF
C
C ****  Print main part of bank
C
      CALL UHTOC (IQ(LGSAT1-4),4,NAME,4)
  10  WRITE (PRUNIT,1010) NAME
 1010 FORMAT (1H1/25X,30(1H*)/25X,'*  Contents of Bank :  ',A4,'  *'/
     &25X,30(1H*)//)
      DO 20 I=1,10
        IF (I.LT.3.OR.I.GT.9) THEN
          WRITE (PRUNIT,1020) I,TITLE(I),IQ(LGSAT1+I)
 1020     FORMAT (1X,I4,2H. ,A38,2H :,I10)
        ELSE
          WRITE (PRUNIT,1030) I,TITLE(I),Q(LGSAT1+I)
 1030     FORMAT (1X,I4,2H. ,A38,2H :,F10.4)
        ENDIF
  20  CONTINUE
C
C ****  Print hits        
C
      NHITS=IQ(LGSAT1+10)
      IF (IFL.EQ.1.OR.NHITS.EQ.0) GOTO 100
      DO 30 I=1,NHITS
        WRITE (PRUNIT,1040) I
 1040   FORMAT (/1X,'Hit #',I2)
        NST=Q(LGSAT1+I*5+6)+0.1
        NSE=Q(LGSAT1+I*5+7)+0.1
        NTU=Q(LGSAT1+I*5+8)+0.1
        DRL=Q(LGSAT1+I*5+9)
        NSP=Q(LGSAT1+I*5+10)+0.1
        CALL SACHAN (NST,NSE,NTU,NCH)
        WRITE (PRUNIT,1050) NST,NSE,NTU,NCH,DRL,NSP
 1050   FORMAT (
     &  1X,'SAMUS station & section numbers for this hit :',2I5/
     &  1X,'Tube number and its electronic address       :',2I5/
     &  1X,'Drift length (in cm) for this hit            :',F10.5/
     &  1X,'Type of the secondary particle made this hit :',I10)
  30  CONTINUE        
C
C ****  Look for another bank if needed
C
  100 IF ( CFL.EQ.'ONE' ) GOTO 999
      IF ( CFL.EQ.'LINEAR' ) THEN
        LGSAT1 = LQ (LGSAT1)
        IF (LGSAT1.NE.0) GOTO 10
      ENDIF
      GOTO 999
C
C ****  Errors: bank is absent
C
  990 WRITE (PRUNIT,1060)
 1060 FORMAT (1H1/1X,'*** PRGSAT: bank GSAT is absent'/)
C
  999 RETURN
      END
