      SUBROUTINE PRTRDT ( PRUNIT, LTRDT, NTRDT, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'TRDT'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LTRDT  [I] : Pointer to the one bank if different from 0
C-                          if equal to zero print all banks
C-
C-             NTRDT  [I] : kept for compatibility
C-             CFL    [C*]: kept for compatibility
C-             IFL    [I] : Kep for compatibility
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  20-OCT-1989   A. Zylberstejn
C-   Updated   9-MAR-1990   J.Fr. Glicenstein  New prints
C-   Updated  14-MAR-1991   A. Zylberstejn
C-   Updated  22-APR-1994   A. Zylberstejn  Simplify
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$LINKS:IZTRDT.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER I,J,PRUNIT, LTRDT, NTRDT, IFL,LTR
      INTEGER ICH,IDT,JDT,LTPRL,ULIKE,KKK,JBYT
      CHARACTER*(*) CFL
      CHARACTER*1 TID(4)
      INTEGER LTRDT1, GZTRDT
      REAL COR,QPRO,ENL(3)
      DATA TID/' ','V','D','F'/
C----------------------------------------------------------------------
      IF(PRUNIT.LE.0)GO TO 999
      IF(LTRDT.NE.0)THEN
        LTRDT1 = LTRDT
      ELSE! HERE, YOU HAVE TO FIND THE FIRST BANK TO BE PRINTED
        LTRDT1 = GZTRDT( )
      END IF
      IF (LTRDT1.LE.0) THEN
        CALL ERRMSG(' No existing trdt bank ','PRTRDT',' ','W')
        GOTO 999
      ENDIF
      WRITE(PRUNIT,1012)
      WRITE(PRUNIT,1014)
      WRITE(PRUNIT,1016)
 1012 FORMAT(' Dump TRDT bank',/,' -------------')
 1014 FORMAT(' Track    Energy     Electron      Energy per layer',
     &'     Cell')
 1016 FORMAT(8X,' tot.  trunc  accep. effic',3X,' 1        2       3',
     &    2X,' mult.')
 1020 FORMAT(1X,I3,2X,F6.1,1X,F6.1,3X,2F4.0,4X,3F6.2,4X,I7)
C     --------
    1 CONTINUE
C     --------
c      print*,' in prtrdt,ltrdt',ltrdt1
      DO ICH=1,3
        ENL(ICH)=0.
c        print*,' in prtrdt,layer',ich, ' ltprl',LQ(LTRDT1-ICH)
        IF(LQ(LTRDT1-ICH).NE.0)ENL(ICH)=Q(LQ(LTRDT1-ICH)+12)
c        if(enl(ich).gt.100.)print*,' in prtrdt,layer',ich,
c     &    ' energie anode',enl(ich)
      END DO
      WRITE(PRUNIT,1020)IQ(LTRDT1-5),(Q(LTRDT1+I),I=4,5),
     &    Q(LTRDT1+31),Q(LTRDT1+32), ENL,IQ(LTRDT1+2)
C
C  ***  Look if another bank is needed
C
c      PRINT*,' in PRTRDT ltrdt',LTRDT
      IF(LTRDT.EQ.0)THEN
        LTRDT1 = LQ( LTRDT1 )
        IF( LTRDT1 .NE. 0 ) GOTO 1
      END IF
c      PRINT*,' in prtrdt arrivee en 20'
   20 CONTINUE
  400 CONTINUE
  999 RETURN
C
 1010 FORMAT(' Number of hit planes, Anodes:',I2,' Cathodes',I2)
 1011 FORMAT(' anodes layer 1 cathodes layer 1',
     &       '  anodes layer 2 cathodes layer 2',
     &       '  anodes layer 3 cathodes layer 3',3(1X,'Nb.Clus.'))
 2100 FORMAT(/
     &    '  ** PRTRDT ** called for ONE without bank pointer and ',
     &    'bank number, LTRDT =',I10,' NTRDT =', I10/)
      END
