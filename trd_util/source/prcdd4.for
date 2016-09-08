      SUBROUTINE PRCDD4 (LUNOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PRINT ZEBRA BANK CDD4 (TRD RAW DATA)
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  14-JAN-1988   A. ZYLBERSTEJN
C-   Updated  12-JUN-1989   A. Zylberstejn  New addressing scheme :call to
C-                                          TCODER
C-   Updated  29-DEC-1989   A. Zylberstejn  Modif. to read version 2
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:FADCCN.INC'
C      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:TRINFO.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INTEGER CHA,CHL, I,IADR,ICR,ILAY,ISH,ISUB,ITRIP,IWIRE,J, K,LL
      INTEGER NWHIT(6),IVERS,VTYPE,IERR
      INTEGER IPR,IS,JSH,MASK16, NCHAN,ND,IEND,NBBYT,NUP,JJ,UBIT,CHA1
      INTEGER SHIFUP,NPULWI,NCHANL, LUNOUT
      LOGICAL FIRST
      DATA IPR/0/
      DATA MASK16/65535/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( LHEAD .EQ. 0 ) THEN
        CALL INTMSG('  **** Error in PRCDD4 ')
        CALL INTMSG(' Header bank LHEAD not booked')
        GO TO 999
      ENDIF
      CALL VZERO(NWHIT,6)
C
C ****  PRINT CDD4
C
      LL = LQ ( LHEAD - IZCDD4 )
      IF ( LL .EQ. 0 ) THEN
        CALL INTMSG('  **** Error in PRCDD4 ')
        CALL INTMSG(' bank CDD4 not booked')
        GO TO 999
      END IF
      IF(FIRST)THEN
        CALL ZRD_VERSION(4,IVERS,VTYPE,IERR)
        IF( IERR .NE. 0) THEN
          CALL ERRMSG('Bank not booked','PRCDD4',
     &              'Bank CDD4 does not exist or wrong data cable','I')
          GO TO 999
        ENDIF
        SHIFUP=0
        NPULWI=0
        NCHANL=0
        IF( IVERS .GT. 0) THEN
          SHIFUP=4
          NPULWI=4
          NCHANL=8
        ENDIF
      END IF
C*
      ND=IQ(LL-1)
      IEND=LL+ND-SHIFUP
      ISH=8
   20 IF(LL.LE.0)GO TO 999
      CHL=iand(IQ(IEND),MASK16) -NCHANL          !CHANNEL LENGTH
C      CHA1=JISHFT(IQ(IEND),-16).AND.MASK16  !CHANNEL ADDRESS
      CHA=IBITS(IQ(IEND),16,16)
C      IF(CHA.NE.CHA1)
      NCHAN=iand(IQ(IEND-1),MASK16) -  NPULWI       !LENGTH
      NCHAN=(NCHAN-2)/4                           !NUMBER OF DATA WORDS
      if(nchan.le.0)go to 999
C GET WIRE AND LAYER
      CALL TCODER(CHA,ILAY,IWIRE,UBIT,1)
      ILAY=ILAY+1
      IWIRE=IWIRE+1
      NWHIT(ILAY)=NWHIT(ILAY)+1
      IWS(2000+(ILAY-1)*300+NWHIT(ILAY))=IWIRE
      K=0
      NBBYT=CHL
      IF(IVERS.LE.0)NBBYT=CHL-3
      NUP=NBBYT/4
      IS=IEND-2-NUP
      IF( IPR. LE.10)THEN
        IPR=IPR+1
        DO 40 I=1,NCHAN
          JJ=IS+I
C  FOR THE TIME BEING ,WRITE THE FADC CONTENT IN IWS (WORKING SPACE)
          DO 38 J=1,4
            K=K+1
            JSH=ISH*(4-J)
C          IWS(K)=JISHFT(IQ(JJ),-JSH).AND.255
            IWS(K)=IBITS(IQ(JJ),JSH,8)
            WS(1000+K)=IWS(K)
   38     CONTINUE
   40   CONTINUE
        IF(PRTRD.GE.9 ) CALL DRAWPT(WS(1001),NFADC,1,LUNOUT)
      END IF
      IEND=IS
      IF(IS.GT.LL-6)GO TO 20
  999 CONTINUE
      WRITE(LUNOUT,*)' BANK CDD4'
      DO 800 ILAY=1,6
        IF(NWHIT(ILAY).LE.0)THEN
          WRITE(LUNOUT,*)' Layer',ILAY,' No hit wires'
        ELSE
          LL=MIN0(20,NWHIT(ILAY))
          WRITE(LUNOUT,1044)ILAY,(IWS(2000+(ILAY-1)*300+I),I=1,LL)
 1044     FORMAT(' Layer',I2,' Hit Wires:',20I4)
          IF(NWHIT(ILAY).GT.LL)WRITE(LUNOUT,'(18X,20I4)')
     +               (IWS(2000+(ILAY-1)*300+I),I=LL+1, NWHIT(ILAY))
        END IF
  800 CONTINUE
      RETURN
 1000 FORMAT(' CHANNEL LENGTH',I4,' CHANNNEL ADDRESS',I8,' NUMBER OF DAT
     +A WORDS ',I8)
 1010 FORMAT(' SUB-ADRESS',I3,' MODULE',I3,' CRATE',I2,' TRIPLET',I2,
     +' LAYER',I2,' WIRE ',I4)
 1011 FORMAT(' SUB-ADRESS',I3,' MODULE',I3,' CRATE',I2,' TRIPLET',I2,
     +' LAYER',I2,' STRIP',I4)
      END
