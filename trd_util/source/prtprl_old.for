      SUBROUTINE PRTPRL_OLD ( PRUNIT, LTRDT ,NTRDT ,CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'TPRL'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LTRDT      : Pointer to the up bank
C-                         =0 if not called from PRTRDT
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  30-OCT-1989   A. Zylberstejn
C-   Updated  17-JAN-1990   A. Zylberstejn
C-   Updated  19-MAR-1990   J.Fr. Glicenstein  Prints clusters
C-   Updated  26-JAN-1993   Alain PLUQUET  PRTPRL_OLD.FOR = old version of 
C-                                         PRTPRL.FOR (for TPRL versions<2)
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:tcntrl.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTPRL.LINK'
C
      INTEGER LZLOC
      INTEGER GZTRDT,I,ICH,ICM,J,LTRDT,NA(3),NC(3),NM
      INTEGER PRUNIT, NTRDT, IFL,LTPRL
      INTEGER IC,K,LL,NCLUST(3)
      REAL COR,QI
      CHARACTER*1 CAR1,DOT
      CHARACTER*3 CAR3(6)
      CHARACTER*4 CAR4
      CHARACTER*6 CAR6(6)
      CHARACTER*(*) CFL
      DATA DOT/'.'/
C----------------------------------------------------------------------
C
C  ***  Print the content of the bank pointed by LTPRL
C
      IF(PRUNIT.LE.0)GO TO 999
C  If LTRDT =0 get the supporting bank
      IF(LTRDT.LE.0 .OR.CFL.NE.'ONE')THEN
        LTRDT = GZTRDT()
        IF (LTRDT.LE.0) RETURN          ! No TRDT bank found
        IF(CFL.EQ.'ONE') THEN
          IF( NTRDT .LE. 0 ) GOTO 980                 ! Error exit
          LTRDT  = LZLOC( IXMAIN, 'TRDT', NTRDT)
          IF (LTRDT.LE.0)    GOTO 980                 ! Error exit
        END IF
      ENDIF
      COR=1.
c      IF(Q(LTRDT+10).GT.0.)COR=Q(LTRDT+4)/Q(LTRDT+10) !global energy
c      ! correction factor
C
   20 NM=0
      DO 10 ICH=1,3
        LTPRL=LQ(LTRDT-ICH)
        NA(ICH)=IQ(LTPRL+14)
        NC(ICH)=IQ(LTPRL+15)
        NM=MAX0(NA(ICH),NC(ICH),NM)
        NCLUST(ICH)=IQ(LTPRL+16)
   10 CONTINUE
      IF(NM.LE.0)GO TO 999
      DO 100 I=1,NM
        DO 60 ICH=1,3
          CAR3(2*ICH-1)='   '
          CAR3(2*ICH)='   '
          CAR6(2*ICH-1)='      '
          CAR6(2*ICH)='      '
          LTPRL=LQ(LTRDT-ICH)
          LL=LTPRL+17+I
          IF(NA(ICH).LE.0)GO TO 60
          IF(I.LE.NA(ICH))THEN
            QI=Q(LL+NA(ICH))*COR
            IF(QI.GT.0.)THEN
              WRITE(CAR3(2*ICH-1),'(i3)')INT(Q(LL))
              WRITE(CAR4,'(i4)')INT(QI)
              WRITE(CAR1,'(i1)')MOD(INT(10.*QI),10)
              CAR6(2*ICH-1)=CAR4//DOT//CAR1
            END IF
          END IF
          IF(I.GT.NC(ICH))GO TO 60
          LL=LL+2*NA(ICH)
          QI=Q(LL+NC(ICH))*COR
          IF(QI.GT.0.)THEN
            WRITE(CAR3(2*ICH),'(i3)')INT(Q(LL))
            WRITE(CAR4,'(i4)')INT(QI)
            WRITE(CAR1,'(i1)')MOD(INT(10.*QI),10)
            CAR6(2*ICH)=CAR4//DOT//CAR1
          END IF
   60   CONTINUE
        IF (I.EQ.1) THEN
          WRITE(PRUNIT,1018)(CAR3(J),CAR6(J),J=1,6),(NCLUST(J),J=1,3)
        ELSE
          WRITE(PRUNIT,1016)(CAR3(J),CAR6(J),J=1,6)
        ENDIF
 1016   FORMAT(6(1X,A3,3X,A6,4X))
 1018   FORMAT(6(1X,A3,3X,A6,4X),3(I4,5X))
  100 CONTINUE
      IF(CFL.NE.'ONE')THEN
        LTRDT = LQ( LTRDT )
        IF( LTRDT .NE. 0 ) GOTO 20
      END IF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LTRDT
 2000 FORMAT(/' ** PRTPRL ** called for LINEAR without valid bank ',
     &        'pointer, LTPRL =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LTRDT,NTRDT
 2100 FORMAT(/
     &      '  ** PRTPRL ** called for ONE without bank pointer and ',
     &        'bank number, LTRDT =',I10,' NTRDT =', I10/)
      GOTO 999
      END
