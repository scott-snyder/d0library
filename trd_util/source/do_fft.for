      SUBROUTINE do_fft
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call the fast Fourier transform analysis
C-                          subroutine FOURAN
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  15-MAY-1991   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:FADCCN.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:trdbgu.INC'
      INCLUDE 'D0$INC:worksp.INC'
      INTEGER cha1,ICH,IER,IWI,UBIT,NW
      INTEGER INAT,LOUT,TRUNIT,TDATA(NMFADC+10),NWIRE(2),tchnb
      REAL YAUX(258,2)
      LOGICAL FIRST,DOPRINT
      EQUIVALENCE (IWS(1),TDATA(1)),(YAUX(1,1),WS(2000))
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        DOPRINT=SWTDBG.EQ.1
       LOUT=TRUNIT()
      END IF
      DOPRINT=DOPRINT .AND. TNEVDG.GT.0
      CALL VZERO(YAUX(1,1),512)
      NWIRE(1)=0
      NWIRE(2)=0
      DO  ICH =  1,  6
        INAT=(ICH-1)/3+1
        DO 10 IWI=1,nwire_per_layer(ich)
          if(.not.TWCOD(TCHNB(IWI,ICH)))go to 10
          CALL TCODER(CHA1,ICH-1,iWi-1,UBIT,2)
          CALL ZDEXPD(4,CHA1,TDATA)
            CALL VFLOAT(TDATA(3),WS(3000),TDATA(1))
            CALL VADD(WS(3000),YAUX(1,INAT),YAUX(1,INAT),TDATA(1))
            NWIRE(INAT)=NWIRE(INAT)+1
   10   CONTINUE
      END DO
          IF(NWIRE(1).NE.0)THEN
            WRITE(LOUT,*)' ANODES'
            CALL VSCALE(YAUX(1,1),1./FLOAT(NWIRE(1)),YAUX(1,1),258)
            CALL FOURAN(YAUX(1,1),258)
          END IF
          IF(NWIRE(2).NE.0)THEN
            CALL VSCALE(YAUX(1,2),1./FLOAT(NWIRE(2)),YAUX(1,2),258)
            WRITE(LOUT,*)' CATHODES'
            CALL FOURAN(YAUX(1,2),258)
          END IF
  999 RETURN
      END
