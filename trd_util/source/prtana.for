      SUBROUTINE PRTANA ( PRUNIT, LTANA_DUMMY, NTANA, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'tana'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             Ltana_dummy  [I] : not used
C-             Ntana  [I] : Bank number, used only if CFL='ONE' and Ltana = 0
C-             CFL    [C*]: not used
C-             IFL    [I] : Defines the amount of printing: 0 means minimum
C-                          printout, 1 is more , 3 gives all, ...

C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Updated  26-JUL-1995   A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT, LTANA, NTANA, IFL,GZTDST,LTDST,ND,GZTHIT
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBWRK.INC'
C      INCLUDE 'D0$INC:WORKSP.INC'
C   -------COMMON,WORKSP :DUMMY COMMON TO USE ARRAYS NOT TRANSMITTED FROM
C                         ONE SUBROUTINE TO ANOTHER.MAY BE REPLACED BY
C                         'ZEBRA' WORKING SPACE.
      INTEGER LENGWS
      PARAMETER (LENGWS=5000)
C      COMMON/WORKSP/WS(LENGWS)
      REAL WS(LENGWS)
      INTEGER IWS(LENGWS)
      EQUIVALENCE(WS,IWS,W(1001))
      INCLUDE 'D0$INC:ZLINKC.INC'
C      INCLUDE 'D0$LINKS:IZtana.LINK'
C----------------------------------------------------------------------
      INTEGER LTANA1, GZTANA, LZLOC, J,LTANA_DUMMY
      CHARACTER*8 CFLAG
      INTEGER LAYER,NTDST,IER
      INTEGER I,JS,K,KS,NA,NC,NCLA,NCLC,JBIT
      INTEGER VERSION
C      INTEGER NWORD
C      PARAMETER (NWORD=300)
C      INTEGER INTEGER_WORD(NWORD)
C      REAL REAL_WORD(NWORD)
      REAL S
      CHARACTER*13 LINE1(9),LINE2(9)
      LOGICAL FOUND,FIRST
      INTEGER EVTI
      DATA FIRST/.TRUE./
      DATA EVTI/0/
    1 FORMAT (132('-')/)
    2 FORMAT (132('=')/)
      IF (PRUNIT.LE.0) GO TO 999
      LTDST=GZTDST()
      IF(LTDST.LE.0)GO TO 999
      IF (FIRST) THEN
        FIRST=.FALSE.
        LINE1(1)='electronic'
        LINE1(2)='epicor'
        LINE1(3)='APC'
        LINE1(4)='pedestal'
        LINE1(5)='sector'
        LINE1(6)='wire'
        LINE1(7)='hv cor'
        LINE1(8)='angular'
        LINE1(9)='gas'
      ENDIF
C----------------------------------------------------------------------
C
C  ***  Print the content of the bank pointed by Ltana1
C
      FOUND=.FALSE.
      VERSION=0
      IF(IQ(LHEAD+7).NE.EVTI .AND. GZTHIT().NE.0)THEN
C        WRITE(PRUNIT,*)' in prtana,lthit',GZTHIT()
        CALL THIT_GET
        EVTI=IQ(LHEAD+7)
      END IF
      DO 800 LAYER=1,3
        LTANA=LQ(LTDST-LAYER)
        IF (LTANA.GT.0) THEN
          FOUND=.TRUE.
          ND=IQ(LTANA-1)
          VERSION=IQ(LTANA+ND)
C          VERSION=Q(LTANA+1)
        ELSE
          WRITE(PRUNIT,*)' layer',LAYER,' no TANA bank'
          GO TO 800
        ENDIF
C      IF (VERSION.LT.2.0) THEN
C        CALL PRTANA_OLD(PRUNIT,LTDST,NTDST,'ONE',IFL)
C      ELSE
        WRITE (PRUNIT,2)
        WRITE (PRUNIT,'(1X,A6,I1,1X,A6,I7)')
     &      'LAYER=',LAYER,'LTANA=',LTANA
        NA=IQ(LTANA+304)
        NC=IQ(LTANA+305)
        NCLA=IQ(LTANA+306)
        NCLC=IQ(LTANA+307)
        S=0.
        IF(NC.NE.0)THEN
          DO I=1,NC
            S=S+Q(LTANA+NA+I+50)
          END DO
        END IF
        WRITE (PRUNIT,'(1X,a30,i10,10x,a30,f10.4)')
     &        'version number', VERSION,' Energy anode',
     &        Q(LTDST+LAYER+1)
        WRITE (PRUNIT,'(1X,2(a30,f10.4,10x))')
     &        'total energy cathodes (in MIP)',S,
     &        'EPICOR', Q(LTANA+1)
        WRITE (PRUNIT,'(1X,2(a30,f10.4,10x))')'A.P.C.',
     &        Q(LTANA+2), 't min on wire with E max', Q(LTANA+45)
        WRITE (PRUNIT,'(1X,2(a30,i10,10x))')'# of hit anodes',
     &        NA, '# of hit cathodes', NC
        WRITE (PRUNIT,'(1X,(a30,i10,10x))')
     &      '# of hits around the track', IQ(LTANA+303)
        WRITE (PRUNIT,'(1X,2(a30,i10,10x))')
     &        '# of hit anode clusters', NCLA,
     &        '# of hit cathode clusters',NCLC
        WRITE (PRUNIT,'(1X,2(a30,f10.4,10x))')
     &        'distance closest anode (cm)',
     &        Q(LTANA+41), 'sin(theta) track', Q(LTANA+42)
        WRITE (PRUNIT,'(1X,2(a30,f10.4,10x))') 'gas correction',
     &         Q(LTANA+43),'hv correction', Q(LTANA+44)
        WRITE (PRUNIT,*)
        WRITE (PRUNIT,*) 'CORRECTION/CALIBRATION'
        IF(IFL.GE.3)WRITE(PRUNIT,*)' status word in prtana',
     +                             IQ(LTANA+300+12)
        DO K=1,9
          IF (JBIT(IQ(LTANA+300+12),K).EQ.1) THEN
            LINE2(K)='OK'
          ELSE
            LINE2(K)='not done'
          ENDIF
        ENDDO
        WRITE (PRUNIT,'(1X,9A13)')  LINE1
        WRITE (PRUNIT,'(1X,9A13)')  LINE2
        WRITE (PRUNIT,*)
        WRITE (PRUNIT,'(1X,5(a16))')'variable',
     &        'hit anode 1','hit anode 2',
     &        'hit anode 3','hit anode 4'
        WRITE (PRUNIT,'(1X,a16,4(f16.1))')
     &          'HV anode',(Q(LTANA+3*K),K=1,4)
        WRITE (PRUNIT,'(1X,a16,4(f16.1))')
     &          'HV window',(Q(LTANA+3*K+1),K=1,4)
        WRITE (PRUNIT,'(1X,a16,4(f16.1))')
     &          'HV potential',(Q(LTANA+3*K+2),K=1,4)
        WRITE (PRUNIT,'(1X,a16,4(f16.6))')
     &          'pedestal',(Q(LTANA+14+K),K=1,4)
        WRITE (PRUNIT,'(1X,a16,4(f16.6))')
     &          'sector cor',(Q(LTANA+20+K),K=1,4)
        WRITE (PRUNIT,'(1X,a16,4(f16.6))')
     &          'wire cor',(Q(LTANA+24+K),K=1,4)
        WRITE (PRUNIT,'(1X,a16,4(f16.6))')
     &          'electronic gain',(Q(LTANA+28+K),K=1,4)
        WRITE (PRUNIT,*)
        IF (NA.LE.0) THEN
          WRITE (PRUNIT,*)'hit anodes : none (or not analyzed).'
          GO TO 800
        ENDIF
        IF(IFL.GE.3)WRITE(PRUNIT,*)' na,nc,ncla,nclc',NA,NC,NCLA,NCLC,
     &          ' pointer',50+NA+NC+5*(NCLA+NCLC)
        IF(IFL.LT.1)GO TO 800
        WRITE (PRUNIT,*) 'hit anodes :'
        J=50+NA+NC+5*(NCLA+NCLC)
        DO I=1,NA
          J=J+1
          IF(GZTHIT().NE.0)THEN
            CALL THIT_UNPACK(IQ(LTANA+350+I),LAYER)
            WRITE (PRUNIT,
     &            '(1X,a30,i4,10x,a15,f6.2,a12,i3,a12,f7.1)')
     &            'wire number ',IQ(LTANA+350+I),
     &            'energy (MIP)',Q(LTANA+50+I),'# of clust.',
     &            IQ(LTANA+300+J),' raw energy',WS(2001)
          ELSE
            WRITE (PRUNIT,'(1X,a30,i4,10x,a15,f6.2,a12,i3)')
     &            'wire number ',IQ(LTANA+350+I),
     &            'energy (MIP)',Q(LTANA+50+I),'# of clust.',
     &            IQ(LTANA+300+J)
          END IF
        ENDDO
        WRITE (PRUNIT,*)
        IF (NC.LE.0) THEN
          WRITE (PRUNIT,*) 'hit cathodes : none (or not analyzed).'
          GO TO 800
        ENDIF
        WRITE (PRUNIT,*) 'hit cathodes :'
        DO I=1,NC
          J=J+1
          WRITE (PRUNIT,'(1X,a30,i4,10x,a15,f6.2,a12,i3)')
     &            'wire number (in [1,256])',IQ(LTANA+350+NA+I),
     &            'energy (MIP)',Q(LTANA+50+NA+I),'# of clust.',
     &            IQ(LTANA+300+J)
        ENDDO
        IF(IFL.LT.2)GO TO 800
        WRITE (PRUNIT,*)
        IF (NCLA.LE.0) THEN
          WRITE (PRUNIT,*)'anode clusters: none (or not analyzed).'
          GO TO 600
        ENDIF
        WRITE (PRUNIT,*) 'anode clusters:   POSITION  ENERGY '
        J=50+NA+NC
        DO I=1,NCLA
          J=J+1
C              IF(VERSION.GE.3)THEN
C              print*,' cluster',i,' pointeur',j
          WRITE (PRUNIT,'(21X,I4,2X,F7.4)')
     &            IQ(LTANA+300+J+1), Q(LTANA+J)
          J=J+4
C              ELSE
C                WRITE(PRUNIT,*)' print of clusters not implemented'
C              END IF
        ENDDO
        WRITE (PRUNIT,*)
  600   IF (NCLC.LE.0) THEN
          WRITE (PRUNIT,*)'cathode clusters: none (or not analyzed).'
          GO TO 800
        ENDIF
        WRITE (PRUNIT,*) 'cathodes clusters:   POSITION  ENERGY '
        DO I=1,NCLC
C              IF(VERSION.GE.3)THEN
          J=J+1
C              print*,' cluster',i,' pointeur left', j
          WRITE (PRUNIT,'(21X,I4,2X,F7.2)')
     &            IQ(LTANA+300+J+1), Q(LTANA+J)
          J=J+4
        ENDDO
        IF (LAYER.LE.2) THEN
          WRITE (PRUNIT,1)
        ELSE
          WRITE (PRUNIT,2)
        ENDIF
  800 CONTINUE
  999 RETURN
      END
