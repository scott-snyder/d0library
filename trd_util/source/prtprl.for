      SUBROUTINE PRTPRL (PRUNIT,LTRDT,NTRDT,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'TPRL'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LTRDT      : Pointer to the up bank
C-             CFL    [C*]: not used
C-             IFL    [I] : not used
C-             NTRDT      : not used
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  30-OCT-1989   A. Zylberstejn
C-   Updated  19-MAR-1990   J.Fr. Glicenstein  Prints clusters
C-   Updated  26-JAN-1993   Alain PLUQUET  versions>=2 of TPRL
C-                                         old version of PRTPRL.FOR
C-                                         is now PRTPRL_OLD.FOR
C-   Updated  22-JUN-1995   A. ZYLBERSTEJN
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTPRL.LINK'
      INTEGER PRUNIT,LTRDT,GZTRDT,LTPRL,LAYER,NTRDT,IFL,IER
      INTEGER I,J,JS,K,KS,NA,NC,NCLA,NCLC,JBIT,LTRDT1
      REAL VERSION
C      INTEGER NWORD
C      PARAMETER (NWORD=300)
      INCLUDE 'D0$INC:trd_nword.INC'
C      INTEGER INTEGER_WORD(NWORD)
C      REAL REAL_WORD(NWORD)
      CHARACTER*13 LINE1(9),LINE2(9)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      CHARACTER*(*) CFL
    1 FORMAT (132('-')/)
    2 FORMAT (132('=')/)
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
      IF (PRUNIT.LE.0) RETURN
      LTRDT1=LTRDT
      IF (LTRDT1.LE.0) THEN
        LTRDT1=GZTRDT()
        IF (LTRDT1.LE.0) RETURN
      ENDIF
   10 CONTINUE
      VERSION=0.
      DO LAYER=1,3
        LTPRL=LQ(LTRDT1-LAYER)
        IF (LTPRL.GT.0) THEN
          VERSION=AINT(Q(LTPRL+1))
          GO TO 12
        ENDIF
      ENDDO
      CALL ERRMSG(' ','PRTPRL ', 'No TPRL bank','W')
      GO TO 999 ! no tprl bank
   12 CONTINUE
      IF (VERSION.LT.2.0) THEN
        CALL PRTPRL_OLD(PRUNIT,LTRDT1,NTRDT,'ONE',IFL)
      ELSE
        WRITE (PRUNIT,2)
        DO LAYER=1,3
          WRITE (PRUNIT,'(1X,A6,I1,1X,A6,I7)')
     &      'LAYER=',LAYER,'LTPRL=',LTPRL
          LTPRL=LQ(LTRDT1-LAYER)
          IF (LTPRL.GT.0) THEN
            CALL UNPACK_TPRL
     &        (LTPRL,VERSION,REAL_WORD,INTEGER_WORD,IER)
            NA=INTEGER_WORD(4)
            NC=INTEGER_WORD(5)
            NCLA=INTEGER_WORD(6)
            NCLC=INTEGER_WORD(7)
            WRITE (PRUNIT,'(1X,a30,f10.1,10x,a30,f10.4)')
     &        'version number',
     &        Q(LTPRL+1),' Energy anode',
     &        Q(LTPRL+12)
            WRITE (PRUNIT,'(1X,2(a30,f10.4,10x))')
     &        'total energy cathodes (in MIP)',
     &        Q(LTPRL+13), 'EPICOR', REAL_WORD(1)
            WRITE (PRUNIT,'(1X,2(a30,f10.4,10x))')
     &        'A.P.C.',
     &        REAL_WORD(2),
     &        't min on wire with E max',
     &        Q(LTPRL+2)
            WRITE (PRUNIT,'(1X,2(a30,i10,10x))')
     &        '# of hit anodes', NA,
     &        '# of hit cathodes', NC
            WRITE (PRUNIT,'(1X,(a30,i10,10x))')
     &        '# of hits around the track',
     &        INTEGER_WORD(3)
            WRITE (PRUNIT,'(1X,2(a30,i10,10x))')
     &        '# of hit anode clusters', NCLA,
     &        '# of hit cathode clusters', NCLC
            WRITE (PRUNIT,'(1X,2(a30,f10.4,10x))')
     &        'distance closest anode (cm)', REAL_WORD(41),
     &        'sin(theta) track', REAL_WORD(42)
            WRITE (PRUNIT,'(1X,2(a30,f10.4,10x))')
     &        'gas correction', REAL_WORD(43),
     &        'hv correction', REAL_WORD(44)
            WRITE (PRUNIT,*)
            WRITE (PRUNIT,*) 'CORRECTION/CALIBRATION'
            DO K=1,9
              IF (JBIT(IQ(LTPRL+4),K).EQ.1) THEN
                LINE2(K)='OK'
              ELSE
                LINE2(K)=' NO  '
              ENDIF
            ENDDO
            WRITE (PRUNIT,'(1X,9A13)')  LINE1
            WRITE (PRUNIT,'(1X,9A13)')  LINE2
            WRITE (PRUNIT,*)
            WRITE (PRUNIT,'(1X,5(a16))') 'variable',
     &        'hit anode 1','hit anode 2',
     &        'hit anode 3','hit anode 4'
            WRITE (PRUNIT,'(1X,a16,4(f16.1))')
     &          'HV anode',(REAL_WORD(3*K),K=1,4)
            WRITE (PRUNIT,'(1X,a16,4(f16.1))')
     &          'HV window',(REAL_WORD(3*K+1),K=1,4)
            WRITE (PRUNIT,'(1X,a16,4(f16.1))')
     &          'HV potential',(REAL_WORD(3*K+2),K=1,4)
            WRITE (PRUNIT,'(1X,a16,4(f16.6))')
     &          'pedestal',(REAL_WORD(14+K),K=1,4)
            WRITE (PRUNIT,'(1X,a16,4(f16.6))')
     &          'sector cor',(REAL_WORD(20+K),K=1,4)
            WRITE (PRUNIT,'(1X,a16,4(f16.6))')
     &          'wire cor',(REAL_WORD(24+K),K=1,4)
            WRITE (PRUNIT,'(1X,a16,4(f16.6))')
     &          'electronic gain',(REAL_WORD(28+K),K=1,4)
            WRITE (PRUNIT,*)
            IF (NA.GT.0) THEN
              WRITE (PRUNIT,*) 'hit anodes :'
              DO I=1,NA
                WRITE (PRUNIT,'(1X,a30,i4,10x,a15,f6.2,a12,i3)')
     &            'wire number ',INTEGER_WORD(50+I),
     &            'energy (MIP)',REAL_WORD(50+I),'# of clust.',
     &            INTEGER_WORD(50+NA+NC+5*(NCLA+NCLC)+I)
              ENDDO
            ELSE
              WRITE (PRUNIT,*)
     &          'hit anodes : none (or not analyzed).'
            ENDIF
            WRITE (PRUNIT,*)
            IF (NC.GT.0) THEN
              WRITE (PRUNIT,*) 'hit cathodes :'
              DO I=1,NC
                WRITE (PRUNIT,'(1X,a30,i4,10x,a15,f6.2,a12,i3)')
     &            'wire number (in [1,256])',INTEGER_WORD(50+NA+I),
     &            'energy (MIP)',REAL_WORD(50+NA+I),'# of clust.',
     &            INTEGER_WORD(50+NA+NC+5*(NCLA+NCLC)+I+NA)
              ENDDO
            ELSE
              WRITE (PRUNIT,*)
     &          'hit cathodes : none (or not analyzed).'
            ENDIF
            WRITE (PRUNIT,*)
            IF (NCLA.GT.0) THEN
              WRITE(PRUNIT,3001)(INTEGER_WORD(50+NA+NC+1+(I-1)*5),
     &          REAL_WORD(50+NA+NC+1+(I-1)*5),I=1,NCLA)
 3001         FORMAT('ANODE CLUSTERS (POSITION, ENERGY):',
     &          6('(',I4,',',F7.2,')'))
            ELSE
              WRITE (PRUNIT,*)
     &          'ANODE CLUSTERS : NONE (OR NOT ANALYZED).'
            ENDIF
            WRITE (PRUNIT,*)
            IF (NCLC.GT.0) THEN
              WRITE(PRUNIT,3002)(INTEGER_WORD(50+NA+NC+NCLA+1+(I-1)*5),
     &          REAL_WORD(50+NA+NC+NCLA+1+(I-1)*5),I=1,NCLA)
 3002         FORMAT('cathodes clusters (POSITION, ENERGY):',
     &          6('(',I4,',',F7.2,')'))
            ELSE
              WRITE (PRUNIT,*)
     &          'CATHODE CLUSTERS : NONE (OR NOT ANALYZED).'
            ENDIF
            J=NWORD-4
            WRITE(PRUNIT,4007)REAL_WORD(J+1),REAL_WORD(J+2),
     &        REAL_WORD(J+3), REAL_WORD(J+4)
            IF (LAYER.LE.2) THEN
              WRITE (PRUNIT,1)
            ELSE
              WRITE (PRUNIT,2)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
  999 RETURN
 4007 FORMAT(' phi,r,z track in layer:',F6.3,F5.2,F8.2,
     &  ' z computed with anodes/cathodes:',F8.2)
      END
