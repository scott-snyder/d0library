C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_DUMP.FOR
C *1     3-FEB-1994 14:36:56 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_DUMP.FOR
      SUBROUTINE KTJET_DUMP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-JAN-1993   Richard V. Astur
c    Updated  07-Nov-1995   ERRMSG instead of Type *
c                           Gordon Watts
c-   Updated  13-Nov_1995   Replace "i" with "i6" in format statements
c-                          Gordon Watts
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPJET.LINK'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      REAL ETA,PHI, THETA, EM,  MASS, E, EN(5), TEMPLATE(8), ET, EE(5)
      REAL ETOT
      REAL EMAP(8,10)
      REAL ETAA, RAT
      INTEGER I, IFIRST, INEXT, ILINK, IER, NJ, IVERS,J
      INTEGER GZISAE, LPJET, LPJHD, GZPJHD, IE, IP
      CHARACTER*4 PATH
      character*80 line

      INCLUDE 'D0$INC:KTJET_UTIL.DEF'
C----------------------------------------------------------------------
      PRINT *,' ************** INPUT= ',IQ(LKVEC+2),' EVENT = ',
     &  IQ(LHEAD+9)
C: KVEC
      ETOT = 0.
      PRINT *,' KVEC '
      PRINT*,
     &'#:       ET       E     ETA     PHI      PX      PY      PZ  ',
     &'   E/p'

      DO i = 1, iq(lkvec+3)
        IF ( iq( point(i) ) .GT. 0 ) THEN
          ETOT = ETOT + Q(P0(I))
          RAT = Q(P0(I))/SQRT(Q(PX(I))**2+Q(PY(I))**2+Q(PZ(I))**2 )
          PRINT 100, I, Q(KTET(I)), Q(P0(I)),Q(KTETA(I)),Q(KTPHI(I)),
     &      Q(PX(I)), Q(PY(I)), Q(PZ(I)),RAT
  100     FORMAT(I4.2,F8.2,F8.2,F8.2,F8.2,F8.2,F8.2,F8.2,F8.2)
        ENDIF
      ENDDO

      PRINT * , 'KMAP'
      PRINT * , 'EX,EY,EZ,E,ET,ETOT:',Q(LKMAP+5),Q(LKMAP+6),Q(LKMAP+7),
     &  Q(LKMAP+8), Q(LKMAP+9), ETOT
      PRINT *,' ******************************************************'
  999 RETURN

      ENTRY KTJET_EMAP
      CALL VZERO( EMAP, 8*10)
      PRINT *,' ************** INPUT= ',IQ(LKVEC+2),' EVENT = ',
     &  IQ(LHEAD+9)
      DO i = 1, iq(lkvec+3)
        IF ( iq( point(i) ) .GT. 0 ) THEN
          ETAA = MAX( -3.99, MIN( 3.99, Q(KTETA(I)) ))
          IE   = (ETAA+4.)/.8 + 1
          IP   = Q(KTPHI(I))/.8 + 1
          IF ( IP .GT. 8 ) IP = IP - 8
          IF ( IP .LT. 1 ) IP = IP + 8
          EMAP(IP,IE)   = EMAP(IP,IE) + Q(KTET(I))
        ENDIF
      ENDDO
      PRINT *,' ******************************************************'
      DO IE = 1,10
        PRINT 9001, EMAP(1,IE),EMAP(2,IE),EMAP(3,IE),EMAP(4,IE), EMAP(5,
     &    IE), EMAP(6,IE),EMAP(7,IE),EMAP(8,IE)
      ENDDO
 9001 FORMAT(' ',8(2X,F5.1))
      RETURN

      ENTRY KTJET_PJET_DUMP1
      LPJHD = GZPJHD()
      DO WHILE (LPJHD .GT. 0 )
        IF ( IQ(LPJHD+2) .EQ. 1 ) THEN
          PRINT 801, IQ(LPJHD+3), Q(LPJHD+4), Q(LPJHD+5)
          LPJET = LQ( LPJHD - 1 )
          DO WHILE (LPJET .GT. 0 )
            PRINT 802, Q(LPJET+2), Q(LPJET+6), Q(LPJET+10), Q(LPJET+8)
            LPJET = LQ(LPJET)
          ENDDO
        ENDIF
        LPJHD = LQ(LPJHD)
      ENDDO
  801 FORMAT(' **** ',I4,' PJET CONE SIZE ', F5.2)
  802 FORMAT(' E:',F7.2,' ET:',F7.2,' ETA:',F7.2,' PHI:',F7.2)
      RETURN

      ENTRY KTJET_CONE_DUMP
      TEMPLATE(1) = 1.
      TEMPLATE(2) = 6.
      TEMPLATE(3) = .7
      CALL SET_CAPH( 'CONE_JET', TEMPLATE, IER )
      IF ( IER .EQ. 0 ) THEN
        CALL GTJETS_TOTAL( NJ, IER)
        IF ( IER .NE. 0) NJ=0
        DO I = 1, NJ
          CALL GTJETS( I, IVERS, EN, THETA, PHI, ETA, IER )
          PRINT 100, I, EN(5), EN(4), ETA, PHI
C          PRINT 288, (EN(J), J=1,5), ETA, PHI
C  288     FORMAT(' E:',5F7.1,' ETA:',F6.2,' PHI:',F6.2)
        ENDDO
      ENDIF
      CALL RESET_CAPH
      RETURN

      ENTRY KTJET_PJET_DUMP
      LPJET = GZISAE()
      I = 0
      IF ( LPJET .GT. 0 ) LPJET = LPJET - IZPJET
      DO WHILE ( LPJET .GT. 0 )
        CALL GTPJET( LPJET, LPJET, ET, EE,MASS, PHI, THETA, ETA)
        IF ( LPJET .GT. 0 ) THEN
          I = I + 1
          PRINT 100, I, ET, EE(4), ETA, PHI
        ENDIF
      ENDDO
      RETURN

      ENTRY KTJET_MAP_DUMP
      PRINT *, ' Which jet? '
      read (6, '(i5)') i
      ETOT = 0.0
      IF ( IQ( POINT(I) ) .GT. 0 ) THEN
        IFIRST = IQ( POINT(I) )
        INEXT  = IQ( POINT(I) )
        if ( inext .le. 0 .or. inext .gt. iq(lkmap+3)) then
           write (line, '(a, i10, i10)') ' bad inext ', inext, point(i)
           call errmsg ('bad-index', 'ktjet_dump', line, 'w')
          inext = ifirst
          goto 141
        endif
  140   ILINK  = IQ( LINK_MAP( INEXT ) )
        IF ( ILINK .GT. 0 ) THEN
          CALL KTJET_GET_LINK_INFO( ETA, PHI, EM, EE, ILINK,
     &        IQ(LKVEC+2))
          ETOT = ETOT + EE(4)
          PRINT 299, ILINK, EE(4),EE(5), ETA, PHI
  299     FORMAT(' ',I5,'ET:',F8.2,'E:',F9.3,' ETA:',F5.2,
     &          'PHI:',F4.2)
        ELSE
          PRINT 230
  230     FORMAT(' BEAM JET')
        ENDIF
        INEXT  = IQ( NEXT_MAP( INEXT ) )
141        IF ( INEXT .NE. IFIRST )  GOTO 140
      ENDIF
      PRINT *, 'ETOT:',ETOT
      RETURN
      END
