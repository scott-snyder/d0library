C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_PRECLUSTER.FOR
C *1     3-FEB-1994 14:40:12 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_PRECLUSTER.FOR
      SUBROUTINE KTJET_PRECLUSTER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Precluster for Kt algorithm.  The idea is to
C-                         precluster particles/cells in order to alleviate
C-                         showering/hadronization.
C-                         For this reason, a simple angular clustering is
C-                         used.
C-                  5/24/93 Try to be more sophisticated to 1) get the number
C-                      of channels lower and 2) put energy that belongs in
C-                      the beampipe back in the beampipe.
C-
C-            1) Use a variable angle for merging depending on whether the
C-              candidates have alot of ET compared to the maximum.
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-JAN-1993  Richard V. Astur
C-   Modified 31-DEC-1994  R. Astur "Change precluster to RCP driven and
C-                                   in eta-phi instead of theta-phi space"
c-   Updated  07-Nov-1995  Use errmsg instead of type *
c-   Updated  13-Nov-1995  Updated format statements to work on ibm
c-                         Gordon Watts
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      INTEGER I, ILAY, ILAY2, GZCAEH, LCAEH, SEED
      INTEGER IETA, IPHI
      INTEGER IDIREC, IS1, IS2, IE1, IE2, IP, J
      REAL A,ET1,ET2,ET,D_R, KTJET_DELTA_R
      INTEGER IET
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      DATA SEED / -1376937 /
      REAL DELTA_R, RETA1, RETA2, RPHI1, RPHI2
      REAL deta,dphi,prec_width_sq

      character *80 line

      INCLUDE 'D0$INC:KTJET_UTIL.DEF'
C----------------------------------STATEMENT FUNCTIONS-------------------
C
C---Delta R in eta-phi space
C
      DELTA_R(RETA1,RETA2,RPHI1,RPHI2) = SQRT( ((RETA1)-(RETA2))**2 +
     &  MIN( MOD(ABS((RPHI1)-(RPHI2)),SNGL(TWOPI)) ,
     &  SNGL(TWOPI)-ABS(MOD(ABS((RPHI1)-(RPHI2)),SNGL(TWOPI))) )**2 )
C------------------------------------------------------------------------
      LCAEH = GZCAEH()
      prec_width_sq = prec_width**2
c      IF ( INPUT_TYPE .EQ. 6 ) RETURN
C
C: Do special clustering for calorimeter cells. Add up longitudinally.
C
      IF ( INPUT_TYPE .NE. 3 ) GOTO 500 ! continue

      DO I = 1, IQ( LCAEH + 3 )     ! Just cells, skip beam jets
        IF ( IQ(LINK_MAP( I )) .GT. 0 .AND. IQ( POINT(I)) .GT. 0 )
     &      THEN
          IETA = IQ( LCAEH + (I-1)*IQ(LCAEH+2) + 12 )
          IPHI = IQ( LCAEH + (I-1)*IQ(LCAEH+2) + 13 )
          ILAY = IQ( LCAEH + (I-1)*IQ(LCAEH+2) + 14 )
          DO ILAY2 = 1, NLYRL
            IF ( ILAY2 .NE. ILAY .AND. PTCAEP(IETA, IPHI, ILAY2 ) .GT.
     &          0) THEN
              IP = PTCAEP( IETA, IPHI, ILAY2 )
              IF ( IQ(POINT(IP)) .GT. 0 ) THEN
                deta = abs(q(kteta(i))-q(kteta(ip)))
                IF(deta.GE.prec_width) GOTO 100
                dphi = abs(q(ktphi(i))-q(ktphi(ip)))
                IF(dphi.GT.pi) dphi = twopi - dphi
                IF(dphi.GE.prec_width) GOTO 100
                IF(deta*deta + dphi*dphi .gt. prec_width_sq) goto 100
C                IF ( DELTA_R( Q(KTETA(I)), Q( KTETA(IP) ), Q( KTPHI(I)
C     &            ), Q( KTPHI(IP) ) ) .LE. PREC_WIDTH ) THEN
C              ENDIF
                CALL KT_MERGE_CELLS( I, IP )
                IF(I.GT.IP)GOTO 700
  100           CONTINUE
              ENDIF
            ENDIF
          ENDDO
        ENDIF
  700   CONTINUE
      ENDDO
C
C: Further. Kill noise
C
          CALL KTJET_COMPRESS( .FALSE. )
          

C
C: Precluster randomly
C
  500 CONTINUE

      DO I = IQ( LKVEC + 3 )-2, 1, -1
        DO J = 1, I-1
          IF ( IQ(POINT(I)) .GT. 0 .AND. IQ(POINT(J)) .GT. 0 ) THEN
            deta = abs(q(kteta(i))-q(kteta(j)))
            IF(deta.GE.prec_width) GOTO 200
            dphi = abs(q(ktphi(i))-q(ktphi(j)))
            IF(dphi.GT.pi) dphi = twopi - dphi
            IF(dphi.GE.prec_width) GOTO 200
            IF(deta*deta + dphi*dphi .gt. prec_width_sq) goto 200
C            IF ( DELTA_R( Q(KTETA(I)), Q( KTETA(J) ), Q( KTPHI(I)
C     &            ), Q( KTPHI(J) )) .LE. PREC_WIDTH ) THEN
            IF ( INPUT_TYPE .EQ. 6 ) THEN
              write (line, '(a, 3i4)') 'MERGING DONE',I,J,IQ(LHEAD+9)
               call errmsg ('merging-done', 'ktjet_precluster',
     $             line, 'i')
            ENDIF
            CALL KT_MERGE_CELLS( I, J )
  200       CONTINUE
C          ENDIF
          ENDIF
        ENDDO
      ENDDO

  999 RETURN
      END
