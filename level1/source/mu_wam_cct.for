      SUBROUTINE MU_WAM_CCT(ICCT,ACRS,BCRS,CCRS,JTRIG,JBITS)
C---------------------------------------------------------------
C  Subroutine to test a list of hit modules for a CCT trigger
C  Created 7-90  M. Fortner
C  Modified 9-91 M. Fortner
C  Defeat the OR by 4 of ACRS,BCRS,CCRS  5-10-92 K. Bazizi
C  Regenerate Trigger logic               8-5-92 K. Bazizi
C  Optimize the CCT logic for |eta|<1.7  12-5-92 K. Bazizi
C
C  ICCT is the CCT segment to be tested
C  MCRS is the bit map from the 13 MACs for the CCT (or'ed by 4)
C  JTRIG is the integer of highest JBITS
C  JBITS is the 12-bit output from the CCT
C
C  ICCT:        LOCATION:
C   1-8           Central Octant 0-7
C  11,13,15,17    North Quadrant 0,2,4,6
C  12,14,16,18    South Quadrant 0,2,4,6
C---------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICCT,ACRS(16,3),BCRS(16,5),CCRS(16,5),JTRIG,JBITS(12)
      INTEGER I,II,III,ICC,IOR,A(12),B(20),C(20)
      INTEGER IA,IB,IC
      INTEGER CCTSUM
      INTEGER IER
      CHARACTER*72 STRING
      LOGICAL FIRST,IPRWCCT
      DATA FIRST /.TRUE./
      DATA IPRWCCT /.FALSE./
c-----------------------------------------------------------------------------

      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IER)     ! Check if error
        IF(IER.EQ.0)THEN
          CALL EZGET('IPR_WCCT',IPRWCCT,IER)
        ELSE
           CALL EZGET_ERROR_TEXT(IER,STRING)
           CALL ERRMSG(' CANNOT PICK MUSIM.RCP',
     &          'MUSIM_WAM_CCT',STRING,'F')
           GOTO 999
        ENDIF

      ENDIF      
C-- OR coarse centroid inputs by 4
      DO II=1,5
        DO I=1,4
          ICC=4*(II-1)+I
          IF (II.LE.3) A(ICC)=0
          B(ICC)=0
          C(ICC)=0
          DO III=1,4
            IOR=4*(I-1)+III
            IF (II.LE.3) A(ICC)=A(ICC)+ACRS(IOR,II)
            B(ICC)=B(ICC)+BCRS(IOR,II)
            C(ICC)=C(ICC)+CCRS(IOR,II)
          ENDDO
        ENDDO
      ENDDO

C-- renormalize A,B,C to 1
      DO I=1,20
        IF (A(I).GT.0.AND.I.LE.12) A(I)=1
        IF (B(I).GT.0) B(I)=1
        IF (C(I).GT.0) C(I)=1
      ENDDO

C-- Logic for central top and sides
      IF (ICCT.LE.5.OR.ICCT.EQ.8) CALL MU_CCT_CF_TOP(A,B,C,JBITS)
C-- Logic for central bottom
      IF (ICCT.EQ.6.OR.ICCT.EQ.7) CALL MU_CCT_CF_BOT(A,B,C,JBITS)
C-- Logic for end top
      IF (ICCT.GE.11.AND.ICCT.LE.14) CALL MU_CCT_EF_TOP(A,B,C,JBITS)
C-- Logic for end bottom
      IF (ICCT.GE.15.AND.ICCT.LE.18) CALL MU_CCT_EF_BOT(A,B,C,JBITS)


      JTRIG=0
      DO I=1,12
        IF (JBITS(I).NE.0) JTRIG=1
      ENDDO

C-- Printouts
      IF (IPRWCCT) THEN

C--
      DO I=1,12
        IF(A(I).NE.0) A(I)=I
      ENDDO
      DO I=1,20
        IF(B(I).NE.0) B(I)=I
        IF(C(I).NE.0) C(I)=I
      ENDDO
C--
         CCTSUM = 0
        DO I = 1, 12
          CCTSUM = CCTSUM + A(I) + B(I) + C(I)
        ENDDO
        DO I = 13, 20
          CCTSUM = CCTSUM + B(I) + C(I)
        ENDDO

        IF(CCTSUM.GT.0) THEN
          I = ICCT
          IF (ICCT.LT.10) I=ICCT-1
          WRITE (6,100) I
  100     FORMAT (/' Checking octant # 'I5)
          WRITE (6,101) C
  101     FORMAT (' C:   '20I3)
          WRITE (6,102) B
  102     FORMAT (' B:   '20I3)
          WRITE (6,103) A
  103     FORMAT (' A:               '12I3)
          WRITE (6,104) JBITS
  104     FORMAT ('  CCT OUTPUT :    '12I3)
        ENDIF
      ENDIF


C---------------------------------------------------------------------
999	CONTINUE
      RETURN
      END
