      SUBROUTINE MU_CCT_EF_TOP(A,B,C,JBITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Tests CCT trigger for End top
C-
C-   Inputs  : A(12),B(20),C(20) - Input coarse centroids (already or'ed by 4)
C-
C-   Outputs : JBITS(12) - CCT trigger output
C-
C-   Controls: None
C-
C-   Created  29-SEP-1991   K. Bazizi
C-   Updated  26-APR-1992   K. Bazizi, G.Lima
C-      This logic is derived from a detailed study of single muon tracks
C-        with 20 < Pt < 50 GeV and run through full D0GEANT (KB,GL)
C-   Updated     NOV-1992   K. Bazizi, G.Lima
C-        Complete reevaluation of CCT logic (version 2):
C-      * Requires A*B*C combinations only, with good pointing to the
C-        interaction region.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER BC(12),A(12),B(20),C(20),JBITS(12),I
      INTEGER IDIM
      PARAMETER (IDIM=70)
      INTEGER IROAD(IDIM),ILOG,IVERS,BIT(6),IER
      LOGICAL TREG(4)
      CHARACTER*80 STRING
      LOGICAL FIRST
      DATA FIRST/.TRUE./

      DATA ILOG / 3 /
      DATA IVERS/2/

C.. Get trigger logic number 
      IF (FIRST) THEN
        FIRST=.FALSE.
C<<
        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IER)     ! Check if error
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' EZPICK ERR','MU_CCT_EF_TOP',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('WAM_CCT_VERS',IVERS,IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' WAM_CCT_VERS','MU_CCT_EF_TOP',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZRSET()
      ENDIF
C
C.. Calculate first stage pal logic
      IF (IVERS.EQ.1) THEN
        BC(1)  = 0
        BC(2)  = 0
        BC(3)  = 0
        BC(4)  = 0
        BC(5)  = B(13)*C(13) + B(14)*C(14)
        BC(6)  = B(13)*C(13)
     &         + B(14)*C(14) + B(14)*C(15)
     &         + B(15)*C(15) + B(15)*C(16)
        BC(7)  = B(14)*C(14) + B(14)*C(15)
     &         + B(15)*C(15) + B(15)*C(16)
        BC(8)  = B(15)*C(15) + B(15)*C(16) + B(16)*C(16)
        BC(9)  = B(17)*C(17) + B(17)*C(18)
     &         + B(18)*C(18) + B(18)*C(19)
        BC(10) = B(17)*C(17) + B(17)*C(18)
     &         + B(18)*C(18) + B(18)*C(19)
     &         + B(19)*C(19) + B(19)*C(20)
        BC(11) = B(18)*C(18) + B(18)*C(19)
     &         + B(19)*C(19) + B(19)*C(20)
        BC(12) = B(19)*C(20)
      ENDIF
      IF (IVERS.EQ.2) THEN
        BC(1)  = 0
        BC(2)  = 0
        BC(3)  = 0
        BC(4)  = 0
        BC(5)  = B(13)*C(4)  + B(13)*C(13) + B(14)*C(13) + B(14)*C(14)
        BC(6)  = B(13)*C(4)  + B(13)*C(13)
     &         + B(14)*C(13) + B(14)*C(14) + B(14)*C(15)
     &         + B(15)*C(14) + B(15)*C(15) + B(15)*C(16)
        BC(7)  = B(14)*C(13) + B(14)*C(14) + B(14)*C(15)
     &         + B(15)*C(15) + B(15)*C(16)
     &         + B(16)*C(16)
        BC(8)  = B(15)*C(15) + B(15)*C(16) + B(16)*C(16)
        BC(9)  = B(17)*C(17) + B(17)*C(18)
     &         + B(18)*C(18) + B(18)*C(19)
        BC(10) = B(17)*C(17) + B(17)*C(18)
     &         + B(18)*C(18) + B(18)*C(19)
     &         + B(19)*C(19) + B(19)*C(20)
        BC(11) = B(17)*C(17) + B(17)*C(18)  
     &         + B(18)*C(18) + B(18)*C(19)
     &         + B(19)*C(19) + B(19)*C(20)
        BC(12) = B(19)*C(19) + B(19)*C(20)
     &         + B(20)*C(19) + B(20)*C(20)
      ENDIF

C.. Renormalize BC(1,2,..,12) to 1 for use in the logic below
      DO I=1,12
        IF(BC(I).GT.0) BC(I)=1
      ENDDO

C.. Second stage pal logic
      DO I=1,12
        JBITS(I)=0
        IF ((A(I)*BC(I)).NE.0) JBITS(I)=1
      ENDDO

C- get the theta distribution
      BIT(1)=JBITS(8)+JBITS(12)
      BIT(2)=JBITS(7)+JBITS(11)
      BIT(3)=JBITS(6)+JBITS(10)
      BIT(4)=JBITS(5)+JBITS(9)
      BIT(5)=JBITS(1)+JBITS(3)
      BIT(6)=JBITS(2)+JBITS(4)
      DO I=1,6
        IF(BIT(I).NE.0) CALL HFILL(1040+ILOG,FLOAT(I),0.,1.)
      ENDDO


C.. compute different roads independently for monitoring
      DO I=1,IDIM
        IROAD(I)=0
      ENDDO


      IROAD( 1)  = A( 5)*B(13)*C(4)
      IROAD( 2)  = A( 5)*B(13)*C(13)
      IROAD( 3)  = A( 5)*B(14)*C(13)
      IROAD( 4)  = A( 5)*B(14)*C(14)

      IROAD( 8)  = A( 6)*B(13)*C(4)
      IROAD( 9)  = A( 6)*B(13)*C(13)
      IROAD(10)  = A( 6)*B(14)*C(13)
      IROAD(11)  = A( 6)*B(14)*C(14)
      IROAD(12)  = A( 6)*B(14)*C(15)
      IROAD(13)  = A( 6)*B(15)*C(14)
      IROAD(14)  = A( 6)*B(15)*C(15)
      IROAD(15)  = A( 6)*B(15)*C(16)

      IROAD(19)  = A( 7)*B(14)*C(13)
      IROAD(20)  = A( 7)*B(14)*C(14)
      IROAD(21)  = A( 7)*B(14)*C(15)
      IROAD(22)  = A( 7)*B(15)*C(15)
      IROAD(23)  = A( 7)*B(15)*C(16)
      IROAD(24)  = A( 7)*B(16)*C(16)

      IROAD(28)  = A( 8)*B(15)*C(15)
      IROAD(29)  = A( 8)*B(15)*C(16)
      IROAD(30)  = A( 8)*B(16)*C(16)

      IROAD(34)  = A( 9)*B(17)*C(17)
      IROAD(35)  = A( 9)*B(17)*C(18)
      IROAD(36)  = A( 9)*B(18)*C(18)
      IROAD(37)  = A( 9)*B(18)*C(19)

      IROAD(41)  = A(10)*B(17)*C(17)
      IROAD(42)  = A(10)*B(17)*C(18)
      IROAD(43)  = A(10)*B(18)*C(18)
      IROAD(44)  = A(10)*B(18)*C(19)
      IROAD(45)  = A(10)*B(19)*C(19)
      IROAD(46)  = A(10)*B(19)*C(20)

      IROAD(50) = A(11)*B(17)*C(17)
      IROAD(51) = A(11)*B(17)*C(18)
      IROAD(52) = A(11)*B(18)*C(18)
      IROAD(53) = A(11)*B(18)*C(19)
      IROAD(54) = A(11)*B(19)*C(19)
      IROAD(55) = A(11)*B(19)*C(20)

      IROAD(59) = A(12)*B(19)*C(19)
      IROAD(60) = A(12)*B(19)*C(20)
      IROAD(61) = A(12)*B(20)*C(19)
      IROAD(62) = A(12)*B(20)*C(20)
             
      DO I=1,IDIM
        IF(IROAD(I).NE.0) CALL HFILL(1030+ILOG,FLOAT(I),0.,1.)
      ENDDO

C      CALL WAMUS_ROADS(2100,A,B,C,JBITS)

  999 RETURN
      END
