      SUBROUTINE MU_CCT_CF_TOP(A,B,C,JBITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Tests CCT trigger for Central top and sides
C-
C-   Inputs  : A(12),B(20),C(20) - Input coarse centroids (already or'ed by 4)
C-
C-   Outputs : JBITS(12) - CCT trigger output
C-
C-   Controls: None
C-
C-   Created     SEP-1991   M. Fortner
C-   Updated  04-SEP-1991   K. Bazizi
C-                  Use a new logic for the CENTRAL muon system which
C-      is derived from a detailed study of 10,000 single muon tracks
C-      with Pt=5 and 50GeV run through D0GEANT
C-   Updated     NOV-1992   K. Bazizi, G.Lima
C-        Complete reevaluation of CCT logic (version 2):
C-      * Allows for A*B and A*C combinations only where B and C layers
C-        have big cracks, except in the punchthrough dominated regions.
C-      * No B*C combinations allowed.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER BC(12),A(12),B(20),C(20),JBITS(12),I
      INTEGER IDIM
      PARAMETER (IDIM=100)
      INTEGER IROAD(IDIM),ILOG
      LOGICAL TWOLAY
      INTEGER IVERS,IER
      CHARACTER*80 STRING

      DATA ILOG/1/
      DATA IVERS/2/

      LOGICAL FIRST
      DATA FIRST/.TRUE./

C.. Get trigger logic number 
      IF (FIRST) THEN
        FIRST=.FALSE.
C<<
        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IER)     ! Check if error
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' EZPICK ERR','MU_CCT_CF_TOP',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('WAM_CCT_VERS',IVERS,IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' WAM_CCT_VERS','MU_CCT_CF_TOP',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZRSET()
      ENDIF
C
C.. Calculate first stage pal logic

      IF ( IVERS.EQ.1 ) THEN
        BC(1) = B(3)*C(1)   + B(3)*C(2)   + B(4)*C(1)
        BC(2) = B(3)*C(1)   + B(3)*C(2)   + B(4)*C(1)
     &        + B(4)*C(2)   + B(5)*C(3)   + B(5)*C(4)
        BC(3) = B(5)*C(4)   + B(6)*C(5)
        BC(4) = B(6)*C(5)   + B(7)*C(5)   + B(7)*C(6)
        BC(5) = B(8)*C(7)   + B(8)*C(8)   + B(9)*C(8)
     &        + B(9)*C(9)
        BC(6) = B(9)*C(9)   + B(10)*C(9)  + B(10)*C(10)
        BC(7) = B(12)*C(12) + B(11)*C(12) + B(11)*C(11)
        BC(8) = B(13)*C(14) + B(13)*C(13) + B(12)*C(13)
     &        + B(12)*C(12)
        BC(9) = B(15)*C(16) + B(14)*C(16) + B(14)*C(15)
        BC(10)= B(16)*C(17) + B(15)*C(16)
        BC(11)= B(18)*C(20) + B(18)*C(19) + B(17)*C(20)
     &        + B(17)*C(19) + B(16)*C(18) + B(16)*C(17)
        BC(12)= B(18)*C(20) + B(18)*C(19) + B(17)*C(20)
      ENDIF
      IF ( IVERS.EQ.2 ) THEN
        BC(1) = B(2)        + B(3)        + B(4)*C(1)   
     &        + B(4)*C(2)               
        BC(2) = B(3)*C(1)   + B(3)*C(2)   + B(4)*C(1)
     &        + B(4)*C(2)   + B(5)*C(3)   + B(5)*C(4)
        BC(3) = B(5)        + B(6)        + C(3)
        BC(4) = B(6)        + B(7)*C(5)   + B(7)*C(6)
     &        + B(7)*C(7)               
        BC(5) = B(8)*C(7)   + B(8)*C(8)   + B(9)           
     &        + C(8)
        BC(6) = B(9)        + B(10)*C(9)  + B(10)*C(10)
     &        + B(10)*C(11) + B(11)*C(11)
        BC(7) = B(12)       + B(11)*C(12) + B(11)*C(11)
     &        + B(11)*C(10) + B(10)*C(10)
        BC(8) = B(13)*C(14) + B(13)*C(13) + B(12)           
     &        + C(13)
        BC(9) = B(15)       + B(14)*C(16) + B(14)*C(15)
     &        + B(14)*C(14)               
        BC(10)= B(16)       + B(15)       + C(18)
        BC(11)= B(18)*C(20) + B(18)*C(19) + B(17)*C(20)
     &        + B(17)*C(19) + B(16)*C(18) + B(16)*C(17)
        BC(12)= B(19)       + B(18)       + B(17)*C(20)   
     &        + B(17)*C(19)               
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

      DO I=1,12
        IF(JBITS(I).NE.0) CALL HFILL(1040+ILOG,FLOAT(I),0.,1.)
      ENDDO

C.. compute different roads independently for monitoring
      DO I=1,IDIM
        IROAD(I)=0
      ENDDO

C.. all ROADS 
        IROAD(2)   = A(1) *B(2) 
        IROAD(3)   = A(1) *B(3)        
        IROAD(4)   = A(1) *B(4) *C(1)
        IROAD(5)   = A(1) *B(4) *C(2)

        IROAD( 9)  = A(2) *B(3) *C(1)
        IROAD(10)  = A(2) *B(3) *C(2)
        IROAD(11)  = A(2) *B(4) *C(1)
        IROAD(12)  = A(2) *B(4) *C(2)
        IROAD(13)  = A(2) *B(5) *C(3)
        IROAD(14)  = A(2) *B(5) *C(4)

        IROAD(18)  = A(3) *B(5)      
        IROAD(19)  = A(3) *B(6)
        IROAD(20)  = A(3) *C(3)      

        IROAD(24)  = A(4) *B(6)      
        IROAD(25)  = A(4) *B(7) *C(5)
        IROAD(26)  = A(4) *B(7) *C(6)
        IROAD(27)  = A(4) *B(7) *C(7)

        IROAD(31)  = A(5) *B(8) *C(7)
        IROAD(32)  = A(5) *B(8) *C(8)
        IROAD(33)  = A(5) *B(9)        
        IROAD(34)  = A(5) *C(8)

        IROAD(38)  = A(6) *B(9)      
        IROAD(39)  = A(6) *B(10)*C(9)
        IROAD(40)  = A(6) *B(10)*C(10)
        IROAD(41)  = A(6) *B(10)*C(11)
        IROAD(42)  = A(6) *B(11)*C(11)

        IROAD(46)  = A(7) *B(10)*C(10)
        IROAD(47)  = A(7) *B(11)*C(10)
        IROAD(48)  = A(7) *B(11)*C(11)
        IROAD(49)  = A(7) *B(11)*C(12)
        IROAD(50)  = A(7) *B(12)     

        IROAD(54)  = A(8) *C(13)
        IROAD(55)  = A(8) *B(12)       
        IROAD(56)  = A(8) *B(13)*C(13)
        IROAD(57)  = A(8) *B(13)*C(14)
               
        IROAD(61)  = A(9) *B(14)*C(14)
        IROAD(62)  = A(9) *B(14)*C(15)
        IROAD(63)  = A(9) *B(14)*C(16)
        IROAD(64)  = A(9) *B(15)     

        IROAD(68)  = A(10)*C(18)      
        IROAD(69)  = A(10)*B(15)
        IROAD(70)  = A(10)*B(16)      
                               
        IROAD(74)  = A(11)*B(16)*C(17)
        IROAD(75)  = A(11)*B(16)*C(18)
        IROAD(76)  = A(11)*B(17)*C(19)
        IROAD(77)  = A(11)*B(17)*C(20)
        IROAD(78)  = A(11)*B(18)*C(19)
        IROAD(79)  = A(11)*B(18)*C(20)

        IROAD(83)   = A(12)*B(17)*C(19)
        IROAD(84)   = A(12)*B(17)*C(20)
        IROAD(85)   = A(12)*B(18)        
        IROAD(86)   = A(12)*B(19) 

      DO I=1,IDIM
        IF(IROAD(I).NE.0) CALL HFILL(1030+ILOG,FLOAT(I),0.,1.)
      ENDDO

C      CALL WAMUS_ROADS(1100,A,B,C,JBITS)

  999 RETURN
      END
