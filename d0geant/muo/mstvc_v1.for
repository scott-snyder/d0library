      SUBROUTINE MSTVC_V1(IPL,ICL1,HITS,NINDX,INDX,ITVC)
C-----------------------------------------------------------------
C-    This simulates TVC outputs for muon PDT hits.
C-
C-  Input:
C-    IPL         I   plane number
C-    ICL1        I   cell number +1 (1,3,....23  every two cells)
C-
C-    HITS(i,j)   F   hit information for j-th hit.
C-    NINDX(m,n)  I   no. of hits in m-th cell in n-th plane.
C-    INDX(k,m,n) I   pointer to HITS for k-th hit in cell(m)
C-                    and plane(n).
C-
C-  Output:
C-    ITVC(a,b)   I   simulated TVC output.
C-                    a=1/2=first/second hit
C-                    b=1/2=drift time/delta time.
C-
C-  S.Kunori  31-Mar-87
C-  S.Igarashi 25_Apr_91  change the subroutine name for version update
C------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER NHDIM,NHMAX
      PARAMETER (NHDIM=6)
      PARAMETER (NHMAX=40)
      INTEGER IPL,ICL1,NINDX(24,4),INDX(NHMAX,24,4)
      INTEGER ITVC(2,2)
      REAL    HITS(NHDIM,NHMAX)
C  -- local variables...
      INTEGER I,JP,JC,JH,NDRFT
      REAL DRFT1(NHMAX)      ! drift time at end of first cell.
      REAL DRFT2(NHMAX)      ! drift time at end of second cell.
      REAL SPDLGT,DWIRE,DELAY,T1,T2
      DATA SPDLGT/30./       ! spead of light in cm/nsec.
      DATA DWIRE /10./       ! wire length to gang a pair of wires.(cm)
      DATA DELAY /80./       ! length of delay line on T2 (nsec).
      ! T1 (T2) is time at end of even (odd) wire.
C
      JP=IPL
      NDRFT=0
      DO 110 JC=ICL1,ICL1+1
C
C           -- check if hits exist.
        IF(NINDX(JC,JP).GT.0) THEN
C           -- store drift times.
          DO 120 JH=1,NINDX(JC,JP)
            I=INDX(JH,JC,JP)
            T1=HITS(4,I)+HITS(5,I)/SPDLGT
            T2=HITS(4,I)+(HITS(5,I)+2.*HITS(6,I)+DWIRE)/SPDLGT
            NDRFT=NDRFT+1
            IF(MOD(JC,2).EQ.1) THEN
              DRFT1(NDRFT)=T1
              DRFT2(NDRFT)=T2
            ELSE
              DRFT1(NDRFT)=T2
              DRFT2(NDRFT)=T1
            ENDIF
  120     CONTINUE
        ENDIF
  110 CONTINUE
C           -- sort time in ascending order.
C              i.e. first array has smallest.
      IF(NDRFT.GE.2) THEN
        CALL FLPSOR(DRFT1,NDRFT)
        CALL FLPSOR(DRFT2,NDRFT)
      ENDIF
C           -- store drift time.
      IF(NDRFT.GT.0) THEN
C              -- first hit.
        ITVC(1,1)=DRFT1(1)
        ITVC(1,2)=DRFT2(1)+DELAY-DRFT1(1)
        IF(NDRFT.GE.2) THEN
C                -- second hit.
          ITVC(2,1)=DRFT1(2)
          ITVC(2,2)=DRFT2(2)+DELAY-DRFT1(2)
        ELSE
          ITVC(2,1)=0
          ITVC(2,2)=0
        ENDIF
      ENDIF
C-------------------debug------------------------------------
      IF(IDEBUG.EQ.1.AND.DDIG.EQ.1.AND.PMUO.GE.2) THEN
        WRITE(LOUT,60) NDRFT,IPL,ICL1
   60   FORMAT(' =MSTVC=      ndrft=',I6,'   ipl/icl1=',2I5)
        IF(NDRFT.GE.1) THEN
          WRITE(LOUT,61) (DRFT1(JP),JP=1,NDRFT)
          WRITE(LOUT,62) (DRFT2(JP),JP=1,NDRFT)
   61     FORMAT('   drft1: ',12F8.1)
   62     FORMAT('   drft2: ',12F8.1)
        ENDIF
        WRITE(LOUT,63) ITVC
   63   FORMAT('   itvc:  ',4I10)
      ENDIF
C-------------------end debug--------------------------------
      RETURN
      END
