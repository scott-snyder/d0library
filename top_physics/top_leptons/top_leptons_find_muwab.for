      SUBROUTINE TOP_LEPTONS_FIND_MUWAB(I_MU_IN,LPMUO_IN,
     1  I_CL_IN,LCLUS_IN,ID_CLUS,LPNUT,I_CL_OUT,LCLUS_OUT,I_BREM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search for possible W-decay Wide Angle bremm events
C-                         and produce reduced lists of bremm or non-bremm
C-                          events 
C-
C-   Inputs  : 
C-                I_MU_IN   - no. of good muon candidates
C-                LPMUO_IN  - input array of bank pointers
C-                I_CL_IN   - no. of good electron/photon candidates
C-                LCLUS_IN  - input array of bank pointers
C-                ID_CLUS   - cluster specification 
C-                            1 = electron, 2 = photon
C-                LPNUT     - pointer to appropriate missing Et bank
C-                I_BREM    - output flag
C-                            1 = return non-brem only
C-                           -1 = return bremm only
C-   Outputs : 
C-                I_CL_OUT  - no. of good electron/photon candidates
C-                LCLUS_OUT - output array of bank pointers
C-   Controls: 
C-                None
C-
C-   Created   6-MAY-1993   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_UTIL_MASS4,TOP_LEPTONS_UTIL_CALC_DR
C
      LOGICAL FIRST,DO_MT3_WAB_CUT
C
      INTEGER I_MU_IN,LPMUO_IN(5)
      INTEGER I_CL_IN,LCLUS_IN(5),I_CL_OUT,LCLUS_OUT(5)
      INTEGER ID_CLUS,I_BREM,LPNUT,ID_ELEC
      INTEGER I,J,IER,JBIT,JCAND(5)
C
      REAL PI,TWOPI,IETA
      REAL VMOD,DILEP_VEC(4),VEC2(4),PNUT3_ETMIN
      REAL M2_MUCLUS,MT3_CLUS,MT3_MIN_WAB,MT3_MAX_WAB
      REAL TOP_LEPTONS_UTIL_MASS4,DRMAX_MUCLUS
      REAL TOP_LEPTONS_UTIL_CALC_DR,DPHI,DETA,DR
C
      DATA PI,TWOPI/3.141593,6.283185/
      DATA FIRST/.TRUE./
C
C *** Read cut parameters from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        IF (IER.EQ.0) CALL EZGET('DO_MT_WAB_CUT',DO_MT3_WAB_CUT,IER)
        IF (IER.EQ.0) CALL EZGET('WAB_PNUT3_ETMIN',PNUT3_ETMIN,IER)
        IF (IER.EQ.0) CALL EZGET('WAB_DR_MAX_MUCLUS',
     1    DRMAX_MUCLUS,IER)
        IF (IER.EQ.0) CALL EZGET('MT_MIN_WAB',MT3_MIN_WAB,IER)
        IF (IER.EQ.0) CALL EZGET('MT_MAX_WAB',MT3_MAX_WAB,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_MUWAB',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      I_CL_OUT=0
      CALL VZERO(LCLUS_OUT,5)
      IF(.NOT.DO_MT3_WAB_CUT) GO TO 888
C
C *** Check to see if we are dealing with PELC or PPHO Banks
C
      IF(I_MU_IN.LT.1.OR.I_CL_IN.LT.1) GO TO 888
C
C *** Check that there is enough Etmiss for a W-decay
C
      IF(Q(LPNUT+7).LT.PNUT3_ETMIN) GO TO 888
C
      IF(ID_CLUS.EQ.1) THEN
C
C *** muon-electron search
C
        CALL VZERO(JCAND,5)
        DO I=1,I_MU_IN
          DO J=1,I_CL_IN
            IF(JCAND(J).GT.-1) THEN
C
C *** Check that cluster is close enough to the muon
C
              DPHI=ABS(Q(LPMUO_IN(I)+17)-Q(LCLUS_IN(J)+10))
              IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
              DETA=Q(LPMUO_IN(I)+16)-Q(LCLUS_IN(J)+9)
              DR=TOP_LEPTONS_UTIL_CALC_DR(DETA,DPHI)
              IF(DR.GT.DRMAX_MUCLUS) GO TO 100
C
C *** check that electron is consistent with 2 mips
C *** (ie. photon conversion)
C
              IETA=ABS(Q(LCLUS_IN(J)+19)/10.)
              IF(IQ(LCLUS_IN(J)+1).LT.3) THEN
                ID_ELEC=IQ(LCLUS_IN(J)+20)
              ELSE
                ID_ELEC=IQ(LCLUS_IN(J)+30)
              ENDIF
              IF(IETA.GT.1.2) THEN
                IF(JBIT(ID_ELEC,24).NE.1) GO TO 100
              ELSE
                IF(JBIT(ID_ELEC,23).NE.1) GO TO 100
              ENDIF
C
              CALL VZERO(DILEP_VEC,4)
              CALL UCOPY(Q(LPMUO_IN(I)+10),DILEP_VEC,4)
              CALL VADD(Q(LCLUS_IN(J)+3),DILEP_VEC(1),DILEP_VEC(1),4)
              M2_MUCLUS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
C
              CALL VZERO(VEC2,4)
              CALL VADD(DILEP_VEC,Q(LPNUT+3),VEC2,2)
              MT3_CLUS=( SQRT(M2_MUCLUS**2+VMOD(DILEP_VEC,2)**2)
     1          +VMOD(Q(LPNUT+3),2) )**2 - VMOD(VEC2,2)**2
              IF(MT3_CLUS.GT.0.) THEN
                MT3_CLUS=SQRT(MT3_CLUS)
              ELSE
                MT3_CLUS=0.
              ENDIF
              IF(MT3_CLUS.GT.MT3_MIN_WAB.AND.MT3_CLUS.LT.MT3_MAX_WAB)
     1          THEN
                JCAND(J)=-1
              ENDIF
            ENDIF
  100       CONTINUE
          ENDDO
        ENDDO
C
C *** copy over output bank pointer arrays - if needed
C
        DO J=1,I_CL_IN
          IF(I_BREM.GT.0) THEN
            IF(JCAND(J).GT.-1) THEN
              I_CL_OUT=I_CL_OUT+1
              LCLUS_OUT(I_CL_OUT)=LCLUS_IN(J)
            ENDIF
          ELSE
            IF(JCAND(J).LT.0) THEN
              I_CL_OUT=I_CL_OUT+1
              LCLUS_OUT(I_CL_OUT)=LCLUS_IN(J)
            ENDIF
          ENDIF
        ENDDO
      ELSE
C
C *** muon-photon search
C
        CALL VZERO(JCAND,5)
        DO I=1,I_MU_IN
          DO J=1,I_CL_IN
            IF(JCAND(J).GT.-1) THEN
C
C *** Check that cluster is close enough to the muon
C
              DPHI=ABS(Q(LPMUO_IN(I)+17)-Q(LCLUS_IN(J)+10))
              IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
              DETA=Q(LPMUO_IN(I)+16)-Q(LCLUS_IN(J)+9)
              DR=TOP_LEPTONS_UTIL_CALC_DR(DETA,DPHI)
              IF(DR.GT.DRMAX_MUCLUS) GO TO 200
C
              CALL VZERO(DILEP_VEC,4)
              CALL UCOPY(Q(LPMUO_IN(I)+10),DILEP_VEC,4)
              CALL VADD(Q(LCLUS_IN(J)+3),DILEP_VEC(1),DILEP_VEC(1),4)
              M2_MUCLUS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
C
              CALL VZERO(VEC2,4)
              CALL VADD(DILEP_VEC,Q(LPNUT+3),VEC2,2)
              MT3_CLUS=( SQRT(M2_MUCLUS**2+VMOD(DILEP_VEC,2)**2)
     1          +VMOD(Q(LPNUT+3),2) )**2 - VMOD(VEC2,2)**2
              IF(MT3_CLUS.GT.0.) THEN
                MT3_CLUS=SQRT(MT3_CLUS)
              ELSE
                MT3_CLUS=0.
              ENDIF
              IF(MT3_CLUS.GT.MT3_MIN_WAB.AND.MT3_CLUS.LT.MT3_MAX_WAB)
     1          THEN
                JCAND(J)=-1
              ENDIF
  200         CONTINUE
            ENDIF
          ENDDO
        ENDDO
C
C *** copy over output bank pointer arrays - if needed
C
        DO J=1,I_CL_IN
          IF(I_BREM.GT.0) THEN
            IF(JCAND(J).GT.-1) THEN
              I_CL_OUT=I_CL_OUT+1
              LCLUS_OUT(I_CL_OUT)=LCLUS_IN(J)
            ENDIF
          ELSE
            IF(JCAND(J).LT.0) THEN
              I_CL_OUT=I_CL_OUT+1
              LCLUS_OUT(I_CL_OUT)=LCLUS_IN(J)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
  888 CONTINUE
C
C *** No cut needed so copy over pointers -according to I_BREM flag
C
      IF(I_BREM.GT.0) THEN
        I_CL_OUT=I_CL_IN
        DO I=1,I_CL_IN
          LCLUS_OUT(I)=LCLUS_IN(I)
          ENDDO
      ENDIF
      RETURN
      END
