      SUBROUTINE TOP_LEPTONS_FIND_MUBREM(I_MU_IN,LPMUO_IN,
     1  I_CL_IN,LCLUS_IN,I_CL_OUT,LCLUS_OUT,ID_CLUS,I_BREM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search for muon bremm events and produce
C-                         reduced lists of bremm or non-bremm events 
C-
C-   Inputs  : 
C-                I_MU_IN   - no. of good muon candidates
C-                LPMUO_IN  - input array of bank pointers
C-                I_CL_IN   - no. of good electron/photon candidates
C-                LCLUS_IN  - input array of bank pointers
C-                ID_CLUS   - cluster specification 
C-                            1 = electron, 2 = photon
C-                I_BREM    - output flag
C-                            1 = return non-brem only
C-                           -1 = return bremm only
C-   Outputs : 
C-                I_CL_OUT  - no. of non-bremm clusters
C-                LCLUS_OUT - output array of cluster bank pointers
C-   Controls: 
C-                None
C-
C-   Created   3-MAY-1993   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_UTIL_CALC_DR
C
      LOGICAL FIRST
C
      INTEGER I_MU_IN,LPMUO_IN(5)
      INTEGER I_CL_IN,LCLUS_IN(5),I_CL_OUT,LCLUS_OUT(5),ID_CLUS
      INTEGER I,J,IER,ICAND(5),JCAND(5),I_BREM
C
      REAL DR_MIN_MUCLUS,DPHI_MUCLUS,DETA_MUCLUS,DR_MUCLUS
      REAL PI,TWOPI,TOP_LEPTONS_UTIL_CALC_DR
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
        CALL EZGET('MU_ELPH_DRMIN',DR_MIN_MUCLUS,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_MUBREM',' ','F')
        FIRST=.FALSE.
      ENDIF
C
C *** Check to see if we are dealing with PELC or PPHO Banks
C
      I_CL_OUT=0
      CALL VZERO(LCLUS_OUT,5)
      IF(I_MU_IN.LT.1) GO TO 999
      IF(ID_CLUS.EQ.1) THEN
C
C *** muon-electron search
C
        CALL VZERO(ICAND,5)
        CALL VZERO(JCAND,5)
        DO I=1,I_MU_IN
          DO J=1,I_CL_IN
            IF(JCAND(J).GT.-1.AND.ICAND(I).GT.-1) THEN
              DPHI_MUCLUS=ABS(Q(LPMUO_IN(I)+17)-Q(LCLUS_IN(J)+10))
              IF(DPHI_MUCLUS.GT.PI) DPHI_MUCLUS=TWOPI-DPHI_MUCLUS
              DETA_MUCLUS=Q(LPMUO_IN(I)+16)-Q(LCLUS_IN(J)+9)
              DR_MUCLUS=TOP_LEPTONS_UTIL_CALC_DR(DETA_MUCLUS,
     1          DPHI_MUCLUS)
              IF(DR_MUCLUS.LT.DR_MIN_MUCLUS) THEN
C
C *** radiative candidate
C
                JCAND(J)=-1
                ICAND(I)=-1
              ENDIF
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
      ELSE
C
C *** muon-photon search
C
        CALL VZERO(ICAND,5)
        CALL VZERO(JCAND,5)
        DO I=1,I_MU_IN
          DO J=1,I_CL_IN
            IF(JCAND(J).GT.-1.AND.ICAND(I).GT.-1) THEN
              DPHI_MUCLUS=ABS(Q(LPMUO_IN(I)+17)-Q(LCLUS_IN(J)+10))
              IF(DPHI_MUCLUS.GT.PI) DPHI_MUCLUS=TWOPI-DPHI_MUCLUS
              DETA_MUCLUS=Q(LPMUO_IN(I)+16)-Q(LCLUS_IN(J)+9)
              DR_MUCLUS=TOP_LEPTONS_UTIL_CALC_DR(DETA_MUCLUS,
     1          DPHI_MUCLUS)
              IF(DR_MUCLUS.LT.DR_MIN_MUCLUS) THEN
C
C *** radiative candidate
C
                JCAND(J)=-1
                ICAND(I)=-1
              ENDIF
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
      END
