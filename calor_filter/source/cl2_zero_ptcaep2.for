      SUBROUTINE CL2_ZERO_PTCAEP2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Based on contents of L2 CAEP, zero PTCAEP2
C-              pointer array.  Remember that it has been zero'd, so that
C-              filling routines can VZERO it completely in case that this
C-              routine has not been called
C-            Based on CPTCAZ
C-
C-       If PTCAEP2_ALL_ZEROS is true do nothing
C-
C-   Inputs  : L2CAEP from /CL2_LINK/, /CAEP/,/PTCAEP2/
C-   Outputs : Zero'd /PTCAEP2/, PTCAEP2_ALL_ZEROS = .FALSE.
C-   Controls: PTCAEP2_ALL_ZEROS
C-
C-   Created  19-JUN-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:CL2_LINK.INC'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER INDEX,LDCAEP,NCH,NR,I
      INTEGER IETA,IPHI,ILYR
      INTEGER NUMALL
C&IF VAXVMS
      BYTE BYT(4)
      EQUIVALENCE (BYT(1),INDEX)
C&ENDIF
C----------------------------------------------------------------------
C
      IF(PTCAEP2_ALL_ZEROS) RETURN      ! don't do it if array is empty
      IF(L2CAEP .GT.0) THEN
        NR=IQ(L2CAEP+2)
        NCH=IQ(L2CAEP+3)
        LDCAEP = L2CAEP - NR
        DO I=1,NCH*NR,NR
          LDCAEP=LDCAEP + NR
          INDEX=IQ(LDCAEP+4)
C&IF VAXVMS
          IETA=BYT(4)
          IPHI=BYT(3)
          ILYR=BYT(2)
C&ENDIF
        PTR2(ILYR,IPHI,IETA)=0
        ENDDO
C
      ELSE
        NUMALL=(2*NETAL+1)*NPHIL*NLYRL
        CALL VZERO(PTR2,NUMALL)
C
      ENDIF
      PTCAEP2_ALL_ZEROS=.TRUE.
C
      RETURN
      END
