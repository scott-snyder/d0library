      SUBROUTINE VLANDA ( DGAS, PLHT, NMEAN)
C======================================================================
C
C   Purpose and Methods : Generate the integrated pulse height
C                         PARAMETERS : IDRAY=1 ILOSS=1 
C
C   Inputs  :    DGAS : Track length in the gaz in the sense wire cell
C   Outputs :    PLHT : Integrated pulse height
C                NMEAN: Mean number of ionization clusters per VTX cell 
C                       (per 0.457 cm)
C
C   Created  30-MAR-1987   K. Nishikawa
C   Copied for VTX  28-May-1987  T. Trippe ******* needs modification for VTX
C-   Updated  13-DEC-1989   Qizhong Li-Demarteau   add a check on BETHE 
C-   Updated   1-JUL-1990   Qizhong Li-Demarteau  remove ADC saturation cut 
C-   Updated  30-MAR-1992   K. Wyatt Merritt  restore an ADC saturation cut! 
C-   Updated   4-MAY-1992   Alexandre Zinchenko  add output parameter NMEAN 
C
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCTRAK.INC'
      INCLUDE 'D0$INC:GCKINE.INC'
C     
      REAL PLHT, DGAS
C
      REAL RNDM
      REAL BETA,BETAS,EM,BETHE,AVNE,AVNES
      REAL EPOS,EDUM,RESOL,DELTA,RR
      REAL EION,EFION,EDCUT
      INTEGER ION,NION
      INTEGER NPRIM
      INTEGER NMEAN 
C
      DATA EION/0.0158/       ! KEV IONIZATION POTEN. FOR Ar
      DATA EFION/0.026/       ! EFFECTIVE SECONDARY IONIZATION POTEN.
      DATA EDCUT/20./         ! DELTA RAY ENERGY CUT IN KEV
      DATA RESOL/0.01/
C======================================================================
C
C **** POISSON=GAUSSIAN WITH MEAN=>10
C
      PLHT=0.
C
      IF ( VECT(7) .EQ. 0. ) GO TO 999
      IF ( GETOT .GE. 0.0005 ) THEN
        BETA=VECT(7)/GETOT
        BETAS=BETA*BETA
        EM=VECT(7)*VECT(7)/AMASS/AMASS
        BETHE=0.1/BETAS*(2*(ALOG(EM/0.000026)-BETAS))
        IF (BETHE .LE. 0) GOTO 999
        AVNE=29.*DGAS*BETHE/2.44               ! MEAN # OF ION IN Ar
        NMEAN=AVNE/DGAS*0.457 
        AVNES=SQRT(AVNE)
        CALL RANNOR(EPOS,EDUM)
        EPOS=EPOS*AVNES+AVNE 
        NPRIM=IFIX(EPOS)
C
C ****  Secondary ionization
C
        PLHT=0.
        IF ( NPRIM .LE. 0 ) GO TO 999
        DO 110 ION=1,NPRIM
          RR=1.-RNDM(0)
          IF ( RR .LE. 0.00000035 ) RR=0.00000035
          DELTA=EION/RR 
          IF ( DELTA .GE. EDCUT ) GO TO 110
          NION=IFIX(DELTA/EFION)
          PLHT=PLHT+FLOAT(1+NION)
110     CONTINUE
      ENDIF
      PLHT = AMIN1(PLHT,5000.) ! Require sensible maximum on pulse ht
C                                to avoid GSAHIT errors that may mask 
C                                more serious problems.
999   RETURN
      END

