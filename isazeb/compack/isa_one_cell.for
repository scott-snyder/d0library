      SUBROUTINE ISA_ONE_CELL(NCELL,IETAC,IPHIC,LAYERC,LOW,HIGH,P,NCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump energy into a single cell; called from
C-                         ISA_CAEPFL
C-   Inputs  : low,high:                 layer number limits
C-             ncell,ietac,iphic,layerc: #cells hit, hit physics coords
C-   Outputs : 
C-      NCH = number of cells hit (cumulative)
C-
C-   Controls: 
C-
C-   Created  28-SEP-1990   Samuel Aronson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      LOGICAL ISA_CAEPFL,ISA_CAEP_DIA
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INTEGER IETAC(*),IPHIC(*),LAYERC(*),PAKADR
      INTEGER LISV1,ID,NCELL,IETA,IPHI,LAYER,LOW,HIGH,NR
      INTEGER NALOC,NCH,LCAEP,GZCAEP,LDCAEP,IDABS,I,IDV,LISP1
      INTEGER ARGSOK
      REAL    P(4),PHI,TH,ETA,PTOT,DIR(3),VTX(3),PCUT(3)
      CHARACTER*4 PATH
      LOGICAL SMEAR
C&IF VAXVMS
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
C&ENDIF
C
C               find cell to dump energy into
      LCAEP=GZCAEP()
      NR=IQ(LCAEP+2)
      LAYER=0
      DO I=1,NCELL
        IF(LAYERC(I).GT.LOW.AND.LAYERC(I).LT.HIGH) THEN
          LAYER=LAYERC(I)
          IPHI=IPHIC(I)
          IETA=IETAC(I)
          GOTO 41
        ENDIF
      ENDDO
   41 IF(LAYER.EQ.0) GO TO 999        ! no cell found
C
C                    find pointer and fill
      IF(PTCAEP(IETA,IPHI,LAYER).EQ.0) THEN
        NCH=NCH+1
        PTCAEP(IETA,IPHI,LAYER)=NCH
        LDCAEP=LCAEP+(NCH-1)*NR
C           pack addresses
C&IF VAXVMS
        BYTES(4)=IETA
        BYTES(3)=IPHI
        BYTES(2)=LAYER
        BYTES(1)=0             ! not smeared
C&ENDIF
        IQ(LDCAEP+4)=PAKADR
        Q(LDCAEP+5)=P(4)
      ELSE
        LDCAEP=LCAEP+(PTCAEP(IETA,IPHI,LAYER)-1)*NR
        Q(LDCAEP+5)=Q(LDCAEP+5)+P(4)
      ENDIF

  999 RETURN
      END
