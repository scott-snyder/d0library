      SUBROUTINE TRD_CLEAN_CLUST(LAYER,TRACK,NCM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select among all cathode clusters the NCM with the
C-                         highest energy
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-MAR-1994   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:TCLUS_PER_WIRE.INC'
      INCLUDE 'D0$INC:TRENER.INC'
      INCLUDE 'D0$INC:worksp.INC'
      INTEGER LOUT,TRUNIT
      LOGICAL DOPRINT,TRD_DO_PRINT,FIRST
      INTEGER LAYER,TRACK
      INTEGER TAG(100),JS,INDEX(100),WIRE,CL,NC,NCLC,NCM,NMAX
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        LOUT=TRUNIT()
        FIRST=.FALSE.
      END IF
      DOPRINT=TRD_DO_PRINT()
      NC=NBHWIR(LAYER,TRACK)
      NCLC=NTOT_CL(LAYER)
      JS=0
      IF(DOPRINT) WRITE(LOUT,*)' nc,nclc',NC,NCLC,' ncm',NCM
C  Tag each cluster with the wire it belongs to
      DO WIRE=1,NC
        DO CL=1,NCL_PER_WIRE(WIRE,LAYER)
          JS=JS+1
          TAG(JS)=WIRE
        END DO
      END DO
      IF(DOPRINT)THEN
        WRITE(LOUT,'(20f6.2)')(100.*FLOAT(TAG(CL))+ECL_PER_WIRE(CL,
     &    LAYER), CL=1,NCLC)
        WRITE(LOUT,*)' ncl_per_wire',(NCL_PER_WIRE(JS,LAYER),JS=1,NC)
      END IF
C  Sort clusters by descending order in energy
      CALL SORTZV(ECL_PER_WIRE(1,LAYER),INDEX,NCLC,1,1,0)
C      write(lout,*)' energy'
C      print'(10f5.2)',(ecl_per_wire(cl,layer),cl=1,nclc)
C       write(lout,*)' tag'
C       print'(20i5)',(tag(cl),cl=1,nclc)
C      write(lout,*)' index'
C      print'(10i5)',(index(cl),cl=1,nclc)
      NMAX=MIN0(NCM,NCLC)
      DO CL =1,NMAX
        JS=INDEX(CL)
        WS(CL)=ECL_PER_WIRE(JS,LAYER)
        IWS(CL+NMAX)=TAG(JS)
        IWS(CL+2*NMAX)=LEFT_CL(JS,LAYER)
        IWS(CL+3*NMAX)=CENTER_CL(JS,LAYER)
        IWS(CL+4*NMAX)=RIGHT_CL(JS,LAYER)
        IWS(CL+5*NMAX)=HEIGTH_CL(JS,LAYER)
      END DO
C       write(lout,*)' ecl ordonne'
C       print'(20f5.2)',(ws(cl),cl=1,nmax)
C       write(lout,*)' tag'
C       print'(20i5)',(iws(cl+nclc),cl=1,nmax)
C  Sort clusters by ascending wire number
      CALL SORTZV(IWS(1+NMAX),INDEX,NMAX,-1,0,0)
      DO CL=1,NMAX
        JS=INDEX(CL)
        ECL_PER_WIRE(CL,LAYER)=WS(JS)
        TAG(CL)=IWS(JS+NMAX)
        LEFT_CL(JS,LAYER)=IWS(JS+2*NMAX)
        CENTER_CL(CL,LAYER)=IWS(JS+3*NMAX)
        RIGHT_CL(CL,LAYER)=IWS(JS+4*NMAX)
        HEIGTH_CL(CL,LAYER)=IWS(JS+5*NMAX)
      END DO
C      write(lout,*)' index'
C      print'(20i5)',(index(cl),cl=1,nmax)
C       write(lout,*)' tag'
C       print'(20i5)',(tag(cl),cl=1,nmax)
C       write(lout,*)' ecl ordonne par fil'
      DO WIRE=1,NC
        NCL_PER_WIRE(WIRE,LAYER)=0
      END DO
      DO CL=1,NMAX
        JS=TAG(CL)
        NCL_PER_WIRE(JS,LAYER)=NCL_PER_WIRE(JS,LAYER)+1
      END DO
      IF(DOPRINT)THEN
        WRITE(LOUT,'(20f6.2)')(100.*FLOAT(TAG(CL))+ECL_PER_WIRE(CL,
     &    LAYER), CL=1,NMAX)
        WRITE(LOUT,*)' ncl_per_wire',(NCL_PER_WIRE(JS,LAYER),JS=1,NC)
      END IF
  999 RETURN
      END
