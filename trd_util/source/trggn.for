      SUBROUTINE TRGGN(FILNAM,WIRE,LAYER,WGAIN,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get wire gain for TRD
C-
C-   Inputs  : FILNAM= File name containing the ped. information
C-                   =GEANT for Monte-Carlo
C-             WIRE = Wire number (from 1)
C-             LAYER= Layer Number (from 1)
C-   Outputs : IERR =0 if no error
C-             WGAIN= Wire gain
C-   Controls:
C-
C-   Created  12-MAY-1989   A. Zylberstejn
C-   Updated  25-JAN-1994   A. Zylberstejn  : fix a problem in layer 3 
C-   Updated  28-JAN-1994   J.P. Cussonneau  WGAIN =1 for new MC data
C-   Updated  29-APR-1994   A. Zylberstejn  Remove swap of wires in layer 3 for
C-                                           run 1b 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZTGAI.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTRD.LINK'
      INCLUDE 'D0$LINKS:IZTWGH.LINK'
      INCLUDE 'D0$INC:TRDWIR.INC'
C
      INTEGER IW,WIRE,LAYER,LOUT,LBK,NBK,IFL,LAUX,IADDR,ADR
      INTEGER LTWGH,LTREL,WI
      LOGICAL FIRST,RUN1A
      LOGICAL MC_DATA
      INTEGER LSTPC,   LTRPD
      REAL WGAIN
      CHARACTER*(*) FILNAM
      INTEGER IBK,IERR,K,NW
      INTEGER TRD_FIND_VER,VERSION 
      DATA LOUT/6/
      DATA FIRST/.TRUE./
C
      IERR=1
C      PRINT*,' DANS TRGGN,LTGAI',LTGAI
C      IF(LTGAI.LE.0)THEN
      IF(FIRST)THEN
        FIRST=.FALSE.
C
C -- FIND VERSION NUMBER FOR MC DATA
C
        IF ( LHEAD.GT.0 ) THEN
          MC_DATA =  IQ(LHEAD+1) .GT. 1000
        ELSE
          MC_DATA =  .FALSE.
        ENDIF
        VERSION = TRD_FIND_VER()

C --
        IF(LSTPH.EQ.0)THEN
          WRITE(LOUT,*)' Error in TRGGN: bank STPH not booked'
          GO TO 999
        END IF
C        PRINT*,' AVANT DEFINITION LSTPC',LSTPC
        LSTPC = LC ( LSTPH - IZSTPC )
        IF(LSTPC.EQ.0)THEN
          WRITE(LOUT,*)' Error in TRGGN: bank STPC not booked'
          GO TO 999
        END IF
C        PRINT*,' AVANT DEFINITION LSTRD',LSTRD
        LSTRD=LC(LSTPC-IZSTRD)
        IF(LSTRD.EQ.0)THEN
          WRITE(LOUT,*)' Error in TRGGN: bank STRD not booked'
          GO TO 999
        END IF
C        PRINT*,' AVANT DEFINITION,LTGAI',LTGAI
        LTGAI=LC(LSTRD-IZTGAI)
        IF(LTGAI.EQ.0) THEN
          WRITE(LOUT,*)' Error in TRGGN: bank TGAI not booked'
          GOTO 999
        ENDIF
C        WRITE (LOUT,1001) IC(LTGAI+1),IC(LTGAI+2)
C        PRINT*,' DANS TRGGN,LSTPC,LSTRD,LTGAI'
C        PRINT'(10X,6I10)',LSTPC,LSTRD,LTGAI
C          CALL DZSHOW(' DANS TRGGN',IXSTP,LSTRD,'DLV',0,0,0,0)
      END IF
C --
      IF (MC_DATA.AND.VERSION.EQ.1) THEN
        WGAIN = 1.
        IERR = 0
        GOTO 10
      ENDIF 
C
      LTWGH=LC(LTGAI-IZTWGH)
      LTREL=LC(LTWGH-LAYER)
C      WRITE(LOUT,*)' LTWGH,LTREL',LTWGH,LTREL
      IW=WIRE
C In layer 3 take same gain for each wire in a doublet
      IF(LAYER.EQ.3)THEN
        IF(RUN1A())THEN
          IF(FILNAM.NE.'GEANT' )IW=(WIRE-1)*2+1
c        ELSE
c          IW=WIRE+2*MOD(WIRE+1,2)-1
        END IF
      END IF
      WGAIN=C(LTREL+IW)
C      WRITE(LOUT,*) 'LAYER,WIRE ',LAYER,WIRE,' wgain',wgain
C      PRINT*,' WGAIN',WGAIN
      IERR=0
   10 CONTINUE
C
  999 RETURN
 1001 FORMAT(///,'  TRD WGAINTAL BANK ,  MIN RELEVANT RUN NUMBER ',I8,
     >'      MAX  ',I8)
 1002 FORMAT(1X,16F7.2)
 1003 FORMAT(/,'   TRD WGAIN BANK NUMBER ',I6)
      END
