      SUBROUTINE GLINE0(LABLIN,PARLIN,MAXPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output parameter list in LINE mode for GETDIS.
C-
C-   Inputs  : LABLIN: Character array of labels, one for each parameter 
C-             PARLIN: Character array of current value of each parameter
C-             MAXPAR: Integer number of parameters to use               
C-   Outputs : None
C-   Controls: None
C-
C-   Created  26-APR-1987   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MAXPAR      
      CHARACTER*(*) LABLIN(1:MAXPAR),PARLIN(1:MAXPAR)
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      CHARACTER*80 MSGLIN,BLNK
      INTEGER I,TRULEN,K,J,LI,CU,LIBGET,ISTAT,LIBPUT
      DATA BLNK/' '/      
C----------------------------------------------------------------------
      CALL OUTMSG(' ')
      DO I=1,MAXPAR             
         K=MIN0(TRULEN(LABLIN(I)),40)       
         J=MIN0(TRULEN(PARLIN(I)),30)    
         WRITE(MSGLIN,981) I,LABLIN(I)(1:K),BLNK(1:50-(K+8)),
     *                     PARLIN(I)(1:J) 
981      FORMAT(' ',I3,'-> ',A,A,A)
         CALL OUTMSG(MSGLIN)
      ENDDO
      CALL OUTMSG(' ')
      RETURN
      END
