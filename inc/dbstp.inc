C----------------------------------------------------------------------
C-
C-   Created  13-MAR-1989   Jan Guida
C-   Updated  25-FEB-1991   S. Abachi
C-
C----------------------------------------------------------------------
C-
      INTEGER MAXCRT
      PARAMETER (MAXCRT = 310)               
      INTEGER LTDIR, LKEYS(0:MAXCRT), LDATA(0:MAXCRT)
      INTEGER NKYS,NXKEY,NDET
      PARAMETER (NKYS = 15)             
      PARAMETER (NXKEY = 8)             
      PARAMETER (NDET= 8)              
      CHARACTER*5 TOPN
      INTEGER LUNDB(NDET),DBLUN,JUNIT,BUNIT,DOPT
      LOGICAL CALL_DBEND,OPTJ,OPTB,SERVER,LFORCE,LVSN,RSERVER,RCSERVER
      LOGICAL CDBEND(NDET)
      COMMON /DBSTP/ LTDIR,LKEYS,LDATA,CALL_DBEND,DBLUN,
     &               JUNIT,BUNIT,OPTJ,OPTB,SERVER,DOPT,
     &               LUNDB,CDBEND,LFORCE,LVSN,RSERVER,RCSERVER,
     &               TOPN
C-
C-----------------------------------------------------------------------
C-      LKEYS(*) = Address of the Key bank for a Crate
C-      LDATA(*) = Address of the data bank for a Crate
C-      LTDIR    = Address of the Top Directory
C-      LUNDB    = Unit Number for upto 3 databases
C-      DBLUN    = Unit number being used
C-      JUNIT    = Unit for journal file
C-      BUNIT    = Unit for backup file
C-      CALL_DBEND = Set true if DBEND is called. set false during db-init.
C------------------------------------------------------------------------
