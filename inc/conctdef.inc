C	=====================================================
C	Parameter Definitions for the Data Acquisition System
C	=====================================================
C
C	Modulename:-
C	============
C	DAQ$CMS:CONCTDEF.INC
C
C	Description:-
C	=============
C	This Include File contains the CON_CT (CONsumer Control Table)
C	entry definitions.
C
C	Creation Date :  8 Mar 1983
C	===========================
C
      Parameter CON_CT_NLOCK  = 0	
      Parameter CON_CT_PLOCK  = 1	
      Parameter CON_CT_NEXT   = 2	
      Parameter CON_CT_PREV   = 3	
      Parameter CON_CT_WNXT   = 4	
      Parameter CON_CT_WPRV   = 5	
      Parameter CON_CT_RQFP   = 6	
      Parameter CON_CT_RQBP   = 7	
      Parameter CON_CT_EVTCT  = 8	
      Parameter CON_CT_EVCT   = 8	
      Parameter CON_CT_EVTCTL = 9	
      Parameter CON_CT_EVCTL  = 9	
      Parameter CON_CT_QUEU   = 10	
      Parameter CON_CT_NAMEA  = 11	
      Parameter CON_CT_NAMEB  = 12	
      Parameter CON_CT_PID    = 13	
      Parameter CON_CT_UIC    = 14	
      Parameter CON_CT_RID    = 15	
      Parameter CON_CT_PRIO   = 16	
      Parameter CON_CT_PARTID = 17	
      Parameter CON_CT_ENUM   = 18	
      Parameter CON_CT_REQ    = 19	
      Parameter CON_CT_ERROR  = 20 	
      Parameter CON_CT_WAIT   = 21      
      Parameter CON_CT_DESC   = 22	
      Parameter CON_CT_AST    = 23	
      Parameter CON_CT_EVFLAG = 24	
      Parameter CON_CT_FREQ   = 25	
      Parameter CON_CT_FRQACT = 26	
      Parameter CON_CT_FRQSEN = 27	
      Parameter CON_CT_ACTUAL = 28	
      Parameter CON_CT_SEEN   = 29	
      Parameter CON_CT_PROD   = 30	
      Parameter CON_CT_ACUSER = 31	
      Parameter CON_CT_SEUSER = 32	
      Parameter CON_CT_PRUSER = 33	
      Parameter CON_CT_LOCKSC = 34	
      Parameter CON_CT_SPACSC = 35	
      Parameter CON_CT_EVNTWC = 36	
      Parameter CON_CT_LOCKWC = 37	
      Parameter CON_CT_SPACWC = 38	
      Parameter CON_CT_DAQBAS = 39	
      Parameter CON_CT_CONSAV = 40	
      Parameter CON_CT_EVTSAV = CON_CT_CONSAV 
      Parameter CON_CT_REQSAV = 41	
      Parameter CON_CT_SUBSAV = 42	
      Parameter CON_CT_PAR1   = 43	
      Parameter CON_CT_PAR2   = 44	
      Parameter CON_CT_PAR3   = 45	
      Parameter CON_CT_PAR4   = 46	
      Parameter CON_CT_PAR5   = 47	
      Parameter CON_CT_PAR6   = 48	
      Parameter CON_CT_PAR7   = 49	
      Parameter CON_CT_PAR8   = 50	
      Parameter CON_CT_PAR9   = 51	
      Parameter CON_CT_LENG   = 52	
C
      Parameter CON_WAIT_REQ  = -1	
      Parameter CON_WAIT_BUSY = 0	
      Parameter CON_WAIT_EVNT = 1	
      Parameter CON_WAIT_SPAC = 2	
      Parameter CON_WAIT_BFM  = 3	
C
      Parameter CON_DESC_RESV   = 0	
      Parameter CON_DESC_CONS   = 1	
      Parameter CON_DESC_PAUS   = 2	
      Parameter CON_DESC_ASYNCH = 3	
      Parameter CON_DESC_OVERAS = 4	
      Parameter CON_DESC_ASWAKE = 5	
      Parameter CON_DESC_VIP    = 6	
      Parameter CON_DESC_VUSE   = 7	
      Parameter CON_DESC_VALL   = 8	
      Parameter CON_DESC_PEND   = 9	
      Parameter CON_DESC_QUEU   = 10	
      Parameter CON_DESC_3264   = 11	
      Parameter CON_DESC_64128  = 12	
      Parameter CON_DESC_FREQ   = 13	
C
C	Request Function Codes
C	======================
C
      Parameter INCLUDE       = 0	
      Parameter EXCLUDE       = 1	
      Parameter GET_EVENT     = 2	
      Parameter RESERVE_EVENT = 3	
      Parameter FREE_EVENT    = 4	
      Parameter GET_SPACE     = 5	
      Parameter SEND_EVENT    = 6	
      Parameter PAUSE         = 7	
      Parameter ADD_REQ       = 8	
      Parameter DEL_REQ       = 9	
      Parameter EVENT_STAT    = 10	
C
