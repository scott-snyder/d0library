C	=====================================================
C	Parameter Definitions for the Data Acquisition System
C	=====================================================
C
C	Modulename:-
C	============
C	DAQ$CMS:DAQDEF.INC
C
C	Description:-
C	=============
C	This Include File contains the DAQ_TABLES Control
C	Table Header Word definitions.
C
C	Creation Date :  8 Mar 1983
C	===========================
C
      Parameter DAQ_TABLE_WORD = 0	
      Parameter DAQ_EVENT_WORD = 1	
      Parameter DAQ_LOCK_FP    = 2	
      Parameter DAQ_LOCK_BP    = 3	
      Parameter DAQ_UNACON_FP  = 4	
      Parameter DAQ_UNACON_BP  = 5	
      Parameter DAQ_ACTCON_FP  = 6	
      Parameter DAQ_ACTCON_BP  = 7	
      Parameter DAQ_EVWAIT_FP  = 8	
      Parameter DAQ_EVWAIT_BP  = 9	
      Parameter DAQ_SPWAIT_FP  = 10	
      Parameter DAQ_SPWAIT_BP  = 11	
      Parameter DAQ_UNAREQ_FP  = 12	
      Parameter DAQ_UNAREQ_BP  = 13	
      Parameter DAQ_UNAEVT_FP  = 14	
      Parameter DAQ_UNAEVT_BP  = 15	
      Parameter DAQ_EVTCT_FP   = 16	
      Parameter DAQ_EVTCT_BP   = 17	
      Parameter DAQ_OCCEV_FP   = 18	
      Parameter DAQ_OCCEV_BP   = 19	
      Parameter DAQ_NUMB_CONCT = 20	
      Parameter DAQ_NUMB_REQCT = 21	
      Parameter DAQ_NUMB_EVTCT = 22	
      Parameter DAQ_HEAD_WORD  = 23	
      Parameter DAQ_CONCT_WORD = 24	
      Parameter DAQ_REQCT_WORD = 25	
      Parameter DAQ_EVTCT_WORD = 26	
      Parameter DAQ_EVENT_HEAD = 27	
      Parameter DAQ_NAMEA      = 28	
      Parameter DAQ_NAMEB      = 29     
      Parameter DAQ_PID        = 30     
      Parameter DAQ_UIC        = 31	
      Parameter DAQ_RID        = 32	
      Parameter DAQ_PRIO       = 33	
      Parameter DAQ_VAX_ID     = 34	
      Parameter DAQ_MODE       = 35	
      Parameter DAQ_ENUM       = 36	
      Parameter DAQ_BMX_CONCT  = 37	
      Parameter DAQ_TIMEOUT    = 38	
      Parameter DAQ_TIMEOUT_B  = 39	
      Parameter DAQ_MIN_HOLE   = 40	
      Parameter DAQ_MAX_RESRV  = 41	
      Parameter DAQ_MAX_REQ    = 42	
      Parameter DAQ_MAX_HOLE   = 43	
      Parameter DAQ_LOW_HOLE   = 44	
      Parameter DAQ_HIGH_HOLE  = 45	
      Parameter DAQ_NEXT_OCC   = 46	
      Parameter DAQ_DEF_TYPE   = 47	
      Parameter DAQ_TOP_TYPE   = 48	
      Parameter DAQ_WORK_1     = 49	
      Parameter DAQ_WORK_2     = 50	
C
      Parameter DAQ_HEAD_LENG  = 52	
C
C	DAQ Mode Bits
C	=============
C
      Parameter DAQ_MODE_ACTIVE  = 0	
      Parameter DAQ_MODE_TRACE   = 1	
      Parameter DAQ_MODE_LOCAL   = 2	
      Parameter DAQ_MODE_BUFMAN  = 3	
      Parameter DAQ_MODE_ALIGN   = 4	
      Parameter DAQ_MODE_DELAY   = 5	
      Parameter DAQ_MODE_DEFREQ  = 6	
      Parameter DAQ_MODE_3264    = 7    
      Parameter DAQ_MODE_64128   = 8	
      Parameter DAQ_MODE_VIP     = 9	
      Parameter DAQ_MODE_UIC     = 10	
      Parameter DAQ_MODE_RID     = 11	
      Parameter DAQ_MODE_VIPONLY = 12	
      Parameter DAQ_MODE_KILL    = 13	
      Parameter DAQ_MODE_ASWAKE  = 14	
      Parameter DAQ_MODE_TOPVIP  = 15	
C
C	DAQ Inclusion Tags
C	==================
C
      Parameter DAQ_TAG_NORMAL   = 0	
      Parameter DAQ_TAG_VIP	 = 1	
      Parameter DAQ_TAG_BUFMPX   = 2	
C
