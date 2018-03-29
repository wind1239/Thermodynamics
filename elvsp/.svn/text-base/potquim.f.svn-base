C     ******************************************************************
C     ******************************************************************

      SUBROUTINE POTQUIM(N,NAT,COMP,ZPQ,A,B,BS,DPBM,AS,DPAM,IER6,G,ATV)
 
      
C     ******************************************************************
C     ******************************************************************


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'
      
      
      DIMENSION DPAM(ND),DPBM(ND),TERM(ND)
      DIMENSION PPAR(ND),G(ND),DLCF(ND)
      DIMENSION CFUG(ND),COMP(ND),ATV(ND)
      
      
      RZ2 = DSQRT(2.0D0)
      
      
      
C     *******************
C     *******************      
      
      IF(IRM.EQ.1)THEN

C
C     *******************
C     *******************


      ZERRO1 = ZPQ - B
      ZERRO2 = (ZPQ/B + 1.0D0 - RZ2)/(ZPQ/B + 1.0D0 + RZ2)
      IF((ZERRO1.LT.0.0D0).OR.(ZERRO2.LT.0.0D0).OR.
     +         (ZPQ.LT.0.0D0).OR.(ZPQ.GT.1.0D0))THEN
         IER6 = 0
         DO 666 I = 1,N
           G(I) = 0.0D0
 666     CONTINUE
      ELSE
         IER6 = 1
         DO 1000 I = 1,N
       	   DLCF(I) = -DLOG(ZERRO1) + 1.0D0/BS*DPBM(I)*(ZPQ - 1.0D0) 
     +             + 1.0D0/(2.0D0*RZ2)*(AS/(R*Te*BS))*(1.0D0/AS
     +             *DPAM(I) - 1.0D0/BS*DPBM(I))*DLOG(ZERRO2)
     	   CFUG(I) = DEXP(DLCF(I))
     	   PPAR(I) = DLOG(COMP(I)*Pe)
     	   G(I) = R*Te*(DLCF(I) + PPAR(I))
 1000      CONTINUE
      ENDIF
      
      
C     *******************
C     *******************      
      
      ELSE

C
C     *******************
C     *******************

      CALL POTCLAS(N,COMP,ZPQ,A,B,BS,AS,G,ATV)


C     *******************
C     *******************      
      
      ENDIF

C
C     *******************
C     *******************
      IF(NAT.EQ.1)THEN
          CALL ATIV(CFUG,COMP,ATV)
      ELSE
        ATV(1) = 0.0D0
        ATV(2) = 0.0D0
      ENDIF

      ITERP = ITERP + 1

      RETURN
      END

