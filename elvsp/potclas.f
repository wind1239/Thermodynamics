C     ******************************************************************
C     ******************************************************************

      SUBROUTINE POTCLAS(N,COMP,ZPQ,A,B,BS,AS,G,ATV)
 
      
C     ******************************************************************
C     ******************************************************************


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'
      
      
      DIMENSION TERM(ND)
      DIMENSION PPAR(ND),G(ND),DLCF(ND)
      DIMENSION CFUG(ND),COMP(ND),ATV(ND)
      
      
      RZ2 = DSQRT(2.0D0)
      TERM(1)=0.0D0
      TERM(2)=0.0D0
      
      WRITE(*,*)'BI(1) :',BI(1)
      WRITE(*,*)'BI(2) :',BI(2)
      WRITE(*,*)'AI(1) :',AI(1)
      WRITE(*,*)'AI(2) :',AI(2)
      WRITE(*,*)'DELT(1,1) :',DELT(1,1)
      WRITE(*,*)'DELT(1,2) :',DELT(1,2)
      WRITE(*,*)'DELT(2,1) :',DELT(1,2)
      WRITE(*,*)'DELT(2,2) :',DELT(2,2)
      WRITE(*,*)'ZPQ :',ZPQ
      WRITE(*,*)'A :',A
      WRITE(*,*)'B :',B
      WRITE(*,*)'AS :',AS
      WRITE(*,*)'BS :',BS
      
      DO 200 L=1,N
      write(*,*)'vamos ver qual eh o barato ???'
        DO 100 M=1,N
          TERM(L)=COMP(M)*DSQRT(AI(M)*AI(L))*(1.0D0-DELT(M,L))+
     &     TERM(L)
 100    CONTINUE
        DLCF(L)=BI(L)/BS*(ZPQ-1.0D0)-DLOG(ZPQ-B)-A/
     &        (2.0D0*RZ2*B)*(2*TERM(L)/AS-BI(L)/BS)*
     &        DLOG((ZPQ+2.414*B)/(ZPQ-0.414*B))
    	CFUG(L) = DEXP(DLCF(L))
     	PPAR(L) = DLOG(COMP(L)*Pe)
     	G(L) = R*Te*(DLCF(L) + PPAR(L))
 200  CONTINUE
 
      RETURN 
      END