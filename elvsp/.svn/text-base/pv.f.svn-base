C     PROGRAMA TESTE PARA O CALCULO DA PRESSAO


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      WRITE(*,*)'T (K) : '
      READ(*,*)T   

      WRITE(*,*)'ATIV : '
      READ(*,*)ATIV   
    

      TC = 562.2D0
      PC = 4890.0D0
      W = 0.212D0
      R = 8.314D-3
      TR = T/TC
      TT = 1.0D0 - TR
      A = -6.98273D0
      B = 1.33213D0
      C = -2.62863D0
      D = -3.33399D0
      MAX = 1000000000
      P1E = 50.0D0
C     ATIV = 0.8734D0

C     COEFICIENTE DE VIRIAL :

      B0 = 8.3D-2-4.22D-1/(TR**1.6)
      B1 = 1.39D-1-1.72D-1/(TR**4.2)
      
      BV = R*TC/PC*(B0 + W*B1)
      WRITE(*,*)'B : ',BV

C     PRESSAO DE VAPOR - METODO DE WAGNER

      WRITE(*,*)'TR : ',TR
      RZ = A*TT + B*(TT**1.5) + C*(TT**3) + D*(TT**6)
      RZ2 = RZ/TR
      PV = PC*DEXP(RZ2)
      WRITE(*,*)'TAU : ',TT
      WRITE(*,*)'SOMATORIO : ', RZ
      WRITE(*,*)'PV : ',PV,' KPa'

C     ATIVIDADE (NEWTON-RAPHSON)

      DO 100 I = 1,MAX
        F = P1E*DEXP(-BV/(R*T)*(PV-P1E))-ATIV*PV
        WRITE(*,*)'F : ',F
        FL = 1.0D0/PV*DEXP(-B/(R*T)*(PV-P1E))+P1E/PV*
     #       DEXP(-B/(R*T)*(PV-P1E))*B/(R*T)
        WRITE(*,*)'FL : ',FL
        P1 = P1E + F/FL
        EPS = (P1-P1E)/P1E
        IF(DABS(EPS).GT.1.0D-5)THEN
          P1E = P1
        ELSE
          WRITE(*,*)'INTR : ',I
          WRITE(*,*)'P1 : ',P1
          STOP
        ENDIF
 100  CONTINUE


      END
