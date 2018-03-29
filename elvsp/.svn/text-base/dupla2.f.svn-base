      function dsignf(fx)
      double precision dsignf,fx
      save
c
      dsignf=+1.0d0
      if (fx.lt.0.d0) dsignf=-1.0d0
c
      return
      end     
	subroutine Dupla (icase,method,init,coef,zvap,zliq,idivg)
c
      implicit double precision (a-h,o-z)
c
      parameter (ndp=6)
      dimension vx(ndp),fx(ndp),iordon(ndp),coef(4),iqual(8)
      character metnam*6
      common /Dupla1/ idiver,maxit
      common /Dupla2/ dxzero,fxzero
c     common /units/ input,iout,inpot
      common /consts/ xinv3,pi2,pi4
      common /dpoly1/ p01,p11,p21,p31,p02,p12,p22,p32
      common /dpoly2/ c0,c1,c2
      common /niters/ niterl,niterv,niter
      common /metnam/ metnam(8)
      data metnam /'Newton','Richmo','_Lee__','Schnab',
     d             'Schmod','LaPort','Dicoto','Cubica'/
      save
c
c
c
c     constantes
c
      xinv3=1.d0/3.d0
      pi2=dacos(-1.d0)*2.d0*xinv3
      pi4=pi2+pi2
c
c	
c
c	
      c0=coef(1)
      c1=coef(2)
      c2=coef(3)
      B=coef(4)
      B2=B*B
c
c
c
c     coeficientes dos polinomios liq-vap
c
      p01=c0
      p11=c1
      p21=c2
      p31=1.0d0
c
      p02=B
      p12=c2
      p22=c1/B
      p32=c0/B2
c
c     Metodos
c
      if (method.eq.0) then
      do 300 i=1,8
      iqual(i)=1
  300 continue
      else
      do 310 i=1,8
      iqual(i)=0
  310 continue
      iqual(method)=1
      endif
c
      idivg=0
      do 80 method=1,8
      if (iqual(method).eq.0) goto 80
c      write(iout,*) ' Metodo de ',metnam(method)
c
c     metodos com ponto inicial
c
      if (method.le.6) then
      init1=01
      init2=02
      do 25 init=init1,init2
c
c     vapor
c
      if (init.eq.01) then
      cstm1=p21-p11/p21
      z0star=cstm1-(p01/p21)/cstm1
      if (z0star.le.B) then
      z0=dmax1(1.0d0,B)
      else
      z0=z0star
      endif
      itype=01
      else if (init.eq.02) then
c
c     liquido
c
      cstm2=p22/p32-p12/p22
      z0star=cstm2+B/(p22*cstm2)
      z0=dmax1(z0star,1.d0)
      itype=02
      endif
c
      z=z0
      niter=0
   10 continue
      niter=niter+1
      polyz0=dpoly(itype,0,z)
      polyz1=dpoly(itype,1,z)
      goto (01,02,03,04,05) method
c
c     Newton
c
   01 continue
      ratio=polyz0/polyz1
      z=z-ratio
      goto 15
c
c     Richmond
c
   02 continue
      ratio=polyz0/polyz1
      polyz2=dpoly(itype,2,z)*0.5d0
      z=z-polyz0/(polyz1-ratio*polyz2)
      goto 15
c
c     Lee
c
   03 continue
      ratio=polyz0/polyz1
      polyz2=dpoly(itype,2,z)*0.5d0
      z=z-polyz0/(polyz1-ratio*(polyz2-ratio))
      goto 15
c
c     Schnabel
c
   04 continue
      polyz2=dpoly(itype,2,z)
      delta=polyz1*polyz1-2.0d0*polyz0*polyz2
      if (delta.gt.0.d0) then
      z=z-2.0d0*polyz0/(polyz1+dsignf(polyz1)*dsqrt(delta))
      else
      ratio=polyz1/polyz2
      zstar=z-ratio
      fstar=dpoly(itype,0,zstar)+ratio*(dpoly(itype,1,zstar)
     f     +ratio*dpoly(itype,2,zstar)*0.5d0)
      if (dabs(fstar).lt.dabs(polyz0)*0.5d0) then
      z=zstar
      else
      ratio=polyz0/polyz1
      z=z-ratio
      endif
      endif
      goto 15
c
c     Schnabel modificado
c
   05 continue
      if (init.eq.01) then
      fator=p31
      else if (init.eq.02) then
      fator=p32
	endif
      ratio=polyz0/polyz1
      polyz2=dpoly(itype,2,z)-2.0d0*fator*ratio
      delta=polyz1*polyz1-2.0d0*polyz0*polyz2
      if (delta.gt.0.d0) then
      z=z-2.0d0*polyz0/(polyz1+dsignf(polyz1)*dsqrt(delta))
      else
      modif=0
c
c     iteracao de Newton apenas
c
      if (modif.eq.1) then
      z=z-ratio
      else
c
c     verificacao da condicao
c
      ratio=polyz1/polyz2
      zstar=z-ratio
      fstar=dpoly(itype,0,zstar)+ratio*(dpoly(itype,1,zstar)
     f     +ratio*dpoly(itype,2,zstar)*0.5d0)
      if (dabs(fstar).lt.dabs(polyz0)*0.5d0) then
      z=zstar
	 else
      z=z-polyz0/(polyz1-ratio*(polyz2*0.5d0-ratio*fator))
      endif
      endif
      endif
   15 continue
c
c     criterios de parada
c
      iconvg=0
      if (dabs(z-z0)/dmax1(dabs(z0),1.d0).le.dxzero) iconvg=1
      if (dabs(polyz0).le.fxzero) iconvg=iconvg+1
c
c     ainda nao convergiu por nenhum dos criterios
c
      if (iconvg.lt.1) then
      if (niter.lt.idiver) then
      z0=z
      goto 10
      else
 	 idivg=idivg+1
c
c     proximo metodo
c
      goto 80
      endif
      endif
c
c     casos nao-teste
c
c     liq/vap
c
      if (init.eq.01) then
      zbig=z
      else
      zbig=B/z
      endif
c
c     liquido
c
      if (init.eq.02) then
      niterl=niter
 	zbiglq=zbig
      else
c
c     vapor
c
      niterv=niter
      zbigvp=zbig
      endif
   25 continue
c
      zliq=zbiglq
      zvap=zbigvp
      else
c
c     metodos com intervalo inicial
c
      if (method.le.7) then
c
c     nao-teste
c
      itype=01
      teta=dmax1(1.0d0,dabs(c0),dabs(c1),dabs(c2))
      S=1.0d0+teta
c
      za=-S
      zb=S
c
      fa=dpoly(itype,0,za)
      fb=dpoly(itype,0,zb)
c
c     verificando se f(a).f(b)<0
c
      if (fa*fb.gt.0.d0) then
      idivg=idivg+1
c
c     proximo metodo
c
      goto 80
      endif
c
c     metodo de laporte
c
      if (method.eq.06) then
c
      vx(1)=9999.d0
      fx(1)=0.d0
c
      vx(2)=za
      fx(2)=fa
c
      vx(3)=zb
      fx(3)=fb
c
      z0=(za*fb-zb*fa)/(fb-fa)
      f0=dpoly(itype,0,z0)
      vx(4)=z0
      fx(4)=f0
      np=4
c
      z=z0
      niter=0
   30 continue
      niter=niter+1
c
c     ordenar fx em ordem crescente
c
      call dordone (01,np,fx,iordon)
      call dordone (02,np,vx,iordon)
c
c     encontrar numero de fx's positivos e negativos
c
      do 40 i=1,np
      if (fx(i).eq.0.d0) then
      izer=i
      goto 45
      endif
   40 continue
   45 continue
      ineg=max0(1,izer-2)
      ipos=min0(np,izer+2)
	 nbneg=izer-ineg
      nbpos=ipos-izer
c
c     limitar vetores vx e fx a 5 valores no maximo
c
      np=0
      do 65 i=ineg,ipos
      np=np+1
      vx(np)=vx(i)
      fx(np)=fx(i)
   65 continue
c
      if (nbneg.eq.1) then
      izer=2
      else
      izer=3
      endif
c
      icaso=nbneg+2*nbpos-3
c
      if ((icaso.eq.01).or.(icaso.eq.03)) then
      z1=vx(izer-2)
      z2=vx(izer-1)
      z3=vx(izer+1)
      f1=fx(izer-2)
      f2=fx(izer-1)
      f3=fx(izer+1)
      zq=dzpq(itype,01,z1,z2,z3,f1,f2,f3)
	 q=1.d0/dabs(zq-z1)
      else
      zq=0.d0
      q=0.d0
      endif
c
      if ((icaso.eq.02).or.(icaso.eq.03)) then
      z1=vx(izer+2)
      z2=vx(izer+1)
      z3=vx(izer-1)
      f1=fx(izer+2)
      f2=fx(izer+1)
      f3=fx(izer-1)
      zp=dzpq(itype,02,z1,z2,z3,f1,f2,f3)
      p=1.d0/dabs(zp-z1)
      else
      zp=0.d0
      p=0.d0
      endif
c
      z=(p*zp+q*zq)/(p+q)
      fz=dpoly(itype,0,z)
      np=np+1
      vx(np)=z
      fx(np)=fz
c
c     continua iteracoes de LaPorte ou
c
c     criterios de parada
c
      iconvg=0
      if (dabs(z-z0)/dmax1(dabs(z0),1.d0).le.dxzero) iconvg=1
      if (dabs(fx(np)).le.fxzero) iconvg=iconvg+1
c
c     ainda nao convergiu por nenhum dos criterios
c
      if (iconvg.lt.1) then
      if (niter.le.maxit) then
      z0=z
      goto 30
      else
c
c     senao pula p/ a dicotomia
c
      za=vx(izer-1)
      fa=fx(izer-1)
      zb=vx(izer+1)
      fb=fx(izer+1)
c
      call dbissec (itype,za,fa,zb,fb,z0,f0)
c
      z=z0
  160 continue
      niter=niter+1
c
c     nova bissecao
c
      call dbissec (itype,za,fa,zb,fb,z,fz)
c
c     criterios de parada
c
      iconvg=0 
      if (dabs(z-z0)/dmax1(dabs(z0),1.d0).le.dxzero) iconvg=1
      if (dabs(fz).le.fxzero) iconvg=iconvg+1
c
c     ainda nao convergiu por nenhum dos criterios
c
      if (iconvg.lt.1) then
      z0=z
      goto 160
      endif
c
c     convergiu na dicotomia
c
      np=1
      fx(1)=fz
      endif
      endif
c
c     convergiu em LaPorte
c
      np=1
      fx(1)=fz
      else
c
c     metodo da dicotomia
c
      call dbissec (itype,za,fa,zb,fb,z0,f0)
c
      z=z0
      niter=0
   60 continue
      niter=niter+1
c
c     nova bissecao
c
      call dbissec (itype,za,fa,zb,fb,z,fz)
c
c     criterios de parada
c
      iconvg=0 
      if (dabs(z-z0)/dmax1(dabs(z0),1.d0).le.dxzero) iconvg=1
      if (dabs(fz).le.fxzero) iconvg=iconvg+1
c
c     ainda nao convergiu por nenhum dos criterios
c
      if (iconvg.lt.1) then
      z0=z
      goto 60
      endif
c
c     convergiu na dicotomia
c
      np=1
      fx(1)=fz
      endif
c
c     nao teste
c
      zbig1=z
c
c     encontrar demais raizes por radicais
c
      if (zbig1.eq.0.d0) then
      beta1=c2
      beta0=c1
      else
      beta1=c2-zbig1
      beta0=c0/zbig1
      endif
c
      determ=beta1*beta1-4.d0*beta0
      if (determ.lt.0.d0) then
      niterl=niter
      zbiglq=zbig1
      niterv=niter
      zbigvp=zbig1
      else
      determ=dsqrt(determ)
c
      if (beta1.lt.0.d0) then
      zbig2=(beta1-determ)*0.5
      else
      zbig2=(beta1+determ)*0.5
      endif
      zbig3=beta0/zbig2
c
c     ordenando as raizes encontradas
c
      nvx=3
      vx(1)=zbig1
      vx(2)=zbig2
      vx(3)=zbig3
c
      call dordone (01,nvx,vx,iordon)
c
      zmin=B
      if (vx(1).lt.zmin) then
      if (vx(2).lt.zmin) then
      zliq=vx(3)
      else
      zliq=vx(2)
      endif
      else
      zliq=vx(1)
      endif
      zvap=vx(3)
c
c     erros provaveis
c
      if ((zliq.lt.0.d0).or.(zvap.lt.0.d0)) then
      idivg=idivg+1
c
c     proximo metodo
c
      goto 80
      endif
c  
      niterl=niter
      zbiglq=zliq
      niterv=niter
      zbigvp=zvap
      endif
      else
c
c     metodo exato : resolucao algebrica da equacao cubica
c
      call cubica (zbig1,zbig2,zbig3,icode)
      niter=0
c
c     ordenando as raizes encontradas
c
      nvx=3
      vx(1)=zbig1
      vx(2)=zbig2
      vx(3)=zbig3
c
      call dordone (01,nvx,vx,iordon)
c
      zmin=B
      if (vx(1).lt.zmin) then
      if (vx(2).lt.zmin) then
      zliq=vx(3)
      else
      zliq=vx(2)
      endif
      else 
      zliq=vx(1)
      endif
      zvap=vx(3)
c
c     erros provaveis
c
      if ((zliq.lt.0.d0).or.(zvap.lt.0.d0)) then
      idivg=idivg+1
c
c     proximo metodo
c
      goto 80
      endif
c  
      niterl=niter
      zbiglq=zliq
      niterv=niter
      zbigvp=zvap
      endif
      endif
c
c     raizes
c
       
   80 continue
c	
C      OPEN(14,FILE='CUBICA.OUT')
C      write(14,*) 'zliq =',zbiglq,' niterl ',niterl
C       write(14,*) 'zvap =',zbigvp,' niterv ',niterv
C       write(14,*)'Fim do ciclo'
      return
      end
c
c
c
      subroutine cubica (zbig1,zbig2,zbig3,icode)
c
      implicit double precision (a-h,o-z)
c
      common /units/ input,iout,inpot
      common /consts/ xinv3,pi2,pi4
      common /dpoly2/ c0,c1,c2
      save
c
      p=c2*xinv3
      p2=p*p
      q=c1*xinv3-p2
      q2=q*q
      r=(c0-c1*p)*0.5d0+p2*p
      r2=r*r
c
      zero=1.d-32
      delta=q*q2+r2
      absdel=dabs(delta)
c
      if (absdel.gt.zero) then
c
c     tres raizes reais diferentes
c
      if (delta.lt.0.d0) then
      icode=03
      sqrtq=dsqrt(-q)
      teta=dacos(r/(q*sqrtq))*xinv3
      sqrtq2=-(sqrtq+sqrtq)
      zbig1=sqrtq2*dcos(teta)+p
      zbig2=sqrtq2*dcos(teta+pi2)+p
      zbig3=sqrtq2*dcos(teta+pi4)+p
      else
c
c     uma soh raiz real
c
      icode=01
      absdel=dsqrt(absdel)
      s1=(r+absdel)
      if (s1.lt.0.d0) then
      s1=-(-s1)**xinv3
      else
      s1=s1**xinv3
      endif
      s2=(r-absdel)
      if (s2.lt.0.d0) then
      s2=-(-s2)**xinv3
      else
      s2=s2**xinv3
      endif
      zbig1=s1+s2+p
      zbig2=0.d0
      zbig3=0.d0
      endif
      else
c
c     tres raizes reais sendo duas iguais
c
	 icode=02
      alfa=r
      if (alfa.lt.0.d0) then
      alfa=-(-alfa)**xinv3
      else
      alfa=alfa**xinv3
      endif
      zbig1=alfa+alfa+p
      zbig2=p-alfa
      zbig3=zbig2
      endif
c
      return
      end
c
c
c
      function dpoly(itype,icase,z)
c
      implicit double precision (a-h,o-z)
c
      common /dpoly1/ p01,p11,p21,p31,p02,p12,p22,p32
      save
c
      goto (11,12) itype
c      print*,' erro! -> itype'
c
c     polinomio vapor

c
   11 continue
      goto (31,32,33) icase+1
   31 continue
      dpoly=p31
      dpoly=dpoly*z-p21
      dpoly=dpoly*z+p11
      dpoly=dpoly*z-p01
      return
   32 continue
      dpoly=3.d0*p31
      dpoly=dpoly*z-2.d0*p21
      dpoly=dpoly*z+p11
      return
   33 continue
      dpoly=6.d0*p31
      dpoly=dpoly*z-2.d0*p21
      return
c
c     polinomio liquido
c
   12 continue
      goto (41,42,43) icase+1
   41 continue
      dpoly=p32
      dpoly=dpoly*z-p22
      dpoly=dpoly*z+p12
      dpoly=dpoly*z-p02
      return
   42 continue
      dpoly=3.d0*p32

      dpoly=dpoly*z-2.d0*p22
      dpoly=dpoly*z+p12
      return
   43 continue
      dpoly=6.d0*p32
      dpoly=dpoly*z-2.d0*p22
      return
      end
c
c
c
      function dzpq(itype,icase,z1,z2,z3,f1,f2,f3)
c
      implicit double precision (a-h,o-z)
      common /units/ input,iout,inpot
      save
c
      z32=z3-z2
      z13=z1-z3
      z12=z1-z2
c
      alfa=z32*(f1*z32+f2*z13-f3*z12)/(z12*z13)
      beta=(-f1*z32*z32-f2*z13*(z12+z32)+f3*z12*z12)/(z12*z13)
      gama=f2
c
      signe=dsignf(-beta)
      v1=(dabs(beta)+dsqrt(beta*beta-4.d0*alfa*gama))*0.5d0*signe/alfa
      v2=gama/(alfa*v1)
      if ((v1.ge.0.0d0).and.(v1.le.1.0d0)) then
      dzpq=z2+v1*z32
      else
      if ((v2.ge.0.0d0).and.(v2.le.1.0d0)) then
      dzpq=z2+v2*z32
      else
c      write(iout,*) ' nenhuma das raizes no intervalo [0,1].'
      if (icase.eq.01) then
      xa=z1
      fa=f1
      xb=z2
      fb=f2
      else
      xa=z2
      fa=f2
      xb=z3
      fb=f3
      endif
c
      call dbissec (itype,xa,fa,xb,fb,xab,fab)
c
      dzpq=xab
      endif
      endif
c
      return
      end
c
c
c
      subroutine dbissec (itype,xa,fa,xb,fb,xab,fab)
c
      implicit double precision (a-h,o-z)
      save
c
      xab=(xa+xb)*0.5
      fab=dpoly(itype,0,xab)
c
      if (fab*fa.lt.0.) then
      xb=xab
      fb=fab
      else
      xa=xab
      fa=fab
      endif
c
      return
      end
c
c
c
      subroutine dordone (itype,ndim,vect,iordon)
c
      implicit double precision (a-h,o-z)
c
      dimension vect(ndim),iordon(ndim),aux(2000)
      save
c
      goto (01,02) itype
c
c     ordonner
c
   01 continue
c
      do 10 i=1,ndim
      iordon(i)=i
   10 continue
	if (ndim.gt.1) then
      do 30 i=1,ndim-1
      vmin=vect(i)
      imin=i
c
      do 20 j=i+1,ndim
      if (vect(j).lt.vmin) then
      vmin=vect(j)
      imin=j
      endif
   20 continue
c
      if (imin.ne.i) then
      vecti=vect(i)
      vect(i)=vect(imin)
      vect(imin)=vecti
c
      jordon=iordon(i)
 
      iordon(i)=iordon(imin)
      iordon(imin)=jordon
      endif
   30 continue
      endif
      return
c
c     re-ordonner
c
   02 continue
      if (ndim.gt.2000) stop ' ordone memory > 2000 '
c
      do 40 i=1,ndim
      aux(i)=vect(iordon(i))
   40 continue
c
      do 50 i=1,ndim
      vect(i)=aux(i)
   50 continue
      return
      end
