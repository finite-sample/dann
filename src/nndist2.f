C Output from Public domain Ratfor, version 1.01
      subroutine nndist2(n,p,class,kkk,rem,x,x0,cv,y,fullsc,which, dist,
     *metric,covw,means,weight,values,vectors,scramat,scratch,xnew,ynew,
     *kback)
      implicit double precision (a-h,o-z)
      integer n, p, class, kkk(3), which(n), y(n), ynew(n),iter, neps, c
     *v,kback(2)
      double precision x(n,p), xnew(n,p),weight(n), means(class,p),dist(
     *n),x0(p),covw(p,p), values(p),vectors(p,p),scramat(p,p),scratch(1)
     *,rate,metric(p,p)
      double precision epsilon, wmax ,cvmax,rem(3)
      integer niter,k
      logical fullsc(2),fullw,singul,scalar
      double precision covmin
      scalar=.false.
      kmetric=kkk(1)
      ktarget=kkk(2)
      iter=kkk(3)
      fullw=fullsc(1)
      scalar=fullsc(2)
      rate=rem(1)
      epsilon=rem(2)
      covmin=rem(3)
      cvmax=1d16
      if(cv.gt.0)then
      do23002 j=1,p 
      x0(j)=x(cv,j)
23002 continue
23003 continue
      endif
      kold=n
      knew=kmetric
      do23004 i=1,n 
      which(i)=i
23004 continue
23005 continue
      do23006 i=1,p
      j=i
23008 if(.not.(j.le.p))goto 23010
      metric(i,j)=0d0
23009 j=j+1
      goto 23008
23010 continue
      metric(i,i)=1d0
23006 continue
23007 continue
      do23011 i=1,n
      weight(i)=0d0
      do23013 j=1,p
      weight(i)=weight(i)+(x(i,j)-x0(j))**2
23013 continue
23014 continue
23011 continue
23012 continue
      if(cv.gt.0)then
      weight(cv)=cvmax
      endif
      niter=iter
      singul=.false.
23017 if((niter.gt.0).and.(knew.gt.ktarget).and..not.singul)then
      call sortdi(weight,n,which,1,kold)
      wmax=weight(knew)
      if(wmax .le. 0d0)then
      do23021 i=1,knew
      weight(i)=1d0
23021 continue
23022 continue
      else
      do23023 i=1,knew
      dd=sqrt(weight(i)/wmax)
      weight(i)=(1d0-dd**3)**3
23023 continue
23024 continue
      endif
      do23025 i=1,kold
      do23027 j=1,p
      xx=0d0
      k=j
23029 if(.not.(k.le.p))goto 23031
      xx=xx+metric(j,k)*(x(which(i),k)-x0(k))
23030 k=k+1
      goto 23029
23031 continue
      xnew(i,j)=xx
23027 continue
23028 continue
      ynew(i)=y(which(i))
23025 continue
23026 continue
      call withmean2(n,p,class,xnew,ynew,knew,weight ,covw,fullw,scalar,
     *singul, means,scratch,dist)
      if(scalar)then
      fullw=.false.
      endif
      if(.not.fullw)then
      do23036 j=1,p
      if(covw(j,j).lt.covmin)then
      covw(j,j)=covmin
      endif
23036 continue
23037 continue
      do23040 j=1,p
      do23042 jj=1,p
      tt=0d0
      do23044 k=1,class
      tt=tt+means(k,j)*means(k,jj)
23044 continue
23045 continue
      scramat(j,jj)=tt/(covw(j,j)*covw(jj,jj))
23042 continue
23043 continue
      scramat(j,j)=scramat(j,j)+epsilon/covw(j,j)
23040 continue
23041 continue
      call chol(scramat,p,scratch,0,0,irank)
      do23046 j=1,p 
      jj=j
23048 if(.not.(jj.le.p))goto 23050
      tt=0d0
      k=j
23051 if(.not.(k.le.jj))goto 23053
      tt=scramat(j,k)*metric(k,jj)+tt
23052 k=k+1
      goto 23051
23053 continue
      metric(j,jj)=tt
23049 jj=jj+1
      goto 23048
23050 continue
23046 continue
23047 continue
      do23054 i=1,kold
      dist(i)=0d0

      do23056 j=1,p
      dist(i)=dist(i)+ (xnew(i,j)**2)/covw(j,j)
23056 continue
23057 continue
      do23058 k=1,class
      scratch(k)=0d0
23058 continue
23059 continue
      do23060 j=1,p
      dd=xnew(i,j) /covw(j,j)
      do23062 k=1,class
      scratch(k)=scratch(k)+means(k,j)*dd
23062 continue
23063 continue
23060 continue
23061 continue
      weight(i)=0d0
      do23064 k=1,class
      weight(i)=weight(i)+scratch(k)**2
23064 continue
23065 continue
      weight(i)=weight(i)+epsilon*dist(i)
23054 continue
23055 continue
      else
      imatz=1
      call rs(p,p,covw,values,imatz,vectors,scratch(1),scratch(p+1),ierr
     *)
      do23066 j=1,p
      if(values(j).lt.covmin)then
      values(j)=covmin
      endif
23066 continue
23067 continue
      do23070 i=1,kold
      dist(i)=0d0

      do23072 j=1,p
      dutx=0d0
      do23074 k=1,p
      dutx=dutx+vectors(k,j)*xnew(i,k)
23074 continue
23075 continue
      dist(i)=dist(i)+dutx*dutx/values(j)
23072 continue
23073 continue
23070 continue
23071 continue
      do23076 j=1,p
      do23078 k=1,p
      dd=0d0
      do23080 jj=1,p
      dd=dd+vectors(j,jj)*vectors(k,jj)/values(jj)
23080 continue
23081 continue
      scramat(j,k)=dd
23078 continue
23079 continue
23076 continue
23077 continue
      do23082 k=1,class
      do23084 j=1,p
      dd=0d0
      do23086 jj=1,p
      dd=dd+means(k,jj)*scramat(jj,j)
23086 continue
23087 continue
      vectors(k,j)=dd
23084 continue
23085 continue
23082 continue
23083 continue
      do23088 i=1,n
      do23090 k=1,class
      scratch(k)=0d0
23090 continue
23091 continue
      do23092 j=1,p
      dd=(x(i,j)-x0(j))
      do23094 k=1,class
      scratch(k)=scratch(k)+vectors(k,j)*dd
23094 continue
23095 continue
23092 continue
23093 continue
      weight(i)=0d0
      do23096 k=1,class
      weight(i)=weight(i)+scratch(k)**2
23096 continue
23097 continue
      weight(i)=weight(i)+epsilon*dist(i)
23088 continue
23089 continue
      endif
      niter=niter-1
      if(rate.lt.1)then
      kold=knew
      knew=kold*rate
      endif
      kback(1)=iter-niter
      kback(2)=kold
      goto 23017
      endif
23018 continue
      return
      end
