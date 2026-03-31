C Output from Public domain Ratfor, version 1.01
      subroutine withmean2(n,p,class,x,y,k,weight,which,covw,fullw,
     *scalar,singul,means,sumw,tmean)
      implicit double precision (a-h,o-z)
      integer n, p, class, k, which(k), y(n)
      double precision x(n,p), weight(k), means(class,p), sumw(class),
     *tmean(p), covw(p,p)
      logical singul,fullw,scalar
      do23000 ic=1,class
      do23002 ip=1,p
      means(ic,ip)=0d0
23002 continue
23003 continue
      sumw(ic)=0d0
23000 continue
23001 continue
      do23004 i=1,k
      ii=which(i)
      ic=y(ii)
      sumw(ic)=sumw(ic)+weight(i)
      do23006 ip=1,p
      means(ic,ip)=means(ic,ip)+weight(i)*x(ii,ip)
23006 continue
23007 continue
23004 continue
23005 continue
      tsumw=0d0
      nonnul=0
      do23008 ip=1,p
      tmean(ip)=0d0
23008 continue
23009 continue
      do23010 ic=1,class 
      tsumw=tsumw+sumw(ic)
      if(sumw(ic).gt.0d0)then
      nonnul=nonnul+1
      endif
      do23014 ip=1,p 
      tmean(ip)=tmean(ip)+means(ic,ip)
23014 continue
23015 continue
23010 continue
23011 continue
      do23016 ip =1,p 
      tmean(ip)=tmean(ip)/tsumw
      do23018 ic=1,class
      if(sumw(ic).gt.0d0)then
      means(ic,ip)=means(ic,ip)/sumw(ic)
      endif
23018 continue
23019 continue
23016 continue
23017 continue
      singul=.true.
      if(nonnul.gt.1)then
      singul=.false.
      endif
      if(fullw)then
      do23026 ip1=1,p
      do23028 ip2=1,p
      covw(ip1,ip2)=0d0
      do23030 i=1,k
      ii=which(i)
      covw(ip1,ip2) =covw(ip1,ip2) +weight(i)*(x(ii,ip1)-means(y(ii),ip1
     *))*(x(ii,ip2)-means(y(ii),ip2))/tsumw
23030 continue
23031 continue
23028 continue
23029 continue
23026 continue
23027 continue
      else
      do23032 ip1=1,p
      ip2=ip1
      covw(ip1,ip2)=0d0
      do23034 i=1,k
      ii=which(i)
      covw(ip1,ip2) =covw(ip1,ip2) +weight(i)*(x(ii,ip1)-means(y(ii),ip1
     *))*(x(ii,ip2)-means(y(ii),ip2))/tsumw
23034 continue
23035 continue
23032 continue
23033 continue
      endif
      if(scalar)then
      ss=0d0
      do23038 j=1,p
      ss=ss+covw(j,j)
23038 continue
23039 continue
      ss=ss/dble(p)
      do23040 j=1,p
      covw(j,j)=ss
23040 continue
23041 continue
      endif
      do23042 j=1,p
      do23044 i=1,class
      means(i,j)= dsqrt(sumw(i)/tsumw)*(means(i,j)-tmean(j))
23044 continue
23045 continue
23042 continue
23043 continue
      return
      end
