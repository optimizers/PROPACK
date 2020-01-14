c
c     (C) Rasmus Munk Larsen, Stanford University, 2000,2004
c
      subroutine ssafescal(n,alpha,x)
c
c     Scale the vector x by 1/alpha avoiding unnecessary under- and overflow.
c

c     %-----------%
c     | Arguments |
c     %-----------%
      implicit none
      integer n
      real alpha, x(*)

c     %------------%
c     | Parameters |
c     %------------%
      real one, zero
      parameter(one = 1.0, zero = 0.0)

c     %-----------------%
c     | Local variables |
c     %-----------------%
      integer i,info
      real sfmin

c     %----------------------%
c     | External Subroutines |
c     %----------------------%
      external psscal,slascl_64

c     %--------------------%
c     | External Functions |
c     %--------------------%
      real slamch_64
      external slamch_64

c     %-----------------%
c     | Data statements |
c     %-----------------%
      save
      data sfmin /-1.0/
      
      if (sfmin.eq.-1.0) then         
         sfmin = slamch_64('s')
      endif

      if (abs(alpha).ge.sfmin) then
         call psscal(n,one/alpha, x, 1)
      else
         call slascl_64('General',i,i,alpha,one,n,1,x,n,info)
      endif
      
      end
