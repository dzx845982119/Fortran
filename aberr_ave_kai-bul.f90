        program main

        real*4 g601(61,21),g1201(61,21),g2401(61,21),g5m(61,21),g30m(61,21),a(21),b(21),c(21)
        real*4 ave6_1(61,21),ave6_2(61,21),ave6_5(61,21),ave6_3(61,21),ave1_2(61,21)
        real*4 ave1_5(61,21),ave1_3(61,21),ave2_5(61,21),ave2_3(61,21),ave5_3(61,21)

        integer h,i,j,k
        character*32 d601,d1201,d2401,d5m,d30m,output
        !  ### input data ###
          print *,'Input 601deta file name?'
          read(5,'(a32)')d601
          write(*,*)d601
          open(10,file=d601,status='unknown')
          do i=1,61
            h=0
            do j=1,21
              h=h+1
              read (10, *) a(j),b(j),c(j)
              if (j>1.and.j<=21.and.c(j)==0) then
              a=0.
              h=h-1 !h=16
                do k=(21-h),21 !k=5,21
                  a(k)=c(h+k-21) !a(5)=c(1)
                end do
                do k=1,21
                  g601(i,k)=a(k)
                  write(*,*)'test:',g601(i,k)
                end do
                go to 25
              end if
            end do
  25        write(*,*)'====================',i      
          end do
          close(10)
  
          print *,'Input 1201deta file name?'
          read(5,'(a32)')d1201
          write(*,*)d1201
          open(20,file=d1201,status='unknown')
          do i=1,61
            h=0
            do j=1,21
              h=h+1
              read (20, *) a(j),b(j),c(j)
              if (j>1.and.j<=21.and.c(j)==0) then
              a=0.
              h=h-1 !h=16
                do k=(21-h),21 !k=5,21
                  a(k)=c(h+k-21) !a(5)=c(1)
                end do
                do k=1,21
                  g1201(i,k)=a(k)
                  write(*,*)'test:',g1201(i,k)
                end do
                go to 26
              end if
            end do
  26        write(*,*)'====================',i      
          end do
          close(20)
  
          print *,'Input 2401deta file name?'
          read(5,'(a32)')d2401
          write(*,*)d2401
          open(30,file=d2401,status='unknown')
          do i=1,61
            h=0
            do j=1,21
              h=h+1
              read (30, *) a(j),b(j),c(j)
              if (j>1.and.j<=21.and.c(j)==0) then
              a=0.
              h=h-1 !h=16
                do k=(21-h),21 !k=5,21
                  a(k)=c(h+k-21) !a(5)=c(1)
                end do
                do k=1,21
                  g2401(i,k)=a(k)
                  write(*,*)'test:',g2401(i,k)
                end do
                go to 27
              end if
            end do
  27        write(*,*)'====================',i      
          end do
          close(30)

          print *,'Input d5mdeta file name?'
          read(5,'(a32)')d5m
          write(*,*)d5m
          open(40,file=d5m,status='unknown')
          do i=1,61
            h=0
            do j=1,21
              h=h+1
              read (40, *) a(j),b(j),c(j)
              if (j>1.and.j<=21.and.c(j)==0) then
              a=0.
              h=h-1 !h=16
                do k=(21-h),21 !k=5,21
                  a(k)=c(h+k-21) !a(5)=c(1)
                end do
                do k=1,21
                  g5m(i,k)=a(k)
                  write(*,*)'test:',g5m(i,k)
                end do
                go to 28
              end if
            end do
  28        write(*,*)'====================',i      
          end do
          close(40)

          print *,'Input d30mdeta file name?'
          read(5,'(a32)')d30m
          write(*,*)d30m
          open(50,file=d30m,status='unknown')
          do i=1,61
            h=0
            do j=1,21
              h=h+1
              read (50, *) a(j),b(j),c(j)
              if (j>1.and.j<=21.and.c(j)==0) then
              a=0.
              h=h-1 !h=16
                do k=(21-h),21 !k=5,21
                  a(k)=c(h+k-21) !a(5)=c(1)
                end do
                do k=1,21
                  g30m(i,k)=a(k)
                  write(*,*)'test:',g30m(i,k)
                end do
                go to 29
              end if
            end do
  29        write(*,*)'====================',i      
          end do
          close(50)

        do i=1,61
          write (*, *)'----------'
          do j=1,21
            ave6_1(i,j)=abs(g601(i,j)-g1201(i,j))
            ave6_2(i,j)=abs(g601(i,j)-g2401(i,j)) !
            ave6_5(i,j)=abs(g601(i,j)-g5m(i,j))
            ave6_3(i,j)=abs(g601(i,j)-g30m(i,j))
            ave1_2(i,j)=abs(g1201(i,j)-g2401(i,j))
            ave1_5(i,j)=abs(g1201(i,j)-g5m(i,j))
            ave1_3(i,j)=abs(g1201(i,j)-g30m(i,j))
            ave2_5(i,j)=abs(g2401(i,j)-g5m(i,j))
            ave2_3(i,j)=abs(g2401(i,j)-g30m(i,j))
            ave5_3(i,j)=abs(g5m(i,j)-g30m(i,j))
            write (*, *)ave6_5(i,j)
          end do
        end do

        !output
        print *,'Input outputeta file name?'
        read(5,'(a32)')output
        write(*,*)output
        open(60,file=output,status='unknown')
        do i=1,61
          do j=1,21   
            write(*,fmt='(f5.2)',advance='no')0.04*(i-1)-0.8
            write(*,fmt='(f5.2)',advance='no')0.5+(0.01*(j-1))
            write(*,*)(ave6_1(i,j)+ave6_2(i,j)+ave6_5(i,j)+ave6_3(i,j)+ave1_2(i,j) &
   &                  +ave1_5(i,j)+ave1_3(i,j)+ave2_5(i,j)+ave2_3(i,j)+ave5_3(i,j))/10

            write(60,*)0.04*(i-1)-0.8,',',0.5+(0.01*(j-1)),',', &
   &                  (ave6_1(i,j)+ave6_2(i,j)+ave6_5(i,j)+ave6_3(i,j)+ave1_2(i,j) &
   &                  +ave1_5(i,j)+ave1_3(i,j)+ave2_5(i,j)+ave2_3(i,j)+ave5_3(i,j))/10
          end do
          write(60,*)' '
        end do
        close(10)
        close(60)

        end program main