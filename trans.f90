program enshu1
  implicit none
  character(len=80), dimension(5):: name
  character(len=1), dimension(5,3000):: seq
  integer:: nseq, i, j
  integer, dimension(5):: nlen
  character(len=1), dimension(5,1000):: amino
  character(len=1), dimension(64):: codon
  integer, dimension(5):: ncodon
  integer, dimension(3):: id
  integer:: k

  data codon/ 'F', 'F', 'L', 'L', 'S', 'S', 'S', 'S', 'Y', 'Y', '.', '.', 'C', 'C', '.', 'W',&
  'L', 'L', 'L', 'L', 'P', 'P', 'P', 'P', 'H', 'H', 'Q', 'Q', 'R', 'R', 'R', 'R', 'I', 'I', 'I', 'M',&
  'T', 'T', 'T', 'T', 'N', 'N', 'K', 'K', 'S', 'S', 'R', 'R', 'V', 'V', 'V', 'V', 'A', 'A', 'A', 'A',&
  'D', 'D', 'E', 'E', 'G', 'G', 'G', 'G'/
  read(*, '(i5)') nseq
  do i=1, nseq
    read(*, '(a80)') name(i)
    read(*, '(i5)') nlen(i)
    read(*, '(60a1)') (seq(i,j), j=1, nlen(i))
  end do
  do i=1, nseq
    ncodon(i) = nlen(i)/3
    do j=1, ncodon(i)
      do k=1,3
        if (seq(i, (j-1)*3+k)== 'T') then
          id(k) =0
        else if (seq(i, (j-1)*3+k)== 'C') then
          id(k)=1
        else if (seq(i, (j-1)*3+k)== 'A') then
          id(k)=2
        else
          id(k)=3
        end if
      end do
      amino(i, j) = codon(id(1)*16 + id(2)*4 +id(3) +1)
    end do
  end do
  write(*, '(i5)') nseq
  do i=1, nseq
    write(*, '(a80)') name(i)
    write(*, '(i5)') ncodon(i)
    write(*, '(60a1)') (amino(i, j), j=1, ncodon(i))
  end do
  stop
end program enshu1
