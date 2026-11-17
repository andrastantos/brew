/* Prints the sizes and field offsets of the newlib types the Brew simulator
   (sim/brew) marshals or might marshal across the syscall boundary, plus a
   few basic ABI facts.  Run on the simulator; diff the output between
   toolchain versions (brew/run-benchmarks.sh does that).  */
#include <stdio.h>
#include <stddef.h>
#include <time.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/times.h>
#include <utime.h>
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>

#define SZ(t) printf("sizeof(%s) = %u\n", #t, (unsigned)sizeof(t))
#define OFF(t, f) printf("  %s.%s @ %u (size %u)\n", #t, #f, \
    (unsigned)offsetof(t, f), (unsigned)sizeof(((t *)0)->f))
#define VAL(c) printf("%s = %ld\n", #c, (long)(c))

int main(void)
{
  SZ(char); SZ(short); SZ(int); SZ(long); SZ(long long); SZ(void *);
  SZ(float); SZ(double); SZ(long double); SZ(wchar_t); SZ(size_t);
  SZ(time_t); SZ(off_t); SZ(clock_t); SZ(ino_t); SZ(dev_t); SZ(mode_t);
  SZ(nlink_t); SZ(uid_t); SZ(gid_t); SZ(blksize_t); SZ(blkcnt_t);
  SZ(jmp_buf); SZ(fpos_t); SZ(FILE);
  SZ(struct timespec); OFF(struct timespec, tv_sec); OFF(struct timespec, tv_nsec);
  SZ(struct timeval); OFF(struct timeval, tv_sec); OFF(struct timeval, tv_usec);
  SZ(struct timezone);
  SZ(struct tms); OFF(struct tms, tms_utime); OFF(struct tms, tms_stime);
  OFF(struct tms, tms_cutime); OFF(struct tms, tms_cstime);
  SZ(struct utimbuf); OFF(struct utimbuf, actime); OFF(struct utimbuf, modtime);
  SZ(struct stat);
  OFF(struct stat, st_dev); OFF(struct stat, st_ino); OFF(struct stat, st_mode);
  OFF(struct stat, st_nlink); OFF(struct stat, st_uid); OFF(struct stat, st_gid);
  OFF(struct stat, st_rdev); OFF(struct stat, st_size); OFF(struct stat, st_blksize);
  OFF(struct stat, st_blocks); OFF(struct stat, st_atim); OFF(struct stat, st_mtim);
  OFF(struct stat, st_ctim);
  SZ(struct tm);
  VAL(CLOCKS_PER_SEC); VAL(EOVERFLOW); VAL(ENOENT); VAL(EINVAL);
  VAL(O_RDONLY); VAL(O_WRONLY); VAL(O_RDWR); VAL(O_CREAT); VAL(O_TRUNC);
  VAL(O_APPEND); VAL(O_EXCL);
  return 0;
}
