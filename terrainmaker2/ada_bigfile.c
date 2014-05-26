#include <stdio.h>
#include <stdint.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <errno.h>

int ada_bigfile_open(const char* filename)
{
	return open(filename, O_RDONLY);
}

void ada_bigfile_close(int fd)
{
	close(fd);
}

const char* ada_bigfile_error(void)
{
	return sys_errlist[errno];
}

off_t ada_bigfile_size(int fd)
{
	struct stat st;
	fstat(fd, &st);
	return st.st_size;
}

void* ada_bigfile_map(int fd)
{
	struct stat st;
	fstat(fd, &st);

	void* ptr = mmap(NULL, st.st_size, PROT_READ, MAP_SHARED,
		fd, 0);
	if (ptr == MAP_FAILED)
		ptr = NULL;
	return ptr;
}

void ada_bigfile_unmap(int fd, void* address)
{
	struct stat st;
	fstat(fd, &st);

	munmap(address, st.st_size);
}

